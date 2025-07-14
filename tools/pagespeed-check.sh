#!/bin/bash

# コマンドライン引数の解析
TARGET_URL="$1"
STRATEGY="${2:-mobile}"
LOCALE="${3:-ja_JP}"
THRESHOLD="${4:-90}"

# 引数チェック
if [ -z "$TARGET_URL" ]; then
    echo "Usage: $0 <url> [strategy] [locale] [threshold]"
    echo "Example: $0 roki.dev mobile ja_JP 65"
    exit 1
fi

# APIキーの確認
if [ -z "$PAGESPEED_API_KEY" ]; then
    echo "Error: PAGESPEED_API_KEY environment variable is required"
    echo "Please set your Google PageSpeed Insights API key"
    exit 1
fi

# URLの正規化（プロトコルがない場合はhttps://を追加）
if [[ ! "$TARGET_URL" =~ ^https?:// ]]; then
    TARGET_URL="https://$TARGET_URL"
fi

echo "Checking PageSpeed Insights for: $TARGET_URL"
echo "Strategy: $STRATEGY, Locale: $LOCALE, Threshold: $THRESHOLD"

# APIリクエストの実行
API_URL="https://www.googleapis.com/pagespeedonline/v5/runPagespeed"
PARAMS="url=$TARGET_URL&strategy=$STRATEGY&locale=$LOCALE&key=$PAGESPEED_API_KEY&category=performance"

echo "Making API request..."
echo "API URL: $API_URL?${PARAMS//$PAGESPEED_API_KEY/***API_KEY***}"

# curlでAPIを呼び出し
RESPONSE=$(curl -s -w "\n%{http_code}" "$API_URL?$PARAMS")
HTTP_CODE=$(echo "$RESPONSE" | tail -n1)
JSON_RESPONSE=$(echo "$RESPONSE" | sed '$d')

echo "HTTP Status Code: $HTTP_CODE"

if [ "$HTTP_CODE" != "200" ]; then
    echo "Error: HTTP $HTTP_CODE"
    echo "Response: $JSON_RESPONSE"
    exit 1
fi

# JSONレスポンスの解析
if command -v jq >/dev/null 2>&1; then
    # jqが利用可能な場合
    ERROR=$(echo "$JSON_RESPONSE" | jq -r '.error.message // empty')
    if [ -n "$ERROR" ]; then
        echo "API Error: $ERROR"
        exit 1
    fi
    
    PERFORMANCE_SCORE=$(echo "$JSON_RESPONSE" | jq -r '.lighthouseResult.categories.performance.score // empty')
    if [ -z "$PERFORMANCE_SCORE" ]; then
        echo "Error: Could not extract performance score from response"
        echo "Response: $JSON_RESPONSE"
        exit 1
    fi
    
    # スコアを100点満点に変換
    PERFORMANCE_SCORE_INT=$(echo "$PERFORMANCE_SCORE * 100" | bc -l | cut -d. -f1)
    
    # その他のメトリクス
    FCP=$(echo "$JSON_RESPONSE" | jq -r '.lighthouseResult.audits["first-contentful-paint"].numericValue // empty')
    LCP=$(echo "$JSON_RESPONSE" | jq -r '.lighthouseResult.audits["largest-contentful-paint"].numericValue // empty')
    FID=$(echo "$JSON_RESPONSE" | jq -r '.lighthouseResult.audits["max-potential-fid"].numericValue // empty')
    CLS=$(echo "$JSON_RESPONSE" | jq -r '.lighthouseResult.audits["cumulative-layout-shift"].numericValue // empty')
    
else
    # jqが利用できない場合、grepとsedで簡易的に解析
    ERROR=$(echo "$JSON_RESPONSE" | grep -o '"message":"[^"]*"' | sed 's/"message":"//;s/"//')
    if [ -n "$ERROR" ]; then
        echo "API Error: $ERROR"
        exit 1
    fi
    
    PERFORMANCE_SCORE=$(echo "$JSON_RESPONSE" | grep -o '"score":[0-9.]*' | sed 's/"score"://')
    if [ -z "$PERFORMANCE_SCORE" ]; then
        echo "Error: Could not extract performance score from response"
        echo "Response: $JSON_RESPONSE"
        exit 1
    fi
    
    # スコアを100点満点に変換（簡易計算）
    PERFORMANCE_SCORE_INT=$(echo "$PERFORMANCE_SCORE * 100" | bc -l | cut -d. -f1)
fi

echo ""
echo "=== PageSpeed Insights Results ==="
echo "Performance Score: ${PERFORMANCE_SCORE_INT}/100"

if [ -n "$FCP" ]; then
    FCP_SEC=$(echo "$FCP / 1000" | bc -l | cut -c1-4)
    echo "First Contentful Paint: ${FCP_SEC}s"
fi

if [ -n "$LCP" ]; then
    LCP_SEC=$(echo "$LCP / 1000" | bc -l | cut -c1-4)
    echo "Largest Contentful Paint: ${LCP_SEC}s"
fi

if [ -n "$FID" ]; then
    FID_SEC=$(echo "$FID / 1000" | bc -l | cut -c1-4)
    echo "First Input Delay: ${FID_SEC}s"
fi

if [ -n "$CLS" ]; then
    echo "Cumulative Layout Shift: $CLS"
fi

# 閾値チェック
if [ "$PERFORMANCE_SCORE_INT" -ge "$THRESHOLD" ]; then
    echo "✅ Performance score ($PERFORMANCE_SCORE_INT) meets threshold ($THRESHOLD)"
    exit 0
else
    echo "❌ Performance score ($PERFORMANCE_SCORE_INT) below threshold ($THRESHOLD)"
    exit 1
fi 