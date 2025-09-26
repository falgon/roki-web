#!/bin/bash
# Docker manifest作成のための共通ユーティリティ関数
# 注意: このスクリプトは現在使用されていません (AMD64のみのビルドに移行したため)
# 将来的なマルチアーキテクチャサポート復活時の参考として保持

set -euo pipefail

# リトライ機能付きコマンド実行
# Usage: retry <command> [args...]
retry() {
    local attempts=0
    local max_attempts=3
    local delay=5

    while true; do
        if "$@"; then
            return 0
        fi
        attempts=$((attempts + 1))
        if (( attempts >= max_attempts )); then
            echo "::error::Command failed after ${attempts} attempts: $*" >&2
            return 1
        fi
        echo "Command failed, retrying in ${delay}s... (${attempts}/${max_attempts})" >&2
        sleep "${delay}"
        delay=$((delay * 2))
    done
}

# manifest listから特定アーキテクチャのdigestを解決
# Usage: resolve_manifest_digest <image_ref> <arch>
resolve_manifest_digest() {
    local ref="$1"
    local arch="$2"
    local tmp
    tmp=$(mktemp)

    if ! docker manifest inspect "$ref" >"$tmp" 2>/dev/null; then
        rm -f "$tmp"
        # マニフェストが存在しない場合は単純にイメージタグとして返す
        echo "$ref"
        return
    fi

    local digest
    digest=$(python3 - "$arch" "$tmp" <<'PYTHON_SCRIPT'
import json
import sys

arch = sys.argv[1]
path = sys.argv[2]

with open(path, 'r', encoding='utf-8') as fp:
    data = json.load(fp)

media_type = data.get('mediaType', '')
if media_type == 'application/vnd.docker.distribution.manifest.list.v2+json':
    # This is a manifest list - should not happen with provenance/sbom disabled
    for manifest in data.get('manifests', []):
        platform = manifest.get('platform', {})
        if platform.get('architecture') == arch:
            digest = manifest.get('digest')
            if digest:
                print(digest)
            break
elif media_type in ['application/vnd.docker.distribution.manifest.v2+json',
                    'application/vnd.oci.image.manifest.v1+json']:
    # This is a single image manifest - use as is
    pass
PYTHON_SCRIPT
)
    rm -f "$tmp"

    digest="${digest//$'\n'/}"  # 改行を削除
    if [[ -n "$digest" ]]; then
        # マニフェストリストから特定のdigestを抽出した
        echo "${ref%@*}@${digest}"
    else
        # 単一イメージまたはフォールバック
        echo "$ref"
    fi
}

# マルチアーキテクチャmanifestの作成とプッシュ
# Usage: create_and_push_manifest <target_image> <amd64_success> <arm64_success>
create_and_push_manifest() {
    local target_image="$1"
    local amd64_success="$2"
    local arm64_success="$3"

    # 成功したアーキテクチャを収集
    local archs=()
    [[ "$amd64_success" == "true" ]] && archs+=(amd64)
    [[ "$arm64_success" == "true" ]] && archs+=(arm64)

    if [ ${#archs[@]} -eq 0 ]; then
        echo "::warning::Skipping manifest for $target_image - no architectures built successfully."
        return 0
    fi

    # 各アーキテクチャの参照を解決（既存のマニフェストリストをクリーンアップ）
    local resolved_refs=()
    for arch in "${archs[@]}"; do
        local arch_ref="${target_image}-${arch}"

        # アーキテクチャ固有タグの既存マニフェストを削除（エラーは無視）
        if docker manifest inspect "$arch_ref" >/dev/null 2>&1; then
            echo "::warning::Removing existing manifest for $arch_ref (should be single image, not manifest list)..."
            docker manifest rm "$arch_ref" 2>/dev/null || true
        fi

        local resolved_ref
        resolved_ref=$(resolve_manifest_digest "$arch_ref" "$arch")
        echo "Resolved ${arch} image reference: ${resolved_ref}"
        resolved_refs+=("$resolved_ref")
    done

    # 既存のmanifestを削除
    if docker manifest inspect "$target_image" >/dev/null 2>&1; then
        echo "Removing existing manifest for $target_image..."
        docker manifest rm "$target_image" || echo "::warning::Failed to remove existing manifest" >&2
    fi

    # 新しいmanifestを作成してプッシュ
    echo "Creating manifest list for: $target_image"
    retry docker manifest create "$target_image" "${resolved_refs[@]}"

    echo "Pushing manifest list: $target_image"
    retry docker manifest push "$target_image"

    echo "::notice::Successfully created and pushed manifest for $target_image with ${#archs[@]} architecture(s)"
}

# ARM64ビルド用のキャッシュ設定を生成
# Usage: generate_arm64_cache_args <registry_base> <to_push>
generate_arm64_cache_args() {
    local registry_base="$1"
    local to_push="$2"
    local cache_args=""

    # GHAキャッシュスコープ
    local gha_scopes=(env-arm64 base-arm64 deps-arm64 build-arm64)

    for scope in "${gha_scopes[@]}"; do
        cache_args+=" --cache-from type=gha,scope=${scope}"
    done

    # レジストリキャッシュ
    if [[ -n "$registry_base" && "$to_push" == "true" ]]; then
        cache_args+=" --cache-from type=registry,ref=${registry_base}:cache-arm64"
        cache_args+=" --cache-from type=registry,ref=${registry_base}:latest-arm64"
    fi

    # キャッシュ出力設定
    for scope in "${gha_scopes[@]}"; do
        cache_args+=" --cache-to type=gha,mode=max,scope=${scope}"
    done

    if [[ -n "$registry_base" && "$to_push" == "true" ]]; then
        cache_args+=" --cache-to type=registry,mode=max,ref=${registry_base}:cache-arm64"
    fi

    echo "$cache_args"
}