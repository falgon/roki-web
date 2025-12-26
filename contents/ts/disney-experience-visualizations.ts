/**
 * Disney Experience可視化のメインエントリポイント
 * タイムラインヒートマップとサークルパッキングを初期化
 */

import { loadVisualizationData, showError } from "./visualizations/base";
import { CirclePacking } from "./visualizations/CirclePacking";
import { TimelineHeatmap } from "./visualizations/TimelineHeatmap";

declare global {
    interface VisualizationErrorInfo {
        type?: VisualizationErrorType;
        status?: number;
    }
}

const DEFAULT_ERROR_MESSAGE = "データの読み込みに失敗しました。ページをリロードしてください。";

// 可視化エラーかどうかを判定する
function isVisualizationError(error: unknown): error is VisualizationErrorInfo {
    if (typeof error !== "object" || error === null) {
        return false;
    }

    const typedError = error as Partial<VisualizationErrorInfo>;
    return typeof typedError.type === "string";
}

// エラータイプに応じてユーザー向けメッセージを返す
function resolveErrorMessage(error: unknown): string {
    if (isVisualizationError(error)) {
        switch (error.type) {
            case "network":
                return "ネットワーク接続を確認してください";
            case "timeout":
                return "通信がタイムアウトしました。再度お試しください";
            case "http":
                return `データの取得に失敗しました（エラーコード: ${error.status ?? "不明"}）`;
            case "parse":
                return "データの形式が不正です";
            default:
                return DEFAULT_ERROR_MESSAGE;
        }
    }

    if (error instanceof DOMException && error.name === "AbortError") {
        return "通信がタイムアウトしました。再度お試しください";
    }

    return DEFAULT_ERROR_MESSAGE;
}

// 空データ用のプレースホルダーを表示する
function showPlaceholder(container: string, message: string): void {
    const containerElement = document.querySelector(container);
    if (!containerElement) {
        return;
    }

    containerElement.innerHTML = "";
    const placeholder = document.createElement("div");
    placeholder.className = "visualization-placeholder";
    placeholder.style.padding = "20px";
    placeholder.style.textAlign = "center";
    placeholder.textContent = message;
    containerElement.appendChild(placeholder);
}

/**
 * 可視化の初期化
 */
async function initializeVisualizations(): Promise<void> {
    let data: VisualizationData;
    try {
        // データを読み込む
        data = await loadVisualizationData("/data/disney-experience-visualization.json");
    } catch (error) {
        console.error("可視化データの読み込みに失敗しました:", error);
        const message = resolveErrorMessage(error);
        showError("#timeline-heatmap", message);
        showError("#circle-packing", message);
        return;
    }

    try {
        // タイムラインヒートマップの設定
        const heatmapConfig = {
            width: 800,
            height: 150,
            margin: { top: 20, right: 20, bottom: 20, left: 60 },
            cellSize: 15,
            cellPadding: 2,
        };

        // yearlyTimeSeriesがある場合は年度選択付きで描画
        if (data.yearlyTimeSeries && data.yearlyTimeSeries.length > 0) {
            const heatmap = new TimelineHeatmap("#timeline-heatmap", heatmapConfig);
            heatmap.render(data.yearlyTimeSeries);
        } else if (data.timeSeries.daily.length === 0) {
            // データが空の場合
            showPlaceholder(
                "#timeline-heatmap",
                "データがありません。体験記録を追加してください。",
            );
        } else {
            // 後方互換性: yearlyTimeSeriesがない場合はtimeSeriesを使用
            const heatmap = new TimelineHeatmap("#timeline-heatmap", heatmapConfig);
            heatmap.render(data.timeSeries);
        }
    } catch (error) {
        console.error("タイムラインヒートマップの描画中にエラーが発生しました:", error);
        showError("#timeline-heatmap", resolveErrorMessage(error));
    }

    try {
        if (data.tagStats.tags.length === 0) {
            showPlaceholder("#circle-packing", "タグデータがありません。");
        } else {
            // サークルパッキングを初期化
            const circlePacking = new CirclePacking("#circle-packing", {
                width: 600,
                height: 600,
                margin: { top: 20, right: 20, bottom: 20, left: 20 },
            });
            circlePacking.render(data.tagStats);
        }
    } catch (error) {
        console.error("サークルパッキングの描画中にエラーが発生しました:", error);
        showError("#circle-packing", resolveErrorMessage(error));
    }
}

/**
 * DOMContentLoadedイベントで初期化
 */
if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", () => {
        initializeVisualizations();
    });
} else {
    // すでにDOMが読み込まれている場合
    initializeVisualizations();
}
