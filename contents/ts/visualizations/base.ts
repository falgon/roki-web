/**
 * D3.js可視化の基盤モジュール
 * 共通ユーティリティ関数とSVG要素の作成を提供
 */

// D3.jsのグローバル変数を宣言
declare const d3: any;

declare global {
    type VisualizationErrorType = "network" | "timeout" | "http" | "parse";

    interface VisualizationError extends Error {
        type: VisualizationErrorType;
        status?: number;
        detail?: string;
    }
}

const RETRY_DELAYS_MS = [1000, 2000, 4000];
const MAX_RETRY_COUNT = 3;
const DEFAULT_TIMEOUT_MS = 10000;

/**
 * 指数バックオフ付きで指定時間内にフェッチを試みる
 * ネットワークエラーまたは5xx系エラーのみリトライ対象
 */
async function fetchWithRetry(
    url: string,
    options: RequestInit = {},
    timeoutMs = DEFAULT_TIMEOUT_MS,
): Promise<Response> {
    let lastError: VisualizationError | undefined;

    for (let attempt = 0; attempt <= MAX_RETRY_COUNT; attempt += 1) {
        const controller = new AbortController();
        const timeoutId = window.setTimeout(() => controller.abort(), timeoutMs);

        try {
            const response = await fetch(url, { ...options, signal: controller.signal });
            window.clearTimeout(timeoutId);

            if (response.ok) {
                return response;
            }

            const isServerError = response.status >= 500 && response.status < 600;
            if (isServerError && attempt < MAX_RETRY_COUNT) {
                lastError = createHttpError(response);
                await wait(RETRY_DELAYS_MS[Math.min(attempt, RETRY_DELAYS_MS.length - 1)]);
                continue;
            }

            throw createHttpError(response);
        } catch (error) {
            window.clearTimeout(timeoutId);

            if (error instanceof DOMException && error.name === "AbortError") {
                lastError = createTypedError(
                    "データの取得がタイムアウトしました",
                    "timeout",
                    error.message,
                );
                break;
            }

            if (error instanceof TypeError) {
                lastError = createTypedError(
                    "データの取得に失敗しました",
                    "network",
                    error.message,
                );
                if (attempt < MAX_RETRY_COUNT) {
                    await wait(RETRY_DELAYS_MS[Math.min(attempt, RETRY_DELAYS_MS.length - 1)]);
                    continue;
                }
                break;
            }

            lastError = createTypedError(
                "データの取得に失敗しました",
                "network",
                error instanceof Error ? error.message : String(error),
            );
            break;
        }
    }

    throw lastError ?? createTypedError("データの取得に失敗しました", "network");
}

/**
 * ユーザーフレンドリーなメッセージ付きのHTTPエラーを生成
 */
function createHttpError(response: Response): VisualizationError {
    const message = mapHttpErrorMessage(response.status);
    const detail = `status: ${response.status}, statusText: ${response.statusText}`;
    return createTypedError(message, "http", detail, response.status);
}

/**
 * エラーメッセージをHTTPステータスごとにマッピング
 */
function mapHttpErrorMessage(status: number): string {
    if (status === 404) {
        return "データファイルが見つかりません";
    }
    if (status === 403) {
        return "データへのアクセスが拒否されました";
    }
    if (status === 500 || status === 502 || status === 503) {
        return "サーバーエラーが発生しました";
    }
    return "データの取得に失敗しました";
}

/**
 * 型付きエラーを生成し、技術情報を detail に格納
 */
function createTypedError(
    message: string,
    type: VisualizationErrorType,
    detail?: string,
    status?: number,
): VisualizationError {
    const error = new Error(message) as VisualizationError;
    error.type = type;
    if (typeof status === "number") {
        error.status = status;
    }
    if (detail) {
        error.detail = detail;
    }
    return error;
}

/**
 * 指定ミリ秒待機するユーティリティ
 */
function wait(ms: number): Promise<void> {
    return new Promise((resolve) => {
        window.setTimeout(resolve, ms);
    });
}

declare global {
    /**
     * SVG要素の基本設定
     */
    interface SVGConfig {
        width: number; // SVGの幅
        height: number; // SVGの高さ
        margin: {
            top: number;
            right: number;
            bottom: number;
            left: number;
        };
    }
}

/**
 * デフォルトのSVG設定
 */
export const defaultSVGConfig: SVGConfig = {
    width: 800,
    height: 600,
    margin: { top: 20, right: 20, bottom: 30, left: 40 },
};

/**
 * JSONデータを読み込む
 * @param url JSONファイルのURL
 * @returns 可視化データのPromise
 */
export async function loadVisualizationData(url: string): Promise<VisualizationData> {
    try {
        const response = await fetchWithRetry(url);
        try {
            const data: VisualizationData = await response.json();
            return data;
        } catch (parseError) {
            throw createTypedError(
                "データの形式が不正です",
                "parse",
                parseError instanceof Error ? parseError.message : String(parseError),
            );
        }
    } catch (error) {
        console.error("可視化データの読み込みに失敗しました:", error);
        throw error;
    }
}

/**
 * SVG要素を作成し、指定されたコンテナに追加する
 * @param container コンテナのセレクタ
 * @param config SVG設定
 * @returns D3セレクション
 */
export function createSVG(container: string, config: SVGConfig = defaultSVGConfig): any {
    // 既存のSVGを削除
    d3.select(container).select("svg").remove();

    // 新しいSVGを作成
    const svg = d3
        .select(container)
        .append("svg")
        .attr("width", config.width + config.margin.left + config.margin.right)
        .attr("height", config.height + config.margin.top + config.margin.bottom)
        .attr("class", "visualization-svg");

    return svg;
}

/**
 * SVG内にグループ要素を作成し、マージンを適用する
 * @param svg SVGセレクション
 * @param config SVG設定
 * @returns グループセレクション
 */
export function createGroup(svg: any, config: SVGConfig = defaultSVGConfig): any {
    return svg
        .append("g")
        .attr("transform", `translate(${config.margin.left}, ${config.margin.top})`);
}

/**
 * ツールチップ要素を作成する
 * @param container コンテナのセレクタ
 * @returns ツールチップのD3セレクション
 */
export function createTooltip(container: string): any {
    // 既存のツールチップを削除
    d3.select(container).select(".visualization-tooltip").remove();

    // 新しいツールチップを作成
    return d3
        .select(container)
        .append("div")
        .attr("class", "visualization-tooltip")
        .style("position", "absolute")
        .style("visibility", "hidden")
        .style("background-color", "rgba(0, 0, 0, 0.8)")
        .style("color", "#fff")
        .style("padding", "8px 12px")
        .style("border-radius", "4px")
        .style("font-size", "12px")
        .style("pointer-events", "none")
        .style("z-index", "1000");
}

/**
 * ツールチップを表示する
 * @param tooltip ツールチップのD3セレクション
 * @param content 表示するコンテンツ
 * @param event マウスイベント
 */
export function showTooltip(tooltip: any, content: string, event: MouseEvent): void {
    tooltip
        .html(content)
        .style("visibility", "visible")
        .style("left", `${event.pageX + 10}px`)
        .style("top", `${event.pageY - 20}px`);
}

/**
 * ツールチップを非表示にする
 * @param tooltip ツールチップのD3セレクション
 */
export function hideTooltip(tooltip: any): void {
    tooltip.style("visibility", "hidden");
}

/**
 * レスポンシブなSVGサイズを計算する
 * @param containerSelector コンテナのセレクタ
 * @param aspectRatio アスペクト比 (高さ / 幅)
 * @returns SVG設定
 */
export function calculateResponsiveSize(containerSelector: string, aspectRatio = 0.75): SVGConfig {
    const container = document.querySelector(containerSelector);
    if (!container) {
        return defaultSVGConfig;
    }

    const containerWidth = container.clientWidth;
    const width = Math.max(300, Math.min(containerWidth - 40, 1200));
    const height = width * aspectRatio;

    return {
        width,
        height,
        margin: {
            top: 20,
            right: 20,
            bottom: 30,
            left: 40,
        },
    };
}

/**
 * 色スケールを作成する（カテゴリカルカラー）
 * @param domain ドメイン配列
 * @returns D3カラースケール
 */
export function createCategoricalColorScale(domain: string[]): any {
    return d3.scaleOrdinal<string>().domain(domain).range(d3.schemeCategory10);
}

/**
 * 色スケールを作成する（連続カラー）
 * @param domain ドメイン配列 [min, max]
 * @param range カラーレンジ
 * @returns D3カラースケール
 */
export function createSequentialColorScale(
    domain: [number, number],
    range: [string, string] = ["#f0f0f0", "#d62728"],
): any {
    return d3.scaleLinear<string>().domain(domain).range(range);
}

/**
 * エラーメッセージを表示する
 * @param container コンテナのセレクタ
 * @param message エラーメッセージ
 */
export function showError(container: string, message: string): void {
    d3.select(container)
        .append("div")
        .attr("class", "visualization-error")
        .attr("role", "alert")
        .style("color", "red")
        .style("padding", "20px")
        .style("text-align", "center")
        .text(message);
}
