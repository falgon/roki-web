/**
 * タイムラインヒートマップ可視化モジュール
 * GitHub Contributions風のヒートマップを作成
 */

import type { ScaleLinear } from "d3";
import { createGroup, createSVG, createTooltip, hideTooltip, showTooltip } from "./base";

// D3.jsのグローバル変数を宣言
declare const d3: typeof import("d3");

declare global {
    /**
     * ヒートマップの設定
     */
    interface HeatmapConfig extends SVGConfig {
        cellSize: number; // セルのサイズ
        cellPadding: number; // セル間の余白
        colorRange: [string, string]; // カラーレンジ [最小値の色, 最大値の色]
    }
}

/**
 * デフォルトのヒートマップ設定
 */
const defaultHeatmapConfig: HeatmapConfig = {
    width: 800,
    height: 150,
    margin: { top: 20, right: 20, bottom: 20, left: 60 },
    cellSize: 15,
    cellPadding: 2,
    colorRange: ["#ebedf0", "#216e39"],
};

/**
 * 年度別データの型定義
 * グローバル型YearlyTimeSeriesDataを利用
 */
type YearData = YearlyTimeSeriesData;

/**
 * 日別カウントから表示する週全体のセルを生成する。入力データは変更しない。
 */
export function createHeatmapCells(dailyData: readonly DailyCount[], year?: number): HeatmapCell[] {
    const sortedData = [...dailyData].sort(
        (a, b) => new Date(a.date).getTime() - new Date(b.date).getTime(),
    );

    let startDate: Date;
    let endDate: Date;

    if (year !== undefined) {
        // 年度全体と、その両端を含む週を表示する
        startDate = new Date(year, 0, 1);
        endDate = new Date(year, 11, 31);
    } else {
        const first = sortedData[0];
        const last = sortedData[sortedData.length - 1];
        if (!first || !last) {
            return [];
        }
        startDate = new Date(first.date);
        endDate = new Date(last.date);
    }

    startDate.setDate(startDate.getDate() - startDate.getDay());
    endDate.setDate(endDate.getDate() + (6 - endDate.getDay()));

    // 同じ日付のカウントは合算する
    const dataMap = new Map<string, number>();
    for (const item of sortedData) {
        const existingCount = dataMap.get(item.date) || 0;
        dataMap.set(item.date, existingCount + item.count);
    }

    const heatmapData: HeatmapCell[] = [];
    let weekIndex = 0;
    const currentDate = new Date(startDate);

    while (currentDate <= endDate) {
        const dateStr = d3.timeFormat("%Y-%m-%d")(currentDate);
        heatmapData.push({
            date: new Date(currentDate),
            count: dataMap.get(dateStr) || 0,
            weekday: currentDate.getDay(),
            weekIndex,
        });

        currentDate.setDate(currentDate.getDate() + 1);
        if (currentDate.getDay() === 0) {
            weekIndex++;
        }
    }

    return heatmapData;
}

/**
 * タイムラインヒートマップクラス
 */
export class TimelineHeatmap {
    private container: string;
    private config: HeatmapConfig;
    private svg: ReturnType<typeof createSVG> | null = null;
    private tooltip: ReturnType<typeof createTooltip> | null = null;
    private allYearData: YearData[] = [];
    private yearSelectorHandler: ((event: Event) => void) | null = null;

    /**
     * コンストラクタ
     * @param container コンテナのセレクタ
     * @param config ヒートマップ設定
     */
    constructor(container: string, config: Partial<HeatmapConfig> = {}) {
        this.container = container;
        this.config = { ...defaultHeatmapConfig, ...config };
    }

    /**
     * ヒートマップを描画する
     * @param data 時系列データまたは年度別データ配列
     */
    public render(data: TimeSeriesData | YearData[]): void {
        // 年度別データ配列の場合
        if (Array.isArray(data)) {
            this.allYearData = data;
            if (data.length === 0) {
                return;
            }

            // 年度選択UIをセットアップ
            this.setupYearSelector();

            // 最新年度のデータで描画
            const latestYear = Math.max(...data.map((d) => d.year));
            this.renderForYear(latestYear);
            return;
        }

        // 単一データの場合（既存の動作を維持）
        this.renderSingleData(data);
    }

    /**
     * 単一データでヒートマップを描画する（既存の動作）
     * @param data 時系列データ
     * @param year 年度（指定された場合は年度全体を表示）
     */
    private renderSingleData(data: TimeSeriesData, year?: number): void {
        // データを変換
        const heatmapData = createHeatmapCells(data.daily, year);

        if (heatmapData.length > 0) {
            const maxWeekIndex = d3.max(heatmapData, (d) => d.weekIndex) || 0;
            const requiredWidth =
                (maxWeekIndex + 1) * (this.config.cellSize + this.config.cellPadding);
            const width = Math.max(this.config.width, requiredWidth);
            this.config = { ...this.config, width };
        }

        // SVGとツールチップを作成
        this.svg = createSVG(this.container, this.config);
        this.tooltip = createTooltip(this.container);

        // SVGへARIA属性を付与し、スクリーンリーダー対応を強化
        this.svg
            .attr("role", "img")
            .attr(
                "aria-label",
                "体験記録のタイムラインヒートマップ。日別の体験回数を色の濃淡で表示しています。各セルをクリックまたはEnterキーで詳細を確認できます。",
            )
            .attr("aria-live", "polite");

        const g = createGroup(this.svg, this.config);

        // カラースケールを作成
        const maxCount = d3.max(heatmapData, (d) => d.count) || 1;
        const colorScale = d3
            .scaleLinear<string>()
            .domain([0, maxCount])
            .range(this.config.colorRange);

        // ストローク幅を段階的に変化させ、色以外の手がかりを提供
        const strokeWidthScale = d3
            .scaleQuantize<number>()
            .domain([0, maxCount])
            .range([1, 2, 3, 4]);

        const formatDateLabel = d3.timeFormat("%Y年%-m月%-d日");
        const formatTooltipContent = (cell: HeatmapCell): string => {
            const formattedDate = formatDateLabel(cell.date);
            return `${formattedDate}<br/>体験記録: ${cell.count}件`;
        };
        const getStrokeWidth = (count: number): number => strokeWidthScale(count);

        const weekdays = ["日", "月", "火", "水", "木", "金", "土"];

        // 曜日ラベルを描画
        g.selectAll(".weekday-label")
            .data(weekdays)
            .enter()
            .append("text")
            .attr("class", "weekday-label")
            .attr("x", -10)
            .attr("y", (_d, i) => i * (this.config.cellSize + this.config.cellPadding) + 12)
            .attr("text-anchor", "end")
            .attr("font-size", "10px")
            .attr("fill", "#666")
            .text((d) => d);

        // ヒートマップセルを描画
        const cells = g
            .selectAll(".heatmap-cell")
            .data(heatmapData)
            .enter()
            .append("rect")
            .attr("class", "heatmap-cell")
            .attr("x", (d) => d.weekIndex * (this.config.cellSize + this.config.cellPadding))
            .attr("y", (d) => d.weekday * (this.config.cellSize + this.config.cellPadding))
            .attr("width", this.config.cellSize)
            .attr("height", this.config.cellSize)
            .attr("rx", 2)
            .attr("ry", 2)
            .attr("fill", (d) => (d.count === 0 ? "#ebedf0" : colorScale(d.count)))
            .attr("stroke", "#fff")
            .attr("stroke-width", (d) => getStrokeWidth(d.count))
            .attr("tabindex", 0)
            .attr("aria-label", (d) => `${formatDateLabel(d.date)} 体験記録${d.count}件`);

        // インタラクションを追加
        cells
            .on("click", (_event: MouseEvent, d) => {
                const dateStr = d3.timeFormat("%Y-%m-%d")(d.date);
                const customEvent = new CustomEvent("heatmap-cell-click", {
                    detail: { date: dateStr },
                    bubbles: true,
                });
                document.dispatchEvent(customEvent);
            })
            .on("mouseover", (event: MouseEvent, d) => {
                if (!(event.currentTarget instanceof SVGElement)) {
                    return;
                }
                const baseStrokeWidth = getStrokeWidth(d.count);
                d3.select(event.currentTarget)
                    .attr("stroke", "#000")
                    .attr("stroke-width", Math.max(baseStrokeWidth, 2));

                const content = formatTooltipContent(d);
                if (this.tooltip) {
                    showTooltip(this.tooltip, content, event);
                }
            })
            .on("mousemove", (event: MouseEvent, d) => {
                const content = formatTooltipContent(d);
                if (this.tooltip) {
                    showTooltip(this.tooltip, content, event);
                }
            })
            .on("mouseout", (event: MouseEvent, d) => {
                if (!(event.currentTarget instanceof SVGElement)) {
                    return;
                }
                const baseStrokeWidth = getStrokeWidth(d.count);
                d3.select(event.currentTarget)
                    .attr("stroke", "#fff")
                    .attr("stroke-width", baseStrokeWidth);

                if (this.tooltip) {
                    hideTooltip(this.tooltip);
                }
            })
            .on("focus", (event: FocusEvent, d) => {
                if (!(event.currentTarget instanceof SVGElement)) {
                    return;
                }
                const baseStrokeWidth = getStrokeWidth(d.count);
                d3.select(event.currentTarget)
                    .attr("stroke", "#000")
                    // フォーカス時は視覚的な枠を強調しつつ、元の段階的な太さを下回らないようにする
                    .attr("stroke-width", Math.max(baseStrokeWidth, 3));
            })
            .on("blur", (event: FocusEvent, d) => {
                if (!(event.currentTarget instanceof SVGElement)) {
                    return;
                }
                const baseStrokeWidth = getStrokeWidth(d.count);
                d3.select(event.currentTarget)
                    .attr("stroke", "#fff")
                    .attr("stroke-width", baseStrokeWidth);

                if (this.tooltip) {
                    hideTooltip(this.tooltip);
                }
            })
            .on("keydown", (event: KeyboardEvent, d) => {
                if (event.key === "Enter" || event.key === " " || event.key === "Spacebar") {
                    event.preventDefault();

                    const target = event.currentTarget;
                    if (!(target instanceof SVGElement)) {
                        return;
                    }
                    const rect = target.getBoundingClientRect();
                    // キーボード操作時もマウスイベントに近い位置でツールチップを表示する
                    const syntheticEvent = {
                        pageX: window.scrollX + rect.x + rect.width / 2,
                        pageY: window.scrollY + rect.y + rect.height / 2,
                    };

                    const content = formatTooltipContent(d);
                    if (this.tooltip) {
                        showTooltip(this.tooltip, content, syntheticEvent);
                    }
                }
            });

        // 凡例を追加
        this.addLegend(g, colorScale, maxCount);
    }

    /**
     * 凡例を追加する
     * @param g グループセレクション
     * @param colorScale カラースケール
     * @param maxCount 最大カウント
     */
    private addLegend(
        g: ReturnType<typeof createGroup>,
        colorScale: ScaleLinear<string, string>,
        maxCount: number,
    ): void {
        const legendData = [
            { label: "少", value: 0 },
            { label: "", value: maxCount * 0.25 },
            { label: "", value: maxCount * 0.5 },
            { label: "", value: maxCount * 0.75 },
            { label: "多", value: maxCount },
        ];

        const legend = g
            .append("g")
            .attr("class", "legend")
            .attr(
                "transform",
                `translate(0, ${7 * (this.config.cellSize + this.config.cellPadding) + 10})`,
            );

        legend
            .append("text")
            .attr("x", 0)
            .attr("y", 0)
            .attr("font-size", "10px")
            .attr("fill", "#666")
            .text("体験頻度:");

        const legendItems = legend
            .selectAll(".legend-item")
            .data(legendData)
            .enter()
            .append("g")
            .attr("class", "legend-item")
            .attr(
                "transform",
                (_d, i) =>
                    `translate(${70 + i * (this.config.cellSize + this.config.cellPadding + 5)}, -10)`,
            );

        legendItems
            .append("rect")
            .attr("width", this.config.cellSize)
            .attr("height", this.config.cellSize)
            .attr("rx", 2)
            .attr("ry", 2)
            .attr("fill", (d) => (d.value === 0 ? "#ebedf0" : colorScale(d.value)));

        legendItems
            .append("text")
            .attr("x", this.config.cellSize / 2)
            .attr("y", this.config.cellSize + 12)
            .attr("text-anchor", "middle")
            .attr("font-size", "9px")
            .attr("fill", "#666")
            .text((d) => d.label);
    }

    /**
     * 年度選択UIをセットアップする
     */
    private setupYearSelector(): void {
        const selector = document.getElementById("timeline-year-select") as HTMLSelectElement;
        if (!selector) {
            return;
        }

        // 既存のイベントリスナーを削除（メモリリーク対策）
        if (this.yearSelectorHandler) {
            selector.removeEventListener("change", this.yearSelectorHandler);
        }

        // セレクタをクリア
        selector.innerHTML = "";

        // 年度を降順でソート
        const years = this.allYearData.map((d) => d.year).sort((a, b) => b - a);

        // オプションを追加
        years.forEach((year) => {
            const option = document.createElement("option");
            option.value = year.toString();
            option.textContent = `${year}年`;
            selector.appendChild(option);
        });

        // 新しいイベントリスナーを設定
        this.yearSelectorHandler = (event: Event) => {
            const selectedYear = parseInt((event.target as HTMLSelectElement).value, 10);
            this.renderForYear(selectedYear);
        };
        selector.addEventListener("change", this.yearSelectorHandler);
    }

    /**
     * 指定された年度のヒートマップを描画する
     * @param year 年度
     */
    private renderForYear(year: number): void {
        // 既存のSVGをクリア
        if (this.svg) {
            this.svg.remove();
            this.svg = null;
        }

        // 指定された年度のデータを取得
        const yearData = this.allYearData.find((d) => d.year === year);
        if (!yearData) {
            return;
        }

        // セレクタの値を更新
        const selector = document.getElementById("timeline-year-select") as HTMLSelectElement;
        if (selector) {
            selector.value = year.toString();
        }

        // データを描画（年度を指定して全日表示）
        this.renderSingleData({ daily: yearData.daily }, year);
    }

    /**
     * ヒートマップをクリアする
     */
    public clear(): void {
        if (this.svg) {
            this.svg.remove();
            this.svg = null;
        }
        if (this.tooltip) {
            this.tooltip.remove();
            this.tooltip = null;
        }
    }
}
