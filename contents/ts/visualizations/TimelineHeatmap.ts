/**
 * タイムラインヒートマップ可視化モジュール
 * GitHub Contributions風のヒートマップを作成
 */

import { createGroup, createSVG, createTooltip, hideTooltip, showTooltip } from "./base";

// D3.jsのグローバル変数を宣言
declare const d3: any;

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
 * タイムラインヒートマップクラス
 */
export class TimelineHeatmap {
    private container: string;
    private config: HeatmapConfig;
    private svg: any = null;
    private tooltip: any = null;
    private allYearData: YearData[] = [];
    private currentYear: number = 0;
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
     */
    private renderSingleData(data: TimeSeriesData): void {
        // データを変換
        const heatmapData = this.transformData(data.daily);

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

        // 週のラベルを取得
        const _weeks = Array.from(new Set(heatmapData.map((d) => d.weekIndex)));
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
            .on("mouseover", (event, d) => {
                const baseStrokeWidth = getStrokeWidth(d.count);
                d3.select(event.currentTarget)
                    .attr("stroke", "#000")
                    .attr("stroke-width", Math.max(baseStrokeWidth, 2));

                const content = formatTooltipContent(d);
                if (this.tooltip) {
                    showTooltip(this.tooltip, content, event);
                }
            })
            .on("mousemove", (event, d) => {
                const content = formatTooltipContent(d);
                if (this.tooltip) {
                    showTooltip(this.tooltip, content, event);
                }
            })
            .on("mouseout", (event, d) => {
                const baseStrokeWidth = getStrokeWidth(d.count);
                d3.select(event.currentTarget)
                    .attr("stroke", "#fff")
                    .attr("stroke-width", baseStrokeWidth);

                if (this.tooltip) {
                    hideTooltip(this.tooltip);
                }
            })
            .on("focus", (event, d) => {
                const baseStrokeWidth = getStrokeWidth(d.count);
                d3.select(event.currentTarget)
                    .attr("stroke", "#000")
                    // フォーカス時は視覚的な枠を強調しつつ、元の段階的な太さを下回らないようにする
                    .attr("stroke-width", Math.max(baseStrokeWidth, 3));
            })
            .on("blur", (event, d) => {
                const baseStrokeWidth = getStrokeWidth(d.count);
                d3.select(event.currentTarget)
                    .attr("stroke", "#fff")
                    .attr("stroke-width", baseStrokeWidth);

                if (this.tooltip) {
                    hideTooltip(this.tooltip);
                }
            })
            .on("keydown", (event, d) => {
                if (event.key === "Enter" || event.key === " " || event.key === "Spacebar") {
                    event.preventDefault();

                    const target = event.currentTarget as SVGRectElement;
                    const rect = target.getBoundingClientRect();
                    // キーボード操作時もマウスイベントに近い位置でツールチップを表示する
                    const syntheticEvent = {
                        pageX: window.scrollX + rect.x + rect.width / 2,
                        pageY: window.scrollY + rect.y + rect.height / 2,
                    } as MouseEvent;

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
     * データを変換する
     * @param dailyData 日別カウントデータ
     * @returns ヒートマップセルデータ
     */
    private transformData(dailyData: DailyCount[]): HeatmapCell[] {
        // 日付でソート
        const sortedData = dailyData.sort(
            (a, b) => new Date(a.date).getTime() - new Date(b.date).getTime(),
        );

        if (sortedData.length === 0) {
            return [];
        }

        // 最初の日付と最後の日付を取得
        const firstDate = new Date(sortedData[0].date);
        const lastDate = new Date(sortedData[sortedData.length - 1].date);

        // 最初の日曜日を計算
        const startDate = new Date(firstDate);
        startDate.setDate(startDate.getDate() - startDate.getDay());

        // 最後の土曜日を計算
        const endDate = new Date(lastDate);
        endDate.setDate(endDate.getDate() + (6 - endDate.getDay()));

        // データをMapに変換
        const dataMap = new Map<string, number>();
        for (const item of sortedData) {
            dataMap.set(item.date, item.count);
        }

        // ヒートマップデータを生成
        const heatmapData: HeatmapCell[] = [];
        let weekIndex = 0;
        const currentDate = new Date(startDate);

        while (currentDate <= endDate) {
            const dateStr = d3.timeFormat("%Y-%m-%d")(currentDate);
            const count = dataMap.get(dateStr) || 0;
            const weekday = currentDate.getDay();

            heatmapData.push({
                date: new Date(currentDate),
                count,
                weekday,
                weekIndex,
            });

            // 次の日に進む
            currentDate.setDate(currentDate.getDate() + 1);

            // 日曜日になったら週のインデックスを増やす
            if (currentDate.getDay() === 0) {
                weekIndex++;
            }
        }

        return heatmapData;
    }

    /**
     * 凡例を追加する
     * @param g グループセレクション
     * @param colorScale カラースケール
     * @param maxCount 最大カウント
     */
    private addLegend(g: any, colorScale: any, maxCount: number): void {
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

        // 現在の年度を保存
        this.currentYear = year;

        // セレクタの値を更新
        const selector = document.getElementById("timeline-year-select") as HTMLSelectElement;
        if (selector) {
            selector.value = year.toString();
        }

        // データを描画
        this.renderSingleData({ daily: yearData.daily });
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
