/**
 * サークルパッキング可視化モジュール
 * タグ訪問頻度を円グラフで表示
 */

import {
    createCategoricalColorScale,
    createGroup,
    createSVG,
    createTooltip,
    hideTooltip,
    showTooltip,
} from "./base";

// D3.jsのグローバル変数を宣言
declare const d3: any;

declare global {
    /**
     * サークルパッキングの設定
     */
    interface CirclePackingConfig extends SVGConfig {
        minRadius: number; // 最小円の半径
        maxRadius: number; // 最大円の半径
        padding: number; // 円間の余白
    }
}

/**
 * デフォルトのサークルパッキング設定
 */
const defaultCirclePackingConfig: CirclePackingConfig = {
    width: 600,
    height: 600,
    margin: { top: 20, right: 20, bottom: 20, left: 20 },
    minRadius: 20,
    maxRadius: 150,
    padding: 5,
};

/**
 * サークルパッキングクラス
 */
export class CirclePacking {
    private container: string;
    private config: CirclePackingConfig;
    private svg: any = null;
    private tooltip: any = null;
    private zoom: any = null;

    /**
     * コンストラクタ
     * @param container コンテナのセレクタ
     * @param config サークルパッキング設定
     */
    constructor(container: string, config: Partial<CirclePackingConfig> = {}) {
        this.container = container;
        this.config = { ...defaultCirclePackingConfig, ...config };
    }

    /**
     * サークルパッキングを描画する
     * @param data タグ統計データ
     */
    public render(data: TagStats): void {
        // データを変換
        const hierarchyData = this.transformData(data);

        // SVGとツールチップを作成
        this.svg = createSVG(this.container, this.config);
        // アクセシビリティのためSVGにARIA属性を付与
        this.svg
            .attr("role", "img")
            .attr(
                "aria-label",
                "タグ別体験記録のサークルパッキング図。各円のサイズが体験回数を表しています。円をクリックまたはEnterキーで詳細を確認できます。マウスホイールまたはピンチでズーム可能です。",
            )
            .attr("aria-live", "polite");
        this.tooltip = createTooltip(this.container);

        const g = createGroup(this.svg, this.config);

        // ズーム機能を設定
        this.zoom = d3
            .zoom<SVGSVGElement, unknown>()
            .scaleExtent([0.5, 5])
            .on("zoom", (event) => {
                g.attr("transform", event.transform);
            });

        this.svg.call(this.zoom);

        // パック関数を作成
        const pack = d3
            .pack<CircleNode>()
            .size([this.config.width, this.config.height])
            .padding(this.config.padding);

        // 階層データを作成
        const root = d3
            .hierarchy<CircleNode>(hierarchyData)
            .sum((d) => d.value || 0)
            .sort((a, b) => (b.value || 0) - (a.value || 0));

        // パックレイアウトを適用
        pack(root);

        // カラースケールを作成
        const colorScale = createCategoricalColorScale(data.tags.map((t) => t.tag));

        // ノードを描画
        const nodes = g
            .selectAll(".circle-node")
            .data(root.descendants().filter((d) => d.depth === 1))
            .enter()
            .append("g")
            .attr("class", "circle-node")
            .attr("transform", (d) => `translate(${d.x},${d.y})`);

        // 円を描画
        const circles = nodes
            .append("circle")
            .attr("r", (d) => d.r || 0)
            .attr("fill", (d) => colorScale(d.data.name))
            .attr("opacity", 0.7)
            .attr("stroke", "#fff")
            .attr("stroke-width", 2)
            .attr("tabindex", 0)
            .attr("aria-label", (d) => `${d.data.name} ${d.value ?? 0}件`);

        // ラベルを描画
        nodes
            .append("text")
            .attr("text-anchor", "middle")
            .attr("dominant-baseline", "middle")
            .attr("font-size", (d) => `${Math.min((d.r || 0) / 3, 18)}px`)
            .attr("font-weight", "bold")
            .attr("fill", "#fff")
            .attr("pointer-events", "none")
            .text((d) => d.data.name);

        // カウント数を描画
        nodes
            .append("text")
            .attr("text-anchor", "middle")
            .attr("dominant-baseline", "middle")
            .attr("dy", (d) => `${Math.min((d.r || 0) / 3, 18) + 5}px`)
            .attr("font-size", (d) => `${Math.min((d.r || 0) / 4, 14)}px`)
            .attr("fill", "#fff")
            .attr("pointer-events", "none")
            .text((d) => `${d.value}件`);

        // インタラクションを追加
        circles
            .on("mouseover", (event, d) => {
                d3.select(event.currentTarget).attr("opacity", 1).attr("stroke-width", 3);

                const content = `${d.data.name}<br/>体験記録: ${d.value}件`;
                if (this.tooltip) {
                    showTooltip(this.tooltip, content, event);
                }
            })
            .on("mousemove", (event, d) => {
                const content = `${d.data.name}<br/>体験記録: ${d.value}件`;
                if (this.tooltip) {
                    showTooltip(this.tooltip, content, event);
                }
            })
            .on("mouseout", (event) => {
                d3.select(event.currentTarget).attr("opacity", 0.7).attr("stroke-width", 2);

                if (this.tooltip) {
                    hideTooltip(this.tooltip);
                }
            })
            .on("click", (_event, d) => {
                // 1. タグフィルターUIを表示
                const toggleBtn = document.getElementById("toggle-tag-filter");
                const filterContainer = document.querySelector(".tag-filter") as HTMLElement;
                if (filterContainer?.style.display === "none" || !filterContainer?.style.display) {
                    toggleBtn?.click();
                }

                // 2. 既存の選択をクリア
                const clearBtn = document.getElementById("clear-selection");
                const selectedTags = document.querySelectorAll(".tag-filter-btn.active");
                if (selectedTags.length > 0) {
                    clearBtn?.click();
                }

                // 3. 対応するタグボタンをクリック（d.data.nameからタグ名を取得）
                const tagButton = document.querySelector(
                    `.tag-filter-btn[data-tag="${d.data.name}"]`,
                ) as HTMLElement;
                tagButton?.click();

                // 4. 体験録一覧タブに切り替え
                const listTab = document.querySelector(
                    '[aria-controls="panel-list"]',
                ) as HTMLElement;
                listTab?.click();
            })
            .on("focus", (event) => {
                d3.select(event.currentTarget).attr("opacity", 1).attr("stroke-width", 4);
            })
            .on("blur", (event) => {
                d3.select(event.currentTarget).attr("opacity", 0.7).attr("stroke-width", 2);

                if (this.tooltip) {
                    hideTooltip(this.tooltip);
                }
            })
            .on("keydown", (event, d) => {
                if (event.key !== "Enter" && event.key !== " " && event.key !== "Spacebar") {
                    return;
                }
                event.preventDefault();

                const target = event.currentTarget as SVGCircleElement;
                const { x, y, width, height } = target.getBoundingClientRect();
                const centerX = x + width / 2;
                const centerY = y + height / 2;
                const syntheticEvent = new MouseEvent("keydown", {
                    clientX: centerX,
                    clientY: centerY,
                    pageX: centerX + window.scrollX,
                    pageY: centerY + window.scrollY,
                });

                const content = `${d.data.name}<br/>体験記録: ${d.value}件`;
                if (this.tooltip) {
                    showTooltip(this.tooltip, content, syntheticEvent);
                }

                d3.select(target).attr("opacity", 1).attr("stroke-width", 4);
            });

        // ズームリセットボタンを追加
        this.addZoomResetButton();
    }

    /**
     * データを変換する
     * @param data タグ統計データ
     * @returns 階層データ
     */
    private transformData(data: TagStats): CircleNode {
        const children: CircleNode[] = data.tags.map((tag) => ({
            name: tag.tag,
            value: tag.count,
        }));

        return {
            name: "root",
            children,
        };
    }

    /**
     * ズームリセットボタンを追加する
     */
    private addZoomResetButton(): void {
        if (!this.svg) return;

        const button = d3
            .select(this.container)
            .append("button")
            .attr("class", "zoom-reset-button")
            .attr("aria-label", "ズームをリセット")
            .style("position", "absolute")
            .style("top", "10px")
            .style("right", "10px")
            .style("padding", "8px 16px")
            .style("background-color", "#007bff")
            .style("color", "#fff")
            .style("border", "none")
            .style("border-radius", "4px")
            .style("cursor", "pointer")
            .style("font-size", "14px")
            .style("z-index", "1000")
            .text("ズームリセット");

        button.on("click", () => {
            if (this.svg && this.zoom) {
                this.svg.transition().duration(750).call(this.zoom.transform, d3.zoomIdentity);
            }
        });

        button.on("mouseover", function () {
            d3.select(this).style("background-color", "#0056b3");
        });

        button.on("mouseout", function () {
            d3.select(this).style("background-color", "#007bff");
        });
    }

    /**
     * サークルパッキングをクリアする
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

        // ズームリセットボタンを削除
        d3.select(this.container).select(".zoom-reset-button").remove();
    }

    /**
     * ズーム機能を有効/無効にする
     * @param enabled 有効にする場合はtrue
     */
    public setZoomEnabled(enabled: boolean): void {
        if (this.svg && this.zoom) {
            if (enabled) {
                this.svg.call(this.zoom);
            } else {
                this.svg.on(".zoom", null);
            }
        }
    }
}
