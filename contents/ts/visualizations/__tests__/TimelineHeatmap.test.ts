/**
 * タイムラインヒートマップのユニットテスト
 */

import { beforeEach, describe, expect, it } from "vitest";
import type { TimeSeriesData } from "../../types/disney-experience";
import { TimelineHeatmap } from "../TimelineHeatmap";

describe("TimelineHeatmap", () => {
    let container: HTMLElement;

    beforeEach(() => {
        // DOMをクリーンアップ
        document.body.innerHTML = "";
        // テスト用のコンテナを作成
        container = document.createElement("div");
        container.id = "test-timeline-heatmap";
        document.body.appendChild(container);
    });

    describe("constructor", () => {
        it("デフォルト設定でインスタンスを作成できる", () => {
            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");
            expect(heatmap).toBeDefined();
        });

        it("カスタム設定でインスタンスを作成できる", () => {
            const heatmap = new TimelineHeatmap("#test-timeline-heatmap", {
                width: 1000,
                height: 200,
                cellSize: 20,
            });
            expect(heatmap).toBeDefined();
        });
    });

    describe("render", () => {
        it("正常なデータで描画できる", () => {
            const data: TimeSeriesData = {
                daily: [
                    { date: "2024-01-01", count: 2 },
                    { date: "2024-01-02", count: 1 },
                    { date: "2024-01-03", count: 3 },
                ],
            };

            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");
            heatmap.render(data);

            // SVGが作成されたことを確認
            const svg = container.querySelector("svg");
            expect(svg).toBeDefined();
        });

        it("空のデータで描画できる", () => {
            const data: TimeSeriesData = {
                daily: [],
            };

            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");
            heatmap.render(data);

            // SVGが作成されたことを確認
            const svg = container.querySelector("svg");
            expect(svg).toBeDefined();
        });

        it("ツールチップが作成される", () => {
            const data: TimeSeriesData = {
                daily: [{ date: "2024-01-01", count: 1 }],
            };

            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");
            heatmap.render(data);

            // ツールチップが作成されたことを確認
            const tooltip = container.querySelector(".visualization-tooltip");
            expect(tooltip).toBeDefined();
        });

        it("曜日ラベルが描画される", () => {
            const data: TimeSeriesData = {
                daily: [{ date: "2024-01-01", count: 1 }],
            };

            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");
            heatmap.render(data);

            // 曜日ラベルが作成されたことを確認
            const weekdayLabels = container.querySelectorAll(".weekday-label");
            expect(weekdayLabels.length).toBe(7); // 日〜土の7つ
        });

        it("ヒートマップセルが描画される", () => {
            const data: TimeSeriesData = {
                daily: [
                    { date: "2024-01-01", count: 1 },
                    { date: "2024-01-02", count: 2 },
                ],
            };

            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");
            heatmap.render(data);

            // ヒートマップセルが作成されたことを確認
            const cells = container.querySelectorAll(".heatmap-cell");
            expect(cells.length).toBeGreaterThan(0);
        });

        it("凡例が描画される", () => {
            const data: TimeSeriesData = {
                daily: [{ date: "2024-01-01", count: 1 }],
            };

            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");
            heatmap.render(data);

            // 凡例が作成されたことを確認
            const legend = container.querySelector(".legend");
            expect(legend).toBeDefined();
        });
    });

    describe("clear", () => {
        it("SVGとツールチップをクリアできる", () => {
            const data: TimeSeriesData = {
                daily: [{ date: "2024-01-01", count: 1 }],
            };

            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");
            heatmap.render(data);

            // 描画されたことを確認
            expect(container.querySelector("svg")).toBeDefined();
            expect(container.querySelector(".visualization-tooltip")).toBeDefined();

            // クリア
            heatmap.clear();

            // SVGとツールチップが削除されたことを確認
            expect(container.querySelector("svg")).toBeNull();
            expect(container.querySelector(".visualization-tooltip")).toBeNull();
        });

        it("何も描画されていない状態でクリアしてもエラーが発生しない", () => {
            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");

            // エラーが発生しないことを確認
            expect(() => heatmap.clear()).not.toThrow();
        });
    });
});
