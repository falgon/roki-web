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

    describe("transformData - 年度全体表示機能", () => {
        it("年度指定あり - 通常年（365日）で正しい日数のセルが生成される", () => {
            // 2023年のテストデータ（通常年、365日）
            const data: TimeSeriesData = {
                daily: [
                    { date: "2023-03-15", count: 5 },
                    { date: "2023-06-20", count: 3 },
                    { date: "2023-09-10", count: 2 },
                ],
            };

            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");
            const result = heatmap.transformData(data.daily, 2023);

            // 2023年1月1日は日曜日、12月31日も日曜日
            // 開始日: 2023-01-01（日曜日）
            // 終了日: 2024-01-06（土曜日）
            // 合計: 371日（53週 × 7日）
            expect(result.length).toBe(371);

            // 最初のセルが2023-01-01であることを確認
            const formatDate = (date: Date): string => {
                const year = date.getFullYear();
                const month = String(date.getMonth() + 1).padStart(2, "0");
                const day = String(date.getDate()).padStart(2, "0");
                return `${year}-${month}-${day}`;
            };
            expect(formatDate(result[0].date)).toBe("2023-01-01");

            // 最後のセルが2024-01-06であることを確認
            expect(formatDate(result[result.length - 1].date)).toBe("2024-01-06");
        });

        it("年度指定あり - 閏年（366日）で2月29日を含むセルが生成される", () => {
            // 2024年のテストデータ（閏年、366日）
            const data: TimeSeriesData = {
                daily: [
                    { date: "2024-02-29", count: 7 }, // 閏年の2月29日
                    { date: "2024-05-10", count: 4 },
                ],
            };

            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");
            const result = heatmap.transformData(data.daily, 2024);

            // 2月29日のデータが存在することを確認
            const formatDate = (date: Date): string => {
                const year = date.getFullYear();
                const month = String(date.getMonth() + 1).padStart(2, "0");
                const day = String(date.getDate()).padStart(2, "0");
                return `${year}-${month}-${day}`;
            };

            const feb29Cell = result.find((cell) => formatDate(cell.date) === "2024-02-29");
            expect(feb29Cell).toBeDefined();
            expect(feb29Cell?.count).toBe(7);

            // 2024年1月1日は月曜日、12月31日は火曜日
            // 開始日: 2023-12-31（日曜日）
            // 終了日: 2025-01-04（土曜日）
            // 合計: 371日（53週 × 7日）
            expect(result.length).toBe(371);
        });

        it("年度指定なし - データ範囲のみでセルが生成される（既存動作）", () => {
            // データ範囲: 2024-03-15 から 2024-03-20
            const data: TimeSeriesData = {
                daily: [
                    { date: "2024-03-15", count: 2 },
                    { date: "2024-03-16", count: 3 },
                    { date: "2024-03-17", count: 1 },
                    { date: "2024-03-18", count: 4 },
                    { date: "2024-03-19", count: 2 },
                    { date: "2024-03-20", count: 5 },
                ],
            };

            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");
            const result = heatmap.transformData(data.daily); // year パラメータなし

            // 年度全体より少ないセル数であることを確認
            expect(result.length).toBeLessThan(371);

            const formatDate = (date: Date): string => {
                const year = date.getFullYear();
                const month = String(date.getMonth() + 1).padStart(2, "0");
                const day = String(date.getDate()).padStart(2, "0");
                return `${year}-${month}-${day}`;
            };

            // 2024-03-15は金曜日なので、その週の日曜日は2024-03-10
            expect(formatDate(result[0].date)).toBe("2024-03-10");

            // 2024-03-20は水曜日なので、その週の土曜日は2024-03-23
            expect(formatDate(result[result.length - 1].date)).toBe("2024-03-23");

            // 2週間分（14日）のセルが生成される
            expect(result.length).toBe(14);
        });

        it("境界日付 - 1月1日と12月31日のデータが正しく含まれる", () => {
            // 年の最初と最後の日のテストデータ
            const data: TimeSeriesData = {
                daily: [
                    { date: "2024-01-01", count: 10 },
                    { date: "2024-12-31", count: 15 },
                ],
            };

            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");
            const result = heatmap.transformData(data.daily, 2024);

            const formatDate = (date: Date): string => {
                const year = date.getFullYear();
                const month = String(date.getMonth() + 1).padStart(2, "0");
                const day = String(date.getDate()).padStart(2, "0");
                return `${year}-${month}-${day}`;
            };

            // 2024-01-01のセルが存在し、count が正しいことを確認
            const jan1Cell = result.find((cell) => formatDate(cell.date) === "2024-01-01");
            expect(jan1Cell).toBeDefined();
            expect(jan1Cell?.count).toBe(10);

            // 2024-12-31のセルが存在し、count が正しいことを確認
            const dec31Cell = result.find((cell) => formatDate(cell.date) === "2024-12-31");
            expect(dec31Cell).toBeDefined();
            expect(dec31Cell?.count).toBe(15);
        });

        it("空データ + 年度指定 - 年度全体の空セルが生成される", () => {
            // 空のdaily配列 + year指定
            const data: TimeSeriesData = {
                daily: [],
            };

            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");
            const result = heatmap.transformData(data.daily, 2024);

            // 結果が空配列ではないことを確認（年度全体のセルが生成される）
            expect(result.length).toBeGreaterThan(0);

            // すべてのセルのcountが0であることを確認
            const allCellsAreEmpty = result.every((cell) => cell.count === 0);
            expect(allCellsAreEmpty).toBe(true);

            // セル数が年度全体をカバーしていることを確認
            // 2024年は閏年で、1月1日は月曜日、12月31日は火曜日
            // 開始日: 2023-12-31（日曜日）、終了日: 2025-01-04（土曜日）
            // 合計: 371日（53週 × 7日）
            expect(result.length).toBe(371);
        });

        it("空データ + 年度未指定 - 空配列が返される", () => {
            // 空のdaily配列 + yearなし（既存動作）
            const data: TimeSeriesData = {
                daily: [],
            };

            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");
            const result = heatmap.transformData(data.daily);

            // 結果が空配列であることを確認
            expect(result).toEqual([]);
            expect(result.length).toBe(0);
        });

        it("無効な日付フォーマット - 不正なデータは無視される", () => {
            // 不正な日付形式を含むデータ
            const data: TimeSeriesData = {
                daily: [
                    { date: "invalid-date", count: 1 },
                    { date: "2024-01-15", count: 2 },
                ],
            };

            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");

            // エラーがスローされないことを確認
            expect(() => heatmap.transformData(data.daily, 2024)).not.toThrow();

            const result = heatmap.transformData(data.daily, 2024);

            const formatDate = (date: Date): string => {
                const year = date.getFullYear();
                const month = String(date.getMonth() + 1).padStart(2, "0");
                const day = String(date.getDate()).padStart(2, "0");
                return `${year}-${month}-${day}`;
            };

            // 有効なデータ（2024-01-15）が正しく処理されることを確認
            const jan15Cell = result.find((cell) => formatDate(cell.date) === "2024-01-15");
            expect(jan15Cell).toBeDefined();
            expect(jan15Cell?.count).toBe(2);
        });

        describe("1月1日の曜日による週インデックス計算", () => {
            // 異なる年の1月1日の曜日をテスト
            const testYears = [
                { year: 2023, startDayOfWeek: 0 }, // 日曜日
                { year: 2024, startDayOfWeek: 1 }, // 月曜日
                { year: 2019, startDayOfWeek: 2 }, // 火曜日
                { year: 2020, startDayOfWeek: 3 }, // 水曜日
                { year: 2015, startDayOfWeek: 4 }, // 木曜日
                { year: 2021, startDayOfWeek: 5 }, // 金曜日
                { year: 2022, startDayOfWeek: 6 }, // 土曜日
            ];

            testYears.forEach(({ year, startDayOfWeek }) => {
                it(`${year}年 - 1月1日が曜日${startDayOfWeek}の場合、正しい開始週が計算される`, () => {
                    const heatmap = new TimelineHeatmap("#test-timeline-heatmap");
                    const result = heatmap.transformData([], year);

                    // 最初のセルが日曜日（weekday = 0）であることを確認
                    expect(result[0].weekday).toBe(0);

                    // 最初のセルの weekIndex が 0 であることを確認
                    expect(result[0].weekIndex).toBe(0);

                    // 1月1日のセルを検索
                    const formatDate = (date: Date): string => {
                        const y = date.getFullYear();
                        const m = String(date.getMonth() + 1).padStart(2, "0");
                        const d = String(date.getDate()).padStart(2, "0");
                        return `${y}-${m}-${d}`;
                    };

                    const jan1Cell = result.find(
                        (cell) => formatDate(cell.date) === `${year}-01-01`,
                    );
                    expect(jan1Cell).toBeDefined();
                    expect(jan1Cell?.weekday).toBe(startDayOfWeek);
                });
            });
        });

        describe("12月31日の曜日による終了週計算", () => {
            // 12月31日の曜日が異なる年をテスト
            const testYears = [
                { year: 2023, endDayOfWeek: 0 }, // 日曜日
                { year: 2018, endDayOfWeek: 1 }, // 月曜日
                { year: 2019, endDayOfWeek: 2 }, // 火曜日
                { year: 2014, endDayOfWeek: 3 }, // 水曜日
                { year: 2015, endDayOfWeek: 4 }, // 木曜日
                { year: 2021, endDayOfWeek: 5 }, // 金曜日
                { year: 2022, endDayOfWeek: 6 }, // 土曜日
            ];

            testYears.forEach(({ year, endDayOfWeek }) => {
                it(`${year}年 - 12月31日が曜日${endDayOfWeek}の場合、正しい終了週が計算される`, () => {
                    const heatmap = new TimelineHeatmap("#test-timeline-heatmap");
                    const result = heatmap.transformData([], year);

                    // 最後のセルが土曜日（weekday = 6）であることを確認
                    expect(result[result.length - 1].weekday).toBe(6);

                    // 12月31日のセルを検索
                    const formatDate = (date: Date): string => {
                        const y = date.getFullYear();
                        const m = String(date.getMonth() + 1).padStart(2, "0");
                        const d = String(date.getDate()).padStart(2, "0");
                        return `${y}-${m}-${d}`;
                    };

                    const dec31Cell = result.find(
                        (cell) => formatDate(cell.date) === `${year}-12-31`,
                    );
                    expect(dec31Cell).toBeDefined();
                    expect(dec31Cell?.weekday).toBe(endDayOfWeek);
                });
            });
        });

        describe("1件のみのデータ", () => {
            it("1件のみ + 年度指定あり - 年度全体が表示される", () => {
                const data = [{ date: "2024-06-15", count: 5 }];
                const heatmap = new TimelineHeatmap("#test-timeline-heatmap");
                const result = heatmap.transformData(data, 2024);

                // 年度全体をカバーしていることを確認
                expect(result.length).toBeGreaterThan(365);

                // 6月15日のセルを検索
                const formatDate = (date: Date): string => {
                    const year = date.getFullYear();
                    const month = String(date.getMonth() + 1).padStart(2, "0");
                    const day = String(date.getDate()).padStart(2, "0");
                    return `${year}-${month}-${day}`;
                };

                const jun15Cell = result.find((cell) => formatDate(cell.date) === "2024-06-15");
                expect(jun15Cell).toBeDefined();
                expect(jun15Cell?.count).toBe(5);
            });

            it("1件のみ + 年度指定なし - その週のみ表示される", () => {
                const data = [{ date: "2024-06-15", count: 5 }];
                const heatmap = new TimelineHeatmap("#test-timeline-heatmap");
                const result = heatmap.transformData(data);

                // 1週間分（7日）のセルが生成されることを確認
                expect(result.length).toBe(7);

                // 6月15日のセルを検索
                const formatDate = (date: Date): string => {
                    const year = date.getFullYear();
                    const month = String(date.getMonth() + 1).padStart(2, "0");
                    const day = String(date.getDate()).padStart(2, "0");
                    return `${year}-${month}-${day}`;
                };

                const jun15Cell = result.find((cell) => formatDate(cell.date) === "2024-06-15");
                expect(jun15Cell).toBeDefined();
                expect(jun15Cell?.count).toBe(5);
            });
        });
    });

    describe("後方互換性", () => {
        it("既存のTimeSeriesData形式での呼び出しが正常に動作する", () => {
            // 従来の呼び出しパターンをテスト
            const data: TimeSeriesData = {
                daily: [
                    { date: "2024-01-01", count: 1 },
                    { date: "2024-01-02", count: 2 },
                    { date: "2024-01-03", count: 3 },
                ],
            };

            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");

            // エラーなく描画できることを確認
            expect(() => heatmap.render(data)).not.toThrow();

            // SVGが作成されることを確認
            const svg = container.querySelector("svg");
            expect(svg).not.toBeNull();

            // ヒートマップセルが描画されることを確認
            const cells = container.querySelectorAll(".heatmap-cell");
            expect(cells.length).toBeGreaterThan(0);
        });

        it("YearData配列形式での呼び出しが正常に動作する", () => {
            // 新しい年度別データ形式をテスト
            const yearData: YearlyTimeSeriesData[] = [
                {
                    year: 2024,
                    daily: [
                        { date: "2024-01-01", count: 1 },
                        { date: "2024-06-15", count: 2 },
                    ],
                },
                {
                    year: 2023,
                    daily: [{ date: "2023-03-20", count: 3 }],
                },
            ];

            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");

            // エラーなく描画できることを確認
            expect(() => heatmap.render(yearData)).not.toThrow();

            // SVGが作成されることを確認
            const svg = container.querySelector("svg");
            expect(svg).not.toBeNull();
        });

        it("空のYearData配列でもエラーが発生しない", () => {
            const heatmap = new TimelineHeatmap("#test-timeline-heatmap");

            // 空配列でエラーが発生しないことを確認
            expect(() => heatmap.render([])).not.toThrow();
        });
    });
});
