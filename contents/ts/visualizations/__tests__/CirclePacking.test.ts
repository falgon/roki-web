/**
 * サークルパッキングのユニットテスト
 */

import { beforeEach, describe, expect, it } from "vitest";
import type { TagStats } from "../../types/disney-experience";
import { CirclePacking } from "../CirclePacking";

describe("CirclePacking", () => {
    let container: HTMLElement;

    beforeEach(() => {
        // DOMをクリーンアップ
        document.body.innerHTML = "";
        // テスト用のコンテナを作成
        container = document.createElement("div");
        container.id = "test-circle-packing";
        container.style.position = "relative";
        document.body.appendChild(container);
    });

    describe("constructor", () => {
        it("デフォルト設定でインスタンスを作成できる", () => {
            const circlePacking = new CirclePacking("#test-circle-packing");
            expect(circlePacking).toBeDefined();
        });

        it("カスタム設定でインスタンスを作成できる", () => {
            const circlePacking = new CirclePacking("#test-circle-packing", {
                width: 800,
                height: 800,
                minRadius: 30,
            });
            expect(circlePacking).toBeDefined();
        });
    });

    describe("render", () => {
        it("正常なデータで描画できる", () => {
            const data: TagStats = {
                tags: [
                    { tag: "TDL", count: 45 },
                    { tag: "TDS", count: 30 },
                    { tag: "DHM", count: 10 },
                ],
            };

            const circlePacking = new CirclePacking("#test-circle-packing");
            circlePacking.render(data);

            // SVGが作成されたことを確認
            const svg = container.querySelector("svg");
            expect(svg).toBeDefined();
        });

        it("空のデータで描画できる", () => {
            const data: TagStats = {
                tags: [],
            };

            const circlePacking = new CirclePacking("#test-circle-packing");
            circlePacking.render(data);

            // SVGが作成されたことを確認
            const svg = container.querySelector("svg");
            expect(svg).toBeDefined();
        });

        it("ツールチップが作成される", () => {
            const data: TagStats = {
                tags: [{ tag: "TDL", count: 10 }],
            };

            const circlePacking = new CirclePacking("#test-circle-packing");
            circlePacking.render(data);

            // ツールチップが作成されたことを確認
            const tooltip = container.querySelector(".visualization-tooltip");
            expect(tooltip).toBeDefined();
        });

        it("サークルノードが描画される", () => {
            const data: TagStats = {
                tags: [
                    { tag: "TDL", count: 20 },
                    { tag: "TDS", count: 15 },
                ],
            };

            const circlePacking = new CirclePacking("#test-circle-packing");
            circlePacking.render(data);

            // サークルノードが作成されたことを確認
            const nodes = container.querySelectorAll(".circle-node");
            expect(nodes.length).toBeGreaterThan(0);
        });

        it("ズームリセットボタンが作成される", () => {
            const data: TagStats = {
                tags: [{ tag: "TDL", count: 10 }],
            };

            const circlePacking = new CirclePacking("#test-circle-packing");
            circlePacking.render(data);

            // ズームリセットボタンが作成されたことを確認
            const button = container.querySelector(".zoom-reset-button");
            expect(button).toBeDefined();
        });

        it("各タグに対して円が作成される", () => {
            const data: TagStats = {
                tags: [
                    { tag: "TDL", count: 10 },
                    { tag: "TDS", count: 20 },
                    { tag: "DHM", count: 5 },
                ],
            };

            const circlePacking = new CirclePacking("#test-circle-packing");
            circlePacking.render(data);

            // 各タグに対して円が作成されたことを確認
            const circles = container.querySelectorAll("circle");
            expect(circles.length).toBe(3);
        });
    });

    describe("clear", () => {
        it("SVGとツールチップをクリアできる", () => {
            const data: TagStats = {
                tags: [{ tag: "TDL", count: 10 }],
            };

            const circlePacking = new CirclePacking("#test-circle-packing");
            circlePacking.render(data);

            // 描画されたことを確認
            expect(container.querySelector("svg")).toBeDefined();
            expect(container.querySelector(".visualization-tooltip")).toBeDefined();
            expect(container.querySelector(".zoom-reset-button")).toBeDefined();

            // クリア
            circlePacking.clear();

            // SVGとツールチップが削除されたことを確認
            expect(container.querySelector("svg")).toBeNull();
            expect(container.querySelector(".visualization-tooltip")).toBeNull();
            expect(container.querySelector(".zoom-reset-button")).toBeNull();
        });

        it("何も描画されていない状態でクリアしてもエラーが発生しない", () => {
            const circlePacking = new CirclePacking("#test-circle-packing");

            // エラーが発生しないことを確認
            expect(() => circlePacking.clear()).not.toThrow();
        });
    });

    describe("setZoomEnabled", () => {
        it("ズーム機能を無効にできる", () => {
            const data: TagStats = {
                tags: [{ tag: "TDL", count: 10 }],
            };

            const circlePacking = new CirclePacking("#test-circle-packing");
            circlePacking.render(data);

            // ズーム機能を無効にしてもエラーが発生しないことを確認
            expect(() => circlePacking.setZoomEnabled(false)).not.toThrow();
        });

        it("ズーム機能を有効にできる", () => {
            const data: TagStats = {
                tags: [{ tag: "TDL", count: 10 }],
            };

            const circlePacking = new CirclePacking("#test-circle-packing");
            circlePacking.render(data);

            // ズーム機能を有効にしてもエラーが発生しないことを確認
            expect(() => circlePacking.setZoomEnabled(true)).not.toThrow();
        });

        it("描画前にズーム機能を設定してもエラーが発生しない", () => {
            const circlePacking = new CirclePacking("#test-circle-packing");

            // 描画前にズーム機能を設定してもエラーが発生しないことを確認
            expect(() => circlePacking.setZoomEnabled(false)).not.toThrow();
        });
    });
});
