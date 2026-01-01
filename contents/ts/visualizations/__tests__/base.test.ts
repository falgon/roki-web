/**
 * D3基盤モジュールのユニットテスト
 */

import { beforeEach, describe, expect, it } from "vitest";
import {
    calculateResponsiveSize,
    createCategoricalColorScale,
    createSequentialColorScale,
    defaultSVGConfig,
    showError,
} from "../base";

describe("base module", () => {
    beforeEach(() => {
        // DOMをクリーンアップ
        document.body.innerHTML = "";
    });

    describe("defaultSVGConfig", () => {
        it("デフォルト設定が正しく定義されている", () => {
            expect(defaultSVGConfig).toEqual({
                width: 800,
                height: 600,
                margin: { top: 20, right: 20, bottom: 30, left: 40 },
            });
        });
    });

    describe("calculateResponsiveSize", () => {
        it("コンテナが存在しない場合はデフォルト設定を返す", () => {
            const result = calculateResponsiveSize("#nonexistent");
            expect(result).toEqual(defaultSVGConfig);
        });

        it("コンテナが存在する場合はレスポンシブサイズを計算する", () => {
            // コンテナを作成
            const container = document.createElement("div");
            container.id = "test-container";
            Object.defineProperty(container, "clientWidth", {
                value: 600,
                writable: true,
            });
            document.body.appendChild(container);

            const result = calculateResponsiveSize("#test-container", 0.75);
            expect(result.width).toBe(560); // 600 - 40
            expect(result.height).toBe(420); // 560 * 0.75
        });

        it("最小幅が300px以下にならない", () => {
            const container = document.createElement("div");
            container.id = "test-container";
            Object.defineProperty(container, "clientWidth", {
                value: 100,
                writable: true,
            });
            document.body.appendChild(container);

            const result = calculateResponsiveSize("#test-container");
            expect(result.width).toBeGreaterThanOrEqual(300);
        });

        it("最大幅が1200pxを超えない", () => {
            const container = document.createElement("div");
            container.id = "test-container";
            Object.defineProperty(container, "clientWidth", {
                value: 2000,
                writable: true,
            });
            document.body.appendChild(container);

            const result = calculateResponsiveSize("#test-container");
            expect(result.width).toBeLessThanOrEqual(1200);
        });
    });

    describe("createCategoricalColorScale", () => {
        it("カテゴリカルカラースケールを作成する", () => {
            const domain = ["A", "B", "C"];
            const scale = createCategoricalColorScale(domain);

            expect(scale("A")).toBeDefined();
            expect(scale("B")).toBeDefined();
            expect(scale("C")).toBeDefined();
        });

        it("同じ値に対して同じ色を返す", () => {
            const domain = ["A", "B", "C"];
            const scale = createCategoricalColorScale(domain);

            const color1 = scale("A");
            const color2 = scale("A");

            expect(color1).toBe(color2);
        });
    });

    describe("createSequentialColorScale", () => {
        it("連続カラースケールを作成する", () => {
            const domain: [number, number] = [0, 100];
            const range: [string, string] = ["#f0f0f0", "#d62728"];
            const scale = createSequentialColorScale(domain, range);

            expect(scale(0)).toBe("rgb(240, 240, 240)");
            expect(scale(100)).toBe("rgb(214, 39, 40)");
        });

        it("中間値に対して中間色を返す", () => {
            const domain: [number, number] = [0, 100];
            const range: [string, string] = ["#ffffff", "#000000"];
            const scale = createSequentialColorScale(domain, range);

            const midColor = scale(50);
            expect(midColor).toBeDefined();
            expect(midColor).not.toBe("rgb(255, 255, 255)");
            expect(midColor).not.toBe("rgb(0, 0, 0)");
        });
    });

    describe("showError", () => {
        it("エラーメッセージを表示する", () => {
            const container = document.createElement("div");
            container.id = "test-container";
            document.body.appendChild(container);

            showError("#test-container", "Test error message");

            const errorDiv = container.querySelector(".visualization-error");
            expect(errorDiv).toBeDefined();
            expect(errorDiv?.textContent).toBe("Test error message");
        });

        it("エラーメッセージのスタイルが正しく設定される", () => {
            const container = document.createElement("div");
            container.id = "test-container";
            document.body.appendChild(container);

            showError("#test-container", "Test error");

            const errorDiv = container.querySelector(".visualization-error") as HTMLElement;
            expect(errorDiv).toBeDefined();
            expect(errorDiv.style.color).toBe("red");
            expect(errorDiv.style.textAlign).toBe("center");
        });
    });
});
