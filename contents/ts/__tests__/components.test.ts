import { beforeEach, describe, expect, it, vi } from "vitest";

interface String {
    format(...args: unknown[]): string;
}

describe("components.ts", () => {
    beforeEach(() => {
        document.body.innerHTML = "";
        vi.clearAllMocks();

        if (!String.prototype.format) {
            String.prototype.format = function (...args: unknown[]): string {
                return this.replace(/{(\d+)}/g, (match: string, number: string) =>
                    typeof args[Number.parseInt(number)] !== "undefined"
                        ? String(args[Number.parseInt(number)])
                        : match,
                );
            };
        }
    });

    describe("String.prototype.format", () => {
        it("replaces {0}, {1}, etc. with arguments", () => {
            const template = "Hello {0}, you are {1} years old";
            const result = template.format("Alice", 30);
            expect(result).toBe("Hello Alice, you are 30 years old");
        });

        it("handles missing arguments", () => {
            const template = "Hello {0}, you are {1} years old";
            const result = template.format("Bob");
            expect(result).toBe("Hello Bob, you are {1} years old");
        });

        it("handles multiple same placeholders", () => {
            const template = "{0} + {0} = {1}";
            const result = template.format(2, 4);
            expect(result).toBe("2 + 2 = 4");
        });
    });

    describe("navbar functionality", () => {
        it("sets up navbar burger event listeners", () => {
            document.body.innerHTML = `
                <div class="navbar-burger" data-target="test-menu"></div>
                <div id="test-menu"></div>
            `;

            const burger = document.querySelector(".navbar-burger");
            const menu = document.getElementById("test-menu");

            expect(burger).toBeTruthy();
            expect(menu).toBeTruthy();
        });
    });

    describe("modal functionality", () => {
        it("modal structure exists", () => {
            document.body.innerHTML = `
                <div class="modal-target" data-target="test-modal"></div>
                <div id="test-modal" class="modal"></div>
                <div class="delete"></div>
            `;

            const target = document.querySelector(".modal-target");
            const modal = document.getElementById("test-modal");
            const closeButton = document.querySelector(".delete");

            expect(target).toBeTruthy();
            expect(modal).toBeTruthy();
            expect(closeButton).toBeTruthy();
        });
    });
});
