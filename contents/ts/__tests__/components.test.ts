import { beforeEach, describe, expect, it, vi } from "vitest";
import "../components";

interface String {
    format(...args: unknown[]): string;
}

declare global {
    function setupNavBar(): void;
    function setupModal(): void;
    function openLink(): void;
    function initStringFormat(): void;
}

describe("components.ts", () => {
    beforeEach(() => {
        document.body.innerHTML = "";
        vi.clearAllMocks();
        (String.prototype as { format?: unknown }).format = undefined;
    });

    describe("setupNavBar", () => {
        it("toggles is-active class on burger and target when clicked", () => {
            document.body.innerHTML = `
                <div class="navbar-burger" data-target="test-menu"></div>
                <div id="test-menu"></div>
            `;

            setupNavBar();

            const burger = document.querySelector(".navbar-burger");
            const menu = document.getElementById("test-menu");

            expect(burger?.classList.contains("is-active")).toBe(false);
            expect(menu?.classList.contains("is-active")).toBe(false);

            (burger as HTMLElement).click();

            expect(burger?.classList.contains("is-active")).toBe(true);
            expect(menu?.classList.contains("is-active")).toBe(true);

            (burger as HTMLElement).click();

            expect(burger?.classList.contains("is-active")).toBe(false);
            expect(menu?.classList.contains("is-active")).toBe(false);
        });

        it("handles multiple navbar burgers", () => {
            document.body.innerHTML = `
                <div class="navbar-burger" data-target="menu1"></div>
                <div class="navbar-burger" data-target="menu2"></div>
                <div id="menu1"></div>
                <div id="menu2"></div>
            `;

            setupNavBar();

            const [burger1, burger2] = document.querySelectorAll(".navbar-burger");
            const menu1 = document.getElementById("menu1");
            const menu2 = document.getElementById("menu2");

            (burger1 as HTMLElement).click();
            expect(burger1?.classList.contains("is-active")).toBe(true);
            expect(menu1?.classList.contains("is-active")).toBe(true);

            (burger2 as HTMLElement).click();
            expect(burger2?.classList.contains("is-active")).toBe(true);
            expect(menu2?.classList.contains("is-active")).toBe(true);
        });

        it("does nothing when target is not found", () => {
            document.body.innerHTML = `
                <div class="navbar-burger" data-target="nonexistent"></div>
            `;

            setupNavBar();

            const burger = document.querySelector(".navbar-burger");
            expect(() => (burger as HTMLElement).click()).not.toThrow();
        });

        it("does nothing when no navbar burgers exist", () => {
            document.body.innerHTML = "<div></div>";
            expect(() => setupNavBar()).not.toThrow();
        });
    });

    describe("setupModal", () => {
        it("opens modal when modal-target is clicked", () => {
            document.body.innerHTML = `
                <div class="modal-target" data-target="test-modal"></div>
                <div id="test-modal" class="modal"></div>
            `;

            setupModal();

            const modalTarget = document.querySelector(".modal-target");
            const modal = document.getElementById("test-modal");

            expect(modal?.classList.contains("is-active")).toBe(false);

            (modalTarget as HTMLElement).click();

            expect(modal?.classList.contains("is-active")).toBe(true);
        });

        it("closes all modals when delete button is clicked", () => {
            document.body.innerHTML = `
                <div class="modal-target" data-target="modal1"></div>
                <div class="modal-target" data-target="modal2"></div>
                <div id="modal1" class="modal"></div>
                <div id="modal2" class="modal"></div>
                <div class="delete"></div>
            `;

            setupModal();

            const [target1, target2] = document.querySelectorAll(".modal-target");
            const modal1 = document.getElementById("modal1");
            const modal2 = document.getElementById("modal2");
            const deleteBtn = document.querySelector(".delete");

            (target1 as HTMLElement).click();
            (target2 as HTMLElement).click();

            expect(modal1?.classList.contains("is-active")).toBe(true);
            expect(modal2?.classList.contains("is-active")).toBe(true);

            (deleteBtn as HTMLElement).click();

            expect(modal1?.classList.contains("is-active")).toBe(false);
            expect(modal2?.classList.contains("is-active")).toBe(false);
        });

        it("closes all modals when modal-background is clicked", () => {
            document.body.innerHTML = `
                <div class="modal-target" data-target="modal1"></div>
                <div id="modal1" class="modal"></div>
                <div class="modal-background"></div>
            `;

            setupModal();

            const target = document.querySelector(".modal-target");
            const modal = document.getElementById("modal1");
            const background = document.querySelector(".modal-background");

            (target as HTMLElement).click();
            expect(modal?.classList.contains("is-active")).toBe(true);

            (background as HTMLElement).click();
            expect(modal?.classList.contains("is-active")).toBe(false);
        });

        it("does nothing when target is not found", () => {
            document.body.innerHTML = `
                <div class="modal-target" data-target="nonexistent"></div>
            `;

            setupModal();

            const modalTarget = document.querySelector(".modal-target");
            expect(() => (modalTarget as HTMLElement).click()).not.toThrow();
        });

        it("does nothing when no modal elements exist", () => {
            document.body.innerHTML = "<div></div>";
            expect(() => setupModal()).not.toThrow();
        });
    });

    describe("openLink", () => {
        it("toggles display from none to block", () => {
            document.body.innerHTML = `
                <div class="open_links" style="display: none;">Link 1</div>
                <div class="open_links" style="display: none;">Link 2</div>
            `;

            openLink();

            const links = document.getElementsByClassName("open_links");
            expect((links[0] as HTMLElement).style.display).toBe("block");
            expect((links[1] as HTMLElement).style.display).toBe("block");
        });

        it("toggles display from block to none", () => {
            document.body.innerHTML = `
                <div class="open_links" style="display: block;">Link 1</div>
            `;

            openLink();

            const link = document.getElementsByClassName("open_links")[0] as HTMLElement;
            expect(link.style.display).toBe("none");
        });

        it("handles multiple toggles", () => {
            document.body.innerHTML = `
                <div class="open_links" style="display: none;">Link</div>
            `;

            const link = document.getElementsByClassName("open_links")[0] as HTMLElement;

            openLink();
            expect(link.style.display).toBe("block");

            openLink();
            expect(link.style.display).toBe("none");

            openLink();
            expect(link.style.display).toBe("block");
        });

        it("does nothing when no open_links elements exist", () => {
            document.body.innerHTML = "<div></div>";
            expect(() => openLink()).not.toThrow();
        });
    });

    describe("initStringFormat", () => {
        it("adds format method to String.prototype", () => {
            expect(String.prototype.format).toBeUndefined();

            initStringFormat();

            expect(String.prototype.format).toBeDefined();
            expect(typeof String.prototype.format).toBe("function");
        });

        it("does not override existing format method", () => {
            const customFormat = vi.fn();
            String.prototype.format = customFormat;

            initStringFormat();

            expect(String.prototype.format).toBe(customFormat);
        });

        it("format method replaces {0}, {1}, etc. with arguments", () => {
            initStringFormat();

            const template = "Hello {0}, you are {1} years old";
            const result = template.format("Alice", 30);
            expect(result).toBe("Hello Alice, you are 30 years old");
        });

        it("format method handles missing arguments", () => {
            initStringFormat();

            const template = "Hello {0}, you are {1} years old";
            const result = template.format("Bob");
            expect(result).toBe("Hello Bob, you are {1} years old");
        });

        it("format method handles multiple same placeholders", () => {
            initStringFormat();

            const template = "{0} + {0} = {1}";
            const result = template.format(2, 4);
            expect(result).toBe("2 + 2 = 4");
        });

        it("format method handles non-string arguments", () => {
            initStringFormat();

            const template = "Number: {0}, Boolean: {1}, Null: {2}";
            const result = template.format(42, true, null);
            expect(result).toBe("Number: 42, Boolean: true, Null: null");
        });
    });
});
