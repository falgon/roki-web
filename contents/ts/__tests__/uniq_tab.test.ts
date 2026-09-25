import { beforeEach, describe, expect, it } from "vitest";
import "../uniq_tab";

function fixtureElements(selector: string): [HTMLElement, HTMLElement, HTMLElement] {
    const elements = document.querySelectorAll<HTMLElement>(selector);
    const [first, second, third] = elements;
    if (elements.length !== 3 || !first || !second || !third) {
        throw new Error(`Expected exactly three fixture elements for ${selector}`);
    }
    return [first, second, third];
}

describe("uniq_tab.ts", () => {
    beforeEach(() => {
        document.body.innerHTML = `
            <ul id="tabs">
                <li class="is-active" data-tab="tab1">Tab 1</li>
                <li data-tab="tab2">Tab 2</li>
                <li data-tab="tab3">Tab 3</li>
            </ul>
            <div id="tab-content">
                <div class="is-active" data-content="tab1">Content 1</div>
                <div data-content="tab2">Content 2</div>
                <div data-content="tab3">Content 3</div>
            </div>
        `;
    });

    describe("updateActiveTab", () => {
        it("removes is-active from all tabs and adds to selected", () => {
            const tabs = fixtureElements("#tabs li");
            const secondTab = tabs[1];

            expect(tabs[0].classList.contains(ACTIVE_CLASS)).toBe(true);
            expect(secondTab.classList.contains(ACTIVE_CLASS)).toBe(false);

            updateActiveTab(tabs, secondTab);

            expect(tabs[0].classList.contains(ACTIVE_CLASS)).toBe(false);
            expect(secondTab.classList.contains(ACTIVE_CLASS)).toBe(true);
        });
    });

    describe("updateActiveContent", () => {
        it("removes is-active from all content and adds to matching data-content", () => {
            const content = fixtureElements("#tab-content div");

            expect(content[0].classList.contains(ACTIVE_CLASS)).toBe(true);
            expect(content[1].classList.contains(ACTIVE_CLASS)).toBe(false);

            updateActiveContent(content, "tab2");

            expect(content[0].classList.contains(ACTIVE_CLASS)).toBe(false);
            expect(content[1].classList.contains(ACTIVE_CLASS)).toBe(true);
        });

        it("handles content with no matching data-content", () => {
            const content = fixtureElements("#tab-content div");

            updateActiveContent(content, "nonexistent");

            expect(content[0].classList.contains(ACTIVE_CLASS)).toBe(false);
            expect(content[1].classList.contains(ACTIVE_CLASS)).toBe(false);
            expect(content[2].classList.contains(ACTIVE_CLASS)).toBe(false);
        });
    });

    describe("initTabs", () => {
        it("sets up click handlers that switch tabs and content", () => {
            const tabs = fixtureElements("#tabs li");
            const contents = fixtureElements("#tab-content div");

            initTabs();

            expect(tabs[0].classList.contains(ACTIVE_CLASS)).toBe(true);
            expect(contents[0].classList.contains(ACTIVE_CLASS)).toBe(true);

            tabs[1].click();

            expect(tabs[0].classList.contains(ACTIVE_CLASS)).toBe(false);
            expect(tabs[1].classList.contains(ACTIVE_CLASS)).toBe(true);
            expect(contents[0].classList.contains(ACTIVE_CLASS)).toBe(false);
            expect(contents[1].classList.contains(ACTIVE_CLASS)).toBe(true);
        });

        it("handles clicking already active tab", () => {
            const tabs = fixtureElements("#tabs li");
            const contents = fixtureElements("#tab-content div");

            initTabs();

            tabs[0].click();

            expect(tabs[0].classList.contains(ACTIVE_CLASS)).toBe(true);
            expect(contents[0].classList.contains(ACTIVE_CLASS)).toBe(true);
        });
    });
});
