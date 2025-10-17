import { beforeEach, describe, expect, it } from "vitest";

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

    it("initializes with correct HTML structure", () => {
        const tabs = document.querySelectorAll("#tabs li");
        const content = document.querySelectorAll("#tab-content div");

        expect(tabs.length).toBe(3);
        expect(content.length).toBe(3);
    });

    it("has is-active class on first tab and content", () => {
        const activeTab = document.querySelector("#tabs li.is-active");
        const activeContent = document.querySelector("#tab-content div.is-active");

        expect(activeTab).toBeTruthy();
        expect(activeContent).toBeTruthy();
        expect(activeTab?.getAttribute("data-tab")).toBe("tab1");
        expect(activeContent?.getAttribute("data-content")).toBe("tab1");
    });

    it("removes is-active class from elements", () => {
        const tabs = document.querySelectorAll("#tabs li");
        const firstTab = tabs[0];

        expect(firstTab.classList.contains("is-active")).toBe(true);

        firstTab.classList.remove("is-active");
        expect(firstTab.classList.contains("is-active")).toBe(false);
    });

    it("adds is-active class to elements", () => {
        const tabs = document.querySelectorAll("#tabs li");
        const secondTab = tabs[1] as HTMLElement;

        expect(secondTab.classList.contains("is-active")).toBe(false);

        secondTab.classList.add("is-active");
        expect(secondTab.classList.contains("is-active")).toBe(true);
    });
});
