const ACTIVE_CLASS = "is-active";

const updateActiveTab = (tabs: Element[], selected: Element): void => {
    for (const tab of tabs) {
        if (tab?.classList.contains(ACTIVE_CLASS)) {
            tab.classList.remove(ACTIVE_CLASS);
        }
    }
    selected.classList.add(ACTIVE_CLASS);
};

const updateActiveContent = (content: Element[], selected: string): void => {
    for (const item of content) {
        if (item?.classList.contains(ACTIVE_CLASS)) {
            item.classList.remove(ACTIVE_CLASS);
        }
        const data = item.getAttribute("data-content");
        if (data === selected) {
            item.classList.add(ACTIVE_CLASS);
        }
    }
};

const initTabs = (): void => {
    const tabs: Element[] = [...document.querySelectorAll("#tabs li")];
    const content: Element[] = [...document.querySelectorAll("#tab-content div")];

    for (const tab of tabs) {
        tab.addEventListener("click", (_e: Event) => {
            const selected = tab.getAttribute("data-tab");
            if (selected) {
                updateActiveTab(tabs, tab);
                updateActiveContent(content, selected);
            }
        });
    }
};

// Expose functions to global scope for testing
if (typeof window !== "undefined") {
    (
        window as typeof window & {
            ACTIVE_CLASS: typeof ACTIVE_CLASS;
            updateActiveTab: typeof updateActiveTab;
            updateActiveContent: typeof updateActiveContent;
            initTabs: typeof initTabs;
        }
    ).ACTIVE_CLASS = ACTIVE_CLASS;
    (window as typeof window & { updateActiveTab: typeof updateActiveTab }).updateActiveTab =
        updateActiveTab;
    (
        window as typeof window & { updateActiveContent: typeof updateActiveContent }
    ).updateActiveContent = updateActiveContent;
    (window as typeof window & { initTabs: typeof initTabs }).initTabs = initTabs;
}

if (typeof document !== "undefined") {
    initTabs();
}
