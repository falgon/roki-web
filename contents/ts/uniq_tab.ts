const TABS: Element[] = [...document.querySelectorAll("#tabs li")];
const CONTENT: Element[] = [...document.querySelectorAll("#tab-content div")];
const ACTIVE_CLASS = "is-active";

const updateActiveTab = (selected: Element): void => {
    for (const tab of TABS) {
        if (tab?.classList.contains(ACTIVE_CLASS)) {
            tab.classList.remove(ACTIVE_CLASS);
        }
    }
    selected.classList.add(ACTIVE_CLASS);
};

const updateActiveContent = (selected: string): void => {
    for (const item of CONTENT) {
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
    for (const tab of TABS) {
        tab.addEventListener("click", (e: Event) => {
            const selected = tab.getAttribute("data-tab");
            if (selected) {
                updateActiveTab(tab);
                updateActiveContent(selected);
            }
        });
    }
};

initTabs();
