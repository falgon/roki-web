const TABS: Element[] = [...document.querySelectorAll("#tabs li")];
const CONTENT: Element[] = [...document.querySelectorAll("#tab-content div")];
const ACTIVE_CLASS = "is-active";

const updateActiveTab = (selected: Element): void => {
    TABS.forEach(function(tab: Element) {
        if (tab && tab.classList.contains(ACTIVE_CLASS)) {
            tab.classList.remove(ACTIVE_CLASS);
        }
    });
    selected.classList.add(ACTIVE_CLASS);
}

const updateActiveContent = (selected: string): void => {
    CONTENT.forEach(function(item: Element) {
        if (item && item.classList.contains(ACTIVE_CLASS)) {
            item.classList.remove(ACTIVE_CLASS);
        }
        const data = item.getAttribute("data-content");
        if (data === selected) {
            item.classList.add(ACTIVE_CLASS);
        }
    });
}

const initTabs = (): void => {
    TABS.forEach((tab: Element) => {
      tab.addEventListener("click", (e: Event) => {
        const selected = tab.getAttribute("data-tab");
        if (selected) {
            updateActiveTab(tab);
            updateActiveContent(selected);
        }
      })
    })
}

initTabs(); 