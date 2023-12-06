const TABS = [...document.querySelectorAll("#tabs li")];
const CONTENT = [...document.querySelectorAll("#tab-content div")];
const ACTIVE_CLASS = "is-active";

const updateActiveTab = (selected) => {
    TABS.forEach(function(tab) {
        if (tab && tab.classList.contains(ACTIVE_CLASS)) {
            tab.classList.remove(ACTIVE_CLASS);
        }
    });
    selected.classList.add(ACTIVE_CLASS);
}

const updateActiveContent = (selected) => {
    CONTENT.forEach(function(item) {
        if (item && item.classList.contains(ACTIVE_CLASS)) {
            item.classList.remove(ACTIVE_CLASS);
        }
        let data = item.getAttribute("data-content");
        if (data === selected) {
            item.classList.add(ACTIVE_CLASS);
        }
    });
}

const initTabs = () => {
    TABS.forEach((tab) => {
      tab.addEventListener("click", (e) => {
        let selected = tab.getAttribute("data-tab");
        updateActiveTab(tab);
        updateActiveContent(selected);
      })
    })
}

initTabs();
