/**
 * Disneyタブマネージャー
 * Bulma Tabsを使用したタブ切り替えと遅延初期化を管理
 */
export class DisneyTabManager {
    private tabs: HTMLElement[];
    private panels: HTMLElement[];
    private initCallbacks: Map<string, () => void>;
    private initializedPanels: Set<string>;
    private hashToPanelMap: Map<string, string>;

    /**
     * コンストラクタ
     * @param tabListSelector タブリストのセレクタ（例: '.tabs'）
     */
    constructor(tabListSelector: string) {
        const tabList = document.querySelector(tabListSelector);
        if (!tabList) {
            throw new Error(`Tab list not found: ${tabListSelector}`);
        }

        // タブとパネルを取得
        this.tabs = Array.from(tabList.querySelectorAll('[role="tab"]'));
        this.panels = [];
        this.tabs.forEach((tab) => {
            const panelId = tab.getAttribute("aria-controls");
            if (panelId) {
                const panel = document.getElementById(panelId);
                if (panel) {
                    this.panels.push(panel);
                }
            }
        });

        this.initCallbacks = new Map();
        this.initializedPanels = new Set();

        // URLハッシュとパネルIDのマッピングを初期化
        this.hashToPanelMap = new Map([
            ["exp_list", "panel-list"],
            ["exp_timeline", "panel-timeline"],
            ["exp_tags", "panel-tags"],
            ["exp_hotels", "panel-hotels"],
        ]);

        // イベントリスナーを設定
        this.setupEventListeners();
    }

    /**
     * パネル初期化コールバックを登録
     * @param panelId パネルのDOM ID
     * @param callback 初期化コールバック
     */
    public registerInitCallback(panelId: string, callback: () => void): void {
        this.initCallbacks.set(panelId, callback);
    }

    /**
     * タブマネージャーを初期化（コールバック登録後に呼び出す）
     */
    public initialize(): void {
        this.initializeActiveTab();
    }

    /**
     * イベントリスナーをセットアップ
     */
    private setupEventListeners(): void {
        this.tabs.forEach((tab) => {
            tab.addEventListener("click", (e) => {
                e.preventDefault();
                this.switchToTab(tab);
            });

            tab.addEventListener("keydown", (e) => {
                this.handleKeyDown(e as KeyboardEvent, tab);
            });
        });
    }

    /**
     * URLハッシュに基づいてタブをアクティブ化
     * @returns アクティブ化に成功した場合true
     */
    private activateTabFromHash(): boolean {
        const hash = window.location.hash.substring(1); // "#"を除去
        if (!hash) {
            return false;
        }

        const panelId = this.hashToPanelMap.get(hash);
        if (!panelId) {
            return false;
        }

        const tab = this.tabs.find((t) => t.getAttribute("aria-controls") === panelId);
        if (tab) {
            this.switchToTab(tab);
            return true;
        }
        return false;
    }

    /**
     * 初期アクティブタブを初期化
     * URLハッシュがある場合はそれに従い、なければ既存のアクティブタブを使用
     */
    private initializeActiveTab(): void {
        // URLハッシュがある場合はそれを優先
        if (this.activateTabFromHash()) {
            return;
        }

        // ハッシュがない場合は既存のアクティブタブを初期化
        const activeTab = this.tabs.find((tab) => tab.getAttribute("aria-selected") === "true");
        if (activeTab) {
            const panelId = activeTab.getAttribute("aria-controls");
            if (panelId) {
                this.initializePanelIfNeeded(panelId);
            }
        }
    }

    /**
     * タブを切り替え
     * @param tab 切り替え先のタブ要素
     */
    private switchToTab(tab: HTMLElement): void {
        // 全タブを非アクティブに
        this.tabs.forEach((t) => {
            t.setAttribute("aria-selected", "false");
            t.setAttribute("tabindex", "-1");
            t.parentElement?.classList.remove("is-active");
        });

        // 選択されたタブをアクティブに
        tab.setAttribute("aria-selected", "true");
        tab.setAttribute("tabindex", "0");
        tab.parentElement?.classList.add("is-active");
        tab.focus();

        // 全パネルを非表示に
        this.panels.forEach((panel) => {
            panel.classList.remove("is-active");
            panel.setAttribute("hidden", "");
        });

        // 対応するパネルを表示
        const panelId = tab.getAttribute("aria-controls");
        if (panelId) {
            const panel = document.getElementById(panelId);
            if (panel) {
                panel.classList.add("is-active");
                panel.removeAttribute("hidden");
                this.initializePanelIfNeeded(panelId);
            }

            // URLハッシュを更新（ページリロードなしでURL変更）
            const hash = Array.from(this.hashToPanelMap.entries()).find(
                ([_, id]) => id === panelId,
            )?.[0];
            if (hash) {
                history.replaceState(null, "", `#${hash}`);
            }
        }
    }

    /**
     * キーボードナビゲーションを処理
     * @param event キーボードイベント
     * @param currentTab 現在のタブ
     */
    private handleKeyDown(event: KeyboardEvent, currentTab: HTMLElement): void {
        const currentIndex = this.tabs.indexOf(currentTab);
        let targetIndex: number | null = null;

        switch (event.key) {
            case "ArrowLeft":
                targetIndex = currentIndex > 0 ? currentIndex - 1 : this.tabs.length - 1;
                event.preventDefault();
                break;
            case "ArrowRight":
                targetIndex = currentIndex < this.tabs.length - 1 ? currentIndex + 1 : 0;
                event.preventDefault();
                break;
            case "Home":
                targetIndex = 0;
                event.preventDefault();
                break;
            case "End":
                targetIndex = this.tabs.length - 1;
                event.preventDefault();
                break;
            case "Enter":
            case " ":
                // EnterまたはSpaceキーでタブをアクティブ化（WCAG 2.1 AA準拠）
                this.switchToTab(currentTab);
                event.preventDefault();
                break;
        }

        if (targetIndex !== null) {
            this.switchToTab(this.tabs[targetIndex]);
        }
    }

    /**
     * パネルを初回のみ初期化
     * @param panelId パネルID
     */
    private initializePanelIfNeeded(panelId: string): void {
        if (this.initializedPanels.has(panelId)) {
            return; // 既に初期化済み
        }

        const callback = this.initCallbacks.get(panelId);
        if (callback) {
            callback();
            this.initializedPanels.add(panelId);
        }
    }
}
