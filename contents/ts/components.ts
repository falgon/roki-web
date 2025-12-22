const setupNavBar = (): void => {
    const navbarBurgers = Array.prototype.slice.call(
        document.querySelectorAll(".navbar-burger"),
        0,
    );

    if (navbarBurgers.length > 0) {
        for (const el of navbarBurgers) {
            el.addEventListener("click", () => {
                const targetId = (el as HTMLElement).dataset.target;
                if (targetId) {
                    const target = document.getElementById(targetId);
                    if (target) {
                        el.classList.toggle("is-active");
                        target.classList.toggle("is-active");
                    }
                }
            });
        }
    }
};

const setupModal = (): void => {
    const modalTargets = Array.from(document.getElementsByClassName("modal-target"));
    const modalCloses = Array.prototype.slice.call(
        document.querySelectorAll(".delete, .modal-background"),
        0,
    );

    for (const modalTarget of modalTargets) {
        modalTarget.addEventListener("click", () => {
            const targetId = (modalTarget as HTMLElement).dataset.target;
            if (targetId) {
                const target = document.getElementById(targetId);
                if (target) {
                    target.classList.add("is-active");
                }
            }
        });
    }

    if (modalCloses.length > 0) {
        for (const el of modalCloses) {
            el.addEventListener("click", () => {
                for (const modalTarget of modalTargets) {
                    const targetId = (modalTarget as HTMLElement).dataset.target;
                    if (targetId) {
                        const target = document.getElementById(targetId);
                        if (target) {
                            target.classList.remove("is-active");
                        }
                    }
                }
            });
        }
    }
};

const openLink = (): void => {
    const ls = Array.from(document.getElementsByClassName("open_links"));
    for (const l of ls) {
        const element = l as HTMLElement;
        element.style.display = element.style.display === "none" ? "block" : "none";
    }
};

// String.prototype.formatの型定義を拡張
declare global {
    interface String {
        format(...args: unknown[]): string;
    }

    interface StringConstructor {
        prototype: {
            format?: (...args: unknown[]) => string;
        };
    }
}

const initStringFormat = (): void => {
    if (!String.prototype.format) {
        String.prototype.format = function (...args: unknown[]): string {
            return this.replace(/{(\d+)}/g, (match: string, number: string) =>
                typeof args[Number.parseInt(number, 10)] !== "undefined"
                    ? String(args[Number.parseInt(number, 10)])
                    : match,
            );
        };
    }
};

// Expose functions to global scope for testing
if (typeof window !== "undefined") {
    (
        window as typeof window & {
            setupNavBar: typeof setupNavBar;
            setupModal: typeof setupModal;
            openLink: typeof openLink;
            initStringFormat: typeof initStringFormat;
        }
    ).setupNavBar = setupNavBar;
    (
        window as typeof window & {
            setupModal: typeof setupModal;
        }
    ).setupModal = setupModal;
    (
        window as typeof window & {
            openLink: typeof openLink;
        }
    ).openLink = openLink;
    (
        window as typeof window & {
            initStringFormat: typeof initStringFormat;
        }
    ).initStringFormat = initStringFormat;
}

// セットアップの重複実行を防ぐフラグ
let isSetupDone = false;

const setupOnce = (): void => {
    if (!isSetupDone) {
        if (document.readyState === "loading") {
            // DOM読み込み中の場合は DOMContentLoaded を待つ
            const handler = (): void => {
                if (!isSetupDone) {
                    // 二重チェック
                    setupNavBar();
                    setupModal();
                    isSetupDone = true;
                }
                document.removeEventListener("DOMContentLoaded", handler);
            };
            document.addEventListener("DOMContentLoaded", handler);
        } else {
            // DOMが既に読み込まれている場合はすぐに実行
            setupNavBar();
            setupModal();
            isSetupDone = true;
        }
    }
};

// 後方互換性のためのエイリアス
const addEventNavBar = setupOnce;
const addEventModal = setupOnce;

// グローバルスコープに公開
if (typeof window !== "undefined") {
    (window as typeof window & { addEventNavBar: typeof addEventNavBar }).addEventNavBar =
        addEventNavBar;
    (window as typeof window & { addEventModal: typeof addEventModal }).addEventModal =
        addEventModal;
}

if (typeof document !== "undefined") {
    document.addEventListener("DOMContentLoaded", () => {
        setupOnce(); // setupNavBar() と setupModal() の代わりに setupOnce() を呼び出す
    });
    initStringFormat();
}
