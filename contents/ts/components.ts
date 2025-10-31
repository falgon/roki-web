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
interface String {
    format(...args: unknown[]): string;
}

interface StringConstructor {
    prototype: {
        format?: (...args: unknown[]) => string;
    };
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

if (typeof document !== "undefined") {
    document.addEventListener("DOMContentLoaded", () => {
        setupNavBar();
        setupModal();
    });
    initStringFormat();
}
