const addEventNavBar = (): void => {
    document.addEventListener("DOMContentLoaded", () => {
        const navbarBurgers = Array.prototype.slice.call(
            document.querySelectorAll(".navbar-burger"),
            0,
        );

        // navbar
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
    });
};

const addEventModal = (): void => {
    document.addEventListener("DOMContentLoaded", () => {
        const modalTargets = Array.from(document.getElementsByClassName("modal-target"));
        const modalCloses = Array.prototype.slice.call(
            document.querySelectorAll(".delete, .modal-background"),
            0,
        );

        // modal
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
    });
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

if (!String.prototype.format) {
    String.prototype.format = function (...args: unknown[]): string {
        return this.replace(/{(\d+)}/g, (match: string, number: string) =>
            typeof args[Number.parseInt(number)] !== "undefined"
                ? String(args[Number.parseInt(number)])
                : match,
        );
    };
}
