const addEventNavBar = (): void => {
    document.addEventListener("DOMContentLoaded", () => {
        const navbarBurgers = Array
            .prototype
            .slice.call(document.querySelectorAll(".navbar-burger"), 0);
            
        // navbar
        if (navbarBurgers.length > 0) {
            navbarBurgers.forEach((el: Element) => {
                el.addEventListener("click", () => {
                    const target = document.getElementById((el as HTMLElement).dataset.target!);
                    if (target) {
                        el.classList.toggle("is-active");
                        target.classList.toggle("is-active");
                    }
                });
            });
        }
    });
}

const addEventModal = (): void => {
    document.addEventListener("DOMContentLoaded", () => {
        const modalTargets = Array.from(document.getElementsByClassName("modal-target"));
        const modalCloses = Array
        .prototype
        .slice.call(document.querySelectorAll(".delete, .modal-background"), 0);
    
        // modal
        modalTargets.forEach((modalTarget: Element) => {
            modalTarget.addEventListener("click", () => {
                const target = document.getElementById((modalTarget as HTMLElement).dataset.target!);
                if (target) {
                    target.classList.add("is-active");
                }
            });
        });

        if (modalCloses.length > 0) {
            modalCloses.forEach((el: Element) => {
                el.addEventListener("click", () => { 
                    modalTargets.forEach((modalTarget: Element) => {
                        const target = document.getElementById((modalTarget as HTMLElement).dataset.target!);
                        if (target) {
                            target.classList.remove("is-active");
                        }
                    });
                });
            })
        }
    });
}

const openLink = (): void => {
    const ls = Array.from(document.getElementsByClassName("open_links"));
    ls.forEach((l: Element) => {
        const element = l as HTMLElement;
        element.style.display = (element.style.display === "none") ? "block" : "none";
    });
}

// String.prototype.formatの型定義を拡張
interface String {
    format(...args: any[]): string;
}

if (!(String.prototype as any).format) {
    (String.prototype as any).format = function(...args: any[]): string {
        return this.replace(/{(\d+)}/g, function(match: string, number: string) { 
            return typeof args[parseInt(number)] !== "undefined"
                ? args[parseInt(number)]
                : match
            ;
        });
    };
} 