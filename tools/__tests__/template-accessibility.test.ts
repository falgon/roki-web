import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { describe, expect, it } from "vitest";

const parseTemplate = (path: string): Document =>
    new DOMParser().parseFromString(
        readFileSync(resolve(import.meta.dirname, "../../contents/templates", path), "utf8"),
        "text/html",
    );

describe("site and blog template accessibility", () => {
    it.each(["site/default.html", "blog/default.html"])("allows zoom in %s", (path) => {
        const viewport = parseTemplate(path).querySelector('meta[name="viewport"]');
        expect(viewport?.getAttribute("content")).toBe("width=device-width,initial-scale=1");
    });

    it.each(["pc", "sp"])("labels the profile image and social links for %s", (layout) => {
        const page = parseTemplate(`site/author-info/${layout}.html`);
        const profile = page.querySelector("img.blured-my-prof");
        expect(profile?.getAttribute("alt")).toContain("$author-name$");
        expect(profile?.getAttribute("src")).toBe("/images/avator/prof1000x1000.png");
        const links = Array.from(page.querySelectorAll(".social-links a"));
        expect(links.map((link) => link.getAttribute("aria-label"))).toEqual([
            "GitHub",
            "X (Twitter)",
            "Zenn",
            "note",
        ]);
    });

    it("marks decorative footer images with empty alt", () => {
        const decoration = parseTemplate("footer/dec.html").querySelector("img.footer-dec");
        const portrait = parseTemplate("blog/footer.html").querySelector("img.blured-my-prof");
        expect(decoration?.getAttribute("alt")).toBe("");
        expect(portrait?.getAttribute("alt")).toBe("");
    });

    it("names the actions for sharing or saving a blog post", () => {
        const links = Array.from(parseTemplate("blog/share-icons.html").querySelectorAll("a"));
        expect(links.map((link) => link.getAttribute("aria-label"))).toEqual([
            "Share on X (Twitter)",
            "Share on Facebook",
            "Share on LINE",
            "Save to Pocket",
        ]);
    });
});

describe("Disney template accessibility", () => {
    const page = parseTemplate("../pages/disney_experience_summary/jp.html");

    it("associates each tab with its panel", () => {
        const tabs = Array.from(
            page.querySelectorAll('[role="tablist"] > [role="presentation"] > [role="tab"]'),
        );
        expect(tabs).toHaveLength(4);
        for (const tab of tabs) {
            const panel = page.getElementById(tab.getAttribute("aria-controls") ?? "");
            expect(panel?.getAttribute("role")).toBe("tabpanel");
            expect(panel?.getAttribute("aria-labelledby")).toBe(tab.id);
        }
    });

    it("uses a named native button to open each hotel's logs", () => {
        const button = page.querySelector("[data-hotel-code] button.hotel-badge");
        expect(button?.getAttribute("type")).toBe("button");
        expect(button?.getAttribute("aria-label")).toContain("$hotel-code$");
    });
});
