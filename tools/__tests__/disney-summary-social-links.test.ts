import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { describe, expect, it } from "vitest";

describe("Disney Experience Summary social links", () => {
    it("shows TikTok after Instagram with matching icon size and spacing", () => {
        const page = document.createElement("div");
        page.innerHTML = readFileSync(
            resolve(import.meta.dirname, "../../contents/pages/disney_experience_summary/jp.html"),
            "utf8",
        );

        const links = Array.from(page.querySelectorAll(".profile-section .sns-links a"));
        expect(links.map((link) => link.getAttribute("href"))).toEqual([
            "/",
            "https://x.com/p0nchi_v",
            "https://www.instagram.com/ponchi.v/",
            "https://www.tiktok.com/@ponchi.v",
        ]);

        const tiktok = links[links.length - 1];
        expect(tiktok?.getAttribute("aria-label")).toBe("TikTok");
        expect(tiktok?.querySelector("i.fab.fa-tiktok.fa-lg")?.getAttribute("aria-hidden")).toBe(
            "true",
        );
        expect(links.map((link) => link.parentElement?.classList.contains("mr-4"))).toEqual([
            true,
            true,
            true,
            false,
        ]);
    });
});
