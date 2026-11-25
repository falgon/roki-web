import { describe, expect, it, vi } from "vitest";

describe("Font Awesome icon export", () => {
    it("exports legacy icon names as the same SVGs as their canonical names", async () => {
        const write = vi.spyOn(process.stdout, "write").mockReturnValue(true);
        let output: string;
        try {
            await import("../fontawesome");
            expect(write).toHaveBeenCalledOnce();
            output = String(write.mock.calls[0]?.[0]);
        } finally {
            write.mockRestore();
        }
        const { fas } = JSON.parse(output);

        for (const [alias, canonical] of [
            ["info-circle", "circle-info"],
            ["question-circle", "circle-question"],
            ["search", "magnifying-glass"],
        ] as const) {
            expect(fas[alias]).toMatchObject({
                tag: "svg",
                attributes: { "data-icon": canonical },
            });
            expect(fas[alias]).toEqual(fas[canonical]);
        }
    });
});
