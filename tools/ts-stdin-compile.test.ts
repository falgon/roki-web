// @vitest-environment node

import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { compileTypeScript } from "./ts-stdin-transform";

const cliPath = fileURLToPath(new URL("./ts-stdin-compile.sh", import.meta.url));

function runCli(input: string) {
    return spawnSync(cliPath, {
        encoding: "utf8",
        input,
    });
}

describe("compileTypeScript", () => {
    it("transpiles TypeScript while preserving the browser global scope", async () => {
        const output = await compileTypeScript("const answer: number = 42;");

        expect(output).toMatch(/^"use strict";\n/);
        expect(output.trimEnd().endsWith("const answer = 42;")).toBe(true);
    });

    it("targets ES2020 and removes comments", async () => {
        const output = await compileTypeScript(`
            // This comment must not be emitted.
            const value: { nested?: number } = {};
            globalThis.result = value.nested ?? 0;
        `);

        expect(output).toContain("value.nested ?? 0");
        expect(output).not.toContain("This comment must not be emitted");
    });

    it("rejects invalid TypeScript syntax", async () => {
        const transform = compileTypeScript("const value: = 1;");

        await expect(transform).rejects.toBeInstanceOf(Error);
        await expect(transform).rejects.toMatchObject({
            errors: expect.arrayContaining([
                expect.objectContaining({
                    location: expect.objectContaining({ line: 1 }),
                    text: expect.stringMatching(/\S/),
                }),
            ]),
        });
    });

    it("preserves TypeScript's ES2020 class-field assignment semantics", async () => {
        const output = await compileTypeScript(`
            let observed = 0;
            class Base {
                set value(next: number) {
                    observed = next;
                }
            }
            class Derived extends Base {
                value = 7;
            }
            new Derived();
        `);

        expect(Function(`${output}\nreturn observed;`)()).toBe(7);
    });
});

describe("ts-stdin-compile CLI", () => {
    it("compiles stdin to stdout", () => {
        const result = runCli("const answer: number = 42;");

        expect(result.status).toBe(0);
        expect(result.stdout).toMatch(/^"use strict";\n/);
        expect(result.stdout.trimEnd().endsWith("const answer = 42;")).toBe(true);
    });

    it("preserves large UTF-8 input across stdin chunks", { timeout: 10_000 }, () => {
        const payload = "あ".repeat(70_000);
        const result = runCli(`const payload: string = ${JSON.stringify(payload)};`);

        expect(result.status).toBe(0);
        expect(result.stdout).toMatch(/^"use strict";\n/);
        expect(Function(`${result.stdout}\nreturn payload;`)()).toBe(payload);
    });

    it("reports invalid syntax with a non-zero exit code", () => {
        const result = runCli("const value: = 1;");

        expect(result.status).toBe(1);
        expect(result.stdout).toBe("");
        expect(result.stderr).not.toBe("");
    });
});
