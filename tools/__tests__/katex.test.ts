// @vitest-environment node

import { spawnSync } from "node:child_process";
import {
    chmodSync,
    mkdirSync,
    mkdtempSync,
    readdirSync,
    readFileSync,
    rmSync,
    writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import {
    parseBatchRenderRequests,
    renderBatch,
    renderBatchJson,
    renderMath,
} from "../katex-render";

const cliPath = fileURLToPath(new URL("../katex_runner.sh", import.meta.url));

function runCli(input: string, args: string[] = []) {
    return spawnSync(cliPath, args, {
        encoding: "utf8",
        input,
    });
}

describe("KaTeX rendering", () => {
    it("keeps single-input CLI compatibility", () => {
        const result = runCli("x+y");

        expect(result.status).toBe(0);
        expect(result.stdout).toBe(renderMath("x+y", false));
        expect(result.stderr).toBe("");
    });

    it("renders display mode through the existing CLI argument", () => {
        const result = runCli("\\sum_{i=1}^{n} i", ["displayMode"]);

        expect(result.status).toBe(0);
        expect(result.stdout).toBe(renderMath("\\sum_{i=1}^{n} i", true));
        expect(result.stdout).toContain("katex-display");
    });

    it("renders batch requests in order with mixed display modes and UTF-8 math", () => {
        const requests = [
            { index: 0, math: "x+y", displayMode: false },
            { index: 1, math: "\\sum_{i=1}^{n} i", displayMode: true },
            { index: 2, math: "\\text{日本語}+α", displayMode: false },
        ];

        const responses = renderBatch(requests);

        expect(responses.map((response) => response.index)).toEqual([0, 1, 2]);
        expect(responses[0]?.html).toBe(renderMath("x+y", false));
        expect(responses[1]?.html).toBe(renderMath("\\sum_{i=1}^{n} i", true));
        expect(responses[1]?.html).toContain("katex-display");
        expect(responses[2]?.html).toContain("日本語");
    });

    it("supports --batch in the runner and preserves UTF-8 over stdin/stdout", () => {
        const requests = [
            { index: 0, math: "\\text{寿司}", displayMode: false },
            { index: 1, math: "\\frac{1}{2}", displayMode: true },
        ];
        const result = runCli(JSON.stringify(requests), ["--batch"]);

        expect(result.status).toBe(0);
        expect(JSON.parse(result.stdout)).toEqual(
            JSON.parse(renderBatchJson(JSON.stringify(requests))),
        );
        expect(result.stdout).toContain("寿司");
    });

    it("rejects malformed batch JSON", () => {
        expect(() => parseBatchRenderRequests(JSON.stringify({ index: 0 }))).toThrow(
            "KaTeX batch request must be a JSON array",
        );
        expect(() => parseBatchRenderRequests(JSON.stringify([{ index: "0" }]))).toThrow(
            "KaTeX batch request item has an invalid shape",
        );
    });

    it("fails the whole batch on a KaTeX error", () => {
        expect(() =>
            renderBatchJson(
                JSON.stringify([{ index: 0, math: "\\notACommand", displayMode: false }]),
            ),
        ).toThrow();
    });

    it("does not share user-defined macros between formulas in a batch", () => {
        expect(() =>
            renderBatchJson(
                JSON.stringify([
                    { index: 0, math: "\\gdef\\foo{x}", displayMode: false },
                    { index: 1, math: "\\foo", displayMode: false },
                ]),
            ),
        ).toThrow(/Undefined control sequence|KaTeX parse error/);
    });

    it("replays stdin on retry, keeps failed stdout out, preserves successful stderr, and cleans runner temp files", () => {
        const testDir = mkdtempSync(join(tmpdir(), "katex-runner-test-"));
        const binDir = join(testDir, "bin");
        const runnerTmpDir = join(testDir, "runner-tmp");
        const stateDir = join(testDir, "state");
        mkdirSync(binDir);
        mkdirSync(runnerTmpDir);
        mkdirSync(stateDir);

        try {
            const fakeNpx = join(binDir, "npx");
            writeFileSync(
                fakeNpx,
                `#!/usr/bin/env bash
set -eu
attempt_file="$FAKE_NPX_STATE_DIR/attempt"
first_input="$FAKE_NPX_STATE_DIR/first-input"
second_input="$FAKE_NPX_STATE_DIR/second-input"
attempt=0
if [ -f "$attempt_file" ]; then
    attempt="$(cat "$attempt_file")"
fi
attempt=$((attempt + 1))
printf '%s' "$attempt" > "$attempt_file"
input="$(cat)"
if [ "$attempt" -eq 1 ]; then
    printf '%s' "$input" > "$first_input"
    printf 'partial stdout from failed attempt'
    printf 'synthetic failure\\n' >&2
    exit 1
fi
printf '%s' "$input" > "$second_input"
cmp "$first_input" "$second_input" >&2
printf 'successful warning\\n' >&2
printf 'success stdout only'
`,
            );
            chmodSync(fakeNpx, 0o755);

            const payload = "retry-payload-寿司";
            const result = spawnSync(cliPath, ["--batch"], {
                encoding: "utf8",
                env: {
                    ...process.env,
                    FAKE_NPX_STATE_DIR: stateDir,
                    PATH: `${binDir}:${process.env.PATH ?? ""}`,
                    TMPDIR: runnerTmpDir,
                },
                input: payload,
            });

            expect(result.status).toBe(0);
            expect(result.stdout).toBe("success stdout only");
            expect(result.stdout).not.toContain("partial stdout");
            expect(result.stderr).toContain("synthetic failure");
            expect(result.stderr).toContain("successful warning");
            expect(readFileSync(join(stateDir, "first-input"), "utf8")).toBe(payload);
            expect(readFileSync(join(stateDir, "second-input"), "utf8")).toBe(payload);
            expect(readdirSync(runnerTmpDir)).toEqual([]);
        } finally {
            rmSync(testDir, { recursive: true, force: true });
        }
    });

    it("returns a failure status with empty stdout when every retry fails", () => {
        const testDir = mkdtempSync(join(tmpdir(), "katex-runner-fail-test-"));
        const binDir = join(testDir, "bin");
        const runnerTmpDir = join(testDir, "runner-tmp");
        const stateDir = join(testDir, "state");
        mkdirSync(binDir);
        mkdirSync(runnerTmpDir);
        mkdirSync(stateDir);

        try {
            const fakeNpx = join(binDir, "npx");
            writeFileSync(
                fakeNpx,
                `#!/usr/bin/env bash
set -eu
attempt_file="$FAKE_NPX_STATE_DIR/attempt"
attempt=0
if [ -f "$attempt_file" ]; then
    attempt="$(cat "$attempt_file")"
fi
attempt=$((attempt + 1))
printf '%s' "$attempt" > "$attempt_file"
cat > /dev/null
printf 'partial failed stdout attempt %s' "$attempt"
printf 'failure attempt %s\\n' "$attempt" >&2
exit 42
`,
            );
            chmodSync(fakeNpx, 0o755);

            const result = spawnSync(cliPath, ["--batch"], {
                encoding: "utf8",
                env: {
                    ...process.env,
                    FAKE_NPX_STATE_DIR: stateDir,
                    PATH: `${binDir}:${process.env.PATH ?? ""}`,
                    TMPDIR: runnerTmpDir,
                },
                input: "always-fails",
            });

            expect(result.status).toBe(42);
            expect(result.stdout).toBe("");
            expect(result.stderr).toContain("failure attempt 1");
            expect(result.stderr).toContain("failure attempt 2");
            expect(result.stderr).toContain("failure attempt 3");
            expect(readFileSync(join(stateDir, "attempt"), "utf8")).toBe("3");
            expect(readdirSync(runnerTmpDir)).toEqual([]);
        } finally {
            rmSync(testDir, { recursive: true, force: true });
        }
    });
});
