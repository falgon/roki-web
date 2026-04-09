// @vitest-environment node

import { chmod, mkdtemp, rm, writeFile } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import { afterEach, describe, expect, it } from "vitest";
import { parseArgs, runCodexExec } from "../gen-disney-logs";

const tempDirectories: string[] = [];

async function createMockCodex(scriptBody: string): Promise<string> {
    const tempDirectory = await mkdtemp(path.join(os.tmpdir(), "gen-disney-logs-test-"));
    const scriptPath = path.join(tempDirectory, "codex");
    await writeFile(scriptPath, scriptBody, "utf8");
    await chmod(scriptPath, 0o755);
    tempDirectories.push(tempDirectory);
    return tempDirectory;
}

afterEach(async () => {
    await Promise.all(
        tempDirectories
            .splice(0)
            .map((directory) => rm(directory, { force: true, recursive: true })),
    );
});

describe("parseArgs", () => {
    it("Codex モデル名とタイムアウト秒数を解析できる", () => {
        expect(
            parseArgs([
                "--codex-model",
                "gpt-5.4",
                "--codex-timeout-seconds",
                "120",
                "https://x.com/example/status/1",
            ]),
        ).toMatchObject({
            codexModel: "gpt-5.4",
            codexTimeoutSeconds: 120,
            urls: ["https://x.com/example/status/1"],
        });
    });

    it("= 形式の Codex タイムアウト秒数を解析できる", () => {
        expect(
            parseArgs([
                "--codex-model=gpt-5.4-mini",
                "--codex-timeout-seconds=45",
                "https://www.instagram.com/p/example/",
            ]),
        ).toMatchObject({
            codexModel: "gpt-5.4-mini",
            codexTimeoutSeconds: 45,
            urls: ["https://www.instagram.com/p/example/"],
        });
    });

    it("不正な Codex タイムアウト秒数を拒否する", () => {
        expect(() =>
            parseArgs(["--codex-timeout-seconds", "0", "https://x.com/example/status/1"]),
        ).toThrow("--codex-timeout-seconds には 1 以上の整数を指定してください。");
    });
});

describe("runCodexExec", () => {
    it("指定モデルで codex exec を実行して結果を返す", async () => {
        const mockBinDirectory = await createMockCodex(`#!/bin/sh
output=""
while [ "$#" -gt 0 ]; do
  case "$1" in
    -o)
      output="$2"
      shift 2
      ;;
    *)
      shift
      ;;
  esac
done
cat >/dev/null
printf 'generated markdown\\n' > "$output"
`);
        const previousPath = process.env.PATH ?? "";
        process.env.PATH = `${mockBinDirectory}:${previousPath}`;

        try {
            await expect(runCodexExec("prompt", "gpt-5.4", 5)).resolves.toBe(
                "generated markdown\n",
            );
        } finally {
            process.env.PATH = previousPath;
        }
    });

    it("タイムアウト超過時は失敗する", async () => {
        const mockBinDirectory = await createMockCodex(`#!/bin/sh
sleep 2
`);
        const previousPath = process.env.PATH ?? "";
        process.env.PATH = `${mockBinDirectory}:${previousPath}`;

        try {
            await expect(runCodexExec("prompt", "gpt-5.4", 1)).rejects.toThrow(
                "codex exec がタイムアウトしました (1 秒)",
            );
        } finally {
            process.env.PATH = previousPath;
        }
    });

    it("SIGTERM を捕捉して 0 終了してもタイムアウト扱いにする", async () => {
        const mockBinDirectory = await createMockCodex(`#!/bin/sh
trap 'exit 0' TERM
sleep 2
`);
        const previousPath = process.env.PATH ?? "";
        process.env.PATH = `${mockBinDirectory}:${previousPath}`;

        try {
            await expect(runCodexExec("prompt", "gpt-5.4", 1)).rejects.toThrow(
                "codex exec がタイムアウトしました (1 秒)",
            );
        } finally {
            process.env.PATH = previousPath;
        }
    });
});
