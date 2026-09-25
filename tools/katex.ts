import * as fs from "node:fs";
import { renderBatchJson, renderMath } from "./katex-render";

// ファイル読み込みのリトライ機能
function readFileWithRetry(
    fd: number,
    encoding: BufferEncoding,
    maxRetries: number = 3,
    delay: number = 100,
): string {
    for (let attempt = 1; attempt <= maxRetries; attempt++) {
        try {
            return fs.readFileSync(fd, encoding);
        } catch (error: unknown) {
            if (
                error instanceof Error &&
                "code" in error &&
                error.code === "EAGAIN" &&
                attempt < maxRetries
            ) {
                console.error(
                    `EAGAINエラーが発生しました。${delay}ms後に再試行します... (${attempt}/${maxRetries})`,
                );
                // 短い遅延を入れてから再試行
                const start = Date.now();
                while (Date.now() - start < delay) {
                    // ビジーウェイト
                }
                continue;
            }
            throw error;
        }
    }
    throw new Error("最大リトライ回数に達しました");
}

try {
    const input = readFileWithRetry(process.stdin.fd, "utf-8");
    const batchMode = process.argv.includes("--batch");
    const displayMode = process.argv.includes("displayMode");

    process.stdout.write(batchMode ? renderBatchJson(input) : renderMath(input, displayMode));
} catch (error: unknown) {
    const message = error instanceof Error ? error.message : String(error);
    console.error("KaTeX処理中にエラーが発生しました:", message);
    process.exit(1);
}
