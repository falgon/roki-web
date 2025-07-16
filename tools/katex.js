const fs = require("fs");
const katex = require("katex");

// ファイル読み込みのリトライ機能
function readFileWithRetry(fd, encoding, maxRetries = 3, delay = 100) {
    for (let attempt = 1; attempt <= maxRetries; attempt++) {
        try {
            return fs.readFileSync(fd, encoding);
        } catch (error) {
            if (error.code === 'EAGAIN' && attempt < maxRetries) {
                console.error(`EAGAINエラーが発生しました。${delay}ms後に再試行します... (${attempt}/${maxRetries})`);
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
}

try {
    const input = readFileWithRetry(process.stdin.fd, "utf-8");
    const displayMode = process.argv.includes("displayMode");
    const mathjaxOpt = {
        displayMode,
        trust: true,
        colorIsTextColor: true
    };

    process.stdout.write(katex.renderToString(input, mathjaxOpt));
} catch (error) {
    console.error("KaTeX処理中にエラーが発生しました:", error.message);
    process.exit(1);
}
