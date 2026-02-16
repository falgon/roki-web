#!/usr/bin/env tsx

import { spawn } from "node:child_process";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

type Platform = "x" | "instagram";
type BrowserContext = import("playwright").BrowserContext;
type Page = import("playwright").Page;

interface TargetUrl {
    platform: Platform;
    url: string;
}

interface SnsContent {
    platform: Platform;
    url: string;
    title: string;
    text: string;
    postedAt: string | null;
    author: string | null;
    fetchError: string | null;
}

interface ParsedArgs {
    urls: string[];
    showHelp: boolean;
    codexModel: string | null;
}

const SCRIPT_DIR = path.dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = path.resolve(SCRIPT_DIR, "..");
const MAX_TEXT_LENGTH = 12000;
const LOG_PREFIX = "[gen-disney-logs]";
const DEFAULT_CODEX_CONFIG_PATH = path.join(os.homedir(), ".codex", "config.toml");
const DISNEY_LOGS_PROMPT = `あなたは roki-web リポジトリ内で Disney 体験録を作成するエージェントです。

## 目的
取得済みSNS投稿データを根拠に、体験録 Markdown を1件作成する。

## 出力ファイルルール
- 作成先は \`contents/disney_experience_summary/logs/{番号}/index.md\`。
- まず既存の番号を調査し、次に使う番号を決定する。
- 予定投稿や明確な外れ値と思われる番号に引きずられず、連続性を重視して判断する。
- 既存ファイルの上書きは禁止。
- \`mkdir -p contents/disney_experience_summary/logs/{番号}\` を実行してから作成する。
- 作成後に \`test -f contents/disney_experience_summary/logs/{番号}/index.md\` で存在確認する。

## frontmatter 仕様
- \`title\`: 投稿内容を要約した自然なタイトル。
- \`date\`: YYYY-MM-DD（複数投稿なら最古日）。
- \`instagram\`: Instagram URL が1件以上ある場合のみ、カンマ区切りで設定。
- \`x\`: X URL が1件以上ある場合のみ、カンマ区切りで設定。
- \`disney-tags\`: 内容から判定可能なタグをカンマ区切りで設定。
- \`images\`: 必要時のみ設定。画像ファイル名をカンマ区切りで指定（例: \`foo.jpg, bar.png\`）。
- \`ai-generated-by\`: 必須。
- \`ai-generated\`: 使用禁止。

## タグ候補
- FSH, TDH, DHM, TSH, DAH, TDL, TDS, DOI

## 本文ルール
- 取得済みデータに書かれている事実だけを使う。
- 箇条書きは禁止。自然な地の文で2〜4段落程度。
- 文体は「だ・である調」を基調に、読みやすいリズムを維持する。

## 最終出力
- 最終出力は「作成済みファイルの Markdown 本文のみ」を返す。
`;

function showUsage(): void {
    console.error("使用方法: tsx tools/gen-disney-logs.ts [--codex-model <model>] <url-1> [<url-2> ...]");
    console.error("対応URL: x.com / twitter.com / instagram.com");
}

function reportProgress(message: string): void {
    const timestamp = new Date().toISOString();
    console.error(`${LOG_PREFIX} ${timestamp} ${message}`);
}

function normalizeText(value: string | null | undefined): string {
    return (value ?? "").replace(/\s+/gu, " ").trim();
}

function parseArgs(args: string[]): ParsedArgs {
    const urls: string[] = [];
    let showHelp = false;
    let codexModel: string | null = null;

    for (let index = 0; index < args.length; index += 1) {
        const arg = args[index] ?? "";
        if (arg === "--help" || arg === "-h") {
            showHelp = true;
            continue;
        }
        if (arg === "--codex-model") {
            const nextArg = args[index + 1];
            if (!nextArg) {
                throw new Error("--codex-model にはモデル名を指定してください。");
            }
            codexModel = normalizeText(nextArg);
            index += 1;
            continue;
        }
        if (arg.startsWith("--codex-model=")) {
            codexModel = normalizeText(arg.slice("--codex-model=".length));
            if (codexModel.length === 0) {
                throw new Error("--codex-model= にはモデル名を指定してください。");
            }
            continue;
        }
        urls.push(arg);
    }

    return { urls, showHelp, codexModel };
}

function detectPlatform(rawUrl: string): TargetUrl {
    let parsedUrl: URL;
    try {
        parsedUrl = new URL(rawUrl);
    } catch {
        throw new Error(`不正なURLです: ${rawUrl}`);
    }

    const host = parsedUrl.hostname.toLowerCase();
    if (
        host === "x.com" ||
        host.endsWith(".x.com") ||
        host === "twitter.com" ||
        host.endsWith(".twitter.com")
    ) {
        return { platform: "x", url: parsedUrl.toString() };
    }
    if (host === "instagram.com" || host.endsWith(".instagram.com")) {
        return { platform: "instagram", url: parsedUrl.toString() };
    }
    throw new Error(`非対応ドメインです: ${rawUrl}`);
}

function trimText(value: string): string {
    return value.length <= MAX_TEXT_LENGTH ? value : `${value.slice(0, MAX_TEXT_LENGTH)}...`;
}

function parseModelFromCodexConfig(configText: string): string | null {
    const matched = configText.match(/^\s*model\s*=\s*"([^"]+)"/mu);
    const modelName = normalizeText(matched?.[1]);
    return modelName.length > 0 ? modelName : null;
}

async function resolveCodexModelName(cliModel: string | null): Promise<string> {
    const cliModelName = normalizeText(cliModel);
    if (cliModelName.length > 0) {
        return cliModelName;
    }

    const envModelName = normalizeText(process.env.CODEX_MODEL);
    if (envModelName.length > 0) {
        return envModelName;
    }

    try {
        const configText = await readFile(DEFAULT_CODEX_CONFIG_PATH, "utf8");
        const configModel = parseModelFromCodexConfig(configText);
        if (configModel) {
            return configModel;
        }
    } catch (error: unknown) {
        const nodeError = error as NodeJS.ErrnoException;
        if (nodeError.code !== "ENOENT") {
            throw error;
        }
    }

    throw new Error(
        "Codexモデル名を解決できませんでした。--codex-model または環境変数 CODEX_MODEL を指定してください。",
    );
}

function parseXStatusId(targetUrl: string): string | null {
    const matched = targetUrl.match(/\/status\/(\d+)/u);
    return matched?.[1] ?? null;
}

function decodeXSnowflakeToIso(statusId: string): string | null {
    try {
        const createdAtMs = Number((BigInt(statusId) >> 22n) + 1288834974657n);
        return Number.isFinite(createdAtMs) ? new Date(createdAtMs).toISOString() : null;
    } catch {
        return null;
    }
}

function chooseFirstNonEmpty(...values: Array<string | null | undefined>): string {
    const found = values.map((value) => normalizeText(value)).find((value) => value.length > 0);
    return found ?? "";
}

function extractAuthorFromInstagramDescription(description: string): string | null {
    const matched = description.match(/^([^:]+)\s+on Instagram:/u);
    return matched?.[1]?.trim() ?? null;
}

async function extractXContent(page: Page, targetUrl: string): Promise<SnsContent> {
    const statusId = parseXStatusId(targetUrl);
    const snapshot = await page.evaluate((targetStatusId) => {
        const title = document.title ?? "";
        const description =
            document.querySelector('meta[property="og:description"]')?.getAttribute("content") ??
            "";
        const articles = Array.from(document.querySelectorAll("article"));
        const targetArticle = targetStatusId
            ? (articles.find((article) =>
                  Array.from(article.querySelectorAll('a[href*="/status/"]')).some((anchor) =>
                      (anchor.getAttribute("href") ?? "").includes(`/status/${targetStatusId}`),
                  ),
              ) ?? articles[0])
            : articles[0];
        const tweetTextNodes =
            targetArticle?.querySelectorAll('div[data-testid="tweetText"] span') ?? [];
        const statusLink = targetStatusId
            ? targetArticle?.querySelector(`a[href*="/status/${targetStatusId}"]`)
            : null;
        const postedAt =
            statusLink?.querySelector("time")?.getAttribute("datetime") ??
            targetArticle?.querySelector("time")?.getAttribute("datetime") ??
            document
                .querySelector('meta[property="article:published_time"]')
                ?.getAttribute("content") ??
            "";
        const author =
            targetArticle?.querySelector('a[role="link"][href^="/"] span')?.textContent ??
            document.querySelector('meta[property="twitter:title"]')?.getAttribute("content") ??
            "";
        const bodyText = targetArticle?.innerText ?? document.body?.innerText ?? "";

        return {
            author,
            bodyText,
            description,
            postedAt,
            title,
            tweetText: Array.from(tweetTextNodes)
                .map((node) => node.textContent ?? "")
                .join(" ")
                .trim(),
        };
    }, statusId);

    const normalizedPostedAt = normalizeText(snapshot.postedAt);
    // Xの引用投稿を拾うと日時がずれるケースがあるため、status ID 由来の日時を優先する。
    const fallbackPostedAt = statusId ? decodeXSnowflakeToIso(statusId) : null;

    return {
        platform: "x",
        url: targetUrl,
        title: chooseFirstNonEmpty(snapshot.title, snapshot.description),
        text: trimText(
            chooseFirstNonEmpty(snapshot.tweetText, snapshot.description, snapshot.bodyText),
        ),
        postedAt: fallbackPostedAt || normalizedPostedAt || null,
        author: normalizeText(snapshot.author) || null,
        fetchError: null,
    };
}

async function extractInstagramContent(page: Page, targetUrl: string): Promise<SnsContent> {
    const snapshot = await page.evaluate(() => {
        const title = document.title ?? "";
        const description =
            document.querySelector('meta[property="og:description"]')?.getAttribute("content") ??
            "";
        const articleText = document.querySelector("article")?.innerText ?? "";
        const bodyText = document.body?.innerText ?? "";
        const postedAt =
            document.querySelector("time")?.getAttribute("datetime") ??
            document
                .querySelector('meta[property="article:published_time"]')
                ?.getAttribute("content") ??
            "";
        const author =
            document.querySelector('header a[role="link"]')?.textContent ??
            document.querySelector('meta[name="twitter:title"]')?.getAttribute("content") ??
            "";

        return {
            articleText,
            author,
            bodyText,
            description,
            postedAt,
            title,
        };
    });

    return {
        platform: "instagram",
        url: targetUrl,
        title: chooseFirstNonEmpty(snapshot.title, snapshot.description),
        text: trimText(
            chooseFirstNonEmpty(snapshot.articleText, snapshot.description, snapshot.bodyText),
        ),
        postedAt: normalizeText(snapshot.postedAt) || null,
        author:
            normalizeText(snapshot.author) ||
            extractAuthorFromInstagramDescription(snapshot.description) ||
            null,
        fetchError: null,
    };
}

function stringifyError(error: unknown): string {
    if (error instanceof Error) {
        return error.message;
    }
    return String(error);
}

async function fetchSingleContent(context: BrowserContext, target: TargetUrl): Promise<SnsContent> {
    const page = await context.newPage();
    try {
        await page.goto(target.url, {
            timeout: 45_000,
            waitUntil: "domcontentloaded",
        });
        await page.waitForLoadState("networkidle", { timeout: 15_000 }).catch(() => undefined);
        await page.waitForTimeout(1_000);
        return target.platform === "x"
            ? await extractXContent(page, target.url)
            : await extractInstagramContent(page, target.url);
    } catch (error: unknown) {
        return {
            platform: target.platform,
            url: target.url,
            title: "",
            text: "",
            postedAt: null,
            author: null,
            fetchError: stringifyError(error),
        };
    } finally {
        await page.close();
    }
}

async function fetchAllContents(targets: TargetUrl[]): Promise<SnsContent[]> {
    let playwright: typeof import("playwright");
    try {
        playwright = await import("playwright");
    } catch {
        throw new Error(
            "playwright パッケージを読み込めません。`npm install --save-dev playwright` を実行してください。",
        );
    }

    const browser = await playwright.chromium.launch({ headless: true });
    const context = await browser.newContext({
        locale: "ja-JP",
        timezoneId: "Asia/Tokyo",
        userAgent:
            "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36",
    });

    try {
        const contents: SnsContent[] = [];
        for (const [index, target] of targets.entries()) {
            reportProgress(`[${index + 1}/${targets.length}] 取得開始: ${target.url}`);
            const content = await fetchSingleContent(context, target);
            if (content.fetchError === null) {
                reportProgress(`[${index + 1}/${targets.length}] 取得成功: ${target.url}`);
            } else {
                reportProgress(
                    `[${index + 1}/${targets.length}] 取得失敗: ${target.url} (${content.fetchError})`,
                );
            }
            contents.push(content);
        }
        const successCount = contents.filter((content) => content.fetchError === null).length;
        reportProgress(
            `投稿取得完了: 成功 ${successCount} 件 / 失敗 ${contents.length - successCount} 件`,
        );
        return contents;
    } finally {
        await context.close();
        await browser.close();
    }
}

function buildCodexPrompt(contents: SnsContent[], codexModelName: string): string {
    const serializedContents = JSON.stringify(contents, null, 2);
    return `${DISNEY_LOGS_PROMPT}

## 追加コンテキスト
- SNS 投稿の取得処理は完了済み。下記JSONだけを根拠として使用する。
- \`ai-generated-by\` には必ず \`${codexModelName}\` を設定する。

\`\`\`json
${serializedContents}
\`\`\`
`;
}

async function runCodexExec(prompt: string, codexModelName: string): Promise<string> {
    const tempDirectory = await mkdtemp(path.join(os.tmpdir(), "gen-disney-logs-"));
    const outputPath = path.join(tempDirectory, "codex-last-message.md");

    try {
        reportProgress(`codex exec を開始します。model=${codexModelName}`);
        await new Promise<void>((resolve, reject) => {
            const child = spawn(
                "codex",
                [
                    "exec",
                    "--skip-git-repo-check",
                    "--sandbox",
                    "workspace-write",
                    "--color",
                    "never",
                    "-m",
                    codexModelName,
                    "-o",
                    outputPath,
                    "-",
                ],
                {
                    cwd: REPO_ROOT,
                    stdio: ["pipe", "ignore", "inherit"],
                },
            );

            child.on("error", reject);
            child.stdin.write(prompt);
            child.stdin.end();
            child.on("close", (code) => {
                if (code === 0) {
                    resolve();
                    return;
                }
                reject(new Error(`codex exec が失敗しました (exit code: ${String(code)})`));
            });
        });

        reportProgress("codex exec が完了しました。");
        return await readFile(outputPath, "utf8");
    } finally {
        await rm(tempDirectory, { force: true, recursive: true });
    }
}

async function main(): Promise<void> {
    const parsedArgs = parseArgs(process.argv.slice(2));
    if (parsedArgs.showHelp || parsedArgs.urls.length === 0) {
        showUsage();
        process.exit(parsedArgs.showHelp ? 0 : 1);
    }

    const codexModelName = await resolveCodexModelName(parsedArgs.codexModel);
    reportProgress(`処理を開始します。入力URL数: ${parsedArgs.urls.length}`);
    reportProgress(`利用するCodexモデル: ${codexModelName}`);
    const targets = [...new Set(parsedArgs.urls)].map((rawUrl) => detectPlatform(rawUrl));
    reportProgress(`重複除去後のURL数: ${targets.length}`);
    reportProgress("SNS投稿内容の取得を開始します。");
    const contents = await fetchAllContents(targets);
    if (contents.every((content) => content.fetchError !== null)) {
        const errorDetails = contents
            .map(
                (content) =>
                    `- ${content.url}: ${content.fetchError ?? "不明なエラーで取得に失敗しました。"}`,
            )
            .join("\n");
        throw new Error(`すべてのURLで投稿内容の取得に失敗しました。\n${errorDetails}`);
    }

    reportProgress("プロンプトを組み立てます。");
    const codexPrompt = buildCodexPrompt(contents, codexModelName);
    const markdown = (await runCodexExec(codexPrompt, codexModelName)).trim();
    reportProgress("処理全体が完了しました。");
    process.stdout.write(`${markdown}\n`);
}

main().catch((error: unknown) => {
    const message = stringifyError(error);
    console.error(`エラー: ${message}`);
    process.exit(1);
});
