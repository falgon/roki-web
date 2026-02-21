#!/usr/bin/env tsx

import { spawn } from "node:child_process";
import type { Dirent } from "node:fs";
import { mkdir, mkdtemp, readdir, readFile, rm, writeFile } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { config as loadDotenv } from "dotenv";

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
    imageUrls: string[];
    fetchError: string | null;
}

interface ParsedArgs {
    urls: string[];
    showHelp: boolean;
    codexModel: string | null;
    selectFirstInstagramImageWithoutHumanCloseup: boolean;
    instagramMinImageCount: number;
}

interface InstagramGraphContext {
    accessToken: string;
    userId: string;
}

interface DownloadInstagramImagesOptions {
    codexModelName: string;
    selectFirstImageWithoutHumanCloseup: boolean;
    minimumImageCount: number;
}

const SCRIPT_DIR = path.dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = path.resolve(SCRIPT_DIR, "..");
const DOTENV_PATH = path.join(REPO_ROOT, ".env");
const MAX_TEXT_LENGTH = 12000;
const DEFAULT_INSTAGRAM_MIN_IMAGE_COUNT = 3;
const LOG_PREFIX = "[gen-disney-logs]";
const INSTAGRAM_ACCESS_TOKEN_ENV_KEY = "INSTAGRAM_ACCESS_TOKEN";
const INSTAGRAM_USER_ID_ENV_KEY = "INSTAGRAM_USER_ID";
const INSTAGRAM_AUTHORIZATION_CODE_ENV_KEY = "INSTAGRAM_AUTHORIZATION_CODE";
const DEFAULT_CODEX_CONFIG_PATH = path.join(os.homedir(), ".codex", "config.toml");
const DISNEY_LOGS_ROOT = path.join(REPO_ROOT, "contents", "disney_experience_summary", "logs");
const DEFAULT_INSTAGRAM_API_VERSION = "v24.0";
const INSTAGRAM_OAUTH_AUTHORIZE_ENDPOINT = "https://www.instagram.com/oauth/authorize";
const INSTAGRAM_OAUTH_ACCESS_TOKEN_ENDPOINT = "https://api.instagram.com/oauth/access_token";
const INSTAGRAM_LONG_LIVED_TOKEN_ENDPOINT = "https://graph.instagram.com/access_token";
const INSTAGRAM_REFRESH_TOKEN_ENDPOINT = "https://graph.instagram.com/refresh_access_token";
const INSTAGRAM_ME_ENDPOINT = "https://graph.instagram.com/me";
const INSTAGRAM_MEDIA_LIMIT_PER_PAGE = 50;
const INSTAGRAM_MEDIA_MAX_PAGE_COUNT = 20;
const WEBHOOK_SITE_API_BASE_URL = "https://webhook.site";
const WEBHOOK_SITE_POLL_INTERVAL_MS = 2_000;
const WEBHOOK_SITE_WAIT_SECONDS = 180;
const INSTAGRAM_MEDIA_FIELDS =
    "id,caption,media_type,media_url,thumbnail_url,permalink,timestamp,username,children{media_type,media_url,thumbnail_url}";
let instagramGraphContextPromise: Promise<InstagramGraphContext | null> | null = null;
const dotenvLoadResult = loadDotenv({ path: DOTENV_PATH, quiet: true });
if (dotenvLoadResult.error) {
    const nodeError = dotenvLoadResult.error as NodeJS.ErrnoException;
    if (nodeError.code !== "ENOENT") {
        throw dotenvLoadResult.error;
    }
}
const DISNEY_LOGS_PROMPT = `あなたは roki-web リポジトリ内で Disney 体験録を作成するエージェントです。

## 目的
取得済みSNS投稿データを根拠に、体験録 Markdown を1件作成する。

## 出力ファイルルール
- 作成先は \`contents/disney_experience_summary/logs/{番号}/index.md\`。
- 使う \`{番号}\` は「追加コンテキスト」で指定する。指定以外の番号は使用禁止。
- 既存ファイルの上書きは禁止。
- \`mkdir -p contents/disney_experience_summary/logs/{番号}\` を実行してから作成する。
- 作成後に \`test -f contents/disney_experience_summary/logs/{番号}/index.md\` で存在確認する。

## frontmatter 仕様
- \`title\`: 投稿内容を要約した自然なタイトル。
- \`date\`: YYYY-MM-DD（複数投稿なら最古日）。
- \`instagram\`: Instagram URL が1件以上ある場合のみ、カンマ区切りで設定。
- \`x\`: X URL が1件以上ある場合のみ、カンマ区切りで設定。
- \`disney-tags\`: 内容から判定可能なタグをカンマ区切りで設定。
- \`images\`: 「追加コンテキスト」で渡された画像ファイル名が1件以上ある場合は必須。画像ファイル名をカンマ区切りで指定（例: \`foo.jpg, bar.png\`）。
- \`ai-generated-by\`: 必須。
- \`ai-generated\`: 使用禁止。

## タグ候補
- FSH, TDH, DHM, TSH, DAH, TDL, TDS, DOI

## 事前スタイル分析（必須）
- 作成前に \`contents/disney_experience_summary/logs\` 配下の既存 \`index.md\` を最低10件読み、文章構成・語彙・情報密度・文の長短の傾向を抽出する。
- 抽出した傾向を今回の本文へ反映する。ただし、出来事としての事実は \`snsContents\` 以外から追加してはいけない。
- 不要なコマンド実行は禁止。build/test/lint/git 操作は行わない。

## 本文ルール
- 取得済みデータに書かれている事実だけを使う。
- 本文は体験録として十分な読み応えを持たせ、原則 5〜7 段落、最低 900 文字以上を目安にする。
- 箇条書きは最小限とし、基本は流れるような地の文で記述する。
- 文体は「だ・である調」を基調にしつつ、ときおり親しみやすい口語表現を自然に織り交ぜる。
- 本文は「体験録」として書く。投稿内容の解説・分析・メタ説明を書いてはいけない。
- 必ず投稿者本人の一人称視点（「私」）で、実際に体験した流れとして書く。
- 禁止表現: 「投稿では」「〜と記されている」「〜と述べられている」「〜がうかがえる」「内容である」「〜という投稿」。
- 感情は「嬉しい」「悲しい」のような単語だけで済ませず、心身の反応が伝わる細やかな表現で書く。
- 情景・味・香り・音・光など、感覚的な描写語を必ず含める。
- 比喩（「まるで〜のようだ」「〜に例えるなら」など）を使う場合は、認知言語学・修辞学の観点で対応関係が明瞭なものに限定し、乱用しない。
- 比喩に学術的背景の妥当性を説明できない場合は、無理に使わない。
- 完璧すぎる人工的な文体を避け、人間らしい自然なゆらぎを残す。
- 個人的な体験と感情を織り交ぜる。
- 20代女性が読んでも興味を持ち、50代男性が読んでも知性を感じる内容にする。
- 文の長さに意図的なバラツキを持たせる。

## 最終出力
- 最終出力は「作成済みファイルの Markdown 本文のみ」を返す。
`;
const NON_HUMAN_CLOSEUP_IMAGE_JUDGE_PROMPT = `次の画像について、人物の写り方を判定してください。

判定基準:
- REJECT: 人物が明らかに主被写体としてアップで写っている（顔や上半身が画面の大部分を占める）
- ACCEPT: それ以外（人物が背景として小さく写る、人物が写っていない、風景や建物や料理が主被写体）

必ず最終回答は1語のみで返してください: ACCEPT または REJECT`;

function showUsage(): void {
    console.error(
        "使用方法: tsx tools/gen-disney-logs.ts [--codex-model <model>] [--instagram-first-nonhuman-image] [--instagram-min-images <number>] <url-1> [<url-2> ...]",
    );
    console.error("対応URL: x.com / twitter.com / instagram.com");
    console.error(
        "任意フラグ: --instagram-first-nonhuman-image（Instagram画像のうち、人物アップを除外しつつ先頭から優先取得）",
    );
    console.error(
        `任意オプション: --instagram-min-images <number>（保存目標の最低枚数。デフォルト: ${DEFAULT_INSTAGRAM_MIN_IMAGE_COUNT}）`,
    );
    console.error(
        "任意環境変数: INSTAGRAM_ACCESS_TOKEN, INSTAGRAM_USER_ID, APP_ID, APP_SECRET, INSTAGRAM_REDIRECT_URI（.env から自動読込）",
    );
}

function reportProgress(message: string): void {
    const timestamp = new Date().toISOString();
    console.error(`${LOG_PREFIX} ${timestamp} ${message}`);
}

function normalizeText(value: string | null | undefined): string {
    return (value ?? "").replace(/\s+/gu, " ").trim();
}

function escapeRegExp(value: string): string {
    return value.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&");
}

function upsertDotenvEntry(dotenvContent: string, key: string, value: string): string {
    const normalizedValue = value.replace(/\r?\n/gu, "").trim();
    const entryLine = `${key}=${normalizedValue}`;
    const entryPattern = new RegExp(`^\\s*${escapeRegExp(key)}\\s*=.*$`, "mu");
    if (entryPattern.test(dotenvContent)) {
        return dotenvContent.replace(entryPattern, entryLine);
    }
    const trimmedContent = dotenvContent.replace(/\s*$/u, "");
    if (trimmedContent.length === 0) {
        return `${entryLine}\n`;
    }
    return `${trimmedContent}\n${entryLine}\n`;
}

function removeDotenvEntry(dotenvContent: string, key: string): string {
    const entryPattern = new RegExp(`^\\s*${escapeRegExp(key)}\\s*=.*(?:\\r?\\n)?`, "gmu");
    const contentWithoutEntry = dotenvContent.replace(entryPattern, "");
    const trimmedContent = contentWithoutEntry.replace(/\s*$/u, "");
    return trimmedContent.length === 0 ? "" : `${trimmedContent}\n`;
}

function parsePositiveIntegerOption(rawValue: string, optionName: string): number {
    const normalizedValue = normalizeText(rawValue);
    if (!/^\d+$/u.test(normalizedValue)) {
        throw new Error(`${optionName} には 1 以上の整数を指定してください。`);
    }
    const parsedValue = Number(normalizedValue);
    if (!Number.isInteger(parsedValue) || parsedValue <= 0) {
        throw new Error(`${optionName} には 1 以上の整数を指定してください。`);
    }
    return parsedValue;
}

function parseArgs(args: string[]): ParsedArgs {
    const urls: string[] = [];
    let showHelp = false;
    let codexModel: string | null = null;
    let selectFirstInstagramImageWithoutHumanCloseup = false;
    let instagramMinImageCount = DEFAULT_INSTAGRAM_MIN_IMAGE_COUNT;

    for (let index = 0; index < args.length; index += 1) {
        const arg = args[index] ?? "";
        if (arg === "--help" || arg === "-h") {
            showHelp = true;
            continue;
        }
        if (arg === "--instagram-first-nonhuman-image") {
            selectFirstInstagramImageWithoutHumanCloseup = true;
            continue;
        }
        if (arg === "--no-instagram-first-nonhuman-image") {
            selectFirstInstagramImageWithoutHumanCloseup = false;
            continue;
        }
        if (arg === "--instagram-min-images") {
            const nextArg = args[index + 1];
            if (!nextArg) {
                throw new Error("--instagram-min-images には整数を指定してください。");
            }
            instagramMinImageCount = parsePositiveIntegerOption(nextArg, "--instagram-min-images");
            index += 1;
            continue;
        }
        if (arg.startsWith("--instagram-min-images=")) {
            instagramMinImageCount = parsePositiveIntegerOption(
                arg.slice("--instagram-min-images=".length),
                "--instagram-min-images",
            );
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

    return {
        urls,
        showHelp,
        codexModel,
        selectFirstInstagramImageWithoutHumanCloseup,
        instagramMinImageCount,
    };
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

function normalizeHttpUrl(rawUrl: string | null | undefined): string | null {
    const normalizedUrl = normalizeText(rawUrl);
    if (normalizedUrl.length === 0) {
        return null;
    }
    try {
        const parsedUrl = new URL(normalizedUrl);
        if (parsedUrl.protocol !== "http:" && parsedUrl.protocol !== "https:") {
            return null;
        }
        return parsedUrl.toString();
    } catch {
        return null;
    }
}

function collectUniqueImageUrls(
    rawUrls: Array<string | null | undefined>,
    limit: number = Number.POSITIVE_INFINITY,
): string[] {
    const uniqueUrls: string[] = [];
    const seenUrls = new Set<string>();
    const hasFiniteLimit = Number.isFinite(limit);
    for (const rawUrl of rawUrls) {
        const normalizedUrl = normalizeHttpUrl(rawUrl);
        if (!normalizedUrl) {
            continue;
        }
        if (seenUrls.has(normalizedUrl)) {
            continue;
        }
        seenUrls.add(normalizedUrl);
        uniqueUrls.push(normalizedUrl);
        if (hasFiniteLimit && uniqueUrls.length >= limit) {
            break;
        }
    }
    return uniqueUrls;
}

function extractAuthorFromInstagramDescription(description: string): string | null {
    const matched = description.match(/^([^:]+)\s+on Instagram:/u);
    return matched?.[1]?.trim() ?? null;
}

interface InstagramOauthAccessTokenResponse {
    access_token?: unknown;
    user_id?: unknown;
}

interface InstagramLongLivedTokenResponse {
    access_token?: unknown;
}

interface InstagramGraphMeResponse {
    id?: unknown;
}

interface InstagramGraphError {
    code?: unknown;
    message?: unknown;
    type?: unknown;
}

interface InstagramGraphErrorResponse {
    error?: InstagramGraphError;
}

interface InstagramGraphMediaNode {
    media_type?: unknown;
    media_url?: unknown;
    thumbnail_url?: unknown;
}

interface InstagramGraphMediaChildren {
    data?: unknown;
}

interface InstagramGraphMediaItem extends InstagramGraphMediaNode {
    caption?: unknown;
    children?: InstagramGraphMediaChildren;
    permalink?: unknown;
    timestamp?: unknown;
    username?: unknown;
}

interface InstagramGraphMediaPaging {
    next?: unknown;
}

interface InstagramGraphMediaListResponse {
    data?: unknown;
    paging?: InstagramGraphMediaPaging;
}

interface WebhookSiteRequest {
    id?: unknown;
    query?: unknown;
    query_string?: unknown;
    uuid?: unknown;
    url?: unknown;
}

interface WebhookSiteRequestsResponse {
    data?: unknown;
}

interface InstagramApiContent {
    author: string | null;
    imageUrls: string[];
    postedAt: string | null;
    text: string | null;
}

function resolveInstagramApiVersion(): string {
    const configuredVersion = normalizeText(process.env.INSTAGRAM_API_VERSION);
    return configuredVersion.length > 0 ? configuredVersion : DEFAULT_INSTAGRAM_API_VERSION;
}

function buildInstagramGraphVersionedUrl(pathname: string): URL {
    const normalizedPathname = pathname.replace(/^\/+/u, "");
    return new URL(
        `https://graph.instagram.com/${resolveInstagramApiVersion()}/${normalizedPathname}`,
    );
}

function normalizeStringField(value: unknown): string | null {
    if (typeof value !== "string") {
        return null;
    }
    const normalizedValue = normalizeText(value);
    return normalizedValue.length > 0 ? normalizedValue : null;
}

function normalizeInstagramUserId(value: unknown): string | null {
    if (typeof value === "number" && Number.isFinite(value)) {
        return String(value);
    }
    return normalizeStringField(value);
}

function sanitizeInstagramAuthorizationCode(code: string): string {
    return normalizeText(code).replace(/#_$/u, "");
}

function sleep(milliseconds: number): Promise<void> {
    return new Promise((resolve) => {
        setTimeout(resolve, milliseconds);
    });
}

function extractWebhookSiteTokenFromRedirectUri(redirectUri: string): string | null {
    try {
        const parsedUrl = new URL(redirectUri);
        const normalizedHost = parsedUrl.hostname.toLowerCase();
        if (normalizedHost !== "webhook.site") {
            return null;
        }
        const [token] = parsedUrl.pathname.split("/").filter((segment) => segment.length > 0);
        return token ? normalizeText(token) : null;
    } catch {
        return null;
    }
}

function normalizeWebhookSiteRequestId(request: WebhookSiteRequest): string | null {
    if (typeof request.uuid === "string") {
        const normalizedUuid = normalizeText(request.uuid);
        return normalizedUuid.length > 0 ? normalizedUuid : null;
    }
    if (typeof request.id === "string") {
        const normalizedId = normalizeText(request.id);
        return normalizedId.length > 0 ? normalizedId : null;
    }
    if (typeof request.id === "number" && Number.isFinite(request.id)) {
        return String(request.id);
    }
    return null;
}

function extractInstagramAuthorizationCodeFromWebhookRequest(
    request: WebhookSiteRequest,
): string | null {
    if (request.query && typeof request.query === "object") {
        const queryPayload = request.query as { code?: unknown };
        const codeFromQuery = sanitizeInstagramAuthorizationCode(
            typeof queryPayload.code === "string" ? queryPayload.code : "",
        );
        if (codeFromQuery.length > 0) {
            return codeFromQuery;
        }
    }

    const queryString = normalizeText(
        typeof request.query_string === "string" ? request.query_string : "",
    );
    if (queryString.length > 0) {
        const codeFromQueryString = sanitizeInstagramAuthorizationCode(
            new URLSearchParams(queryString).get("code") ?? "",
        );
        if (codeFromQueryString.length > 0) {
            return codeFromQueryString;
        }
    }

    if (typeof request.url === "string") {
        try {
            const codeFromUrl = sanitizeInstagramAuthorizationCode(
                new URL(request.url).searchParams.get("code") ?? "",
            );
            if (codeFromUrl.length > 0) {
                return codeFromUrl;
            }
        } catch {
            // URL形式でない場合は無視する。
        }
    }
    return null;
}

async function fetchWebhookSiteRequests(token: string): Promise<WebhookSiteRequest[]> {
    const requestUrl = new URL(`/token/${token}/requests`, WEBHOOK_SITE_API_BASE_URL);
    requestUrl.searchParams.set("sorting", "newest");
    requestUrl.searchParams.set("per_page", "10");

    const response = await fetch(requestUrl, {
        headers: { Accept: "application/json" },
        method: "GET",
    });
    const responseBody = await response.text();
    if (!response.ok) {
        throw new Error(
            `webhook.site 取得に失敗しました: ${String(response.status)} ${response.statusText} ${trimText(responseBody)}`,
        );
    }

    try {
        const payload = JSON.parse(responseBody) as WebhookSiteRequestsResponse;
        if (!Array.isArray(payload.data)) {
            return [];
        }
        return payload.data.filter(
            (item): item is WebhookSiteRequest => typeof item === "object" && item !== null,
        );
    } catch {
        throw new Error(
            `webhook.site レスポンスのJSON解析に失敗しました: ${trimText(responseBody)}`,
        );
    }
}

async function waitForInstagramAuthorizationCodeFromWebhookSite(
    authorizationUrl: string,
    token: string,
): Promise<string | null> {
    reportProgress(
        `Instagram 認可コードが必要です。次のURLをブラウザで開いて認可してください: ${authorizationUrl}`,
    );
    reportProgress(`webhook.site で認可コードを最大 ${WEBHOOK_SITE_WAIT_SECONDS} 秒待機します。`);

    const knownRequestIds = new Set<string>();
    try {
        const initialRequests = await fetchWebhookSiteRequests(token);
        for (const request of initialRequests) {
            const requestId = normalizeWebhookSiteRequestId(request);
            if (requestId) {
                knownRequestIds.add(requestId);
            }
        }
    } catch (error: unknown) {
        reportProgress(
            `初期の webhook.site 取得に失敗しました。待機処理は継続します: ${stringifyError(error)}`,
        );
    }

    const deadlineMs = Date.now() + WEBHOOK_SITE_WAIT_SECONDS * 1_000;
    while (Date.now() <= deadlineMs) {
        await sleep(WEBHOOK_SITE_POLL_INTERVAL_MS);
        try {
            const requests = await fetchWebhookSiteRequests(token);
            for (const request of requests) {
                const requestId = normalizeWebhookSiteRequestId(request);
                if (requestId && knownRequestIds.has(requestId)) {
                    continue;
                }
                if (requestId) {
                    knownRequestIds.add(requestId);
                }
                const authorizationCode =
                    extractInstagramAuthorizationCodeFromWebhookRequest(request);
                if (authorizationCode !== null) {
                    reportProgress("webhook.site から Instagram 認可コードを取得しました。");
                    return authorizationCode;
                }
            }
        } catch (error: unknown) {
            reportProgress(
                `webhook.site のポーリング中にエラーが発生しました: ${stringifyError(error)}`,
            );
        }
    }

    reportProgress(
        "webhook.site で認可コードを取得できませんでした。ブラウザ認可を完了して再実行してください。",
    );
    return null;
}

async function resolveInstagramAuthorizationCode(): Promise<string | null> {
    if (!hasInstagramLoginCredentials()) {
        return null;
    }
    const redirectUri = normalizeText(process.env.INSTAGRAM_REDIRECT_URI);
    const webhookSiteToken = extractWebhookSiteTokenFromRedirectUri(redirectUri);
    if (webhookSiteToken === null) {
        return null;
    }
    const authorizationUrl = buildInstagramAuthorizationUrl();
    return await waitForInstagramAuthorizationCodeFromWebhookSite(
        authorizationUrl,
        webhookSiteToken,
    );
}

function hasInstagramLoginCredentials(): boolean {
    return (
        normalizeText(process.env.APP_ID).length > 0 &&
        normalizeText(process.env.APP_SECRET).length > 0 &&
        normalizeText(process.env.INSTAGRAM_REDIRECT_URI).length > 0
    );
}

function hasInstagramGraphApiConfiguration(): boolean {
    return (
        normalizeText(process.env.INSTAGRAM_ACCESS_TOKEN).length > 0 ||
        hasInstagramLoginCredentials()
    );
}

function isLikelyInstagramTokenInvalidError(errorMessage: string): boolean {
    const normalizedMessage = errorMessage.toLowerCase();
    return (
        normalizedMessage.includes("invalid access token") ||
        normalizedMessage.includes("access token is invalid") ||
        normalizedMessage.includes("cannot parse access token") ||
        normalizedMessage.includes("session has expired")
    );
}

function isLikelyInstagramRefreshTooEarlyError(errorMessage: string): boolean {
    const normalizedMessage = errorMessage.toLowerCase();
    return (
        normalizedMessage.includes("can only be refreshed") ||
        normalizedMessage.includes("not yet eligible for refresh") ||
        normalizedMessage.includes("not due for refresh") ||
        normalizedMessage.includes("once every 24 hours") ||
        (normalizedMessage.includes("refresh") && normalizedMessage.includes("24 hour"))
    );
}

function isLikelyInstagramAuthorizationCodeExpiredError(errorMessage: string): boolean {
    const normalizedMessage = errorMessage.toLowerCase();
    return (
        normalizedMessage.includes("authorization code has expired") ||
        normalizedMessage.includes("code has expired") ||
        normalizedMessage.includes("invalid authorization code") ||
        normalizedMessage.includes("you must provide a valid client_secret and code") ||
        normalizedMessage.includes("you must provide a valid client secret and code")
    );
}

function extractInstagramGraphError(responseBody: string): string {
    try {
        const payload = JSON.parse(responseBody) as InstagramGraphErrorResponse;
        const errorPayload = payload.error;
        if (!errorPayload || typeof errorPayload !== "object") {
            return trimText(responseBody);
        }
        const message = normalizeStringField(errorPayload.message);
        const type = normalizeStringField(errorPayload.type);
        const code =
            typeof errorPayload.code === "number" && Number.isFinite(errorPayload.code)
                ? String(errorPayload.code)
                : normalizeStringField(errorPayload.code);
        const metadata = [type, code].filter((value): value is string => value !== null).join(":");
        if (message && metadata.length > 0) {
            return `${message} (${metadata})`;
        }
        if (message) {
            return message;
        }
    } catch {
        // JSONとして解釈できない場合は本文をそのまま扱う。
    }
    return trimText(responseBody);
}

function buildInstagramAuthorizationUrl(): string {
    const appId = normalizeText(process.env.APP_ID);
    const redirectUri = normalizeText(process.env.INSTAGRAM_REDIRECT_URI);
    if (appId.length === 0 || redirectUri.length === 0) {
        throw new Error("APP_ID と INSTAGRAM_REDIRECT_URI を設定してください。");
    }
    const requestUrl = new URL(INSTAGRAM_OAUTH_AUTHORIZE_ENDPOINT);
    requestUrl.searchParams.set("client_id", appId);
    requestUrl.searchParams.set("redirect_uri", redirectUri);
    requestUrl.searchParams.set("response_type", "code");
    requestUrl.searchParams.set("scope", "instagram_business_basic");
    return requestUrl.toString();
}

async function exchangeInstagramCodeForShortLivedToken(
    authorizationCode: string,
): Promise<InstagramGraphContext> {
    const appId = normalizeText(process.env.APP_ID);
    const appSecret = normalizeText(process.env.APP_SECRET);
    const redirectUri = normalizeText(process.env.INSTAGRAM_REDIRECT_URI);
    if (appId.length === 0 || appSecret.length === 0 || redirectUri.length === 0) {
        throw new Error(
            "認可コード交換には APP_ID, APP_SECRET, INSTAGRAM_REDIRECT_URI が必要です。",
        );
    }

    const requestBody = new URLSearchParams({
        client_id: appId,
        client_secret: appSecret,
        code: authorizationCode,
        grant_type: "authorization_code",
        redirect_uri: redirectUri,
    });
    const response = await fetch(INSTAGRAM_OAUTH_ACCESS_TOKEN_ENDPOINT, {
        body: requestBody.toString(),
        headers: {
            Accept: "application/json",
            "Content-Type": "application/x-www-form-urlencoded",
        },
        method: "POST",
    });
    const responseBody = await response.text();
    if (!response.ok) {
        throw new Error(
            `Instagram OAuth 認可コード交換に失敗しました: ${String(response.status)} ${response.statusText} ${extractInstagramGraphError(responseBody)}`,
        );
    }

    const payload = JSON.parse(responseBody) as InstagramOauthAccessTokenResponse;
    const accessToken = normalizeStringField(payload.access_token);
    const userId = normalizeInstagramUserId(payload.user_id);
    if (accessToken === null || userId === null) {
        throw new Error(
            `Instagram OAuth レスポンスに access_token / user_id が含まれていません: ${trimText(responseBody)}`,
        );
    }
    return { accessToken, userId };
}

async function exchangeInstagramShortLivedTokenToLongLivedToken(
    shortLivedToken: string,
): Promise<string> {
    const appSecret = normalizeText(process.env.APP_SECRET);
    if (appSecret.length === 0) {
        throw new Error("長期トークン交換には APP_SECRET が必要です。");
    }

    const requestUrl = new URL(INSTAGRAM_LONG_LIVED_TOKEN_ENDPOINT);
    requestUrl.searchParams.set("grant_type", "ig_exchange_token");
    requestUrl.searchParams.set("client_secret", appSecret);
    requestUrl.searchParams.set("access_token", shortLivedToken);
    const response = await fetch(requestUrl, {
        headers: { Accept: "application/json" },
        method: "GET",
    });
    const responseBody = await response.text();
    if (!response.ok) {
        throw new Error(
            `Instagram 長期トークン交換に失敗しました: ${String(response.status)} ${response.statusText} ${extractInstagramGraphError(responseBody)}`,
        );
    }

    const payload = JSON.parse(responseBody) as InstagramLongLivedTokenResponse;
    const longLivedToken = normalizeStringField(payload.access_token);
    if (longLivedToken === null) {
        throw new Error(
            `Instagram 長期トークン交換レスポンスに access_token が含まれていません: ${trimText(responseBody)}`,
        );
    }
    return longLivedToken;
}

async function refreshInstagramLongLivedToken(accessToken: string): Promise<string> {
    const requestUrl = new URL(INSTAGRAM_REFRESH_TOKEN_ENDPOINT);
    requestUrl.searchParams.set("grant_type", "ig_refresh_token");
    requestUrl.searchParams.set("access_token", accessToken);
    const response = await fetch(requestUrl, {
        headers: { Accept: "application/json" },
        method: "GET",
    });
    const responseBody = await response.text();
    if (!response.ok) {
        throw new Error(
            `Instagram 長期トークン更新に失敗しました: ${String(response.status)} ${response.statusText} ${extractInstagramGraphError(responseBody)}`,
        );
    }

    const payload = JSON.parse(responseBody) as InstagramLongLivedTokenResponse;
    const refreshedToken = normalizeStringField(payload.access_token);
    if (refreshedToken === null) {
        throw new Error(
            `Instagram 長期トークン更新レスポンスに access_token が含まれていません: ${trimText(responseBody)}`,
        );
    }
    return refreshedToken;
}

async function fetchInstagramUserId(accessToken: string): Promise<string | null> {
    const requestUrl = new URL(INSTAGRAM_ME_ENDPOINT);
    requestUrl.searchParams.set("fields", "id");
    requestUrl.searchParams.set("access_token", accessToken);

    const response = await fetch(requestUrl, {
        headers: { Accept: "application/json" },
        method: "GET",
    });
    if (!response.ok) {
        return null;
    }

    const responseBody = await response.text();
    try {
        const payload = JSON.parse(responseBody) as InstagramGraphMeResponse;
        return normalizeInstagramUserId(payload.id);
    } catch {
        return null;
    }
}

async function persistInstagramCredentialsToDotenv(
    accessToken: string,
    userId: string,
): Promise<void> {
    let currentDotenvContent = "";
    try {
        currentDotenvContent = await readFile(DOTENV_PATH, "utf8");
    } catch (error: unknown) {
        const nodeError = error as NodeJS.ErrnoException;
        if (nodeError.code !== "ENOENT") {
            throw error;
        }
    }

    const updatedDotenvContent = removeDotenvEntry(
        upsertDotenvEntry(
            upsertDotenvEntry(
                currentDotenvContent,
                INSTAGRAM_ACCESS_TOKEN_ENV_KEY,
                normalizeText(accessToken),
            ),
            INSTAGRAM_USER_ID_ENV_KEY,
            normalizeText(userId),
        ),
        INSTAGRAM_AUTHORIZATION_CODE_ENV_KEY,
    );
    if (updatedDotenvContent !== currentDotenvContent) {
        await writeFile(DOTENV_PATH, updatedDotenvContent, "utf8");
    }
    process.env.INSTAGRAM_ACCESS_TOKEN = normalizeText(accessToken);
    process.env.INSTAGRAM_USER_ID = normalizeText(userId);
    delete process.env.INSTAGRAM_AUTHORIZATION_CODE;
}

async function issueInstagramGraphContextFromAuthorizationCode(): Promise<InstagramGraphContext | null> {
    let authorizationCode = await resolveInstagramAuthorizationCode();
    if (authorizationCode === null) {
        return null;
    }

    let shortLivedContext: InstagramGraphContext;
    try {
        shortLivedContext = await exchangeInstagramCodeForShortLivedToken(authorizationCode);
    } catch (error: unknown) {
        const errorMessage = stringifyError(error);
        if (!isLikelyInstagramAuthorizationCodeExpiredError(errorMessage)) {
            throw error;
        }
        reportProgress("認可コードが無効または期限切れです。新しい認可コードの取得を試みます。");
        const refreshedAuthorizationCode = await resolveInstagramAuthorizationCode();
        if (
            refreshedAuthorizationCode === null ||
            refreshedAuthorizationCode === authorizationCode
        ) {
            throw new Error(
                `Instagram OAuth 認可コード交換に失敗しました。新しい認可コードを取得できませんでした: ${errorMessage}`,
            );
        }
        authorizationCode = refreshedAuthorizationCode;
        try {
            shortLivedContext = await exchangeInstagramCodeForShortLivedToken(authorizationCode);
        } catch (refreshedError: unknown) {
            throw new Error(
                `Instagram OAuth 認可コード交換に失敗しました。APP_SECRET または認可コードが正しいか確認してください: ${stringifyError(refreshedError)}`,
            );
        }
    }

    const longLivedToken = await exchangeInstagramShortLivedTokenToLongLivedToken(
        shortLivedContext.accessToken,
    );
    try {
        await persistInstagramCredentialsToDotenv(longLivedToken, shortLivedContext.userId);
        reportProgress(
            ".env に INSTAGRAM_ACCESS_TOKEN と INSTAGRAM_USER_ID を保存しました（認可コード経由）。",
        );
    } catch (error: unknown) {
        reportProgress(`Instagram認証情報の .env 保存に失敗しました: ${stringifyError(error)}`);
    }
    reportProgress("認可コードから長期アクセストークンを発行しました。");
    return { accessToken: longLivedToken, userId: shortLivedContext.userId };
}

async function issueInstagramGraphContext(): Promise<InstagramGraphContext | null> {
    const configuredAccessToken = normalizeText(process.env.INSTAGRAM_ACCESS_TOKEN);
    const configuredUserId = normalizeText(process.env.INSTAGRAM_USER_ID);
    if (configuredAccessToken.length > 0) {
        let accessToken = configuredAccessToken;
        try {
            accessToken = await refreshInstagramLongLivedToken(configuredAccessToken);
            reportProgress("Instagram 長期アクセストークンを更新しました。");
        } catch (error: unknown) {
            const errorMessage = stringifyError(error);
            if (isLikelyInstagramRefreshTooEarlyError(errorMessage)) {
                reportProgress(
                    "Instagram 長期アクセストークンの更新タイミング前のため既存トークンを利用します。",
                );
            } else if (
                isLikelyInstagramTokenInvalidError(errorMessage) &&
                hasInstagramLoginCredentials()
            ) {
                const contextFromAuthorizationCode =
                    await issueInstagramGraphContextFromAuthorizationCode();
                if (contextFromAuthorizationCode !== null) {
                    reportProgress(
                        "既存アクセストークンが無効だったため、認可コードからトークンを再発行しました。",
                    );
                    return contextFromAuthorizationCode;
                }
                const authorizationUrl = buildInstagramAuthorizationUrl();
                throw new Error(
                    `INSTAGRAM_ACCESS_TOKEN が無効です。次のURLで再認可してください: ${authorizationUrl}`,
                );
            }
            reportProgress(
                `Instagram 長期アクセストークン更新に失敗したため既存トークンを利用します: ${errorMessage}`,
            );
        }

        const resolvedUserId =
            configuredUserId || (await fetchInstagramUserId(accessToken)) || configuredUserId;
        if (resolvedUserId.length === 0) {
            throw new Error(
                "INSTAGRAM_USER_ID を解決できません。環境変数 INSTAGRAM_USER_ID を設定してください。",
            );
        }
        try {
            await persistInstagramCredentialsToDotenv(accessToken, resolvedUserId);
            reportProgress(".env の Instagram 認証情報を更新しました。");
        } catch (error: unknown) {
            reportProgress(`Instagram認証情報の .env 更新に失敗しました: ${stringifyError(error)}`);
        }
        return { accessToken, userId: resolvedUserId };
    }

    const contextFromAuthorizationCode = await issueInstagramGraphContextFromAuthorizationCode();
    if (contextFromAuthorizationCode !== null) {
        return contextFromAuthorizationCode;
    }

    if (!hasInstagramLoginCredentials()) {
        return null;
    }
    const authorizationUrl = buildInstagramAuthorizationUrl();
    throw new Error(
        `INSTAGRAM_ACCESS_TOKEN が未設定です。次のURLで認可して再実行してください: ${authorizationUrl}`,
    );
}

async function getInstagramGraphContext(): Promise<InstagramGraphContext | null> {
    if (instagramGraphContextPromise === null) {
        instagramGraphContextPromise = issueInstagramGraphContext();
    }
    return await instagramGraphContextPromise;
}

function extractInstagramPostKey(rawUrl: string | null | undefined): string | null {
    const normalizedUrl = normalizeText(rawUrl);
    if (normalizedUrl.length === 0) {
        return null;
    }
    try {
        const parsedUrl = new URL(normalizedUrl);
        const matched = parsedUrl.pathname.match(/^\/(p|reel|tv)\/([^/]+)/u);
        if (!matched?.[1] || !matched[2]) {
            return null;
        }
        return `${matched[1].toLowerCase()}:${matched[2]}`;
    } catch {
        return null;
    }
}

function isSameInstagramPostUrl(leftUrl: string | null | undefined, rightUrl: string): boolean {
    const leftKey = extractInstagramPostKey(leftUrl);
    const rightKey = extractInstagramPostKey(rightUrl);
    return leftKey !== null && rightKey !== null && leftKey === rightKey;
}

function parseInstagramGraphMediaItems(
    payload: InstagramGraphMediaListResponse,
): InstagramGraphMediaItem[] {
    if (!Array.isArray(payload.data)) {
        return [];
    }
    return payload.data.filter(
        (item): item is InstagramGraphMediaItem => typeof item === "object" && item !== null,
    );
}

function extractImageCandidatesFromInstagramNode(
    node: InstagramGraphMediaNode,
): Array<string | null> {
    const mediaType = normalizeText(
        typeof node.media_type === "string" ? node.media_type : "",
    ).toUpperCase();
    const mediaUrl = normalizeStringField(node.media_url);
    const thumbnailUrl = normalizeStringField(node.thumbnail_url);
    if (mediaType === "VIDEO") {
        return [thumbnailUrl, mediaUrl];
    }
    return [mediaUrl, thumbnailUrl];
}

function extractImageUrlsFromInstagramMediaItem(mediaItem: InstagramGraphMediaItem): string[] {
    const mediaType = normalizeText(
        typeof mediaItem.media_type === "string" ? mediaItem.media_type : "",
    ).toUpperCase();
    if (mediaType === "CAROUSEL_ALBUM") {
        const childItems = Array.isArray(mediaItem.children?.data) ? mediaItem.children.data : [];
        const childUrls = childItems.flatMap((childItem) => {
            if (typeof childItem !== "object" || childItem === null) {
                return [];
            }
            const node = childItem as InstagramGraphMediaNode;
            return extractImageCandidatesFromInstagramNode(node);
        });
        return collectUniqueImageUrls(childUrls);
    }
    return collectUniqueImageUrls(extractImageCandidatesFromInstagramNode(mediaItem));
}

async function fetchInstagramMediaByApi(targetUrl: string): Promise<InstagramApiContent> {
    const instagramGraphContext = await getInstagramGraphContext();
    if (instagramGraphContext === null) {
        throw new Error(
            "Instagram API 認証情報を解決できません。INSTAGRAM_ACCESS_TOKEN か認可コードフローを確認してください。",
        );
    }

    let requestUrl = buildInstagramGraphVersionedUrl(`${instagramGraphContext.userId}/media`);
    requestUrl.searchParams.set("fields", INSTAGRAM_MEDIA_FIELDS);
    requestUrl.searchParams.set("limit", String(INSTAGRAM_MEDIA_LIMIT_PER_PAGE));
    requestUrl.searchParams.set("access_token", instagramGraphContext.accessToken);

    for (let pageIndex = 0; pageIndex < INSTAGRAM_MEDIA_MAX_PAGE_COUNT; pageIndex += 1) {
        const response = await fetch(requestUrl, {
            headers: { Accept: "application/json" },
            method: "GET",
        });
        const responseBody = await response.text();
        if (!response.ok) {
            throw new Error(
                `Instagram Graph API の投稿一覧取得に失敗しました: ${String(response.status)} ${response.statusText} ${extractInstagramGraphError(responseBody)}`,
            );
        }

        let payload: InstagramGraphMediaListResponse;
        try {
            payload = JSON.parse(responseBody) as InstagramGraphMediaListResponse;
        } catch {
            throw new Error(
                `Instagram Graph API レスポンスのJSON解析に失敗しました: ${trimText(responseBody)}`,
            );
        }
        const mediaItems = parseInstagramGraphMediaItems(payload);
        const targetMediaItem = mediaItems.find((mediaItem) =>
            isSameInstagramPostUrl(
                typeof mediaItem.permalink === "string" ? mediaItem.permalink : null,
                targetUrl,
            ),
        );
        if (targetMediaItem) {
            return {
                author: normalizeStringField(targetMediaItem.username),
                imageUrls: extractImageUrlsFromInstagramMediaItem(targetMediaItem),
                postedAt: normalizeStringField(targetMediaItem.timestamp),
                text: normalizeStringField(targetMediaItem.caption),
            };
        }

        const nextPageUrl = normalizeHttpUrl(
            typeof payload.paging?.next === "string" ? payload.paging.next : null,
        );
        if (!nextPageUrl) {
            break;
        }
        requestUrl = new URL(nextPageUrl);
        if (!requestUrl.searchParams.has("access_token")) {
            requestUrl.searchParams.set("access_token", instagramGraphContext.accessToken);
        }
    }
    throw new Error(`Instagram Graph API 上で対象投稿が見つかりませんでした: ${targetUrl}`);
}

function extractImageExtensionFromUrl(imageUrl: string): string | null {
    try {
        const pathname = new URL(imageUrl).pathname;
        const extension = path.extname(pathname).toLowerCase();
        if (
            extension === ".jpg" ||
            extension === ".jpeg" ||
            extension === ".png" ||
            extension === ".webp" ||
            extension === ".gif"
        ) {
            return extension;
        }
        return null;
    } catch {
        return null;
    }
}

function detectImageExtensionFromContentType(contentType: string | null): string | null {
    const normalizedContentType = normalizeText(contentType).toLowerCase();
    if (normalizedContentType.includes("image/jpeg")) {
        return ".jpg";
    }
    if (normalizedContentType.includes("image/png")) {
        return ".png";
    }
    if (normalizedContentType.includes("image/webp")) {
        return ".webp";
    }
    if (normalizedContentType.includes("image/gif")) {
        return ".gif";
    }
    return null;
}

function resolveImageExtension(imageUrl: string, contentType: string | null): string {
    return (
        detectImageExtensionFromContentType(contentType) ||
        extractImageExtensionFromUrl(imageUrl) ||
        ".jpg"
    );
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
        imageUrls: [],
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

    let instagramApiContent: InstagramApiContent | null = null;
    let instagramApiFetchError: string | null = null;
    try {
        instagramApiContent = await fetchInstagramMediaByApi(targetUrl);
    } catch (error: unknown) {
        instagramApiFetchError = stringifyError(error);
        reportProgress(`Instagram Graph API 画像取得に失敗しました: ${instagramApiFetchError}`);
    }
    const imageUrls = collectUniqueImageUrls(instagramApiContent?.imageUrls ?? []);

    return {
        platform: "instagram",
        url: targetUrl,
        title: chooseFirstNonEmpty(snapshot.title, snapshot.description),
        text: trimText(
            chooseFirstNonEmpty(
                instagramApiContent?.text,
                snapshot.articleText,
                snapshot.description,
                snapshot.bodyText,
            ),
        ),
        postedAt: chooseFirstNonEmpty(instagramApiContent?.postedAt, snapshot.postedAt) || null,
        author:
            chooseFirstNonEmpty(instagramApiContent?.author, snapshot.author) ||
            extractAuthorFromInstagramDescription(snapshot.description) ||
            null,
        imageUrls,
        fetchError: instagramApiFetchError,
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
            imageUrls: [],
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

async function resolveNextLogNumber(): Promise<number> {
    let entries: Dirent[];
    try {
        entries = await readdir(DISNEY_LOGS_ROOT, { withFileTypes: true });
    } catch (error: unknown) {
        const nodeError = error as NodeJS.ErrnoException;
        if (nodeError.code === "ENOENT") {
            return 1;
        }
        throw error;
    }
    const existingNumbers = entries
        .map((entry): number | null => {
            if (entry.isDirectory() && /^\d+$/u.test(entry.name)) {
                return Number(entry.name);
            }
            if (entry.isFile()) {
                const matched = entry.name.match(/^(\d+)\.md$/u);
                if (matched?.[1]) {
                    return Number(matched[1]);
                }
            }
            return null;
        })
        .filter((value): value is number => value !== null && Number.isFinite(value));

    if (existingNumbers.length === 0) {
        return 1;
    }
    return Math.max(...existingNumbers) + 1;
}

function buildLogDirectoryRelativePath(logNumber: number): string {
    return `contents/disney_experience_summary/logs/${String(logNumber)}`;
}

async function downloadInstagramImages(
    contents: SnsContent[],
    logNumber: number,
    options: DownloadInstagramImagesOptions,
): Promise<string[]> {
    const instagramImageUrls = collectUniqueImageUrls(
        contents
            .filter((content) => content.platform === "instagram" && content.fetchError === null)
            .flatMap((content) => content.imageUrls),
    );
    if (instagramImageUrls.length === 0) {
        return [];
    }

    const logDirectoryRelativePath = buildLogDirectoryRelativePath(logNumber);
    const logDirectoryAbsolutePath = path.join(REPO_ROOT, logDirectoryRelativePath);
    await mkdir(logDirectoryAbsolutePath, { recursive: true });

    const targetImageCount = options.minimumImageCount;
    const savedFileNames: string[] = [];
    for (const imageUrl of instagramImageUrls) {
        try {
            const response = await fetch(imageUrl, { method: "GET" });
            if (!response.ok) {
                throw new Error(`${String(response.status)} ${response.statusText}`);
            }
            const imageBuffer = Buffer.from(await response.arrayBuffer());
            if (imageBuffer.length === 0) {
                throw new Error("画像データが空です。");
            }

            const extension = resolveImageExtension(imageUrl, response.headers.get("content-type"));
            if (options.selectFirstImageWithoutHumanCloseup) {
                const selected = await isLikelyNoHumanCloseupByCodex(
                    imageBuffer,
                    extension,
                    options.codexModelName,
                );
                if (!selected) {
                    reportProgress(`人物アップ判定で除外しました: ${imageUrl}`);
                    continue;
                }
                reportProgress(`人物アップ判定を通過しました: ${imageUrl}`);
            }

            const nextIndex = savedFileNames.length + 1;
            const fileName = `image-${String(nextIndex)}${extension}`;
            const filePath = path.join(logDirectoryAbsolutePath, fileName);
            await writeFile(filePath, imageBuffer);
            savedFileNames.push(fileName);
            reportProgress(`Instagram画像を保存しました: ${logDirectoryRelativePath}/${fileName}`);
            if (savedFileNames.length >= targetImageCount) {
                break;
            }
        } catch (error: unknown) {
            reportProgress(
                `Instagram画像の保存に失敗しました (${imageUrl}): ${stringifyError(error)}`,
            );
        }
    }

    if (savedFileNames.length < targetImageCount) {
        reportProgress(
            `Instagram画像は目標最低枚数 (${targetImageCount}) に達しませんでした。取得件数: ${savedFileNames.length} 件`,
        );
    }
    return savedFileNames;
}

function buildCodexPrompt(
    contents: SnsContent[],
    codexModelName: string,
    logNumber: number,
    downloadedImageFiles: string[],
): string {
    const serializedContents = JSON.stringify(contents, null, 2);
    const serializedImageFiles = JSON.stringify(downloadedImageFiles, null, 2);
    const logDirectoryRelativePath = buildLogDirectoryRelativePath(logNumber);
    const targetFilePath = `${logDirectoryRelativePath}/index.md`;
    return `${DISNEY_LOGS_PROMPT}

## 追加コンテキスト
- 今回の作成先は \`${targetFilePath}\` で固定。番号調査は不要。
- SNS 投稿の取得処理は完了済み。下記JSONだけを根拠として使用する。
- Instagram画像の事前保存を実行済み。下記ファイル名配列を \`images\` に反映すること。
  - 配列が空なら \`images\` を設定しない。
  - 配列が1件以上なら \`images\` を必ず設定し、値はこの配列順でカンマ区切りにする。
- \`ai-generated-by\` には必ず \`${codexModelName}\` を設定する。

\`downloadedImageFiles\`:
\`\`\`json
${serializedImageFiles}
\`\`\`

\`snsContents\`:
\`\`\`json
${serializedContents}
\`\`\`
`;
}

async function runCodexExec(
    prompt: string,
    codexModelName: string,
    imagePaths: string[] = [],
): Promise<string> {
    const tempDirectory = await mkdtemp(path.join(os.tmpdir(), "gen-disney-logs-"));
    const outputPath = path.join(tempDirectory, "codex-last-message.md");

    try {
        reportProgress(`codex exec を開始します。model=${codexModelName}`);
        reportProgress(
            "codex exec では project_doc_max_bytes=0 を指定し、AGENTS.md 読み込みを無効化します。",
        );
        if (imagePaths.length > 0) {
            reportProgress(`codex exec に画像を添付します。枚数: ${imagePaths.length}`);
        }
        await new Promise<void>((resolve, reject) => {
            const codexArgs = [
                "exec",
                "--skip-git-repo-check",
                "--sandbox",
                "workspace-write",
                "--color",
                "never",
                "-c",
                "project_doc_max_bytes=0",
                "-m",
                codexModelName,
            ];
            for (const imagePath of imagePaths) {
                codexArgs.push("--image", imagePath);
            }
            codexArgs.push("-o", outputPath, "-");

            const child = spawn("codex", codexArgs, {
                cwd: REPO_ROOT,
                stdio: ["pipe", "ignore", "inherit"],
            });

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

function parseAcceptOrReject(output: string): "ACCEPT" | "REJECT" | null {
    const normalizedOutput = normalizeText(output).toUpperCase();
    const hasAccept = /\bACCEPT\b/u.test(normalizedOutput);
    const hasReject = /\bREJECT\b/u.test(normalizedOutput);
    if (hasAccept && !hasReject) {
        return "ACCEPT";
    }
    if (hasReject && !hasAccept) {
        return "REJECT";
    }
    return null;
}

async function isLikelyNoHumanCloseupByCodex(
    imageBuffer: Buffer,
    imageExtension: string,
    codexModelName: string,
): Promise<boolean> {
    const tempDirectory = await mkdtemp(path.join(os.tmpdir(), "gen-disney-logs-image-judge-"));
    const tempImagePath = path.join(tempDirectory, `target${imageExtension}`);
    try {
        await writeFile(tempImagePath, imageBuffer);
        const codexOutput = await runCodexExec(
            NON_HUMAN_CLOSEUP_IMAGE_JUDGE_PROMPT,
            codexModelName,
            [tempImagePath],
        );
        const judgeResult = parseAcceptOrReject(codexOutput);
        if (judgeResult === null) {
            reportProgress(
                `人物アップ判定の出力が不正だったため除外扱いにします: ${trimText(codexOutput)}`,
            );
            return false;
        }
        return judgeResult === "ACCEPT";
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
    reportProgress(
        `Instagram画像の最低取得枚数: ${parsedArgs.instagramMinImageCount}（足りない場合は取得可能な範囲で保存）`,
    );
    if (parsedArgs.selectFirstInstagramImageWithoutHumanCloseup) {
        reportProgress(
            "人物アップ除外モードを有効化しました。Instagram画像は先頭から判定し、条件を満たす画像のみ保存します。",
        );
    }
    if (hasInstagramGraphApiConfiguration()) {
        reportProgress("Instagram API with Instagram Login でのみ画像取得します。");
    } else {
        reportProgress("Instagram API 用の認証情報が未設定のため、Instagram画像は取得しません。");
    }
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

    const nextLogNumber = await resolveNextLogNumber();
    reportProgress(`今回の作成番号を確定しました: ${nextLogNumber}`);
    reportProgress("Instagram画像の保存を開始します。");
    const downloadedImageFiles = await downloadInstagramImages(contents, nextLogNumber, {
        codexModelName,
        selectFirstImageWithoutHumanCloseup:
            parsedArgs.selectFirstInstagramImageWithoutHumanCloseup,
        minimumImageCount: parsedArgs.instagramMinImageCount,
    });
    reportProgress(`Instagram画像の保存完了: ${downloadedImageFiles.length} 件`);

    reportProgress("プロンプトを組み立てます。");
    const codexPrompt = buildCodexPrompt(
        contents,
        codexModelName,
        nextLogNumber,
        downloadedImageFiles,
    );
    const markdown = (await runCodexExec(codexPrompt, codexModelName)).trim();
    reportProgress("処理全体が完了しました。");
    process.stdout.write(`${markdown}\n`);
}

main().catch((error: unknown) => {
    const message = stringifyError(error);
    console.error(`エラー: ${message}`);
    process.exit(1);
});
