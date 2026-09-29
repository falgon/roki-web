import * as katex from "katex";

export interface BatchRenderRequest {
    index: number;
    math: string;
    displayMode: boolean;
}

export interface BatchRenderResponse {
    index: number;
    html: string;
}

function isRecord(value: unknown): value is Record<string, unknown> {
    return typeof value === "object" && value !== null && !Array.isArray(value);
}

function parseBatchRenderRequest(value: unknown): BatchRenderRequest {
    if (!isRecord(value)) {
        throw new Error("KaTeX batch request item must be an object");
    }

    if (
        typeof value.index !== "number" ||
        !Number.isInteger(value.index) ||
        value.index < 0 ||
        typeof value.math !== "string" ||
        typeof value.displayMode !== "boolean"
    ) {
        throw new Error("KaTeX batch request item has an invalid shape");
    }

    return {
        index: value.index,
        math: value.math,
        displayMode: value.displayMode,
    };
}

export function parseBatchRenderRequests(input: string): BatchRenderRequest[] {
    const parsed: unknown = JSON.parse(input);

    if (!Array.isArray(parsed)) {
        throw new Error("KaTeX batch request must be a JSON array");
    }

    return parsed.map(parseBatchRenderRequest);
}

export function renderMath(math: string, displayMode: boolean): string {
    const mathjaxOpt: katex.KatexOptions = {
        displayMode,
        trust: true,
        colorIsTextColor: true,
    };

    return katex.renderToString(math, mathjaxOpt);
}

export function renderBatch(requests: BatchRenderRequest[]): BatchRenderResponse[] {
    return requests.map((request) => ({
        index: request.index,
        html: renderMath(request.math, request.displayMode),
    }));
}

export function renderBatchJson(input: string): string {
    return JSON.stringify(renderBatch(parseBatchRenderRequests(input)));
}
