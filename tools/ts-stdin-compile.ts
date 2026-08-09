#!/usr/bin/env tsx

import { compileTypeScript } from "./ts-stdin-transform";

async function main(): Promise<void> {
    let input = "";
    process.stdin.setEncoding("utf8");

    for await (const chunk of process.stdin) {
        input += chunk;
    }

    process.stdout.write(await compileTypeScript(input));
}

main().catch((error: unknown) => {
    console.error(error instanceof Error ? error.message : error);
    process.exitCode = 1;
});
