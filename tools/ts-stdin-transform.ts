import { transform } from "esbuild";

export async function compileTypeScript(input: string): Promise<string> {
    const result = await transform(input, {
        loader: "ts",
        target: "es2020",
        banner: '"use strict";',
        legalComments: "none",
        sourcemap: false,
        tsconfigRaw: {
            compilerOptions: {
                useDefineForClassFields: false,
            },
        },
    });

    return result.code;
}
