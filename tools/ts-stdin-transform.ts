import { transform } from "esbuild";

/**
 * Transpiles TypeScript syntax to classic-script ES2020 JavaScript while retaining
 * the strict-mode and class-field semantics required by the site build.
 *
 * This is syntax-only transpilation; it does not perform TypeScript type checking.
 *
 * @throws When esbuild rejects the input or cannot complete the transform.
 */
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
