import { defineConfig } from "vite";
import { resolve } from "path";

/**
 * Vite Production Build Configuration for Disney Experience Visualizations
 *
 * この設定ファイルは、Hakyllビルドプロセス内で使用されます。
 * - 出力形式: IIFE (Immediately Invoked Function Expression)
 * - 外部依存: d3 (CDNから読み込まれる前提)
 * - エントリーポイント: contents/ts/disney-experience-visualizations.ts
 */
export default defineConfig({
    build: {
        // ライブラリモードでビルド
        lib: {
            entry: resolve(__dirname, "contents/ts/disney-experience-visualizations.ts"),
            name: "DisneyExperienceVisualizations", // グローバル変数名（使用されない可能性あり）
            formats: ["iife"], // IIFE形式で出力
            fileName: () => "disney-experience-visualizations.js",
        },

        // Rollupオプション
        rollupOptions: {
            // 外部依存として扱う（バンドルに含めない）
            external: ["d3"],
            output: {
                // グローバル変数のマッピング
                globals: {
                    d3: "d3",
                },
                // IIFE形式の出力設定
                format: "iife",
                // 出力ファイル名
                entryFileNames: "disney-experience-visualizations.js",
                // ソースマップを生成しない
                sourcemap: false,
                // コメントを保持
                preserveModules: false,
            },
        },

        // 出力ディレクトリ（Hakyllから上書き可能）
        outDir: "dist",

        // アセットをインライン化しない
        assetsInlineLimit: 0,

        // 圧縮を有効化（terserを使用）
        minify: "terser",
        terserOptions: {
            compress: {
                drop_console: false, // console.logを削除しない
                drop_debugger: true,
            },
            format: {
                comments: false, // コメントを削除
            },
        },

        // CSSコード分割を無効化
        cssCodeSplit: false,

        // 警告を表示
        reportCompressedSize: true,

        // チャンク分割を無効化（単一ファイル出力）
        chunkSizeWarningLimit: 1000,
    },

    // 本番環境モード
    mode: "production",

    // ログレベル
    logLevel: "info",
});
