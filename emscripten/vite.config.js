import { defineConfig } from "vite";
import { svelte } from "@sveltejs/vite-plugin-svelte";
import { directoryPlugin } from "vite-plugin-list-directory-contents";

// https://vitejs.dev/config/
export default defineConfig({
    plugins: [svelte(), directoryPlugin({ baseDir: __dirname })],
    test: {
	include: ["tests/**/*.js"],
    },
    optimizeDeps: {
	exclude: ["@ffmpeg/ffmpeg", "@ffmpeg/util"],
    },
    server: {
	headers: {
	    "Cross-Origin-Opener-Policy": "same-origin",
	    "Cross-Origin-Embedder-Policy": "require-corp",
	},
    },
});
