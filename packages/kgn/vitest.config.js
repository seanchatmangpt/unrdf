import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    globals: true,
    include: ["test/**/*.test.js", "*.test.js"],
    exclude: ["node_modules/**", "dist/**"],
    testTimeout: 10000,
    hookTimeout: 10000,
  },
});
