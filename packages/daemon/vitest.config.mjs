import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    testTimeout: 5000,
    environment: "node",
    globals: false,
    isolate: true,
    reporters: ["verbose"],
    coverage: {
      provider: "v8",
      reporter: ["text", "json"],
    },
  },
});
