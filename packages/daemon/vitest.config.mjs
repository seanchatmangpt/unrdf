import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    testTimeout: 5000,
    environment: "node",
    globals: false,
    isolate: true,
    reporters: ["verbose"],
    include: ["test/**/*.test.mjs"],
    exclude: [
      "node_modules/**",
      "dist/**",
      "test/integration/**",
      "test/debug/**",
      "test/groq-integration.test.mjs",
      "test/groq-otel-traced.test.mjs",
      "test/groq-rdf-generation.test.mjs",
      "test/open-ontologies-integration*.test.mjs",
      "test/yawl-integration-*.test.mjs",
      "test/e2e-daemon-yawl.test.mjs",
    ],
    coverage: {
      provider: "v8",
      reporter: ["text", "json"],
    },
  },
});
