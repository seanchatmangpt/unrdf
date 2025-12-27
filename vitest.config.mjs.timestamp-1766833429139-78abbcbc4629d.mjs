// ../../vitest.config.mjs
import { defineConfig } from "file:///home/user/unrdf/node_modules/.pnpm/vitest@1.6.1_@types+node@24.10.4_@vitest+browser@4.0.16_@vitest+ui@4.0.16_happy-dom@20._cf52876a11af7c5646287023f383a5d9/node_modules/vitest/dist/config.js";
var isCI = process.env.CI === "true";
var isFast = process.env.TEST_MODE === "fast";
var vitest_config_default = defineConfig({
  test: {
    // Parallel execution - 2-4x speedup
    pool: "forks",
    poolOptions: {
      forks: {
        maxForks: 10
      }
    },
    // Test timeout - 5s SLA (Andon Principle)
    testTimeout: 5e3,
    // Environment
    environment: "node",
    // File patterns - 80/20 core test suite
    include: [
      "test/diff.test.mjs",
      "test/dark-matter-80-20.test.mjs",
      "test/cli/cli-package.test.mjs",
      "test/knowledge-engine/parse-contract.test.mjs",
      "test/knowledge-engine/query-contract.test.mjs",
      "test/knowledge-engine/utils/circuit-breaker.test.mjs",
      "test/browser/browser-shims.test.mjs",
      "test/browser/indexeddb-store.test.mjs",
      "test/validation/otel-validation-v3.1.test.mjs",
      "test/streaming/streaming.test.mjs"
    ],
    exclude: [
      "node_modules/**",
      "dist/**",
      "test/fixtures/**",
      "test/utils/**",
      "test/knowledge-engine/sandbox/executor-detection.test.mjs",
      "test/knowledge-engine/sandbox/isolated-vm.test.mjs",
      "test/browser/browser-compatibility.test.mjs",
      "test/browser/playwright.spec.mjs",
      "test/cli/baseline-cli.test.mjs",
      "test/project-engine.test.mjs",
      "test/project-engine/code-complexity-js.test.mjs",
      "test/project-engine/initialize.test.mjs",
      "**/benchmarks/**",
      isFast ? "test/**/*.integration.test.mjs" : null
    ].filter(Boolean),
    // Multi-reporter: verbose for dev, junit for CI
    reporter: isCI ? ["junit", "default"] : ["verbose"],
    // JUnit output for CI/CD pipelines
    outputFile: {
      junit: "./test-results/junit.xml"
    },
    // Standard settings
    globals: false,
    isolate: true,
    passWithNoTests: true,
    // Coverage configuration (when --coverage used)
    coverage: {
      provider: "v8",
      reporter: ["text", "json", "html"],
      exclude: [
        "**/node_modules/**",
        "**/dist/**",
        "**/*.test.mjs",
        "**/*.spec.mjs"
      ]
    },
    // Watch mode optimization (80/20 DX)
    watchExclude: [
      "**/node_modules/**",
      "**/dist/**",
      "**/coverage/**",
      "**/.git/**",
      "**/test-results/**"
    ]
  }
});
export {
  vitest_config_default as default
};
//# sourceMappingURL=data:application/json;base64,ewogICJ2ZXJzaW9uIjogMywKICAic291cmNlcyI6IFsiLi4vLi4vdml0ZXN0LmNvbmZpZy5tanMiXSwKICAic291cmNlc0NvbnRlbnQiOiBbImNvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9kaXJuYW1lID0gXCIvaG9tZS91c2VyL3VucmRmXCI7Y29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2ZpbGVuYW1lID0gXCIvaG9tZS91c2VyL3VucmRmL3ZpdGVzdC5jb25maWcubWpzXCI7Y29uc3QgX192aXRlX2luamVjdGVkX29yaWdpbmFsX2ltcG9ydF9tZXRhX3VybCA9IFwiZmlsZTovLy9ob21lL3VzZXIvdW5yZGYvdml0ZXN0LmNvbmZpZy5tanNcIjsvKipcbiAqIEBmaWxlb3ZlcnZpZXcgRFgtb3B0aW1pemVkIFZpdGVzdCBjb25maWd1cmF0aW9uIGZvciB1bnJkZlxuICogLSBQYXJhbGxlbCBleGVjdXRpb24gd2l0aCBtYXhDb25jdXJyZW5jeVxuICogLSBNdWx0aXBsZSByZXBvcnRlcnMgKHZlcmJvc2UgKyBqdW5pdCBmb3IgQ0kpXG4gKiAtIEZhc3QgdGVzdCBtb2RlcyB2aWEgZW52aXJvbm1lbnQgdmFyaWFibGVzXG4gKi9cbmltcG9ydCB7IGRlZmluZUNvbmZpZyB9IGZyb20gXCJ2aXRlc3QvY29uZmlnXCI7XG5cbmNvbnN0IGlzQ0kgPSBwcm9jZXNzLmVudi5DSSA9PT0gXCJ0cnVlXCI7XG5jb25zdCBpc0Zhc3QgPSBwcm9jZXNzLmVudi5URVNUX01PREUgPT09IFwiZmFzdFwiO1xuXG5leHBvcnQgZGVmYXVsdCBkZWZpbmVDb25maWcoe1xuICB0ZXN0OiB7XG4gICAgLy8gUGFyYWxsZWwgZXhlY3V0aW9uIC0gMi00eCBzcGVlZHVwXG4gICAgcG9vbDogXCJmb3Jrc1wiLFxuICAgIHBvb2xPcHRpb25zOiB7XG4gICAgICBmb3Jrczoge1xuICAgICAgICBtYXhGb3JrczogMTAsXG4gICAgICB9LFxuICAgIH0sXG5cbiAgICAvLyBUZXN0IHRpbWVvdXQgLSA1cyBTTEEgKEFuZG9uIFByaW5jaXBsZSlcbiAgICB0ZXN0VGltZW91dDogNTAwMCxcblxuICAgIC8vIEVudmlyb25tZW50XG4gICAgZW52aXJvbm1lbnQ6IFwibm9kZVwiLFxuXG4gICAgLy8gRmlsZSBwYXR0ZXJucyAtIDgwLzIwIGNvcmUgdGVzdCBzdWl0ZVxuICAgIGluY2x1ZGU6IFtcbiAgICAgIFwidGVzdC9kaWZmLnRlc3QubWpzXCIsXG4gICAgICBcInRlc3QvZGFyay1tYXR0ZXItODAtMjAudGVzdC5tanNcIixcbiAgICAgIFwidGVzdC9jbGkvY2xpLXBhY2thZ2UudGVzdC5tanNcIixcbiAgICAgIFwidGVzdC9rbm93bGVkZ2UtZW5naW5lL3BhcnNlLWNvbnRyYWN0LnRlc3QubWpzXCIsXG4gICAgICBcInRlc3Qva25vd2xlZGdlLWVuZ2luZS9xdWVyeS1jb250cmFjdC50ZXN0Lm1qc1wiLFxuICAgICAgXCJ0ZXN0L2tub3dsZWRnZS1lbmdpbmUvdXRpbHMvY2lyY3VpdC1icmVha2VyLnRlc3QubWpzXCIsXG4gICAgICBcInRlc3QvYnJvd3Nlci9icm93c2VyLXNoaW1zLnRlc3QubWpzXCIsXG4gICAgICBcInRlc3QvYnJvd3Nlci9pbmRleGVkZGItc3RvcmUudGVzdC5tanNcIixcbiAgICAgIFwidGVzdC92YWxpZGF0aW9uL290ZWwtdmFsaWRhdGlvbi12My4xLnRlc3QubWpzXCIsXG4gICAgICBcInRlc3Qvc3RyZWFtaW5nL3N0cmVhbWluZy50ZXN0Lm1qc1wiLFxuICAgIF0sXG5cbiAgICBleGNsdWRlOiBbXG4gICAgICBcIm5vZGVfbW9kdWxlcy8qKlwiLFxuICAgICAgXCJkaXN0LyoqXCIsXG4gICAgICBcInRlc3QvZml4dHVyZXMvKipcIixcbiAgICAgIFwidGVzdC91dGlscy8qKlwiLFxuICAgICAgXCJ0ZXN0L2tub3dsZWRnZS1lbmdpbmUvc2FuZGJveC9leGVjdXRvci1kZXRlY3Rpb24udGVzdC5tanNcIixcbiAgICAgIFwidGVzdC9rbm93bGVkZ2UtZW5naW5lL3NhbmRib3gvaXNvbGF0ZWQtdm0udGVzdC5tanNcIixcbiAgICAgIFwidGVzdC9icm93c2VyL2Jyb3dzZXItY29tcGF0aWJpbGl0eS50ZXN0Lm1qc1wiLFxuICAgICAgXCJ0ZXN0L2Jyb3dzZXIvcGxheXdyaWdodC5zcGVjLm1qc1wiLFxuICAgICAgXCJ0ZXN0L2NsaS9iYXNlbGluZS1jbGkudGVzdC5tanNcIixcbiAgICAgIFwidGVzdC9wcm9qZWN0LWVuZ2luZS50ZXN0Lm1qc1wiLFxuICAgICAgXCJ0ZXN0L3Byb2plY3QtZW5naW5lL2NvZGUtY29tcGxleGl0eS1qcy50ZXN0Lm1qc1wiLFxuICAgICAgXCJ0ZXN0L3Byb2plY3QtZW5naW5lL2luaXRpYWxpemUudGVzdC5tanNcIixcbiAgICAgIFwiKiovYmVuY2htYXJrcy8qKlwiLFxuICAgICAgaXNGYXN0ID8gXCJ0ZXN0LyoqLyouaW50ZWdyYXRpb24udGVzdC5tanNcIiA6IG51bGwsXG4gICAgXS5maWx0ZXIoQm9vbGVhbiksXG5cbiAgICAvLyBNdWx0aS1yZXBvcnRlcjogdmVyYm9zZSBmb3IgZGV2LCBqdW5pdCBmb3IgQ0lcbiAgICByZXBvcnRlcjogaXNDSVxuICAgICAgPyBbXCJqdW5pdFwiLCBcImRlZmF1bHRcIl1cbiAgICAgIDogW1widmVyYm9zZVwiXSxcblxuICAgIC8vIEpVbml0IG91dHB1dCBmb3IgQ0kvQ0QgcGlwZWxpbmVzXG4gICAgb3V0cHV0RmlsZToge1xuICAgICAganVuaXQ6IFwiLi90ZXN0LXJlc3VsdHMvanVuaXQueG1sXCIsXG4gICAgfSxcblxuICAgIC8vIFN0YW5kYXJkIHNldHRpbmdzXG4gICAgZ2xvYmFsczogZmFsc2UsXG4gICAgaXNvbGF0ZTogdHJ1ZSxcbiAgICBwYXNzV2l0aE5vVGVzdHM6IHRydWUsXG5cbiAgICAvLyBDb3ZlcmFnZSBjb25maWd1cmF0aW9uICh3aGVuIC0tY292ZXJhZ2UgdXNlZClcbiAgICBjb3ZlcmFnZToge1xuICAgICAgcHJvdmlkZXI6IFwidjhcIixcbiAgICAgIHJlcG9ydGVyOiBbXCJ0ZXh0XCIsIFwianNvblwiLCBcImh0bWxcIl0sXG4gICAgICBleGNsdWRlOiBbXG4gICAgICAgIFwiKiovbm9kZV9tb2R1bGVzLyoqXCIsXG4gICAgICAgIFwiKiovZGlzdC8qKlwiLFxuICAgICAgICBcIioqLyoudGVzdC5tanNcIixcbiAgICAgICAgXCIqKi8qLnNwZWMubWpzXCIsXG4gICAgICBdLFxuICAgIH0sXG5cbiAgICAvLyBXYXRjaCBtb2RlIG9wdGltaXphdGlvbiAoODAvMjAgRFgpXG4gICAgd2F0Y2hFeGNsdWRlOiBbXG4gICAgICBcIioqL25vZGVfbW9kdWxlcy8qKlwiLFxuICAgICAgXCIqKi9kaXN0LyoqXCIsXG4gICAgICBcIioqL2NvdmVyYWdlLyoqXCIsXG4gICAgICBcIioqLy5naXQvKipcIixcbiAgICAgIFwiKiovdGVzdC1yZXN1bHRzLyoqXCIsXG4gICAgXSxcbiAgfSxcbn0pO1xuIl0sCiAgIm1hcHBpbmdzIjogIjtBQU1BLFNBQVMsb0JBQW9CO0FBRTdCLElBQU0sT0FBTyxRQUFRLElBQUksT0FBTztBQUNoQyxJQUFNLFNBQVMsUUFBUSxJQUFJLGNBQWM7QUFFekMsSUFBTyx3QkFBUSxhQUFhO0FBQUEsRUFDMUIsTUFBTTtBQUFBO0FBQUEsSUFFSixNQUFNO0FBQUEsSUFDTixhQUFhO0FBQUEsTUFDWCxPQUFPO0FBQUEsUUFDTCxVQUFVO0FBQUEsTUFDWjtBQUFBLElBQ0Y7QUFBQTtBQUFBLElBR0EsYUFBYTtBQUFBO0FBQUEsSUFHYixhQUFhO0FBQUE7QUFBQSxJQUdiLFNBQVM7QUFBQSxNQUNQO0FBQUEsTUFDQTtBQUFBLE1BQ0E7QUFBQSxNQUNBO0FBQUEsTUFDQTtBQUFBLE1BQ0E7QUFBQSxNQUNBO0FBQUEsTUFDQTtBQUFBLE1BQ0E7QUFBQSxNQUNBO0FBQUEsSUFDRjtBQUFBLElBRUEsU0FBUztBQUFBLE1BQ1A7QUFBQSxNQUNBO0FBQUEsTUFDQTtBQUFBLE1BQ0E7QUFBQSxNQUNBO0FBQUEsTUFDQTtBQUFBLE1BQ0E7QUFBQSxNQUNBO0FBQUEsTUFDQTtBQUFBLE1BQ0E7QUFBQSxNQUNBO0FBQUEsTUFDQTtBQUFBLE1BQ0E7QUFBQSxNQUNBLFNBQVMsbUNBQW1DO0FBQUEsSUFDOUMsRUFBRSxPQUFPLE9BQU87QUFBQTtBQUFBLElBR2hCLFVBQVUsT0FDTixDQUFDLFNBQVMsU0FBUyxJQUNuQixDQUFDLFNBQVM7QUFBQTtBQUFBLElBR2QsWUFBWTtBQUFBLE1BQ1YsT0FBTztBQUFBLElBQ1Q7QUFBQTtBQUFBLElBR0EsU0FBUztBQUFBLElBQ1QsU0FBUztBQUFBLElBQ1QsaUJBQWlCO0FBQUE7QUFBQSxJQUdqQixVQUFVO0FBQUEsTUFDUixVQUFVO0FBQUEsTUFDVixVQUFVLENBQUMsUUFBUSxRQUFRLE1BQU07QUFBQSxNQUNqQyxTQUFTO0FBQUEsUUFDUDtBQUFBLFFBQ0E7QUFBQSxRQUNBO0FBQUEsUUFDQTtBQUFBLE1BQ0Y7QUFBQSxJQUNGO0FBQUE7QUFBQSxJQUdBLGNBQWM7QUFBQSxNQUNaO0FBQUEsTUFDQTtBQUFBLE1BQ0E7QUFBQSxNQUNBO0FBQUEsTUFDQTtBQUFBLElBQ0Y7QUFBQSxFQUNGO0FBQ0YsQ0FBQzsiLAogICJuYW1lcyI6IFtdCn0K
