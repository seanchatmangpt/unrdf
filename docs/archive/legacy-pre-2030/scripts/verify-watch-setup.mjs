#!/usr/bin/env node
/**
 * @fileoverview Verify watch mode setup
 * Checks all watch mode components are properly configured
 *
 * Usage:
 *   node scripts/verify-watch-setup.mjs
 */

import { existsSync, readFileSync } from "fs";
import { resolve } from "path";

const checks = [];
let passed = 0;
let failed = 0;

function check(name, condition, fix = null) {
  if (condition) {
    console.log(`✅ ${name}`);
    passed++;
  } else {
    console.log(`❌ ${name}`);
    if (fix) {
      console.log(`   Fix: ${fix}`);
    }
    failed++;
  }
}

console.log("🔍 Verifying Watch Mode Setup\n");

// Check package.json has watch dependencies
const packageJson = JSON.parse(
  readFileSync(resolve("package.json"), "utf-8"),
);

check(
  "chokidar dependency in package.json",
  packageJson.devDependencies?.chokidar,
  "Run: pnpm add -D -w chokidar",
);

check(
  "concurrently dependency in package.json",
  packageJson.devDependencies?.concurrently,
  "Run: pnpm add -D -w concurrently",
);

check(
  "node-notifier dependency in package.json",
  packageJson.devDependencies?.["node-notifier"],
  "Run: pnpm add -D -w node-notifier",
);

// Check package.json has watch scripts
check(
  "dev:watch script in package.json",
  packageJson.scripts?.["dev:watch"]?.includes("watch-dev.mjs"),
  'Add: "dev:watch": "node scripts/watch-dev.mjs"',
);

check(
  "dev:test script in package.json",
  packageJson.scripts?.["dev:test"]?.includes("vitest"),
  'Add: "dev:test": "vitest --watch"',
);

check(
  "dev:types script in package.json",
  packageJson.scripts?.["dev:types"]?.includes("typecheck-watch.mjs"),
  'Add: "dev:types": "node scripts/typecheck-watch.mjs"',
);

check(
  "dev:full script in package.json",
  packageJson.scripts?.["dev:full"]?.includes("concurrently"),
  'Add: "dev:full": "concurrently ..."',
);

// Check watch scripts exist
check(
  "watch-dev.mjs script exists",
  existsSync(resolve("scripts/watch-dev.mjs")),
  "Create: scripts/watch-dev.mjs",
);

check(
  "typecheck-watch.mjs script exists",
  existsSync(resolve("scripts/typecheck-watch.mjs")),
  "Create: scripts/typecheck-watch.mjs",
);

check(
  "test-watch-performance.mjs script exists",
  existsSync(resolve("scripts/test-watch-performance.mjs")),
  "Create: scripts/test-watch-performance.mjs",
);

// Check vitest config
const vitestConfig = readFileSync(resolve("vitest.config.mjs"), "utf-8");

check(
  "vitest.config.mjs has watchExclude",
  vitestConfig.includes("watchExclude"),
  "Add watchExclude to vitest.config.mjs",
);

// Check documentation
check(
  "watch-mode-guide.md exists",
  existsSync(resolve("docs/watch-mode-guide.md")),
  "Create: docs/watch-mode-guide.md",
);

// Summary
console.log("\n" + "=".repeat(50));
console.log(`✅ Passed: ${passed}`);
console.log(`❌ Failed: ${failed}`);
console.log("=".repeat(50));

if (failed === 0) {
  console.log("\n✅ Watch mode setup is complete!");
  console.log("\nNext steps:");
  console.log("1. Run: pnpm install (if dependencies not yet installed)");
  console.log("2. Test: pnpm dev (starts file watcher)");
  console.log("3. Test: pnpm dev:test (starts test watcher)");
  console.log("4. Test: pnpm dev:types (starts type checker)");
  console.log("5. Test: pnpm dev:full (starts all three)");
  console.log("\nSee docs/watch-mode-guide.md for full documentation");
} else {
  console.log("\n⚠️  Some checks failed - see fixes above");
  process.exit(1);
}
