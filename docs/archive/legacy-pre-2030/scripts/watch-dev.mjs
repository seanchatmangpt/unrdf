#!/usr/bin/env node
/**
 * @fileoverview Smart watch mode for UNRDF development
 * 80/20 Strategy: Watch only changed packages for instant feedback
 *
 * Features:
 * - File watcher with chokidar (fast, reliable)
 * - Auto rebuild on source changes (<2s target)
 * - Auto re-run tests on test changes (<1s target)
 * - Desktop notifications for test results
 * - Clear, timestamped console output
 *
 * Usage:
 *   node scripts/watch-dev.mjs
 *   PACKAGE=core node scripts/watch-dev.mjs  # Watch single package
 */

import { watch } from "chokidar";
import { spawn } from "child_process";
import { resolve, relative } from "path";
import notifier from "node-notifier";

const WATCHED_PATTERNS = [
  "packages/*/src/**/*.mjs",
  "packages/*/test/**/*.test.mjs",
];

const EXCLUDE_PATTERNS = [
  "**/node_modules/**",
  "**/dist/**",
  "**/coverage/**",
  "**/.git/**",
];

/**
 * @param {string} msg
 */
function log(msg) {
  const timestamp = new Date().toISOString().slice(11, 19);
  console.log(`[${timestamp}] ${msg}`);
}

/**
 * @param {string} cmd
 * @param {string[]} args
 * @returns {Promise<{code: number, stdout: string, stderr: string}>}
 */
function runCommand(cmd, args) {
  return new Promise((resolve) => {
    const startTime = Date.now();
    const proc = spawn(cmd, args, {
      stdio: ["ignore", "pipe", "pipe"],
      shell: true,
    });

    let stdout = "";
    let stderr = "";

    proc.stdout?.on("data", (data) => {
      stdout += data.toString();
    });

    proc.stderr?.on("data", (data) => {
      stderr += data.toString();
    });

    proc.on("close", (code) => {
      const duration = ((Date.now() - startTime) / 1000).toFixed(2);
      resolve({
        code: code ?? 0,
        stdout,
        stderr,
        duration,
      });
    });
  });
}

/**
 * @param {string} path
 * @returns {string | null}
 */
function getPackageFromPath(path) {
  const match = path.match(/packages\/([^/]+)\//);
  return match ? match[1] : null;
}

/**
 * @param {string} pkg
 */
async function rebuildPackage(pkg) {
  log(`🔨 Rebuilding @unrdf/${pkg}...`);

  const result = await runCommand("pnpm", [
    "-C",
    `packages/${pkg}`,
    "build",
  ]);

  if (result.code === 0) {
    log(`✅ Build successful (${result.duration}s)`);
    return true;
  } else {
    log(`❌ Build failed (${result.duration}s)`);
    console.error(result.stderr);
    return false;
  }
}

/**
 * @param {string} pkg
 */
async function runTests(pkg) {
  log(`🧪 Running tests for @unrdf/${pkg}...`);

  const result = await runCommand("timeout", [
    "5s",
    "pnpm",
    "-C",
    `packages/${pkg}`,
    "test:fast",
  ]);

  const passed = result.stdout.match(/(\d+) passed/)?.[1] ?? "0";
  const failed = result.stdout.match(/(\d+) failed/)?.[1] ?? "0";

  if (result.code === 0) {
    log(`✅ Tests passed: ${passed} (${result.duration}s)`);

    notifier.notify({
      title: `✅ @unrdf/${pkg}`,
      message: `${passed} tests passed`,
      sound: false,
      timeout: 2,
    });

    return true;
  } else {
    log(`❌ Tests failed: ${passed} passed, ${failed} failed (${result.duration}s)`);

    notifier.notify({
      title: `❌ @unrdf/${pkg}`,
      message: `${failed} tests failed`,
      sound: true,
      timeout: 5,
    });

    return false;
  }
}

/**
 * @param {string} path
 */
async function handleFileChange(path) {
  const relativePath = relative(process.cwd(), path);
  const pkg = getPackageFromPath(relativePath);

  if (!pkg) {
    return;
  }

  log(`📝 File changed: ${relativePath}`);

  // Determine if it's a source or test file
  const isTest = relativePath.includes("/test/");

  if (isTest) {
    // Test file changed - just re-run tests (fast path)
    await runTests(pkg);
  } else {
    // Source file changed - rebuild then test
    const buildSuccess = await rebuildPackage(pkg);

    if (buildSuccess) {
      await runTests(pkg);
    }
  }
}

// Debounce file changes (avoid multiple rebuilds)
let changeTimeout = null;
let pendingChanges = new Set();

/**
 * @param {string} path
 */
function debouncedHandleChange(path) {
  pendingChanges.add(path);

  if (changeTimeout) {
    clearTimeout(changeTimeout);
  }

  changeTimeout = setTimeout(() => {
    const changes = Array.from(pendingChanges);
    pendingChanges.clear();

    // Process only the first change (avoid cascading rebuilds)
    if (changes.length > 0) {
      handleFileChange(changes[0]).catch((err) => {
        log(`❌ Error: ${err.message}`);
      });
    }
  }, 300);
}

// Start watching
log("👀 Starting watch mode...");
log(`Watching: ${WATCHED_PATTERNS.join(", ")}`);

const watcher = watch(WATCHED_PATTERNS, {
  ignored: EXCLUDE_PATTERNS,
  persistent: true,
  ignoreInitial: true,
  awaitWriteFinish: {
    stabilityThreshold: 100,
    pollInterval: 50,
  },
});

watcher
  .on("change", (path) => debouncedHandleChange(path))
  .on("add", (path) => debouncedHandleChange(path))
  .on("ready", () => {
    log("✅ Watch mode ready - make changes to see instant feedback!");
    log("Press Ctrl+C to exit");
  })
  .on("error", (error) => {
    log(`❌ Watcher error: ${error.message}`);
  });

// Graceful shutdown
process.on("SIGINT", () => {
  log("👋 Stopping watch mode...");
  watcher.close();
  process.exit(0);
});
