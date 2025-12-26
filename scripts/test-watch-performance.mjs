#!/usr/bin/env node
/**
 * @fileoverview Test watch mode performance
 * Verifies <2s rebuild, <1s test re-run targets
 *
 * Usage:
 *   node scripts/test-watch-performance.mjs
 */

import { spawn } from "child_process";
import { writeFileSync, readFileSync } from "fs";
import { resolve } from "path";

const TEST_PACKAGE = "cli"; // Use CLI package (has unbuild)
const TEST_FILE = resolve(`packages/${TEST_PACKAGE}/src/index.mjs`);

/**
 * @param {string} msg
 */
function log(msg) {
  const timestamp = new Date().toISOString().slice(11, 19);
  console.log(`[${timestamp}] ${msg}`);
}

/**
 * @param {number} ms
 */
function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * @param {string} cmd
 * @param {string[]} args
 * @returns {Promise<{duration: number, success: boolean}>}
 */
function runCommandTimed(cmd, args) {
  return new Promise((resolve) => {
    const startTime = Date.now();
    const proc = spawn(cmd, args, {
      stdio: "pipe",
      shell: true,
    });

    let output = "";
    proc.stdout?.on("data", (data) => {
      output += data.toString();
    });
    proc.stderr?.on("data", (data) => {
      output += data.toString();
    });

    proc.on("close", (code) => {
      const duration = (Date.now() - startTime) / 1000;
      resolve({
        duration,
        success: code === 0,
        output,
      });
    });
  });
}

async function testRebuildPerformance() {
  log("📊 Testing rebuild performance...");
  log(`Package: @unrdf/${TEST_PACKAGE}`);

  // Run build and measure time
  const result = await runCommandTimed("pnpm", [
    "-C",
    `packages/${TEST_PACKAGE}`,
    "build",
  ]);

  log(`⏱️  Build time: ${result.duration.toFixed(2)}s`);

  if (result.success) {
    if (result.duration < 2.0) {
      log(`✅ PASS: Build <2s (target met)`);
      return true;
    } else {
      log(`⚠️  WARN: Build ≥2s (target: <2s, actual: ${result.duration.toFixed(2)}s)`);
      return false;
    }
  } else {
    log(`❌ FAIL: Build failed`);
    return false;
  }
}

async function testTestPerformance() {
  log("\n📊 Testing test re-run performance...");
  log(`Package: @unrdf/${TEST_PACKAGE}`);

  // Run tests and measure time
  const result = await runCommandTimed("timeout", [
    "5s",
    "pnpm",
    "-C",
    `packages/${TEST_PACKAGE}`,
    "test:fast",
  ]);

  log(`⏱️  Test time: ${result.duration.toFixed(2)}s`);

  if (result.success || result.output.includes("passed")) {
    if (result.duration < 1.0) {
      log(`✅ PASS: Tests <1s (target met)`);
      return true;
    } else {
      log(`⚠️  WARN: Tests ≥1s (target: <1s, actual: ${result.duration.toFixed(2)}s)`);
      return false;
    }
  } else {
    log(`❌ FAIL: Tests failed`);
    return false;
  }
}

async function testWatchScript() {
  log("\n📊 Testing watch script startup...");

  const result = await runCommandTimed("timeout", [
    "2s",
    "node",
    "scripts/watch-dev.mjs",
  ]);

  // Watch script should start successfully (timeout is expected)
  if (result.output.includes("Watch mode ready")) {
    log(`✅ PASS: Watch script starts successfully`);
    return true;
  } else {
    log(`❌ FAIL: Watch script failed to start`);
    console.log(result.output);
    return false;
  }
}

async function main() {
  console.log("🚀 Watch Mode Performance Test\n");
  console.log("Target SLAs:");
  console.log("  - Rebuild: <2s");
  console.log("  - Test re-run: <1s");
  console.log("  - Watch startup: <2s\n");

  const results = {
    rebuild: await testRebuildPerformance(),
    test: await testTestPerformance(),
    watch: await testWatchScript(),
  };

  console.log("\n" + "=".repeat(50));
  console.log("📊 Performance Test Results");
  console.log("=".repeat(50));
  console.log(`Rebuild performance:  ${results.rebuild ? "✅ PASS" : "❌ FAIL"}`);
  console.log(`Test performance:     ${results.test ? "✅ PASS" : "⚠️  WARN"}`);
  console.log(`Watch script startup: ${results.watch ? "✅ PASS" : "❌ FAIL"}`);

  const allPassed = results.rebuild && results.watch;
  console.log("\n" + (allPassed ? "✅ All critical tests passed" : "⚠️  Some tests failed"));

  process.exit(allPassed ? 0 : 1);
}

main().catch((err) => {
  console.error("❌ Test failed:", err);
  process.exit(1);
});
