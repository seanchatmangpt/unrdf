#!/usr/bin/env node
/**
 * @fileoverview Test execution summary script
 * Provides clear, actionable test output with timing and failure details
 *
 * Usage:
 *   pnpm test | node scripts/test-summary.mjs
 *   pnpm test:yawl | node scripts/test-summary.mjs
 */

import { createInterface } from "node:readline";
import { stdin, stdout } from "node:process";

const results = {
  packages: {},
  totalPassed: 0,
  totalFailed: 0,
  totalDuration: 0,
  failures: [],
};

const rl = createInterface({
  input: stdin,
  output: stdout,
  terminal: false,
});

let currentPackage = null;
let currentTest = null;

rl.on("line", (line) => {
  // Pass through all lines
  console.log(line);

  // Extract package name
  const pkgMatch = line.match(/packages\/([^\/]+)\s+test/);
  if (pkgMatch) {
    currentPackage = pkgMatch[1];
    if (!results.packages[currentPackage]) {
      results.packages[currentPackage] = {
        passed: 0,
        failed: 0,
        duration: 0,
      };
    }
  }

  // Extract test results
  const testMatch = line.match(/Tests\s+(\d+)\s+failed.*?(\d+)\s+passed/);
  if (testMatch && currentPackage) {
    results.packages[currentPackage].failed = parseInt(testMatch[1]);
    results.packages[currentPackage].passed = parseInt(testMatch[2]);
    results.totalFailed += parseInt(testMatch[1]);
    results.totalPassed += parseInt(testMatch[2]);
  }

  // Extract duration
  const durationMatch = line.match(/Duration\s+([\d.]+)s/);
  if (durationMatch && currentPackage) {
    results.packages[currentPackage].duration = parseFloat(durationMatch[1]);
    results.totalDuration += parseFloat(durationMatch[1]);
  }

  // Extract failure details
  const failMatch = line.match(/FAIL.*?(test\/.*?\.test\.mjs)/);
  if (failMatch) {
    currentTest = failMatch[1];
  }

  // Extract assertion errors with line numbers
  const assertMatch = line.match(/❯\s+(.*?\.test\.mjs):(\d+):(\d+)/);
  if (assertMatch && currentPackage) {
    results.failures.push({
      package: currentPackage,
      file: assertMatch[1],
      line: assertMatch[2],
      col: assertMatch[3],
    });
  }
});

rl.on("close", () => {
  // Print summary
  console.log("\n");
  console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  console.log("📊 TEST EXECUTION SUMMARY");
  console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  console.log("");

  const packageNames = Object.keys(results.packages);
  if (packageNames.length > 0) {
    console.log("📦 PACKAGE RESULTS:");
    packageNames.forEach((pkg) => {
      const res = results.packages[pkg];
      const status = res.failed > 0 ? "❌" : "✅";
      console.log(
        `  ${status} ${pkg.padEnd(20)} ${res.passed} passed, ${res.failed} failed (${res.duration.toFixed(2)}s)`,
      );
    });
    console.log("");
  }

  console.log("📈 OVERALL:");
  console.log(`  Total Passed:  ${results.totalPassed}`);
  console.log(`  Total Failed:  ${results.totalFailed}`);
  console.log(`  Total Duration: ${results.totalDuration.toFixed(2)}s`);
  console.log("");

  if (results.failures.length > 0) {
    console.log("❌ FAILED TESTS:");
    results.failures.forEach((fail, i) => {
      console.log(`  ${i + 1}. ${fail.package}/${fail.file}:${fail.line}`);
    });
    console.log("");
    console.log("💡 TO DEBUG:");
    console.log(`  pnpm test:${results.failures[0].package}`);
    console.log(`  # Or run specific test:`);
    console.log(`  cd packages/${results.failures[0].package}`);
    console.log(`  pnpm test ${results.failures[0].file}`);
    console.log("");
  }

  console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

  process.exit(results.totalFailed > 0 ? 1 : 0);
});
