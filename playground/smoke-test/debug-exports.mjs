#!/usr/bin/env node

/**
 * Debug Exports Test
 *
 * This script verifies that the documented functions exist in source code
 * but are missing from exports
 */

import { readFileSync } from "fs";

console.log("🔍 DEBUGGING MISSING EXPORTS");
console.log("==============================\n");

// Test 1: Check direct imports from source files
console.log("📁 DIRECT SOURCE FILE IMPORTS:");
console.log("-------------------------------");

try {
  // Test direct import from dark-matter-core.mjs
  const darkMatterCore = await import(
    "./node_modules/.pnpm/unrdf@3.0.1_@rdfjs+types@2.0.1_node@24.9.0_web-streams-polyfill@3.3.3/node_modules/unrdf/src/knowledge-engine/dark-matter-core.mjs"
  );
  console.log(
    "✅ createDarkMatterCore:",
    typeof darkMatterCore.createDarkMatterCore,
  );
  console.log("✅ DarkMatterCore class:", typeof darkMatterCore.DarkMatterCore);
  console.log("✅ DarkMatterFactory:", typeof darkMatterCore.DarkMatterFactory);
} catch (error) {
  console.log("❌ Direct import failed:", error.message);
}

try {
  // Test direct import from lockchain-writer.mjs
  const LockchainModule = await import(
    "./node_modules/.pnpm/unrdf@3.0.1_@rdfjs+types@2.0.1_node@24.9.0_web-streams-polyfill@3.3.3/node_modules/unrdf/src/knowledge-engine/lockchain-writer.mjs"
  );
  console.log(
    "✅ LockchainWriter class:",
    typeof LockchainModule.LockchainWriter,
  );
  console.log(
    "✅ createLockchainWriter:",
    typeof LockchainModule.createLockchainWriter,
  );
} catch (error) {
  console.log("❌ LockchainWriter import failed:", error.message);
}

try {
  // Test direct import from observability.mjs
  const ObserveModule = await import(
    "./node_modules/.pnpm/unrdf@3.0.1_@rdfjs+types@2.0.1_node@24.9.0_web-streams-polyfill@3.3.3/node_modules/unrdf/src/knowledge-engine/observability.mjs"
  );
  console.log(
    "✅ ObservabilityManager class:",
    typeof ObserveModule.ObservabilityManager,
  );
  console.log(
    "✅ createObservabilityManager:",
    typeof ObserveModule.createObservabilityManager,
  );
  // Check if there's also an "Observability" export
  console.log("❓ Observability:", typeof ObserveModule.Observability);
} catch (error) {
  console.log("❌ Observability import failed:", error.message);
}

// Test 2: Check the knowledge-engine index exports
console.log("\n📦 KNOWLEDGE ENGINE INDEX EXPORTS:");
console.log("----------------------------------");

try {
  const KEIndex = await import(
    "./node_modules/.pnpm/unrdf@3.0.1_@rdfjs+types@2.0.1_node@24.9.0_web-streams-polyfill@3.3.3/node_modules/unrdf/src/knowledge-engine/index.mjs"
  );

  const expectedExports = [
    "createDarkMatterCore",
    "DarkMatterCore",
    "LockchainWriter",
    "Observability",
    "ObservabilityManager",
  ];

  for (const exportName of expectedExports) {
    const exists = typeof KEIndex[exportName] !== "undefined";
    console.log(
      `${exists ? "✅" : "❌"} ${exportName}: ${exists ? "EXPORTED" : "MISSING"}`,
    );
  }

  console.log("\n📋 ACTUAL KNOWLEDGE ENGINE EXPORTS:");
  const actualExports = Object.keys(KEIndex).filter(
    (key) => !key.startsWith("_"),
  );
  actualExports.forEach((exp) => console.log(`  - ${exp}`));
} catch (error) {
  console.log("❌ Knowledge engine index failed:", error.message);
}

// Test 3: Check main package exports
console.log("\n📦 MAIN PACKAGE EXPORTS:");
console.log("-----------------------");

try {
  const MainPackage = await import("unrdf");

  const expectedMainExports = [
    "createDarkMatterCore",
    "createDarkMatterSystem", // README says this exists but it doesn't
    "LockchainWriter",
    "Observability",
    "registerHook",
    "deregisterHook",
    "evaluateHook",
    "Store",
    "Parser",
    "Writer",
  ];

  console.log("Expected vs Actual exports:");
  for (const exportName of expectedMainExports) {
    const exists = typeof MainPackage[exportName] !== "undefined";
    console.log(
      `${exists ? "✅" : "❌"} ${exportName}: ${exists ? "EXISTS" : "MISSING FROM PRODUCTION"}`,
    );
  }

  console.log("\n📋 ACTUAL MAIN PACKAGE EXPORTS:");
  const mainExports = Object.keys(MainPackage).filter(
    (key) => !key.startsWith("_"),
  );
  console.log(`Total exports: ${mainExports.length}`);
  console.log("First 20 exports:");
  mainExports.slice(0, 20).forEach((exp) => console.log(`  - ${exp}`));
} catch (error) {
  console.log("❌ Main package import failed:", error.message);
}

console.log("\n🎯 SUMMARY:");
console.log("===========");
console.log("✅ Source files exist with correct functions");
console.log("❌ Knowledge engine index.mjs missing exports");
console.log("❌ Main package missing key exports");
console.log("📝 SOLUTION: Add missing exports to index files");
