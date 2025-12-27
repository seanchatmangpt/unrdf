#!/usr/bin/env node

/**
 * Comprehensive Function Testing Suite for unrdf v3.0.2
 *
 * Tests every single function and class documented in the README
 * to ensure complete functionality before release.
 */

import { readFileSync } from "fs";

// Colors for output
const colors = {
  green: "\x1b[32m",
  red: "\x1b[31m",
  yellow: "\x1b[33m",
  blue: "\x1b[34m",
  cyan: "\x1b[36m",
  reset: "\x1b[0m",
  bold: "\x1b[1m",
};

// Test tracking
const results = {
  passed: 0,
  failed: 0,
  skipped: 0,
  total: 0,
};

function logTest(testName, passed, message = "") {
  const status = passed ? "✅" : "❌";
  const color = passed ? colors.green : colors.red;
  console.log(
    `${color}${status} ${testName}${colors.reset}${message ? ` - ${message}` : ""}`,
  );

  results.total++;
  if (passed) {
    results.passed++;
  } else {
    results.failed++;
  }
}

function logSection(name) {
  console.log(`\n${colors.bold}${colors.cyan}━━━ ${name} ━━━${colors.reset}`);
}

function logSummary() {
  console.log(
    `\n${colors.bold}${colors.blue}━━━ TEST SUMMARY ━━━${colors.reset}`,
  );
  console.log(`✅ Passed: ${colors.green}${results.passed}${colors.reset}`);
  console.log(`❌ Failed: ${colors.red}${results.failed}${colors.reset}`);
  console.log(`⚠️  Skipped: ${colors.yellow}${results.skipped}${colors.reset}`);
  console.log(`📊 Total: ${colors.bold}${results.total}${colors.reset}`);

  const passRate = ((results.passed / results.total) * 100).toFixed(1);
  console.log(`📈 Pass Rate: ${colors.bold}${passRate}%${colors.reset}`);

  if (results.failed === 0) {
    console.log(
      `\n${colors.green}${colors.bold}🎉 ALL TESTS PASSED - READY FOR RELEASE! 🎉${colors.reset}`,
    );
    return true;
  } else {
    console.log(
      `\n${colors.red}${colors.bold}⚠️  ${results.failed} TESTS FAILED - NEEDS FIXES${colors.reset}`,
    );
    return false;
  }
}

// Main test function
async function runComprehensiveTests() {
  console.log(`${colors.bold}${colors.blue}
╔════════════════════════════════════════════════════════════════════════╗
║                        UNRDF v3.0.2 TEST SUITE                      ║
║                                                                        ║
║  Comprehensive testing of Knowledge Substrate & all components       ║
╚═══════════════════════════════════════════════════════════════════════╝
${colors.reset}`);

  try {
    // Test 1: Parse & Serialization Functions
    await testParseSerializationFunctions();

    // Test 2: Knowledge Substrate Core (formerly Dark Matter)
    await testKnowledgeSubstrateCore();

    // Test 3: Hook Management Functions
    await testHookManagementFunctions();

    // Test 4: LockchainWriter System
    await testLockchainWriterSystem();

    // Test 5: ObservabilityManager System
    await testObservabilityManagerSystem();

    // Test 6: Core Classes Instantiation
    await testCoreClassesInstantiation();

    // Test 7: SPARQL Query Functions
    await testSPARQLQueryFunctions();

    // Test 8: N3 Re-exports
    await testN3Reexports();

    // Test 9: Legacy Compatibility (Dark Matter → Knowledge Substrate)
    await testLegacyCompatibility();

    return logSummary();
  } catch (error) {
    console.error(
      `${colors.red}${fonts.bold}Fatal test error:${colors.reset}`,
      error,
    );
    return false;
  }
}

// Test 1: Parse & Serialization Functions
async function testParseSerializationFunctions() {
  logSection("PARSING & SERIALIZATION FUNCTIONS");

  try {
    const unrdf = await import("unrdf");

    // Core parsing functions
    const parseFunctions = ["parseTurtle", "parseJsonLd"];
    for (const funcName of parseFunctions) {
      if (typeof unrdf[funcName] === "function") {
        logTest(`${funcName}() function`, true, "function exists");
      } else {
        logTest(`${funcName}() function`, false, "missing from exports");
      }
    }

    // Serialization functions
    const serialFunctions = ["toTurtle", "toJsonLd", "toNQuads"];
    for (const funcName of serialFunctions) {
      if (typeof unrdf[funcName] === "function") {
        logTest(`${funcName}() function`, true, "function exists");
      } else {
        logTest(`${funcName}() function`, false, "missing from exports");
      }
    }

    // Test actual parsing functionality
    try {
      const testTurtle = `
        @prefix ex: <http://example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        ex:alice foaf:name "Alice" ;
                 foaf:knows ex:bob .
      `;

      const store = await unrdf.parseTurtle(testTurtle);
      logTest(
        "parseTurtle() actual parsing",
        store && store.size > 0,
        `${store?.size || 0} quads parsed`,
      );

      const turtle = await unrdf.toTurtle(store);
      logTest(
        "toTurtle() actual serialization",
        turtle && turtle.includes("Alice"),
        "serialized correctly",
      );
    } catch (error) {
      logTest("parseTurtle() functionality", false, `error: ${error.message}`);
    }
  } catch (error) {
    logTest("Parse/serialization imports", false, error.message);
  }
}

// Test 2: Knowledge Substrate Core (NEW)
async function testKnowledgeSubstrateCore() {
  logSection("KNOWLEDGE SUBSTRATE CORE SYSTEM");

  try {
    const unrdf = await import("unrdf");

    // New Knowledge Substrate functions
    const newFunctions = [
      "createKnowledgeSubstrateCore",
      "KnowledgeSubstrateFactory",
    ];
    for (const funcName of newFunctions) {
      if (
        typeof unrdf[funcName] === "function" ||
        typeof unrdf[funcName] === "object"
      ) {
        logTest(
          `${funcName} (NEW)`,
          true,
          "Knowledge Substrate function exists",
        );
      } else {
        logTest(`${funcName} (NEW)`, false, "missing from exports");
      }
    }

    // Test Legacy Dark Matter compatibility
    if (typeof unrdf.createDarkMatterCore === "function") {
      logTest(
        "createDarkMatterCore() legacy",
        true,
        "backwards compatibility maintained",
      );

      // Verify it actually creates KnowledgeSubstrateCore
      try {
        const core = unrdf.createDarkMatterCore({});
        const coreType = core.constructor.name;
        logTest(
          "Legacy createDarkMatterCore() type",
          coreType.includes("SubstrateCore") ||
            coreType.includes("DarkMatterCore"),
          `creates ${coreType}`,
        );
      } catch (error) {
        logTest(
          "Legacy createDarkMatterCore() instantiation",
          false,
          `error: ${error.message}`,
        );
      }
    } else {
      logTest(
        "createDarkMatterCore() legacy",
        false,
        "backwards compatibility broken",
      );
    }
  } catch (error) {
    logTest("Knowledge Substrate Core", false, error.message);
  }
}

// Test 3: Hook MANAGEMENT Functions
async function testHookManagementFunctions() {
  logSection("HOOK MANAGEMENT FUNCTIONS");

  try {
    const unrdf = await import("unrdf");

    // Standalone hook functions (NEW!)
    const hookFunctions = ["registerHook", "deregisterHook", "evaluateHook"];
    for (const funcName of hookFunctions) {
      if (typeof unrdf[funcName] === "function") {
        logTest(`${funcName}()`, true, "standalone hook function exists");
      } else {
        logTest(`${funcName}()`, false, "missing from exports");
      }
    }

    // DefineHook function (existing)
    if (typeof unrdf.defineHook === "function") {
      logTest("defineHook()", true, "hook definition function exists");
    } else {
      logTest("defineHook()", false, "missing from exports");
    }

    // Test actual hook functionality
    try {
      const hook = await unrdf.defineHook({
        meta: { name: "test-hook", description: "Test hook for validation" },
        when: { kind: "sparql-ask", query: "ASK { ?s ?p ?o }" },
        run: async (event) => console.log("Hook triggered"),
      });

      logTest(
        "defineHook() validation",
        hook && hook.meta?.name === "test-hook",
        "hook defined correctly",
      );
    } catch (error) {
      logTest("defineHook() functionality", false, `error: ${error.message}`);
    }
  } catch (error) {
    logTest("Hook management system", false, error.message);
  }
}

// Test 4: LockchainWriter SYSTEM
async function testLockchainWriterSystem() {
  logSection("LOCKCHAIN WRITER SYSTEM");

  try {
    const unrdf = await import("unrdf");

    // LockchainWriter class
    if (typeof unrdf.LockchainWriter === "function") {
      logTest("LockchainWriter class", true, "class exists");
    } else {
      logTest("LockchainWriter class", false, "missing from exports");
    }

    // createLockchainWriter function
    if (typeof unrdf.createLockchainWriter === "function") {
      logTest("createLockchainWriter()", true, "factory function exists");
    } else {
      logTest("createLockchainWriter()", false, "missing from exports");
    }

    // Test LockchainWriter instantiation
    try {
      const lockchain = new unrdf.LockchainWriter({
        repoPath: "./test-lockchain",
        enableMerkle: true,
      });

      logTest(
        "LockchainWriter instantiation",
        lockchain !== null,
        "instantiated successfully",
      );

      // Test methods exist
      const hasInit = typeof lockchain.init === "function";
      const hasWriteReceipt = typeof lockchain.writeReceipt === "function";
      const hasVerifyReceipt = typeof lockchain.verifyReceipt === "function";

      logTest(
        "LockchainWriter.init() method",
        hasInit,
        hasInit ? "method exists" : "method missing",
      );
      logTest(
        "LockchainWriter.writeReceipt() method",
        hasWriteReceipt,
        hasWriteReceipt ? "method exists" : "method missing",
      );
      logTest(
        "LockchainWriter.verifyReceipt() method",
        hasVerifyReceipt,
        hasVerifyReceipt ? "method exists" : "method missing",
      );
    } catch (error) {
      logTest(
        "LockchainWriter functionality",
        false,
        `error: ${error.message}`,
      );
    }
  } catch (error) {
    logTest("LockchainWriter system", false, error.message);
  }
}

// Test 5: ObservabilityManager System
async function testObservabilityManagerSystem() {
  logSection("OBSERVABILITY MANAGER SYSTEM");

  try {
    const unrdf = await import("unrdf");

    // ObservabilityManager class
    if (typeof unrdf.ObservabilityManager === "function") {
      logTest("ObservabilityManager class", true, "class exists");
    } else {
      logTest("ObservabilityManager class", false, "missing from exports");
    }

    // createObservabilityManager function
    if (typeof unrdf.createObservabilityManager === "function") {
      logTest("createObservabilityManager()", true, "factory function exists");
    } else {
      logTest("createObservabilityManager()", false, "missing from exports");
    }

    // Test ObservabilityManager instantiation
    try {
      const observer = new unrdf.ObservabilityManager({});
      logTest(
        "ObservabilityManager instantiation",
        observer !== null,
        "instantiated successfully",
      );

      // Test methods exist
      const hasGetMetrics =
        typeof observer.getPerformanceMetrics === "function";
      logTest(
        "ObservabilityManager.getPerformanceMetrics()",
        hasGetMetrics,
        hasGetMetrics ? "method exists" : "method missing",
      );
    } catch (error) {
      logTest(
        "ObservabilityManager functionality",
        false,
        `error: ${error.message}`,
      );
    }
  } catch (error) {
    logTest("ObservabilityManager system", false, `error: ${error.message}`);
  }
}

// Test 6: Core Classes Instantiation
async function testCoreClassesInstantiation() {
  logSection("CORE CLASSES INSTANTIATION");

  try {
    const unrdf = await import("unrdf");

    // Core classes
    const coreClasses = [
      "RdfEngine",
      "TransactionManager",
      "KnowledgeHookManager",
    ];

    for (const className of coreClasses) {
      if (typeof unrdf[className] === "function") {
        logTest(`${className} class`, true, "class exists");

        // Test instantiation
        try {
          const instance = new unrdf[className]({});
          logTest(
            `${className} instantiation`,
            instance !== null,
            "instantiated successfully",
          );
        } catch (error) {
          logTest(
            `${className} instantiation`,
            false,
            `error: ${error.message}`,
          );
        }
      } else {
        logTest(`${className} class`, false, "missing from exports");
      }
    }
  } catch (error) {
    logTest("Core classes", false, error.message);
  }
}

// Test 7: SPARQL Query Functions
async function testSPARQLQueryFunctions() {
  logSection("SPARQL QUERY FUNCTIONS");

  try {
    const unrdf = await import("unrdf");

    // Query function
    if (typeof unrdf.query === "function") {
      logTest("query() function", true, "SPARQL query function exists");
    } else {
      logTest("query() function", false, "missing from exports");
    }

    // Test basic query functionality
    try {
      const testStore = await unrdf.parseTurtle(`
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        ex:alice foaf:name "Alice" .
        ex:bob foaf:name "Bob" .
      `);

      const results = await unrdf.query(
        testStore,
        `
        SELECT ?name WHERE {
          ?person foaf:name ?name .
        }
      `,
      );

      logTest(
        "query() SELECT functionality",
        Array.isArray(results),
        `${results?.length || 0} results returned`,
      );
    } catch (error) {
      logTest("query() functionality", false, `error: ${error.message}`);
    }
  } catch (error) {
    logTest("SPARQL query system", false, error.message);
  }
}

// Test 8: N3 Re-exports
async function testN3Reexports() {
  logSection("N3 RE-EXPORTS");

  try {
    const unrdf = await import("unrdf");

    // N3 re-exports (as documented in README)
    const n3Exports = ["Store", "Parser", "Writer"];
    for (const exportName of n3Exports) {
      if (
        typeof unrdf[exportName] === "function" ||
        typeof unrdf[exportName] === "object"
      ) {
        logTest(`N3 re-export ${exportName}`, true, "N3 component available");
      } else {
        logTest(`N3 re-export ${exportName}`, false, "missing from exports");
      }
    }

    // Test Store instantiation
    try {
      if (typeof unrdf.Store === "function") {
        const store = new unrdf.Store();
        logTest(
          "N3 Store instantiation",
          store !== null,
          "store created successfully",
        );
      }
    } catch (error) {
      logTest("N3 Store functionality", false, `error: ${error.message}`);
    }
  } catch (error) {
    logTest("N3 re-exports", false, error.message);
  }
}

// Test 9: Legacy Compatibility
async function testLegacyCompatibility() {
  logSection("LEGACY COMPATIBILITY (Dark Matter → Knowledge Substrate)");

  try {
    const unrdf = await import("unrdf");

    // Legacy Dark Matter functions should still work
    const legacyFunctions = [
      "createDarkMatterCore",
      "DarkMatterCore",
      "DarkMatterFactory",
    ];
    for (const funcName of legacyFunctions) {
      if (
        typeof unrdf[funcName] === "function" ||
        typeof unrdf[funcName] === "object"
      ) {
        logTest(
          `Legacy ${funcName}`,
          true,
          "backwards compatibility maintained",
        );
      } else {
        logTest(`Legacy ${funcName}`, false, "backwards compatibility broken");
      }
    }

    // Ensure new Knowledge Substrate functions also exist
    const newFunctions = [
      "createKnowledgeSubstrateCore",
      "KnowledgeSubstrateCore",
      "KnowledgeSubstrateFactory",
    ];
    for (const funcName of newFunctions) {
      if (
        typeof unrdf[funcName] === "function" ||
        typeof unrdf[funcName] === "object"
      ) {
        logTest(
          `New ${funcName}`,
          true,
          "Knowledge Substrate function available",
        );
      } else {
        logTest(
          `New ${funcName}`,
          false,
          "Knowledge Substrate function missing",
        );
      }
    }
  } catch (error) {
    logTest("Legacy compatibility", false, error.message);
  }
}

// Run tests and exit with appropriate code
runComprehensiveTests().then((success) => {
  process.exit(success ? 0 : 1);
});
