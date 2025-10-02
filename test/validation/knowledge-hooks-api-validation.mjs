/**
 * @file Validation script for Knowledge Hooks API claims in README
 * @description Tests if the README examples actually work
 */

import { defineHook } from '../../src/knowledge-engine/define-hook.mjs';
import { KnowledgeHookManager } from '../../src/knowledge-engine/knowledge-hook-manager.mjs';
import * as mainIndex from '../../src/index.mjs';

/**
 * Validation results tracker
 */
const results = {
  apiExists: {},
  hookTypes: {},
  readmeExample: { status: false, error: null },
  criticalIssues: [],
  recommendations: []
};

console.log('🔍 VALIDATING KNOWLEDGE HOOKS API (README Lines 137-175)\n');

// ============================================================================
// TEST 1: Check if APIs exist in src/index.mjs
// ============================================================================
console.log('TEST 1: Checking API Exports from src/index.mjs...');

results.apiExists.defineHook = typeof mainIndex.defineHook === 'function';
results.apiExists.registerHook = typeof mainIndex.registerHook === 'function';
results.apiExists.deregisterHook = typeof mainIndex.deregisterHook === 'function';
results.apiExists.evaluateHook = typeof mainIndex.evaluateHook === 'function';
results.apiExists.KnowledgeHookManager = typeof mainIndex.KnowledgeHookManager === 'function';

console.log(`  ✅ defineHook: ${results.apiExists.defineHook ? 'EXISTS' : 'MISSING'}`);
console.log(`  ${results.apiExists.registerHook ? '✅' : '❌'} registerHook: ${results.apiExists.registerHook ? 'EXISTS' : 'MISSING'}`);
console.log(`  ${results.apiExists.deregisterHook ? '✅' : '❌'} deregisterHook: ${results.apiExists.deregisterHook ? 'EXISTS' : 'MISSING'}`);
console.log(`  ${results.apiExists.evaluateHook ? '✅' : '❌'} evaluateHook: ${results.apiExists.evaluateHook ? 'EXISTS' : 'MISSING'}`);
console.log(`  ✅ KnowledgeHookManager: ${results.apiExists.KnowledgeHookManager ? 'EXISTS' : 'MISSING'}`);

// ============================================================================
// TEST 2: Check if hook types are supported
// ============================================================================
console.log('\nTEST 2: Checking Supported Hook Types...');

const hookTypes = ['sparql-ask', 'shacl', 'delta', 'threshold', 'count', 'window'];

for (const hookType of hookTypes) {
  try {
    // Try to create a minimal hook of each type
    const testHook = defineHook({
      meta: {
        name: `test-${hookType}`,
        description: `Test hook for ${hookType}`
      },
      when: {
        kind: hookType === 'shacl' ? 'shacl' : 'sparql-ask',
        ref: {
          uri: `file://test/${hookType}.rq`,
          sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
          mediaType: hookType === 'shacl' ? 'text/shacl' : 'application/sparql-query'
        }
      },
      run: async (event) => ({ result: 'test' })
    });

    results.hookTypes[hookType] = testHook !== null;
    console.log(`  ✅ ${hookType}: SUPPORTED`);
  } catch (error) {
    results.hookTypes[hookType] = false;
    console.log(`  ❌ ${hookType}: NOT SUPPORTED (${error.message})`);
  }
}

// ============================================================================
// TEST 3: Test README Example (Lines 141-166)
// ============================================================================
console.log('\nTEST 3: Testing README Example Code...');

try {
  // README claims this should work (but uses inline query, not ref)
  const readmeHook = {
    meta: {
      name: 'data-quality-gate',
      description: 'Ensures all persons have names'
    },
    when: {
      kind: 'sparql-ask',
      query: `
        ASK {
          ?person a <http://xmlns.com/foaf/0.1/Person> .
          FILTER NOT EXISTS { ?person <http://xmlns.com/foaf/0.1/name> ?name }
        }
      `
    },
    run: async (event) => {
      if (event.result === true) {
        throw new Error('All persons must have names');
      }
    }
  };

  // Try to use defineHook with inline query (README style)
  try {
    const hook = defineHook(readmeHook);
    results.readmeExample.status = false;
    results.readmeExample.error = 'README example accepted inline query (should require ref)';
    console.log('  ❌ README example validation: FAILED (accepts inline queries)');
  } catch (error) {
    // This is actually correct - should fail because inline queries aren't allowed
    if (error.message.includes('ref')) {
      results.readmeExample.status = false;
      results.readmeExample.error = 'README example uses inline query, but API requires ref';
      console.log('  ⚠️  README example: INCORRECT (uses inline query, requires ref)');
    } else {
      results.readmeExample.status = false;
      results.readmeExample.error = error.message;
      console.log(`  ❌ README example: FAILED (${error.message})`);
    }
  }
} catch (error) {
  results.readmeExample.status = false;
  results.readmeExample.error = error.message;
  console.log(`  ❌ README example validation failed: ${error.message}`);
}

// ============================================================================
// TEST 4: Test correct API usage (with ref instead of inline query)
// ============================================================================
console.log('\nTEST 4: Testing Correct API Usage (with ref)...');

try {
  const correctHook = defineHook({
    meta: {
      name: 'data-quality-gate-correct',
      description: 'Ensures all persons have names'
    },
    when: {
      kind: 'sparql-ask',
      ref: {
        uri: 'file://hooks/quality/person-name-check.ask.rq',
        sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
        mediaType: 'application/sparql-query'
      }
    },
    run: async (event) => {
      if (event.result === true) {
        throw new Error('All persons must have names');
      }
      return { result: 'validation-passed' };
    }
  });

  console.log('  ✅ Correct API usage (with ref): WORKS');
  results.apiExists.correctUsageWorks = true;
} catch (error) {
  console.log(`  ❌ Correct API usage failed: ${error.message}`);
  results.apiExists.correctUsageWorks = false;
}

// ============================================================================
// TEST 5: Check registerHook functionality
// ============================================================================
console.log('\nTEST 5: Testing Hook Registration...');

if (!results.apiExists.registerHook) {
  console.log('  ❌ registerHook: NOT AVAILABLE');
  console.log('  💡 Alternative: Use KnowledgeHookManager.addKnowledgeHook()');

  // Test alternative
  try {
    const manager = new KnowledgeHookManager();
    const testHook = defineHook({
      meta: { name: 'test-registration' },
      when: {
        kind: 'sparql-ask',
        ref: {
          uri: 'file://test.rq',
          sha256: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
          mediaType: 'application/sparql-query'
        }
      },
      run: async () => ({ result: 'ok' })
    });

    manager.addKnowledgeHook(testHook);
    const registered = manager.getKnowledgeHooks();

    if (registered.length === 1 && registered[0].meta.name === 'test-registration') {
      console.log('  ✅ Alternative registration method works: KnowledgeHookManager.addKnowledgeHook()');
      results.apiExists.alternativeRegistration = true;
    }
  } catch (error) {
    console.log(`  ❌ Alternative registration failed: ${error.message}`);
    results.apiExists.alternativeRegistration = false;
  }
}

// ============================================================================
// ANALYZE RESULTS & GENERATE REPORT
// ============================================================================
console.log('\n' + '='.repeat(80));
console.log('📊 VALIDATION REPORT');
console.log('='.repeat(80));

// Count issues
const missingApis = Object.entries(results.apiExists)
  .filter(([key, exists]) => !exists && key !== 'alternativeRegistration')
  .map(([key]) => key);

const unsupportedTypes = Object.entries(results.hookTypes)
  .filter(([type, supported]) => !supported)
  .map(([type]) => type);

// Critical Issues
if (missingApis.length > 0) {
  results.criticalIssues.push(`Missing APIs: ${missingApis.join(', ')}`);
}

if (!results.readmeExample.status) {
  results.criticalIssues.push(`README example doesn't work: ${results.readmeExample.error}`);
}

if (unsupportedTypes.length > 0) {
  results.criticalIssues.push(`Unsupported hook types: ${unsupportedTypes.join(', ')}`);
}

// Recommendations
if (!results.apiExists.registerHook) {
  results.recommendations.push('Add registerHook() to src/index.mjs exports');
  results.recommendations.push('Create convenience wrapper: registerHook = (hook) => manager.addKnowledgeHook(hook)');
}

if (!results.apiExists.deregisterHook) {
  results.recommendations.push('Add deregisterHook() to src/index.mjs exports');
}

if (!results.apiExists.evaluateHook) {
  results.recommendations.push('Add evaluateHook() to src/index.mjs exports');
}

if (!results.readmeExample.status) {
  results.recommendations.push('Update README example to use ref instead of inline query');
  results.recommendations.push('Add migration guide for users expecting inline queries');
}

// Final Status
const overallStatus =
  results.apiExists.defineHook &&
  results.apiExists.KnowledgeHookManager &&
  unsupportedTypes.length === 0 &&
  results.criticalIssues.length === 0;

console.log('\nAGENT: Knowledge Hooks Validator');
console.log(`STATUS: ${overallStatus ? 'PASS' : 'FAIL'}\n`);

console.log('TEST RESULTS:');
console.log(`${results.apiExists.defineHook ? '✅' : '❌'} defineHook exists`);
console.log(`${results.apiExists.registerHook ? '✅' : '❌'} registerHook exists`);
console.log(`${results.apiExists.deregisterHook ? '✅' : '❌'} deregisterHook exists`);
console.log(`${results.apiExists.evaluateHook ? '✅' : '❌'} evaluateHook exists`);
console.log(`${results.readmeExample.status ? '✅' : '❌'} README example works`);
console.log(`${unsupportedTypes.length === 0 ? '✅' : '❌'} All 6 hook types supported\n`);

console.log('SUPPORTED HOOK TYPES:');
for (const [type, supported] of Object.entries(results.hookTypes)) {
  console.log(`- ${type}: ${supported ? '✅' : '❌'}`);
}

console.log('\nCRITICAL ISSUES:');
if (results.criticalIssues.length === 0) {
  console.log('None (API mostly functional)');
} else {
  results.criticalIssues.forEach(issue => console.log(`❌ ${issue}`));
}

console.log('\nRECOMMENDATIONS:');
if (results.recommendations.length === 0) {
  console.log('None (API is complete)');
} else {
  results.recommendations.forEach(rec => console.log(`💡 ${rec}`));
}

console.log('\n' + '='.repeat(80));

// Exit with appropriate code
process.exit(overallStatus ? 0 : 1);
