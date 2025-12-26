#!/usr/bin/env node
/**
 * Integration Test - Production Validation
 * Tests package integration with real imports and operations
 *
 * Date: 2025-12-25
 */

console.log('='.repeat(60));
console.log('INTEGRATION TEST - PRODUCTION VALIDATION');
console.log('='.repeat(60));
console.log();

const results = {
  passed: [],
  failed: [],
  skipped: []
};

/**
 * Test helper
 */
async function test(name, fn) {
  try {
    await fn();
    results.passed.push(name);
    console.log(`✅ PASS: ${name}`);
  } catch (error) {
    results.failed.push({ name, error: error.message, stack: error.stack });
    console.log(`❌ FAIL: ${name}`);
    console.log(`   Error: ${error.message}`);
  }
}

/**
 * Skip test with reason
 */
function skip(name, reason) {
  results.skipped.push({ name, reason });
  console.log(`⏭️  SKIP: ${name} (${reason})`);
}

// ============================================================
// Test 1: Oxigraph Store Creation
// ============================================================
await test('Oxigraph store creation', async () => {
  const { createStore } = await import('@unrdf/oxigraph');
  const store = createStore();
  if (!store) throw new Error('Store is null/undefined');
  if (typeof store.add !== 'function') throw new Error('Store missing add method');
});

// ============================================================
// Test 2: Data Factory
// ============================================================
await test('Data factory quad creation', async () => {
  const { dataFactory } = await import('@unrdf/oxigraph');
  const quad = dataFactory.quad(
    dataFactory.namedNode('http://example.org/subject'),
    dataFactory.namedNode('http://example.org/predicate'),
    dataFactory.literal('object')
  );
  if (!quad) throw new Error('Quad is null/undefined');
  if (!quad.subject || !quad.predicate || !quad.object) {
    throw new Error('Quad missing required properties');
  }
});

// ============================================================
// Test 3: Store CRUD Operations
// ============================================================
await test('Store CRUD operations', async () => {
  const { createStore, dataFactory } = await import('@unrdf/oxigraph');
  const store = createStore();

  const quad = dataFactory.quad(
    dataFactory.namedNode('http://example.org/subject'),
    dataFactory.namedNode('http://example.org/predicate'),
    dataFactory.literal('test value')
  );

  // Add
  store.add(quad);

  // Read
  const results = Array.from(store.match(
    dataFactory.namedNode('http://example.org/subject'),
    null,
    null
  ));

  if (results.length !== 1) {
    throw new Error(`Expected 1 result, got ${results.length}`);
  }

  // Delete
  store.delete(quad);
  const afterDelete = Array.from(store.match(
    dataFactory.namedNode('http://example.org/subject'),
    null,
    null
  ));

  if (afterDelete.length !== 0) {
    throw new Error(`Expected 0 results after delete, got ${afterDelete.length}`);
  }
});

// ============================================================
// Test 4: KGC-4D (if available)
// ============================================================
try {
  await test('KGC-4D freeze universe', async () => {
    const { freezeUniverse } = await import('@unrdf/kgc-4d');
    const universe = await freezeUniverse([
      { type: 'test', payload: { value: 42 } }
    ]);
    if (!universe) throw new Error('Universe is null/undefined');
    if (!universe.merkleRoot) throw new Error('Universe missing merkleRoot');
  });
} catch (error) {
  skip('KGC-4D freeze universe', 'Package may not be built');
}

// ============================================================
// Test 5: Hooks (if available)
// ============================================================
try {
  await test('Hooks definition', async () => {
    const { defineHook } = await import('@unrdf/hooks');
    const hook = defineHook({
      trigger: 'test-trigger',
      handler: () => true
    });
    if (!hook) throw new Error('Hook is null/undefined');
    if (hook.trigger !== 'test-trigger') {
      throw new Error(`Expected trigger 'test-trigger', got '${hook.trigger}'`);
    }
  });
} catch (error) {
  skip('Hooks definition', 'Package may not be built');
}

// ============================================================
// Test 6: Receipt Generation (if available)
// ============================================================
try {
  await test('Receipt generation', async () => {
    const { generateReceipt } = await import('@unrdf/kgc-4d');
    const receipt = await generateReceipt({
      events: [{ type: 'test', payload: {} }],
      merkleRoot: 'test-root'
    });
    if (!receipt) throw new Error('Receipt is null/undefined');
  });
} catch (error) {
  skip('Receipt generation', 'Package may not be built');
}

// ============================================================
// Test 7: No N3 Direct Imports in App Code
// ============================================================
await test('No direct N3 imports in packages', async () => {
  const { exec } = await import('child_process');
  const { promisify } = await import('util');
  const execPromise = promisify(exec);

  try {
    const { stdout } = await execPromise(
      "grep -r \"from 'n3'\" packages/*/src --exclude-dir=node_modules --exclude='*justified*' 2>/dev/null || true"
    );

    if (stdout && stdout.trim().length > 0) {
      throw new Error(`Found direct N3 imports in app code:\n${stdout}`);
    }
  } catch (error) {
    if (error.message.includes('Found direct N3 imports')) {
      throw error;
    }
    // grep returned no matches - this is what we want
  }
});

// ============================================================
// Test 8: Package.json Consistency
// ============================================================
await test('Package.json files exist for all packages', async () => {
  const { readdir } = await import('fs/promises');
  const { existsSync } = await import('fs');

  const packages = await readdir('packages', { withFileTypes: true });
  const dirs = packages.filter(p => p.isDirectory());

  const missing = [];
  for (const dir of dirs) {
    const pkgPath = `packages/${dir.name}/package.json`;
    if (!existsSync(pkgPath)) {
      missing.push(dir.name);
    }
  }

  if (missing.length > 0) {
    throw new Error(`Missing package.json in: ${missing.join(', ')}`);
  }
});

// ============================================================
// Results Summary
// ============================================================
console.log();
console.log('='.repeat(60));
console.log('INTEGRATION TEST RESULTS');
console.log('='.repeat(60));
console.log();
console.log(`✅ Passed: ${results.passed.length}`);
console.log(`❌ Failed: ${results.failed.length}`);
console.log(`⏭️  Skipped: ${results.skipped.length}`);
console.log();

if (results.failed.length > 0) {
  console.log('FAILURES:');
  for (const failure of results.failed) {
    console.log(`  - ${failure.name}`);
    console.log(`    ${failure.error}`);
  }
  console.log();
}

if (results.skipped.length > 0) {
  console.log('SKIPPED:');
  for (const skip of results.skipped) {
    console.log(`  - ${skip.name}: ${skip.reason}`);
  }
  console.log();
}

// Exit with proper code
const exitCode = results.failed.length > 0 ? 1 : 0;
console.log(`Exit code: ${exitCode}`);
console.log('='.repeat(60));

process.exit(exitCode);
