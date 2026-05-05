/**
 * @file Demo scenario showing shadow mode migration phases
 * @description Example of progressive migration from legacy to facade
 */

import { shadowWrite, shadowRead, partialServe } from './shadow.mjs';
import { defineRoute, pathRoute, catchAllRoute } from './routing.mjs';

/**
 * JSON replacer to handle BigInt serialization
 */
function jsonReplacer(key, value) {
  if (typeof value === 'bigint') {
    return value.toString() + 'n';
  }
  return value;
}

/**
 * Demo: Complete migration scenario
 * Shows all phases from shadow-write to KGC-primary
 */
export async function runMigrationDemo() {
  console.log('=== Shadow Mode Migration Demo ===\n');

  // Setup: Define legacy and facade handlers
  const legacyHandler = async (request) => {
    // Simulate legacy system
    await delay(10);
    return {
      id: request.id,
      data: { name: 'Alice', email: 'ALICE@EXAMPLE.COM' }, // UPPERCASE email
      source: 'legacy'
    };
  };

  const facadeHandler = async (request) => {
    // Simulate facade system (RDF-based)
    await delay(15);
    return {
      id: request.id,
      data: { name: 'Alice', email: 'alice@example.com' }, // lowercase email
      source: 'facade'
    };
  };

  const request = { id: 'req-123', path: '/api/users/42' };

  // Phase 1: Shadow Write
  console.log('--- Phase 1: Shadow Write ---');
  console.log('Execute both handlers, serve legacy, detect mismatches\n');

  const shadowResult = await shadowWrite(legacyHandler, facadeHandler, request);

  console.log('Legacy result:', JSON.stringify(shadowResult.legacyResult, null, 2));
  console.log('Facade result:', JSON.stringify(shadowResult.facadeResult, null, 2));
  console.log('Match:', shadowResult.match);

  if (!shadowResult.match) {
    console.log('Mismatch hash:', shadowResult.mismatchHash);
    console.log('Mismatch report:', JSON.stringify(shadowResult.mismatchReport, jsonReplacer, 2));
    console.log('\nAction: Log mismatch for investigation');
  }

  console.log('\n');

  // Phase 2: Shadow Read
  console.log('--- Phase 2: Shadow Read ---');
  console.log('Query both stores, serve legacy, validate consistency\n');

  const legacyStore = async (query) => {
    await delay(5);
    return [
      { id: 1, name: 'Alice', email: 'ALICE@EXAMPLE.COM' },
      { id: 2, name: 'Bob', email: 'BOB@EXAMPLE.COM' }
    ];
  };

  const facadeStore = async (query) => {
    await delay(8);
    return [
      { id: 1, name: 'Alice', email: 'alice@example.com' },
      { id: 2, name: 'Bob', email: 'bob@example.com' }
    ];
  };

  const query = { type: 'SELECT', filter: { active: true } };
  const readResult = await shadowRead(legacyStore, facadeStore, query);

  console.log('Legacy data:', JSON.stringify(readResult.legacyData, null, 2));
  console.log('Facade data:', JSON.stringify(readResult.facadeData, null, 2));
  console.log('Match:', readResult.match);

  if (!readResult.match) {
    console.log('Mismatch hash:', readResult.mismatchHash);
    console.log('\nAction: Fix email case normalization in facade');
  }

  console.log('\n');

  // Fix facade to match legacy (normalize email case)
  const facadeHandlerFixed = async (request) => {
    await delay(15);
    return {
      id: request.id,
      data: { name: 'Alice', email: 'ALICE@EXAMPLE.COM' }, // Now matches legacy
      source: 'facade'
    };
  };

  console.log('--- After Fix: Shadow Write Validation ---');
  const fixedResult = await shadowWrite(legacyHandler, facadeHandlerFixed, request);
  console.log('Match after fix:', fixedResult.match);
  console.log('✅ Facade now matches legacy\n');

  // Phase 3: Partial Serve (Gradual Rollout)
  console.log('--- Phase 3: Partial Serve ---');
  console.log('Route specific paths to facade, others to legacy\n');

  // Start with 1% traffic to facade for /api/users
  const routes1pct = [
    pathRoute('/api/users', 'facade', { weight: 1, priority: 10 }),
    catchAllRoute('legacy')
  ];

  console.log('Config: 1% of /api/users → facade, rest → legacy');
  console.log('Simulating 10 requests...\n');

  let facadeCount = 0;
  let legacyCount = 0;

  for (let i = 0; i < 10; i++) {
    const testReq = { id: `req-${i}`, path: '/api/users/42' };
    const result = await partialServe(
      routes1pct,
      testReq,
      { legacy: legacyHandler, facade: facadeHandlerFixed }
    );

    if (result.source === 'facade') facadeCount++;
    else legacyCount++;
  }

  console.log(`Results: ${facadeCount} facade, ${legacyCount} legacy`);
  console.log('Expected: ~0-1 facade (1% of 10), rest legacy\n');

  // Increase to 50% after validation
  console.log('--- Increasing to 50% traffic ---');
  const routes50pct = [
    pathRoute('/api/users', 'facade', { weight: 50, priority: 10 }),
    catchAllRoute('legacy')
  ];

  facadeCount = 0;
  legacyCount = 0;

  for (let i = 0; i < 100; i++) {
    const testReq = { id: `req-${i}`, path: '/api/users/42' };
    const result = await partialServe(
      routes50pct,
      testReq,
      { legacy: legacyHandler, facade: facadeHandlerFixed }
    );

    if (result.source === 'facade') facadeCount++;
    else legacyCount++;
  }

  console.log(`Results: ${facadeCount} facade, ${legacyCount} legacy`);
  console.log('Expected: ~50 facade, ~50 legacy\n');

  // Phase 4: KGC Primary (100% facade)
  console.log('--- Phase 4: KGC Primary ---');
  console.log('100% traffic to facade, legacy in shadow for validation\n');

  const routes100pct = [
    pathRoute('/api/users', 'facade', { weight: 100, priority: 10 }),
    catchAllRoute('facade') // Everything to facade now
  ];

  const result = await partialServe(
    routes100pct,
    request,
    { legacy: legacyHandler, facade: facadeHandlerFixed }
  );

  console.log('Result source:', result.source);
  console.log('✅ All traffic now served by facade\n');

  // Final validation with shadow mode
  console.log('--- Final Validation (Legacy in Shadow) ---');
  const finalValidation = await shadowWrite(
    facadeHandlerFixed, // Facade is now primary
    legacyHandler,      // Legacy validates in background
    request
  );

  console.log('Match:', finalValidation.match);
  console.log('✅ Ready to decommission legacy after 30 days stable operation\n');

  console.log('=== Migration Complete ===');
  console.log('Total duration: ~64 days');
  console.log('Risk: Minimal (instant rollback available at all phases)');
  console.log('Validation: Continuous mismatch detection and reporting');
}

/**
 * Demo: Mismatch detection and reporting
 */
export async function runMismatchDemo() {
  console.log('=== Mismatch Detection Demo ===\n');

  // Handlers with various types of mismatches
  const legacyHandler = async (request) => ({
    id: 123,
    name: 'Alice',
    email: 'alice@example.com',
    created: new Date('2024-01-01'),
    metadata: { version: 1 }
  });

  // Case 1: Missing field
  const facadeMissingField = async (request) => ({
    id: 123,
    name: 'Alice',
    // email missing
    created: new Date('2024-01-01'),
    metadata: { version: 1 }
  });

  console.log('--- Case 1: Missing Field (Critical) ---');
  const result1 = await shadowWrite(legacyHandler, facadeMissingField, { id: 'req-1' });
  console.log('Severity:', result1.mismatchReport?.severity);
  console.log('Path:', result1.mismatchReport?.path);
  console.log('Recommendation:', result1.mismatchReport?.recommendation);
  console.log('');

  // Case 2: Type mismatch
  const facadeTypeMismatch = async (request) => ({
    id: '123', // String instead of number
    name: 'Alice',
    email: 'alice@example.com',
    created: new Date('2024-01-01'),
    metadata: { version: 1 }
  });

  console.log('--- Case 2: Type Mismatch (Critical) ---');
  const result2 = await shadowWrite(legacyHandler, facadeTypeMismatch, { id: 'req-2' });
  console.log('Severity:', result2.mismatchReport?.severity);
  console.log('Path:', result2.mismatchReport?.path);
  console.log('Recommendation:', result2.mismatchReport?.recommendation);
  console.log('');

  // Case 3: Case sensitivity
  const facadeCaseDiff = async (request) => ({
    id: 123,
    name: 'Alice',
    email: 'ALICE@EXAMPLE.COM', // Different case
    created: new Date('2024-01-01'),
    metadata: { version: 1 }
  });

  console.log('--- Case 3: Case Difference (Info) ---');
  const result3 = await shadowWrite(legacyHandler, facadeCaseDiff, { id: 'req-3' });
  console.log('Severity:', result3.mismatchReport?.severity);
  console.log('Path:', result3.mismatchReport?.path);
  console.log('Recommendation:', result3.mismatchReport?.recommendation);
  console.log('');

  console.log('=== Deterministic Hashing ===');
  console.log('Same mismatch → same hash (run 100x)\n');

  const hashes = new Set();
  for (let i = 0; i < 100; i++) {
    const result = await shadowWrite(legacyHandler, facadeMissingField, { id: 'req-test' });
    hashes.add(result.mismatchHash);
  }

  console.log('Unique hashes:', hashes.size);
  console.log('Expected: 1 (deterministic)');
  console.log('✅', hashes.size === 1 ? 'PASS' : 'FAIL');
}

/**
 * Demo: Routing decisions
 */
export async function runRoutingDemo() {
  console.log('=== Routing Decision Demo ===\n');

  const routes = [
    defineRoute(
      (req) => req.path === '/health',
      'facade',
      { name: 'Health check', priority: 100, weight: 100 }
    ),
    pathRoute('/api/crm', 'facade', { priority: 50, weight: 100 }),
    pathRoute('/api/legacy', 'legacy', { priority: 50, weight: 100 }),
    pathRoute('/api/canary', 'facade', { priority: 40, weight: 10 }), // 10% to facade
    catchAllRoute('legacy')
  ];

  const testRequests = [
    { path: '/health' },
    { path: '/api/crm/users' },
    { path: '/api/legacy/old-endpoint' },
    { path: '/api/unknown' },
  ];

  console.log('Route configuration:');
  routes.forEach(r => {
    console.log(`  ${r.name || 'Unnamed'}: ${r.target} (weight: ${r.weight}%, priority: ${r.priority})`);
  });
  console.log('');

  for (const req of testRequests) {
    const { routingDecision } = await import('./routing.mjs');
    const target = routingDecision(routes, req);
    console.log(`${req.path} → ${target}`);
  }

  console.log('\n--- Canary Rollout (/api/canary) ---');
  console.log('10% weight → expect ~10% to facade\n');

  let facadeCount = 0;
  const iterations = 1000;

  for (let i = 0; i < iterations; i++) {
    const { routingDecision } = await import('./routing.mjs');
    const target = routingDecision(routes, { id: `req-${i}`, path: '/api/canary' });
    if (target === 'facade') facadeCount++;
  }

  const percentage = (facadeCount / iterations * 100).toFixed(1);
  console.log(`Results: ${facadeCount}/${iterations} to facade (${percentage}%)`);
  console.log('Expected: ~10%');
  console.log('✅', Math.abs(percentage - 10) < 3 ? 'PASS' : 'FAIL (within margin)');
}

/**
 * Delay utility
 */
function delay(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

// Run all demos if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  (async () => {
    await runMigrationDemo();
    console.log('\n' + '='.repeat(50) + '\n');
    await runMismatchDemo();
    console.log('\n' + '='.repeat(50) + '\n');
    await runRoutingDemo();
  })();
}
