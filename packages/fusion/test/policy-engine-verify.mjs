/**
 * @file Policy Engine Verification (No Test Framework)
 * @description Direct verification of policy engine functionality
 *
 * Tests:
 * 1. SPARQL condition pass/fail
 * 2. Hook execution on decision
 * 3. Deterministic receipt emission
 */

import { createPolicyRegistry } from '../src/policy-engine.mjs';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { defineHook } from '@unrdf/hooks';

let passed = 0;
let failed = 0;

function assert(condition, message) {
  if (condition) {
    console.log(`✅ PASS: ${message}`);
    passed++;
  } else {
    console.log(`❌ FAIL: ${message}`);
    failed++;
    throw new Error(`Assertion failed: ${message}`);
  }
}

async function test1_sparqlConditionPass() {
  console.log('\n=== TEST 1: SPARQL Condition PASS ===');
  const registry = await createPolicyRegistry();
  const store = createStore();

  // Add test data - age >= 18
  const foaf = 'http://xmlns.com/foaf/0.1/';
  store.add(
    dataFactory.quad(
      dataFactory.namedNode('http://example.org/alice'),
      dataFactory.namedNode(`${foaf}age`),
      dataFactory.literal('25', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    )
  );

  // Register policy
  registry.registerPolicy({
    name: 'AdultOnly',
    conditions: [
      {
        sparql: `
          PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          ASK {
            <http://example.org/alice> foaf:age ?age .
            FILTER (?age >= 18)
          }
        `,
      },
    ],
    actions: [],
  });

  // Evaluate
  const result = await registry.evaluatePolicy(
    store,
    { subject: 'http://example.org/alice' },
    'AdultOnly'
  );

  assert(result.decision === 'allow', 'Decision should be allow');
  assert(result.conditionResults.length === 1, 'Should have 1 condition result');
  assert(result.conditionResults[0] === true, 'Condition should pass');
  assert(result.policy === 'AdultOnly', 'Policy name should match');
}

async function test2_sparqlConditionFail() {
  console.log('\n=== TEST 2: SPARQL Condition FAIL ===');
  const registry = await createPolicyRegistry();
  const store = createStore();

  // Add test data - age < 18
  const foaf = 'http://xmlns.com/foaf/0.1/';
  store.add(
    dataFactory.quad(
      dataFactory.namedNode('http://example.org/bob'),
      dataFactory.namedNode(`${foaf}age`),
      dataFactory.literal('16', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    )
  );

  // Register policy
  registry.registerPolicy({
    name: 'AdultOnly',
    conditions: [
      {
        sparql: `
          PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          ASK {
            <http://example.org/bob> foaf:age ?age .
            FILTER (?age >= 18)
          }
        `,
      },
    ],
    actions: [],
  });

  // Evaluate
  const result = await registry.evaluatePolicy(
    store,
    { subject: 'http://example.org/bob' },
    'AdultOnly'
  );

  assert(result.decision === 'deny', 'Decision should be deny');
  assert(result.conditionResults.length === 1, 'Should have 1 condition result');
  assert(result.conditionResults[0] === false, 'Condition should fail');
  assert(result.message.includes('Condition'), 'Should have failure message');
}

async function test3_hookExecution() {
  console.log('\n=== TEST 3: Hook Execution on Decision ===');
  const registry = await createPolicyRegistry();
  const store = createStore();

  let hookExecuted = false;
  let hookContext = null;

  // Define and register hook
  const auditHook = defineHook({
    id: 'auditLog',
    trigger: 'before-add',
    validate: (quad, options) => {
      hookExecuted = true;
      hookContext = options;
      return { valid: true };
    },
  });
  registry.registerHook('auditLog', auditHook);

  // Register policy
  registry.registerPolicy({
    name: 'AuditPolicy',
    conditions: [{ sparql: 'ASK { }' }], // Always passes
    actions: [{ hook: 'auditLog', args: { event: 'test_audit', limit: 1000 } }],
  });

  // Create test quad
  const testQuad = dataFactory.quad(
    dataFactory.namedNode('http://example.org/s'),
    dataFactory.namedNode('http://example.org/p'),
    dataFactory.literal('o')
  );

  // Evaluate
  const result = await registry.evaluatePolicy(store, testQuad, 'AuditPolicy');

  assert(result.decision === 'allow', 'Decision should be allow');
  assert(hookExecuted === true, 'Hook should be executed');
  assert(hookContext !== null, 'Hook context should be set');
  assert(hookContext.event === 'test_audit', 'Hook event should match');
  assert(hookContext.limit === 1000, 'Hook limit should match');
  assert(result.actionResults.length === 1, 'Should have 1 action result');
  assert(result.actionResults[0].valid === true, 'Action should be valid');
}

async function test4_deterministicReceipts() {
  console.log('\n=== TEST 4: Deterministic Receipt Emission ===');
  const registry = await createPolicyRegistry();
  const store = createStore();

  registry.registerPolicy({
    name: 'DeterministicPolicy',
    conditions: [],
    actions: [],
  });

  const resource = { id: 'test-resource', value: 42 };

  // First decision
  const decision1 = await registry.evaluatePolicy(store, resource, 'DeterministicPolicy');
  const timestamp = decision1.timestamp;

  // Second decision with same timestamp (force determinism)
  const decision2 = await registry.evaluatePolicy(store, resource, 'DeterministicPolicy');
  decision2.timestamp = timestamp; // Force same timestamp

  const receipt1 = await registry.emitPolicyReceipt(decision1, resource);
  const receipt2 = await registry.emitPolicyReceipt(decision2, resource);

  assert(receipt1.hash === receipt2.hash, 'Receipts should have identical hashes');
  assert(receipt1.canonical === receipt2.canonical, 'Canonical forms should match');
  assert(receipt1.hash.length === 64, 'SHA-256 hash should be 64 chars (hex)');

  console.log(`  Receipt hash: ${receipt1.hash}`);
}

async function test5_receiptContent() {
  console.log('\n=== TEST 5: Receipt Content Verification ===');
  const registry = await createPolicyRegistry();
  const store = createStore();

  const testHook = defineHook({
    id: 'testHook',
    trigger: 'before-add',
    validate: () => ({ valid: true }),
  });
  registry.registerHook('testHook', testHook);

  registry.registerPolicy({
    name: 'CompletePolicy',
    conditions: [{ sparql: 'ASK { }' }],
    actions: [{ hook: 'testHook' }],
  });

  const testQuad = dataFactory.quad(
    dataFactory.namedNode('http://example.org/s'),
    dataFactory.namedNode('http://example.org/p'),
    dataFactory.literal('o')
  );

  const decision = await registry.routeDecision(store, testQuad, 'CompletePolicy');

  assert(decision.receipt !== undefined, 'Receipt should exist');
  assert(decision.receiptHash !== undefined, 'Receipt hash should exist');
  assert(decision.receipt.hash === decision.receiptHash, 'Hashes should match');
  assert(decision.receipt.policy === 'CompletePolicy', 'Policy name should be in receipt');
  assert(decision.receipt.decision === 'allow', 'Decision should be in receipt');
  assert(typeof decision.receipt.timestamp === 'number', 'Timestamp should be number');
  assert(Array.isArray(decision.receipt.conditionResults), 'Condition results should be array');
  assert(Array.isArray(decision.receipt.actionResults), 'Action results should be array');
  assert(decision.receipt.canonical !== undefined, 'Canonical form should exist');
}

async function test6_integration() {
  console.log('\n=== TEST 6: Full Integration (SPARQL + Hook + Receipt) ===');
  const registry = await createPolicyRegistry();
  const store = createStore();

  // Add user data
  const foaf = 'http://xmlns.com/foaf/0.1/';
  store.add(
    dataFactory.quad(
      dataFactory.namedNode('http://example.org/user1'),
      dataFactory.namedNode(`${foaf}age`),
      dataFactory.literal('22', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    )
  );

  // Track hook execution
  const auditLog = [];
  const auditHook = defineHook({
    id: 'audit',
    trigger: 'before-add',
    validate: (quad, options) => {
      auditLog.push({ quad, options });
      return { valid: true };
    },
  });
  registry.registerHook('audit', auditHook);

  // Register policy
  registry.registerPolicy({
    name: 'CustomerCredit',
    conditions: [
      {
        sparql: `
          PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          ASK {
            <http://example.org/user1> foaf:age ?age .
            FILTER (?age >= 18)
          }
        `,
      },
    ],
    actions: [{ hook: 'audit', args: { event: 'credit_approved', limit: 1000 } }],
  });

  // Execute policy
  const testQuad = dataFactory.quad(
    dataFactory.namedNode('http://example.org/purchase1'),
    dataFactory.namedNode('http://example.org/amount'),
    dataFactory.literal('500')
  );

  const result = await registry.routeDecision(store, testQuad, 'CustomerCredit');

  // Verify complete flow
  assert(result.decision === 'allow', 'Decision should be allow');
  assert(result.conditionResults.length === 1, 'Should have 1 condition');
  assert(result.conditionResults[0] === true, 'Condition should pass');
  assert(result.actionResults.length === 1, 'Should have 1 action');
  assert(result.actionResults[0].valid === true, 'Action should be valid');
  assert(result.receiptHash !== undefined, 'Should have receipt hash');
  assert(result.receipt !== undefined, 'Should have receipt');
  assert(auditLog.length === 1, 'Hook should be executed once');
  assert(auditLog[0].options.event === 'credit_approved', 'Event should match');
  assert(auditLog[0].options.limit === 1000, 'Limit should match');
}

async function runTests() {
  console.log('🚀 Policy Engine Verification Tests\n');
  console.log('Testing unified policy/hooks/conditions API...\n');

  try {
    await test1_sparqlConditionPass();
    await test2_sparqlConditionFail();
    await test3_hookExecution();
    await test4_deterministicReceipts();
    await test5_receiptContent();
    await test6_integration();

    console.log('\n' + '='.repeat(60));
    console.log(`✅ All tests passed! (${passed}/${passed + failed})`);
    console.log('='.repeat(60));
    console.log('\n✨ Policy engine unified successfully!');
    console.log('   - SPARQL conditions: WORKING');
    console.log('   - Hook execution: WORKING');
    console.log('   - Receipt emission: WORKING (deterministic)');
    process.exit(0);
  } catch (error) {
    console.log('\n' + '='.repeat(60));
    console.log(`❌ Tests failed! (${passed}/${passed + failed} passed)`);
    console.log('='.repeat(60));
    console.error('\nError:', error.message);
    process.exit(1);
  }
}

runTests();
