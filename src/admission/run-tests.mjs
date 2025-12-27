/**
 * @fileoverview Simple test runner for admission engine
 * Standalone runner that doesn't require vitest installation
 */

import { DeltaCapsule } from './delta-capsule.mjs';
import { AdmissionEngine } from './admission-engine.mjs';
import {
  guardEditIndustrialSubstrate,
  guardRedefineProtectedTerm,
  guardWeakenCorporateCanon,
  isProtectedNamespace,
  isCanonicalTerm
} from './forbidden-operations.mjs';
import {
  Q_typing,
  Q_noncollision,
  Q_monotone,
  Q_determinism,
  Q_provenance,
  Q_bounds
} from './invariants.mjs';

// Test utilities
let testCount = 0;
let passCount = 0;
let failCount = 0;

function assert(condition, message) {
  testCount++;
  if (!condition) {
    failCount++;
    console.error(`❌ FAIL: ${message}`);
    throw new Error(message);
  } else {
    passCount++;
  }
}

function assertEqual(actual, expected, message) {
  assert(actual === expected, `${message} (expected: ${expected}, got: ${actual})`);
}

function createQuad(s, p, o, g = null) {
  return {
    subject: {
      termType: s.startsWith('_:') ? 'BlankNode' : 'NamedNode',
      value: s
    },
    predicate: {
      termType: 'NamedNode',
      value: p
    },
    object: {
      termType: o.startsWith('http') ? 'NamedNode' : 'Literal',
      value: o
    },
    graph: g ? {
      termType: 'NamedNode',
      value: g
    } : undefined
  };
}

function createValidDelta() {
  return new DeltaCapsule({
    partition: {
      namespace: 'http://example.org/overlay/',
      name: 'test-overlay',
      protected: false
    },
    changes: [{
      operation: 'add',
      quads: [
        createQuad(
          'http://example.org/overlay/subject1',
          'http://example.org/overlay/predicate1',
          'http://example.org/overlay/object1'
        )
      ]
    }],
    invariants: [
      { name: 'Q_typing', enabled: true },
      { name: 'Q_noncollision', enabled: true },
      { name: 'Q_monotone', enabled: true },
      { name: 'Q_determinism', enabled: true },
      { name: 'Q_provenance', enabled: true },
      { name: 'Q_bounds', enabled: true }
    ],
    provenance: {
      agent: 'test-agent',
      timestamp: new Date().toISOString(),
      source: 'test',
      justification: 'test data'
    }
  });
}

// Test Suite
console.log('🧪 Running Admission Engine Tests\n');

// Test 1: Valid additive-only Δ → ALLOW
console.log('Test 1: Valid additive-only Δ → ALLOW');
try {
  const engine = new AdmissionEngine({ strictMode: true });
  const delta = createValidDelta();
  const decision = await engine.admitCapsule(delta);

  assertEqual(decision.allowed, true, 'Decision should be ALLOW');
  assertEqual(decision.decision, 'ALLOW', 'Decision type should be ALLOW');
  assertEqual(decision.checks.forbiddenOperations.passed, true, 'Guards should pass');
  assertEqual(decision.checks.invariants.passed, true, 'Invariants should pass');

  console.log('✅ PASS: Valid additive capsule allowed\n');
} catch (error) {
  console.error(`❌ FAIL: ${error.message}\n`);
}

// Test 2: Invalid Δ (edits protected NS) → DENY
console.log('Test 2: Invalid Δ (edits protected NS) → DENY');
try {
  const engine = new AdmissionEngine({ strictMode: true });
  const delta = new DeltaCapsule({
    partition: { namespace: 'http://example.org/', name: 'test', protected: false },
    changes: [{
      operation: 'delete',
      quads: [
        createQuad(
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://www.w3.org/2000/01/rdf-schema#label',
          'Type'
        )
      ]
    }],
    invariants: [{ name: 'Q_typing', enabled: true }],
    provenance: { agent: 'test', timestamp: new Date().toISOString() }
  });

  const decision = await engine.admitCapsule(delta);

  assertEqual(decision.allowed, false, 'Decision should be DENY');
  assertEqual(decision.decision, 'DENY', 'Decision type should be DENY');
  assertEqual(decision.checks.forbiddenOperations.passed, false, 'Guards should fail');
  assert(decision.checks.forbiddenOperations.blockedBy.includes('EditIndustrialSubstrate'),
         'Should be blocked by EditIndustrialSubstrate');

  console.log('✅ PASS: Protected namespace edit denied\n');
} catch (error) {
  console.error(`❌ FAIL: ${error.message}\n`);
}

// Test 3: Invalid Δ (redefines substrate term) → DENY
console.log('Test 3: Invalid Δ (redefines substrate term) → DENY');
try {
  const engine = new AdmissionEngine({ strictMode: true });
  const delta = new DeltaCapsule({
    partition: { namespace: 'http://example.org/', name: 'test', protected: false },
    changes: [{
      operation: 'add',
      quads: [
        createQuad(
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://example.org/MyClass'
        )
      ]
    }],
    invariants: [{ name: 'Q_typing', enabled: true }],
    provenance: { agent: 'test', timestamp: new Date().toISOString() }
  });

  const decision = await engine.admitCapsule(delta);

  assertEqual(decision.allowed, false, 'Decision should be DENY');
  assert(decision.checks.forbiddenOperations.blockedBy.includes('RedefineProtectedTerm'),
         'Should be blocked by RedefineProtectedTerm');

  console.log('✅ PASS: Substrate term redefinition denied\n');
} catch (error) {
  console.error(`❌ FAIL: ${error.message}\n`);
}

// Test 4: Invalid Δ (namespace collision) → DENY
console.log('Test 4: Invalid Δ (namespace collision) → DENY');
try {
  const engine = new AdmissionEngine({ strictMode: true });
  const delta = new DeltaCapsule({
    partition: { namespace: 'http://example.org/', name: 'test', protected: false },
    changes: [{
      operation: 'add',
      quads: [
        createQuad(
          'http://www.w3.org/2002/07/owl#myCustomClass',
          'http://example.org/p',
          'test'
        )
      ]
    }],
    invariants: [{ name: 'Q_noncollision', enabled: true }],
    provenance: { agent: 'test', timestamp: new Date().toISOString() }
  });

  const decision = await engine.admitCapsule(delta);

  assertEqual(decision.allowed, false, 'Decision should be DENY');
  assertEqual(decision.checks.invariants.passed, false, 'Invariants should fail');

  console.log('✅ PASS: Namespace collision denied\n');
} catch (error) {
  console.error(`❌ FAIL: ${error.message}\n`);
}

// Test 5: All invariants run
console.log('Test 5: All invariants run and failures logged');
try {
  const delta = createValidDelta();

  const results = {
    typing: Q_typing(delta),
    noncollision: Q_noncollision(delta),
    monotone: Q_monotone(delta),
    determinism: Q_determinism(delta),
    provenance: Q_provenance(delta),
    bounds: Q_bounds(delta)
  };

  assert(results.typing.passed, 'Q_typing should pass');
  assert(results.noncollision.passed, 'Q_noncollision should pass');
  assert(results.monotone.passed, 'Q_monotone should pass');
  assert(results.determinism.passed, 'Q_determinism should pass');
  assert(results.provenance.passed, 'Q_provenance should pass');
  assert(results.bounds.passed, 'Q_bounds should pass');

  console.log('✅ PASS: All invariants executed and passed\n');
} catch (error) {
  console.error(`❌ FAIL: ${error.message}\n`);
}

// Test 6: Helper functions
console.log('Test 6: Helper functions work correctly');
try {
  assert(isProtectedNamespace('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
         'Should identify RDF namespace as protected');
  assert(!isProtectedNamespace('http://example.org/custom'),
         'Should not identify custom namespace as protected');
  assert(isCanonicalTerm('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
         'Should identify rdf:type as canonical');
  assert(!isCanonicalTerm('http://example.org/custom'),
         'Should not identify custom term as canonical');

  console.log('✅ PASS: Helper functions working correctly\n');
} catch (error) {
  console.error(`❌ FAIL: ${error.message}\n`);
}

// Test 7: DeltaCapsule validation
console.log('Test 7: DeltaCapsule structure validation');
try {
  const delta = createValidDelta();
  const validation = DeltaCapsule.validate(delta);

  assert(validation.valid, 'Valid delta should pass validation');
  assertEqual(delta.isAdditiveOnly(), true, 'Should be additive only');
  assert(delta.getHash().length === 64, 'Hash should be 64 characters (SHA-256)');

  console.log('✅ PASS: DeltaCapsule validation working\n');
} catch (error) {
  console.error(`❌ FAIL: ${error.message}\n`);
}

// Test 8: Engine statistics
console.log('Test 8: Engine tracks statistics');
try {
  const engine = new AdmissionEngine({ auditLog: true });

  const validDelta = createValidDelta();
  await engine.admitCapsule(validDelta);

  const stats = engine.getStats();
  assertEqual(stats.totalProcessed, 1, 'Should have processed 1 capsule');
  assertEqual(stats.allowed, 1, 'Should have allowed 1 capsule');
  assertEqual(stats.denied, 0, 'Should have denied 0 capsules');

  const log = engine.getAuditLog();
  assertEqual(log.length, 1, 'Audit log should have 1 entry');
  assertEqual(log[0].decision, 'ALLOW', 'Audit entry should show ALLOW');

  console.log('✅ PASS: Engine statistics and audit log working\n');
} catch (error) {
  console.error(`❌ FAIL: ${error.message}\n`);
}

// Summary
console.log('═══════════════════════════════════════════════');
console.log(`📊 Test Summary:`);
console.log(`   Total Assertions: ${testCount}`);
console.log(`   ✅ Passed: ${passCount}`);
console.log(`   ❌ Failed: ${failCount}`);
console.log(`   Success Rate: ${((passCount / testCount) * 100).toFixed(1)}%`);
console.log('═══════════════════════════════════════════════');

if (failCount === 0) {
  console.log('\n🎉 All tests passed!\n');
  process.exit(0);
} else {
  console.log(`\n❌ ${failCount} test(s) failed\n`);
  process.exit(1);
}
