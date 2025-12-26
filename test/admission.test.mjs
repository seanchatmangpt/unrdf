/**
 * @file Admission Tests
 * @description Tests for admission control, invariants, and decisions
 */

import { test, describe } from 'node:test';
import assert from 'node:assert/strict';
import { Universe } from '../src/admission/universe.mjs';
import {
  AdmissionController,
  AdditiveOnlyInvariant,
  SubstrateImmutabilityInvariant,
  ProtectedNamespaceInvariant,
  CanonicalConstraintInvariant,
  TypeConsistencyInvariant,
  SchemaCoherenceInvariant,
} from '../src/admission/admission.mjs';

describe('Admission Tests', () => {
  test('[TEST] Admission - Valid additive delta should be ALLOWED', async () => {
    console.log('[START] Testing valid additive delta');
    const universe = new Universe();
    await universe.loadFromTTL('');

    const controller = new AdmissionController();
    const delta = {
      additions: [
        {
          subject: 'https://unrdf.org/studios/myProject',
          predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          object: 'https://unrdf.org/studios/Project',
        },
      ],
      deletions: [],
    };

    const result = controller.admit(delta, universe);

    console.log(`[ASSERT] Decision: ${result.decision}`);
    assert.equal(result.decision, 'ALLOW', 'Valid additive delta should be allowed');
    console.log('[ASSERT] No violations');
    assert.equal(result.violations.length, 0);
    console.log('[RESULT] pass');
  });

  test('[TEST] Admission - Delta redefining substrate term should be DENIED', async () => {
    console.log('[START] Testing substrate redefinition denial');
    const universe = new Universe();
    await universe.loadFromTTL('');

    const controller = new AdmissionController();
    const delta = {
      additions: [
        {
          subject: 'https://unrdf.org/substrate/Entity',
          predicate: 'http://www.w3.org/2000/01/rdf-schema#comment',
          object: 'Redefined substrate term',
        },
      ],
      deletions: [],
    };

    const result = controller.admit(delta, universe);

    console.log(`[ASSERT] Decision: ${result.decision}`);
    assert.equal(result.decision, 'DENY', 'Substrate redefinition should be denied');
    console.log(`[ASSERT] Violations present: ${result.violations.length}`);
    assert.ok(result.violations.length > 0);
    console.log('[RESULT] pass');
  });

  test('[TEST] Admission - Delta colliding with protected namespace should be DENIED', async () => {
    console.log('[START] Testing protected namespace collision');
    const universe = new Universe();
    await universe.loadFromTTL('');

    const controller = new AdmissionController();
    // Define new term IN protected namespace (subject starts with protected NS)
    const delta = {
      additions: [
        {
          subject: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#customNewTerm', // Subject in protected NS
          predicate: 'http://www.w3.org/2000/01/rdf-schema#label',
          object: 'Custom term in protected namespace',
        },
      ],
      deletions: [],
    };

    const result = controller.admit(delta, universe);

    console.log(`[ASSERT] Decision: ${result.decision}`);
    assert.equal(result.decision, 'DENY', 'Protected namespace collision should be denied');
    console.log(`[ASSERT] Violations: ${result.violations.length}`);
    assert.ok(result.violations.length > 0);
    console.log('[RESULT] pass');
  });

  test('[TEST] Admission - Delta weakening canonical constraint should be DENIED', async () => {
    console.log('[START] Testing constraint weakening denial');
    const universe = new Universe();
    await universe.loadFromTTL('');

    const controller = new AdmissionController();
    const delta = {
      additions: [
        {
          subject: 'https://unrdf.org/shape/test',
          predicate: 'http://www.w3.org/ns/shacl#minCount',
          object: '0',
        },
      ],
      deletions: [],
    };

    const result = controller.admit(delta, universe);

    console.log(`[ASSERT] Decision: ${result.decision}`);
    assert.equal(result.decision, 'DENY', 'Constraint weakening should be denied');
    console.log('[RESULT] pass');
  });

  test('[TEST] Admission - All 6 invariants execute in sequence', async () => {
    console.log('[START] Testing invariant execution');
    const universe = new Universe();
    await universe.loadFromTTL('');

    const controller = new AdmissionController();
    const invariants = controller.getInvariants();

    console.log(`[ASSERT] Invariant count: ${invariants.length}`);
    assert.equal(invariants.length, 6, 'Should have exactly 6 invariants');

    console.log('[ASSERT] Verifying invariant names');
    const names = invariants.map(i => i.name);
    assert.ok(names.includes('AdditiveOnly'), 'Should include AdditiveOnly invariant');
    assert.ok(names.includes('SubstrateImmutability'), 'Should include SubstrateImmutability');
    assert.ok(names.includes('ProtectedNamespace'), 'Should include ProtectedNamespace');
    assert.ok(names.includes('CanonicalConstraint'), 'Should include CanonicalConstraint');
    assert.ok(names.includes('TypeConsistency'), 'Should include TypeConsistency');
    assert.ok(names.includes('SchemaCoherence'), 'Should include SchemaCoherence');

    console.log('[RESULT] pass');
  });

  test('[TEST] Admission - Forbidden DELETE operation triggers denial', async () => {
    console.log('[START] Testing forbidden DELETE operation');
    const universe = new Universe();
    await universe.loadFromTTL('');

    const controller = new AdmissionController();
    const delta = {
      additions: [],
      deletions: [
        {
          subject: 'https://unrdf.org/test',
          predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          object: 'https://unrdf.org/test/Type',
        },
      ],
    };

    const result = controller.admit(delta, universe);

    console.log(`[ASSERT] Decision: ${result.decision}`);
    assert.equal(result.decision, 'DENY', 'DELETE operation should be denied');
    console.log('[RESULT] pass');
  });

  test('[TEST] AdditiveOnlyInvariant - Denies deletions', async () => {
    console.log('[START] Testing AdditiveOnlyInvariant');
    const universe = new Universe();
    await universe.loadFromTTL('');

    const invariant = new AdditiveOnlyInvariant();
    const delta = {
      additions: [],
      deletions: [{ subject: 'test', predicate: 'test', object: 'test' }],
    };

    const result = invariant.check(delta, universe);

    console.log(`[ASSERT] Decision: ${result.decision}`);
    assert.equal(result.decision, 'DENY');
    assert.ok(result.violations.length > 0);
    console.log('[RESULT] pass');
  });

  test('[TEST] SubstrateImmutabilityInvariant - Protects substrate terms', async () => {
    console.log('[START] Testing SubstrateImmutabilityInvariant');
    const universe = new Universe();
    await universe.loadFromTTL('');

    const invariant = new SubstrateImmutabilityInvariant();
    const delta = {
      additions: [
        {
          subject: 'https://unrdf.org/substrate/test',
          predicate: 'test',
          object: 'test',
        },
      ],
      deletions: [],
    };

    const result = invariant.check(delta, universe);

    console.log(`[ASSERT] Decision: ${result.decision}`);
    assert.equal(result.decision, 'DENY');
    console.log('[RESULT] pass');
  });

  test('[TEST] ProtectedNamespaceInvariant - Guards standard namespaces', async () => {
    console.log('[START] Testing ProtectedNamespaceInvariant');
    const universe = new Universe();
    await universe.loadFromTTL('');

    const invariant = new ProtectedNamespaceInvariant();
    // Test that defining new terms IN protected namespaces is denied
    const delta = {
      additions: [
        {
          subject: 'http://www.w3.org/2002/07/owl#customTerm', // Subject in protected NS
          predicate: 'http://www.w3.org/2000/01/rdf-schema#label',
          object: 'test',
        },
      ],
      deletions: [],
    };

    const result = invariant.check(delta, universe);

    console.log(`[ASSERT] Decision: ${result.decision}`);
    assert.equal(result.decision, 'DENY');
    console.log('[RESULT] pass');
  });

  test('[TEST] CanonicalConstraintInvariant - Prevents constraint weakening', async () => {
    console.log('[START] Testing CanonicalConstraintInvariant');
    const universe = new Universe();
    await universe.loadFromTTL('');

    const invariant = new CanonicalConstraintInvariant();
    const delta = {
      additions: [
        {
          subject: 'test',
          predicate: 'http://www.w3.org/ns/shacl#minCount',
          object: '0',
        },
      ],
      deletions: [],
    };

    const result = invariant.check(delta, universe);

    console.log(`[ASSERT] Decision: ${result.decision}`);
    assert.equal(result.decision, 'DENY');
    console.log('[RESULT] pass');
  });

  test('[TEST] TypeConsistencyInvariant - Validates type consistency', async () => {
    console.log('[START] Testing TypeConsistencyInvariant');
    const universe = new Universe();
    await universe.loadFromTTL('');

    const invariant = new TypeConsistencyInvariant();
    const delta = {
      additions: [{ subject: 'test', predicate: 'test', object: 'test' }],
      deletions: [],
    };

    const result = invariant.check(delta, universe);

    console.log(`[ASSERT] Decision: ${result.decision}`);
    assert.equal(result.decision, 'ALLOW');
    console.log('[RESULT] pass');
  });

  test('[TEST] SchemaCoherenceInvariant - Validates schema coherence', async () => {
    console.log('[START] Testing SchemaCoherenceInvariant');
    const universe = new Universe();
    await universe.loadFromTTL('');

    const invariant = new SchemaCoherenceInvariant();
    const delta = {
      additions: [{ subject: 'test', predicate: 'test', object: 'test' }],
      deletions: [],
    };

    const result = invariant.check(delta, universe);

    console.log(`[ASSERT] Decision: ${result.decision}`);
    assert.equal(result.decision, 'ALLOW');
    console.log('[RESULT] pass');
  });

  test('[TEST] Admission - Multiple violations are captured', async () => {
    console.log('[START] Testing multiple violations');
    const universe = new Universe();
    await universe.loadFromTTL('');

    const controller = new AdmissionController();
    // Multiple subjects in protected namespaces
    const delta = {
      additions: [
        {
          subject: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#custom1',
          predicate: 'http://www.w3.org/2000/01/rdf-schema#label',
          object: 'test',
        },
        {
          subject: 'http://www.w3.org/2000/01/rdf-schema#custom2',
          predicate: 'http://www.w3.org/2000/01/rdf-schema#label',
          object: 'test',
        },
      ],
      deletions: [],
    };

    const result = controller.admit(delta, universe);

    console.log(`[ASSERT] Decision: ${result.decision}`);
    assert.equal(result.decision, 'DENY');
    console.log(`[ASSERT] Multiple violations: ${result.violations.length}`);
    assert.ok(result.violations.length >= 1);
    console.log('[RESULT] pass');
  });

  test('[TEST] Admission - Empty delta is allowed', async () => {
    console.log('[START] Testing empty delta');
    const universe = new Universe();
    await universe.loadFromTTL('');

    const controller = new AdmissionController();
    const delta = {
      additions: [],
      deletions: [],
    };

    const result = controller.admit(delta, universe);

    console.log(`[ASSERT] Decision: ${result.decision}`);
    assert.equal(result.decision, 'ALLOW');
    console.log('[RESULT] pass');
  });
});

console.log('\n=== Admission Test Suite Complete ===');
