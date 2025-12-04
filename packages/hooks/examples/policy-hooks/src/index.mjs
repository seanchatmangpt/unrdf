/**
 * @file Policy Hooks Example
 *
 * Demonstrates:
 * - Defining custom policy hooks
 * - RDF access control policies
 * - Data validation constraints
 * - Hook execution and results
 *
 * @module hooks-example-policy
 */

import { namedNode, literal, quad, createStore, addQuad } from '@unrdf/core';
import {
  defineHook,
  createHookRegistry,
  registerHook,
  executeHook,
  executeHooksByTrigger,
} from '@unrdf/hooks';
import { DataFactory } from 'n3';

/* ========================================================================= */
/* Policy Hook Definitions                                                   */
/* ========================================================================= */

/**
 * Access Control List (ACL) Policy Hook
 *
 * Only allows quads from trusted namespaces.
 */
const aclPolicy = defineHook({
  name: 'acl-policy',
  trigger: 'before-add',
  validate: quad => {
    const trustedNamespaces = [
      'http://example.org/',
      'http://xmlns.com/foaf/0.1/',
      'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    ];

    const subjectIRI = quad.subject.termType === 'NamedNode' ? quad.subject.value : '';
    const predicateIRI = quad.predicate.value;

    return trustedNamespaces.some(ns => subjectIRI.startsWith(ns) || predicateIRI.startsWith(ns));
  },
  metadata: {
    description: 'ACL policy - only allow quads from trusted namespaces',
    policy: 'security',
  },
});

/**
 * Data Type Policy Hook
 *
 * Enforces strict typing on foaf:age - must be integer.
 */
const dataTypePolicy = defineHook({
  name: 'data-type-policy',
  trigger: 'before-add',
  validate: quad => {
    if (quad.predicate.value === 'http://xmlns.com/foaf/0.1/age') {
      if (quad.object.termType !== 'Literal') {
        return false;
      }
      const value = parseInt(quad.object.value, 10);
      return !isNaN(value) && value >= 0 && value <= 150;
    }
    return true;
  },
  metadata: {
    description: 'Data type policy - foaf:age must be valid integer 0-150',
    policy: 'validation',
  },
});

/**
 * Privacy Policy Hook
 *
 * Redacts email addresses unless explicitly allowed.
 */
const privacyPolicy = defineHook({
  name: 'privacy-policy',
  trigger: 'before-add',
  transform: quad => {
    if (quad.predicate.value === 'http://xmlns.com/foaf/0.1/mbox') {
      if (quad.object.termType === 'Literal') {
        // Redact email - replace with placeholder
        return DataFactory.quad(
          quad.subject,
          quad.predicate,
          DataFactory.literal('[REDACTED]'),
          quad.graph
        );
      }
    }
    return quad;
  },
  metadata: {
    description: 'Privacy policy - redact email addresses',
    policy: 'privacy',
  },
});

/**
 * Provenance Policy Hook
 *
 * Requires all quads to have provenance metadata.
 */
const provenancePolicy = defineHook({
  name: 'provenance-policy',
  trigger: 'before-add',
  validate: quad => {
    // In a real implementation, this would check for provenance metadata
    // For example, check that quad.graph is not the default graph
    return quad.graph && quad.graph.termType === 'NamedNode';
  },
  metadata: {
    description: 'Provenance policy - require provenance metadata',
    policy: 'audit',
  },
});

/* ========================================================================= */
/* Example Usage                                                             */
/* ========================================================================= */

/**
 * Create and configure hook registry with policies.
 */
function setupPolicyRegistry() {
  const registry = createHookRegistry();

  // Register all policy hooks
  registerHook(registry, aclPolicy);
  registerHook(registry, dataTypePolicy);
  registerHook(registry, privacyPolicy);
  registerHook(registry, provenancePolicy);

  return registry;
}

/**
 * Test quads against policy hooks.
 */
function testPolicies() {
  const registry = setupPolicyRegistry();
  const store = createStore();

  console.log('🔒 Policy Hooks Example\n');
  console.log('='.repeat(60));

  // Test 1: Trusted namespace (should pass ACL)
  console.log('\n✅ Test 1: Trusted namespace');
  const q1 = quad(
    namedNode('http://example.org/alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice'),
    namedNode('http://example.org/graph1')
  );

  const hooks1 = [aclPolicy, dataTypePolicy, privacyPolicy, provenancePolicy];
  const results1 = executeHooksByTrigger(hooks1, 'before-add', q1);
  const passedCount1 = results1.results.filter(r => r.valid).length;
  console.log(`   Passed: ${passedCount1}/${results1.results.length}`);
  console.log(`   Failed: ${results1.results.length - passedCount1}/${results1.results.length}`);
  if (!results1.valid) {
    results1.results.filter(r => !r.valid).forEach(r => console.log(`   ❌ ${r.hookName}: ${r.error}`));
  }

  // Test 2: Untrusted namespace (should fail ACL)
  console.log('\n❌ Test 2: Untrusted namespace');
  const q2 = quad(
    namedNode('http://untrusted.org/bob'),
    namedNode('http://untrusted.org/property'),
    literal('Bob'),
    namedNode('http://example.org/graph1')
  );

  const hooks2 = [aclPolicy, dataTypePolicy, privacyPolicy, provenancePolicy];
  const results2 = executeHooksByTrigger(hooks2, 'before-add', q2);
  const passedCount2 = results2.results.filter(r => r.valid).length;
  console.log(`   Passed: ${passedCount2}/${results2.results.length}`);
  console.log(`   Failed: ${results2.results.length - passedCount2}/${results2.results.length}`);
  if (!results2.valid) {
    results2.results.filter(r => !r.valid).forEach(r => console.log(`   ❌ ${r.hookName}: ${r.error}`));
  }

  // Test 3: Valid age (should pass data type policy)
  console.log('\n✅ Test 3: Valid age constraint');
  const q3 = quad(
    namedNode('http://example.org/alice'),
    namedNode('http://xmlns.com/foaf/0.1/age'),
    literal('30'),
    namedNode('http://example.org/graph1')
  );

  const hooks3 = [aclPolicy, dataTypePolicy, privacyPolicy, provenancePolicy];
  const results3 = executeHooksByTrigger(hooks3, 'before-add', q3);
  const passedCount3 = results3.results.filter(r => r.valid).length;
  console.log(`   Passed: ${passedCount3}/${results3.results.length}`);
  console.log(`   Failed: ${results3.results.length - passedCount3}/${results3.results.length}`);

  // Test 4: Invalid age (should fail data type policy)
  console.log('\n❌ Test 4: Invalid age constraint');
  const q4 = quad(
    namedNode('http://example.org/bob'),
    namedNode('http://xmlns.com/foaf/0.1/age'),
    literal('999'),
    namedNode('http://example.org/graph1')
  );

  const hooks4 = [aclPolicy, dataTypePolicy, privacyPolicy, provenancePolicy];
  const results4 = executeHooksByTrigger(hooks4, 'before-add', q4);
  const passedCount4 = results4.results.filter(r => r.valid).length;
  console.log(`   Passed: ${passedCount4}/${results4.results.length}`);
  console.log(`   Failed: ${results4.results.length - passedCount4}/${results4.results.length}`);
  if (!results4.valid) {
    results4.results.filter(r => !r.valid).forEach(r => console.log(`   ❌ ${r.hookName}: ${r.error}`));
  }

  // Test 5: Email privacy transformation
  console.log('\n🔐 Test 5: Privacy policy transformation');
  const q5 = quad(
    namedNode('http://example.org/alice'),
    namedNode('http://xmlns.com/foaf/0.1/mbox'),
    literal('alice@example.org'),
    namedNode('http://example.org/graph1')
  );

  const privacyResult = executeHook(privacyPolicy, q5);
  console.log(`   Status: ${privacyResult.valid ? 'PASSED' : 'FAILED'}`);
  if (privacyResult.quad && privacyResult.quad.object.value !== q5.object.value) {
    console.log(`   Original: ${q5.object.value}`);
    console.log(`   Transformed: ${privacyResult.quad.object.value}`);
  }

  // Test 6: Missing provenance (should fail provenance policy)
  console.log('\n❌ Test 6: Missing provenance');
  const q6 = quad(
    namedNode('http://example.org/charlie'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Charlie')
    // No graph parameter - uses default graph
  );

  const hooks6 = [aclPolicy, dataTypePolicy, privacyPolicy, provenancePolicy];
  const results6 = executeHooksByTrigger(hooks6, 'before-add', q6);
  const passedCount6 = results6.results.filter(r => r.valid).length;
  console.log(`   Passed: ${passedCount6}/${results6.results.length}`);
  console.log(`   Failed: ${results6.results.length - passedCount6}/${results6.results.length}`);
  if (!results6.valid) {
    results6.results.filter(r => !r.valid).forEach(r => console.log(`   ❌ ${r.hookName}: ${r.error}`));
  }

  console.log('\n' + '='.repeat(60));
  console.log('✨ Policy Hooks Example Complete\n');
}

/* ========================================================================= */
/* Export API                                                                */
/* ========================================================================= */

export {
  aclPolicy,
  dataTypePolicy,
  privacyPolicy,
  provenancePolicy,
  setupPolicyRegistry,
  testPolicies,
};

// Run example if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  testPolicies();
}
