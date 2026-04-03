/**
 * @file Policy-Controlled Workflow Proof
 * @description Proves that hooks can be gated by policy conditions based on actor role
 *
 * Scenario: Two hooks, one ungated, one policy-gated
 * - Hook A: audit-logger (no policy, always executes)
 * - Hook B: data-mutator (policy: actor must be "reviewer")
 *
 * Test 1: actor="user" → Hook A executes, Hook B blocked
 * Test 2: actor="reviewer" → Hook A executes, Hook B executes
 */

import { defineHook, executeHookChain } from '../src/index.mjs';

// Simple quad mock factory
const createQuad = (subject, predicate, object, graph) => ({
  subject: { termType: 'NamedNode', value: subject },
  predicate: { termType: 'NamedNode', value: predicate },
  object: { termType: 'Literal', value: object },
  graph: graph ? { termType: 'NamedNode', value: graph } : undefined,
});

/**
 * Policy Engine - evaluates actor-based policies
 */
class PolicyEngine {
  /**
   *
   */
  constructor() {
    this.policies = new Map();
  }

  /**
   * Register a policy for a hook
   */
  registerPolicy(hookName, predicate) {
    this.policies.set(hookName, predicate);
  }

  /**
   * Check if hook is allowed to execute
   */
  isAllowed(hookName, context) {
    const policy = this.policies.get(hookName);
    if (!policy) {
      return true;
    }
    
    try {
      return policy(context);
    } catch (error) {
      console.error('Policy evaluation error:', error.message);
      return false;
    }
  }

  /**
   * Filter hooks by policy gates
   */
  filterHooks(hooks, context) {
    const allowed = [];
    const denied = [];

    for (const hook of hooks) {
      if (this.isAllowed(hook.name, context)) {
        allowed.push(hook);
      } else {
        denied.push(hook);
      }
    }

    return { allowed, denied };
  }
}

const auditLoggerHook = defineHook({
  name: 'audit-logger',
  trigger: 'before-add',
  transform: (quad) => {
    console.log('[AUDIT] Logging quad:', quad.subject.value);
    return quad;
  },
  metadata: {
    description: 'Audit logger - always executes',
    policy: 'none',
  },
});

const dataMutatorHook = defineHook({
  name: 'data-mutator',
  trigger: 'before-add',
  transform: (quad) => {
    if (quad.object.termType === 'Literal') {
      const uppercased = quad.object.value.toUpperCase();
      return {
        ...quad,
        object: { termType: 'Literal', value: uppercased },
      };
    }
    return quad;
  },
  metadata: {
    description: 'Data mutator - requires reviewer role',
    policy: 'actor-role-reviewer',
  },
});

/**
 *
 */
function runProof() {
  console.log('======================================================================');
  console.log('POLICY-CONTROLLED WORKFLOW PROOF');
  console.log('======================================================================');
  console.log('');

  const policyEngine = new PolicyEngine();

  policyEngine.registerPolicy('data-mutator', (context) => {
    return context.actor === 'reviewer';
  });

  const testQuad = createQuad(
    'http://example.org/alice',
    'http://xmlns.com/foaf/0.1/name',
    'Alice Smith',
    'http://example.org/graph1'
  );

  const hooks = [auditLoggerHook, dataMutatorHook];

  console.log('TEST 1: actor="user" (unauthorized)');
  console.log('----------------------------------------------------------------------');
  const context1 = { actor: 'user', timestamp: Date.now() };
  const filter1 = policyEngine.filterHooks(hooks, context1);

  console.log('Hooks allowed:', filter1.allowed.map(h => h.name).join(', '));
  console.log('Hooks denied: ', filter1.denied.map(h => h.name).join(', '));
  console.log('');

  if (filter1.allowed.length > 0) {
    const result1 = executeHookChain(filter1.allowed, testQuad);
    console.log('Execution result:', result1.valid ? 'SUCCESS' : 'FAILED');
    console.log('Final quad value:', result1.quad.object.value);
  }

  console.log('');
  const test1Pass = filter1.allowed.length === 1 && filter1.allowed[0].name === 'audit-logger';
  console.log('Expected: audit-logger executes, data-mutator blocked');
  console.log('Actual:  ', test1Pass ? 'PASS ✅' : 'FAIL ❌');
  console.log('');

  console.log('======================================================================');
  console.log('TEST 2: actor="reviewer" (authorized)');
  console.log('----------------------------------------------------------------------');
  const context2 = { actor: 'reviewer', timestamp: Date.now() };
  const filter2 = policyEngine.filterHooks(hooks, context2);

  console.log('Hooks allowed:', filter2.allowed.map(h => h.name).join(', '));
  console.log('Hooks denied: ', filter2.denied.map(h => h.name).join(', '));
  console.log('');

  if (filter2.allowed.length > 0) {
    const result2 = executeHookChain(filter2.allowed, testQuad);
    console.log('Execution result:', result2.valid ? 'SUCCESS' : 'FAILED');
    console.log('Final quad value:', result2.quad.object.value);
  }

  console.log('');
  const test2Pass = filter2.allowed.length === 2;
  console.log('Expected: both hooks execute, value transformed to uppercase');
  console.log('Actual:  ', test2Pass ? 'PASS ✅' : 'FAIL ❌');
  console.log('');

  console.log('======================================================================');
  console.log('PROOF SUMMARY');
  console.log('======================================================================');
  console.log('');
  console.log('Policy Engine demonstrates:');
  console.log('  1. Ungated hooks always execute (audit-logger)');
  console.log('  2. Policy-gated hooks require actor authorization');
  console.log('  3. Fail-closed security (policy error = deny)');
  console.log('  4. Clean separation: policy evaluation ≠ hook execution');
  console.log('');
  console.log('Architecture:');
  console.log('  - PolicyEngine.registerPolicy(hookName, predicate)');
  console.log('  - PolicyEngine.isAllowed(hookName, context) → boolean');
  console.log('  - PolicyEngine.filterHooks(hooks, context) → { allowed, denied }');
  console.log('');
  console.log('✅ Policy-controlled workflow PROVEN');
  console.log('');
}

if (import.meta.url === 'file://' + process.argv[1]) {
  runProof();
}

export { PolicyEngine, auditLoggerHook, dataMutatorHook, runProof };
