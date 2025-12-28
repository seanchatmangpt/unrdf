/**
 * @file Policy-Controlled Workflow Proof (Standalone)
 * @description Proves that hooks can be gated by policy conditions based on actor role
 */

const createQuad = (subject, predicate, object, graph) => ({
  subject: { termType: 'NamedNode', value: subject },
  predicate: { termType: 'NamedNode', value: predicate },
  object: { termType: 'Literal', value: object },
  graph: graph ? { termType: 'NamedNode', value: graph } : undefined,
});

/**
 *
 */
class PolicyEngine {
  /**
   *
   */
  constructor() {
    this.policies = new Map();
    this.auditLog = [];
  }

  /**
   *
   */
  registerPolicy(hookName, predicate) {
    this.policies.set(hookName, predicate);
  }

  /**
   *
   */
  isAllowed(hookName, context) {
    const policy = this.policies.get(hookName);
    
    this.auditLog.push({
      timestamp: new Date().toISOString(),
      hookName,
      context,
      hasPolicy: !!policy,
      decision: null,
    });
    
    if (!policy) {
      this.auditLog[this.auditLog.length - 1].decision = 'allow (no policy)';
      return true;
    }
    
    try {
      const allowed = policy(context);
      this.auditLog[this.auditLog.length - 1].decision = allowed ? 'allow' : 'deny';
      return allowed;
    } catch (error) {
      console.error('Policy evaluation error:', error.message);
      this.auditLog[this.auditLog.length - 1].decision = 'deny (error)';
      return false;
    }
  }

  /**
   *
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

  /**
   *
   */
  getAuditLog() {
    return this.auditLog;
  }
}

function executeHook(hook, quad) {
  const result = { valid: true, quad, hookName: hook.name };
  
  try {
    if (hook.transform) {
      result.quad = hook.transform(quad);
    }
    return result;
  } catch (error) {
    result.valid = false;
    result.error = error.message;
    return result;
  }
}

function executeHookChain(hooks, quad) {
  const results = [];
  let currentQuad = quad;
  let chainValid = true;
  let chainError = undefined;

  for (const hook of hooks) {
    const result = executeHook(hook, currentQuad);
    results.push(result);

    if (!result.valid) {
      chainValid = false;
      chainError = result.error;
      break;
    }

    if (result.quad) {
      currentQuad = result.quad;
    }
  }

  return {
    valid: chainValid,
    quad: currentQuad,
    results,
    error: chainError,
  };
}

const auditLoggerHook = {
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
};

const dataMutatorHook = {
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
};

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
  const test1Pass = filter1.allowed.length === 1 && 
                     filter1.allowed[0].name === 'audit-logger' &&
                     filter1.denied.length === 1 &&
                     filter1.denied[0].name === 'data-mutator';
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
  const test2Pass = filter2.allowed.length === 2 &&
                     filter2.denied.length === 0;
  console.log('Expected: both hooks execute, value transformed to uppercase');
  console.log('Actual:  ', test2Pass ? 'PASS ✅' : 'FAIL ❌');
  console.log('');

  console.log('======================================================================');
  console.log('AUDIT LOG');
  console.log('======================================================================');
  console.log('');
  const auditLog = policyEngine.getAuditLog();
  auditLog.forEach((entry, idx) => {
    console.log(`[${idx + 1}] ${entry.hookName}`);
    console.log(`    Actor: ${entry.context.actor}`);
    console.log(`    Decision: ${entry.decision}`);
  });
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
  console.log('  5. Full audit trail of policy decisions');
  console.log('');
  console.log('Architecture:');
  console.log('  - PolicyEngine.registerPolicy(hookName, predicate)');
  console.log('  - PolicyEngine.isAllowed(hookName, context) → boolean');
  console.log('  - PolicyEngine.filterHooks(hooks, context) → { allowed, denied }');
  console.log('  - PolicyEngine.getAuditLog() → audit trail');
  console.log('');
  const overallPass = test1Pass && test2Pass;
  console.log(overallPass ? '✅ Policy-controlled workflow PROVEN' : '❌ Tests FAILED');
  console.log('');
  
  return overallPass;
}

if (import.meta.url === 'file://' + process.argv[1]) {
  const passed = runProof();
  process.exit(passed ? 0 : 1);
}

export { PolicyEngine, auditLoggerHook, dataMutatorHook, runProof };
