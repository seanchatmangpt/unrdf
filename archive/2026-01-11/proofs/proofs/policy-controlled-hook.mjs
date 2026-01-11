#!/usr/bin/env node

/**
 * @file Policy-Controlled Hook Execution Proof
 * @module proofs/policy-controlled-hook
 *
 * @description
 * Demonstrates policy-based hook gating with actor-based RBAC.
 *
 * Scenario:
 * - Hook A: No policy (always runs)
 * - Hook B: Policy-gated (actor=reviewer only)
 *
 * Test 1: actor="user" → Hook A ✅ executed, Hook B ❌ blocked
 * Test 2: actor="reviewer" → Hook A ✅ executed, Hook B ✅ executed
 *
 * Expected Behavior:
 * - Policy predicates gate hook execution
 * - Hooks without policies always execute
 * - Actor-based RBAC works as expected
 *
 * STANDALONE VERSION - No external dependencies
 */

/* ========================================================================= */
/* Minimal RDF Quad Implementation (No Dependencies)                        */
/* ========================================================================= */

/**
 * Create a named node
 */
function namedNode(value) {
  return { termType: 'NamedNode', value };
}

/**
 * Create a literal
 */
function literal(value) {
  return { termType: 'Literal', value };
}

/**
 * Create a quad
 */
function quad(subject, predicate, object, graph = null) {
  return { subject, predicate, object, graph };
}

/* ========================================================================= */
/* Minimal Hook Definition (Simplified)                                     */
/* ========================================================================= */

/**
 * Define a hook (simplified version)
 * @param {Object} config - Hook configuration
 * @returns {Object} - Hook definition
 */
function defineHook(config) {
  if (!config.name) throw new Error('Hook name is required');
  if (!config.trigger) throw new Error('Hook trigger is required');
  if (!config.validate && !config.transform) {
    throw new Error('Hook must define either validate or transform');
  }

  return {
    name: config.name,
    trigger: config.trigger,
    validate: config.validate,
    transform: config.transform,
    metadata: config.metadata || {}
  };
}

/**
 * Execute a hook (simplified version)
 * @param {Object} hook - Hook definition
 * @param {Object} quad - RDF quad
 * @returns {Object} - Execution result
 */
function executeHook(hook, quad) {
  const result = {
    valid: true,
    quad: quad,
    hookName: hook.name
  };

  try {
    // Execute validation if present
    if (hook.validate) {
      const validationResult = hook.validate(quad);
      if (!validationResult) {
        result.valid = false;
        result.error = `Validation failed for hook: ${hook.name}`;
        return result;
      }
    }

    // Execute transformation if present
    if (hook.transform) {
      const transformed = hook.transform(quad);
      result.quad = transformed;
    }

    return result;
  } catch (error) {
    result.valid = false;
    result.error = error.message;
    return result;
  }
}

/* ========================================================================= */
/* Hook Definitions                                                          */
/* ========================================================================= */

/**
 * Hook A: No policy - always executes
 */
const hookA = defineHook({
  name: 'admit-hook-A',
  trigger: 'before-add',
  validate: (quad) => {
    // Simple validation: subject must be NamedNode
    return quad.subject.termType === 'NamedNode';
  },
  metadata: {
    description: 'Always-on hook (no policy)',
    policy: null
  }
});

/**
 * Hook B: Policy-gated - only executes if actor=reviewer
 */
const hookB = defineHook({
  name: 'admit-hook-B',
  trigger: 'before-add',
  validate: (quad) => {
    // Business logic: only allow triples with 'approve' predicate
    return quad.predicate.value === 'http://example.org/approve';
  },
  metadata: {
    description: 'Reviewer-only hook (policy-gated)',
    policy: 'reviewer-check',
    condition: {
      kind: 'sparql-ask',
      query: 'ASK { ?actor <http://xmlns.com/foaf/0.1/role> "reviewer" }'
    }
  }
});

/* ========================================================================= */
/* Policy Evaluator                                                          */
/* ========================================================================= */

/**
 * Simple policy evaluator for actor-based RBAC.
 * @param {Object} hook - Hook definition
 * @param {string} actor - Actor identifier
 * @returns {boolean} - True if policy allows execution
 */
function evaluatePolicy(hook, actor) {
  // If no policy, always allow
  if (!hook.metadata?.policy) {
    return true;
  }

  // Get condition from hook metadata
  const condition = hook.metadata.condition;
  if (!condition) {
    console.warn(`[WARN] Hook ${hook.name} has policy ${hook.metadata.policy} but no condition`);
    return false;
  }

  // For this proof, we simulate SPARQL ASK evaluation
  // In production, this would use ConditionEvaluator.isSatisfied()
  if (condition.kind === 'sparql-ask') {
    // Check if actor has 'reviewer' role
    // Simulated: reviewer role check
    const actorRoles = {
      'user': [],
      'reviewer': ['reviewer'],
      'admin': ['reviewer', 'admin']
    };

    const roles = actorRoles[actor] || [];
    const hasReviewerRole = roles.includes('reviewer');

    return hasReviewerRole;
  }

  // Unknown condition type - deny by default
  return false;
}

/**
 * Execute hook with policy gating.
 * @param {Object} hook - Hook definition
 * @param {Object} quad - RDF quad
 * @param {string} actor - Actor identifier
 * @returns {Object} - Execution result with policy status
 */
function executeWithPolicy(hook, quad, actor) {
  // Evaluate policy first
  const policyAllowed = evaluatePolicy(hook, actor);

  if (!policyAllowed) {
    // Policy BLOCKED execution
    return {
      valid: false,
      quad,
      hookName: hook.name,
      blocked: true,
      error: `Policy blocked: ${hook.metadata?.policy || 'unknown'} (actor: ${actor})`
    };
  }

  // Policy ALLOWED - execute hook
  const result = executeHook(hook, quad);
  return {
    ...result,
    blocked: false
  };
}

/* ========================================================================= */
/* Test Runner                                                               */
/* ========================================================================= */

/**
 * Run proof with specific actor.
 * @param {string} actor - Actor identifier
 */
function runProof(actor) {
  console.log('\n' + '='.repeat(70));
  console.log(`🔐 Policy-Controlled Hook Execution Proof`);
  console.log(`   Actor: ${actor}`);
  console.log('='.repeat(70));

  // Create test quad
  const testQuad = quad(
    namedNode('http://example.org/doc1'),
    namedNode('http://example.org/approve'),
    literal('approved')
  );

  console.log('\n📋 Test Quad:');
  console.log(`   Subject:   ${testQuad.subject.value}`);
  console.log(`   Predicate: ${testQuad.predicate.value}`);
  console.log(`   Object:    ${testQuad.object.value}`);

  // Execute Hook A (no policy - should always execute)
  console.log('\n\n🔹 Hook A: admit-hook-A (no policy)');
  console.log('   Expected: ✅ EXECUTED (no policy gates)');
  const resultA = executeWithPolicy(hookA, testQuad, actor);

  if (resultA.blocked) {
    console.log(`   ❌ FAILED: Hook blocked unexpectedly`);
    console.log(`      Error: ${resultA.error}`);
  } else if (resultA.valid) {
    console.log(`   ✅ EXECUTED: Validation passed`);
  } else {
    console.log(`   ⚠️  EXECUTED: Validation failed (business logic)`);
    console.log(`      Error: ${resultA.error}`);
  }

  // Execute Hook B (policy-gated - only reviewer)
  console.log('\n🔹 Hook B: admit-hook-B (actor=reviewer only)');
  const expectedBlocked = actor !== 'reviewer' && actor !== 'admin';
  console.log(`   Expected: ${expectedBlocked ? '❌ BLOCKED' : '✅ EXECUTED'} (policy: reviewer-check)`);
  const resultB = executeWithPolicy(hookB, testQuad, actor);

  if (resultB.blocked) {
    console.log(`   ❌ BLOCKED: Policy denied execution`);
    console.log(`      Reason: ${resultB.error}`);
  } else if (resultB.valid) {
    console.log(`   ✅ EXECUTED: Policy allowed + validation passed`);
  } else {
    console.log(`   ⚠️  EXECUTED: Policy allowed, validation failed (business logic)`);
    console.log(`      Error: ${resultB.error}`);
  }

  // Summary
  console.log('\n' + '─'.repeat(70));
  console.log('📊 Summary:');
  console.log(`   Hook A (no policy):     ${resultA.blocked ? '❌ BLOCKED' : resultA.valid ? '✅ PASSED' : '⚠️  FAILED'}`);
  console.log(`   Hook B (reviewer only): ${resultB.blocked ? '❌ BLOCKED' : resultB.valid ? '✅ PASSED' : '⚠️  FAILED'}`);

  // Validation
  const test1Pass = !resultA.blocked && resultA.valid;
  const test2Pass = expectedBlocked ? resultB.blocked : (resultB.valid && !resultB.blocked);
  const allPass = test1Pass && test2Pass;

  console.log('\n' + (allPass ? '✅' : '❌') + ' Proof: ' + (allPass ? 'PASS' : 'FAIL'));

  if (!allPass) {
    console.log('   Issues:');
    if (!test1Pass) console.log('   - Hook A should always execute (no policy)');
    if (!test2Pass) {
      if (expectedBlocked) {
        console.log('   - Hook B should be BLOCKED for non-reviewers');
      } else {
        console.log('   - Hook B should EXECUTE for reviewers');
      }
    }
  }

  console.log('='.repeat(70) + '\n');

  return allPass;
}

/* ========================================================================= */
/* Main Entry Point                                                          */
/* ========================================================================= */

// Parse command-line arguments
const args = process.argv.slice(2);
const actorArg = args.find(arg => arg.startsWith('--actor='));
const actor = actorArg ? actorArg.split('=')[1] : 'user';

// Run proof
const success = runProof(actor);

// Exit with appropriate code
process.exit(success ? 0 : 1);
