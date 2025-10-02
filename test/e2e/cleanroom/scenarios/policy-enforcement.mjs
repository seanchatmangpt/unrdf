/**
 * @file P0 Scenario: Policy Enforcement
 * @module scenarios/policy-enforcement
 *
 * @description
 * Policy Pack workflow: Apply → Validate → Test → Audit
 * Represents 20% of real-world usage.
 */

import { ScenarioBuilder } from '../scenario-framework.mjs';
import assert from 'assert';

/**
 * Policy Enforcement scenario - P0 (Core Workflow)
 */
export const policyEnforcementScenario = new ScenarioBuilder()
  .name('Policy Enforcement')
  .description('Apply policy pack and validate compliance')
  .priority('P0')
  .tag('core')
  .tag('policy')
  .tag('compliance')

  // Step 1: Apply policy pack
  .step({
    name: 'Apply policy pack',
    command: 'node cli/unrdf.mjs policy apply test/e2e/cleanroom/fixtures/compliance-pack.json',
    expectedExitCode: 0,
    expectedOutput: /✅\s+Policy pack applied:/,
  })

  // Step 2: Validate compliance
  .step({
    name: 'Validate compliance',
    command: 'node cli/unrdf.mjs policy validate --strict',
    expectedExitCode: 0,
    expectedOutput: /Status:.*✅\s+PASSED|Validation\s+PASSED/,
  })

  // Step 3: Test policy (dry run)
  .step({
    name: 'Test policy dry run',
    command: 'node cli/unrdf.mjs policy test test/e2e/cleanroom/fixtures/compliance-pack.json --dry-run',
    expectedExitCode: 0,
  })

  // Expected OTEL spans
  .expectSpan('cli.policy.apply')
  .expectSpan('sidecar.policy.activate')
  .expectSpan('cli.policy.validate')
  .expectSpan('sidecar.hook.evaluateAll')
  .expectSpan('cli.policy.test')

  .assert(async (ctx) => {
    const traces = await ctx.getTraces();
    assert(traces.length > 0, 'Should have policy traces');
  })

  .build();

/**
 * Policy Violation Detection scenario - P0 (Error Handling)
 */
export const policyViolationScenario = new ScenarioBuilder()
  .name('Policy Violation Detection')
  .description('Detect and handle policy violations')
  .priority('P0')
  .tag('core')
  .tag('policy')
  .tag('violation')

  // Step 1: Apply strict policy
  .step({
    name: 'Apply strict policy pack',
    command: 'node cli/unrdf.mjs policy apply test/e2e/cleanroom/fixtures/strict-policy.json',
    expectedExitCode: 0,
  })

  // Step 2: Attempt non-compliant operation
  .step({
    name: 'Attempt non-compliant operation',
    command: 'node cli/unrdf.mjs store import test/e2e/cleanroom/fixtures/non-compliant-data.ttl',
    expectedExitCode: 1, // Should fail
    expectedOutput: /Policy violation|Not compliant/,
  })

  // Step 3: View violation audit log
  .step({
    name: 'View violation audit log',
    command: 'node cli/unrdf.mjs policy audit --violations-only',
    expectedExitCode: 0,
    expectedOutput: /\d+ violations?/,
  })

  .expectSpan('cli.policy.apply')
  .expectSpan('cli.store.import')
  .expectSpan('sidecar.policy.validate')
  .expectSpan('sidecar.policy.violation')

  .assert(async (ctx) => {
    const stepResults = ctx.results;
    const violationStep = stepResults.find(r =>
      r.step === 'Attempt non-compliant operation'
    );

    assert(violationStep?.result.exitCode === 1,
      'Non-compliant operation should be rejected');
  })

  .build();

/**
 * Multi-Policy Stack scenario - P1 (Advanced)
 */
export const multiPolicyStackScenario = new ScenarioBuilder()
  .name('Multi-Policy Stack')
  .description('Apply and manage multiple policy packs simultaneously')
  .priority('P1')
  .tag('advanced')
  .tag('policy')
  .tag('multi-policy')

  // Apply multiple policy packs
  .step({
    name: 'Apply base policy',
    command: 'node cli/unrdf.mjs policy apply test/e2e/cleanroom/fixtures/base-policy.json',
    expectedExitCode: 0,
  })

  .step({
    name: 'Apply compliance policy',
    command: 'node cli/unrdf.mjs policy apply test/e2e/cleanroom/fixtures/compliance-pack.json',
    expectedExitCode: 0,
  })

  .step({
    name: 'Apply security policy',
    command: 'node cli/unrdf.mjs policy apply test/e2e/cleanroom/fixtures/security-policy.json',
    expectedExitCode: 0,
  })

  // List active policies
  .step({
    name: 'List active policies',
    command: 'node cli/unrdf.mjs policy list --active',
    expectedExitCode: 0,
    expectedOutput: /3 active policies/,
  })

  // Validate against all policies
  .step({
    name: 'Validate against all policies',
    command: 'node cli/unrdf.mjs policy validate --all',
    expectedExitCode: 0,
  })

  .expectSpan('sidecar.policy.activate')
  .expectSpan('sidecar.policy.stack.evaluate')

  .assert(async (ctx) => {
    const spans = await ctx.getSpans();
    const policySpans = spans.filter(s =>
      (s.operationName || s.name || '').includes('policy')
    );

    assert(policySpans.length >= 3,
      'Should have spans for all policy operations');
  })

  .build();

/**
 * Policy Performance Validation scenario - P1 (Performance)
 */
export const policyPerformanceScenario = new ScenarioBuilder()
  .name('Policy Performance Validation')
  .description('Validate policy evaluation performance under load')
  .priority('P1')
  .tag('performance')
  .tag('policy')
  .tag('benchmark')

  // Apply policy
  .step({
    name: 'Apply policy for benchmarking',
    command: 'node cli/unrdf.mjs policy apply test/e2e/cleanroom/fixtures/compliance-pack.json',
    expectedExitCode: 0,
  })

  // Perform 5 validations to measure performance
  .step({
    name: 'Validation 1',
    command: 'node cli/unrdf.mjs policy validate',
    expectedExitCode: 0,
  })
  .step({
    name: 'Validation 2',
    command: 'node cli/unrdf.mjs policy validate',
    expectedExitCode: 0,
  })
  .step({
    name: 'Validation 3',
    command: 'node cli/unrdf.mjs policy validate',
    expectedExitCode: 0,
  })
  .step({
    name: 'Validation 4',
    command: 'node cli/unrdf.mjs policy validate',
    expectedExitCode: 0,
  })
  .step({
    name: 'Validation 5',
    command: 'node cli/unrdf.mjs policy validate',
    expectedExitCode: 0,
  })

  .expectSpan('sidecar.policy.validate')

  .assert(async (ctx) => {
    const spans = await ctx.getSpans();
    const validateSpans = spans.filter(s =>
      s.operationName === 'sidecar.policy.validate' ||
      s.name === 'sidecar.policy.validate'
    );

    // Calculate average validation time
    const durations = validateSpans.map(s => s.duration || 0);
    const avg = durations.reduce((a, b) => a + b, 0) / durations.length;

    console.log(`    📊 Policy validation avg: ${avg}µs`);
    assert(avg < 5000, `Policy validation avg ${avg}µs should be < 5000µs`);
  })

  .build();

/**
 * Policy Audit Trail scenario - P1 (Compliance)
 */
export const policyAuditScenario = new ScenarioBuilder()
  .name('Policy Audit Trail')
  .description('Comprehensive policy audit trail and reporting')
  .priority('P1')
  .tag('compliance')
  .tag('policy')
  .tag('audit')

  // Apply policy
  .step({
    name: 'Apply audited policy',
    command: 'node cli/unrdf.mjs policy apply test/e2e/cleanroom/fixtures/compliance-pack.json --audit',
    expectedExitCode: 0,
  })

  // Perform operations that generate audit events
  .step({
    name: 'Operation 1 - compliant',
    command: 'node cli/unrdf.mjs store import test/e2e/cleanroom/fixtures/test-data.ttl',
    expectedExitCode: 0,
  })

  .step({
    name: 'Operation 2 - attempt non-compliant',
    command: 'node cli/unrdf.mjs store import test/e2e/cleanroom/fixtures/non-compliant-data.ttl',
    expectedExitCode: 1,
  })

  // Generate audit report
  .step({
    name: 'Generate audit report',
    command: 'node cli/unrdf.mjs policy audit --format=json --output=/tmp/audit-report.json',
    expectedExitCode: 0,
  })

  .expectSpan('sidecar.policy.audit.record')

  .assert(async (ctx) => {
    // Verify audit report was generated
    const fs = await import('fs/promises');
    try {
      await fs.access('/tmp/audit-report.json');
      console.log('    ✓ Audit report generated');
    } catch {
      throw new Error('Audit report not generated');
    }
  })

  .build();

export default {
  policyEnforcementScenario,
  policyViolationScenario,
  multiPolicyStackScenario,
  policyPerformanceScenario,
  policyAuditScenario,
};
