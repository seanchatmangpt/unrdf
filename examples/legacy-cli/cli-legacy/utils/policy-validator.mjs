/**
 * @file Policy Validation Engine
 * @module cli/utils/policy-validator
 *
 * @description
 * Policy pack validation engine that runs compliance checks against RDF graphs
 * with comprehensive OTEL instrumentation and violation reporting.
 */

import { createStore } from '../../../../packages/oxigraph/src/index.mjs';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { PolicyPackManager } from '../../../../src/knowledge-engine/policy-pack.mjs';
import { evaluateHook } from './hook-evaluator.mjs';

const tracer = trace.getTracer('unrdf-policy-validator');

/**
 * Validate RDF graph against policy pack
 * @param {Store} store - N3 store with RDF data
 * @param {string} policyPackName - Name of policy pack
 * @param {Object} [options] - Validation options
 * @returns {Promise<Object>} Validation report
 */
export async function validatePolicy(store, policyPackName, options = {}) {
  return await tracer.startActiveSpan('policy.validate', async span => {
    try {
      span.setAttribute('policy.pack', policyPackName);
      span.setAttribute('store.size', store.size);
      span.setAttribute('validation.strict', options.strict || false);

      const startTime = Date.now();

      // Load policy pack - either from file path or from installed packs
      const policyPackManager = new PolicyPackManager(options.basePath || process.cwd());
      let policyPack;

      if (options.policyFilePath) {
        // Load directly from file path - handle both formats
        const { readFile: readFileAsync } = await import('node:fs/promises');
        const { writeFile: writeFileAsync } = await import('node:fs/promises');
        const fileContent = await readFileAsync(options.policyFilePath, 'utf-8');
        const policyData = JSON.parse(fileContent);

        // Detect format: full policy pack vs simple policy
        if (policyData.id && policyData.meta && policyData.config) {
          // Full PolicyPack format - use as-is
          policyPack = await policyPackManager.loadPolicyPack(options.policyFilePath);
        } else if (policyData.policies || policyData.hooks) {
          // Simple format - wrap it into a policy pack
          const { randomUUID } = await import('crypto');
          const wrapped = {
            id: randomUUID(),
            meta: {
              name: policyData.name || 'applied-policy',
              version: policyData.version || '1.0.0',
              description: policyData.description || 'Applied policy',
            },
            config: {
              enabled: true,
              priority: 50,
              strictMode: false,
              timeout: 30000,
              retries: 1,
            },
            hooks: policyData.hooks || [],
            conditions: policyData.conditions || [],
            resources: policyData.resources || [],
          };

          // Write temporary manifest
          const tmpPath = `/tmp/policy-validate-${Date.now()}.json`;
          await writeFileAsync(tmpPath, JSON.stringify(wrapped));
          policyPack = await policyPackManager.loadPolicyPack(tmpPath);
        } else {
          throw new Error('Invalid policy format: must have id/meta/config or policies/hooks');
        }
      } else {
        // Load from installed policy packs
        await policyPackManager.loadAllPolicyPacks();
        policyPack = policyPackManager.getPolicyPack(policyPackName);
      }

      if (!policyPack) {
        throw new Error(`Policy pack not found: ${policyPackName}`);
      }

      span.setAttribute('policy.hooks', policyPack.hooks?.length || 0);

      // Run all policy hooks
      const hookResults = [];
      const violations = [];

      for (const hook of policyPack.hooks || []) {
        try {
          const result = await evaluateHook(hook, store, options);
          hookResults.push({
            hook: hook.meta?.name || 'unnamed',
            fired: result.fired,
            executionTime: result.executionTime,
            type: result.type,
          });

          // In validation, hooks firing = violations detected
          if (result.fired) {
            violations.push({
              hook: hook.meta?.name || 'unnamed',
              severity: hook.meta?.severity || 'warning',
              message: hook.meta?.description || 'Policy violation detected',
              details: result,
            });
          }
        } catch (error) {
          span.recordException(error);
          hookResults.push({
            hook: hook.meta?.name || 'unnamed',
            error: error.message,
          });

          if (options.strict) {
            throw new Error(`Hook evaluation failed: ${error.message}`);
          }
        }
      }

      const executionTime = Date.now() - startTime;
      const passed = violations.length === 0;

      span.setAttribute('validation.passed', passed);
      span.setAttribute('validation.violations', violations.length);
      span.setAttribute('validation.executionTime', executionTime);
      span.setStatus({ code: SpanStatusCode.OK });

      const result = {
        passed,
        policyPack: policyPackName,
        executionTime,
        hooksEvaluated: hookResults.length,
        violations,
        hookResults,
        timestamp: new Date().toISOString(),
      };

      // Log to audit trail
      await logAuditEntry(result).catch(err => {
        // Don't fail validation if audit logging fails
        console.warn(`Warning: Failed to log audit entry: ${err.message}`);
      });

      return result;
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      throw error;
    } finally {
      span.end();
    }
  });
}

/**
 * Generate validation report in various formats
 * @param {Object} validationResult - Validation result
 * @param {string} [format='text'] - Output format (text, json, markdown)
 * @returns {string} Formatted report
 */
export function formatValidationReport(validationResult, format = 'text') {
  if (format === 'json') {
    return JSON.stringify(validationResult, null, 2);
  }

  if (format === 'markdown') {
    return formatMarkdownReport(validationResult);
  }

  // Default text format
  return formatTextReport(validationResult);
}

/**
 * Format validation report as text
 * @param {Object} result - Validation result
 * @returns {string} Text report
 */
function formatTextReport(result) {
  const lines = [];

  lines.push(`\n${'='.repeat(60)}`);
  lines.push(`  POLICY VALIDATION REPORT`);
  lines.push(`${'='.repeat(60)}\n`);

  lines.push(`Policy Pack: ${result.policyPack}`);
  lines.push(`Status: ${result.passed ? '✅ PASSED' : '❌ FAILED'}`);
  lines.push(`Hooks Evaluated: ${result.hooksEvaluated}`);
  lines.push(`Violations: ${result.violations.length}`);
  lines.push(`Execution Time: ${result.executionTime}ms`);
  lines.push(`Timestamp: ${result.timestamp}\n`);

  if (result.violations.length > 0) {
    lines.push(`${'─'.repeat(60)}`);
    lines.push(`VIOLATIONS:\n`);

    for (const violation of result.violations) {
      const icon = violation.severity === 'error' ? '❌' : '⚠️';
      lines.push(`${icon} ${violation.hook}`);
      lines.push(`   Severity: ${violation.severity.toUpperCase()}`);
      lines.push(`   Message: ${violation.message}`);
      if (violation.details?.violations?.length > 0) {
        lines.push(`   Details: ${violation.details.violations.length} SHACL violations`);
      }
      lines.push('');
    }
  }

  lines.push(`${'─'.repeat(60)}`);
  lines.push(`HOOK RESULTS:\n`);

  for (const hookResult of result.hookResults) {
    if (hookResult.error) {
      lines.push(`❌ ${hookResult.hook}: ERROR - ${hookResult.error}`);
    } else {
      const status = hookResult.fired ? '🔥 FIRED' : '✓ PASSED';
      lines.push(`${status} ${hookResult.hook} (${hookResult.executionTime}ms)`);
    }
  }

  lines.push(`\n${'='.repeat(60)}\n`);

  return lines.join('\n');
}

/**
 * Format validation report as markdown
 * @param {Object} result - Validation result
 * @returns {string} Markdown report
 */
function formatMarkdownReport(result) {
  const lines = [];

  lines.push(`# Policy Validation Report\n`);
  lines.push(`**Policy Pack:** ${result.policyPack}  `);
  lines.push(`**Status:** ${result.passed ? '✅ PASSED' : '❌ FAILED'}  `);
  lines.push(`**Hooks Evaluated:** ${result.hooksEvaluated}  `);
  lines.push(`**Violations:** ${result.violations.length}  `);
  lines.push(`**Execution Time:** ${result.executionTime}ms  `);
  lines.push(`**Timestamp:** ${result.timestamp}\n`);

  if (result.violations.length > 0) {
    lines.push(`## Violations\n`);

    for (const violation of result.violations) {
      const icon = violation.severity === 'error' ? '❌' : '⚠️';
      lines.push(`### ${icon} ${violation.hook}\n`);
      lines.push(`- **Severity:** ${violation.severity.toUpperCase()}`);
      lines.push(`- **Message:** ${violation.message}`);

      if (violation.details?.violations?.length > 0) {
        lines.push(`- **SHACL Violations:** ${violation.details.violations.length}`);
      }
      lines.push('');
    }
  }

  lines.push(`## Hook Results\n`);
  lines.push('| Hook | Status | Time |');
  lines.push('|------|--------|------|');

  for (const hookResult of result.hookResults) {
    if (hookResult.error) {
      lines.push(`| ${hookResult.hook} | ❌ ERROR | - |`);
    } else {
      const status = hookResult.fired ? '🔥 FIRED' : '✓ PASSED';
      lines.push(`| ${hookResult.hook} | ${status} | ${hookResult.executionTime}ms |`);
    }
  }

  return lines.join('\n');
}

/**
 * Log validation result to audit trail
 * @param {Object} validationResult - Validation result to log
 * @returns {Promise<void>}
 */
async function logAuditEntry(validationResult) {
  const { appendFile, mkdir } = await import('node:fs/promises');
  const { join } = await import('node:path');

  const auditDir = join(process.cwd(), '.unrdf');
  const auditLogPath = join(auditDir, 'policy-audit.log');

  // Ensure .unrdf directory exists
  try {
    await mkdir(auditDir, { recursive: true });
  } catch (err) {
    if (err.code !== 'EEXIST') throw err;
  }

  // Create simplified audit entry
  const auditEntry = {
    timestamp: validationResult.timestamp,
    policyPack: validationResult.policyPack,
    passed: validationResult.passed,
    hooksEvaluated: validationResult.hooksEvaluated,
    violations: validationResult.violations.map(v => ({
      hook: v.hook,
      severity: v.severity,
      message: v.message,
    })),
    executionTime: validationResult.executionTime,
  };

  // Append as JSONL (one JSON object per line)
  await appendFile(auditLogPath, JSON.stringify(auditEntry) + '\n', 'utf-8');
}

export default {
  validatePolicy,
  formatValidationReport,
};
