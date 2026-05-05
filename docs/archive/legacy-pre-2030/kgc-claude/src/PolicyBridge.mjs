/**
 * PolicyBridge - Connect KGC-Claude WorkItems with unrdf Policy Packs
 *
 * Enables policy-driven admission control for WorkItems by bridging:
 * - @unrdf/hooks policy system → KGC-Claude WorkItem lifecycle
 *
 * Guards:
 * - No circular dependencies (one-way import)
 * - Policy timeout ≤ 5s (enforce sandboxing)
 * - Deny-by-default (explicit allow only)
 *
 * @module @unrdf/kgc-claude/PolicyBridge
 */

import { z } from 'zod';
import { readFileSync, existsSync } from 'fs';
import { join, dirname } from 'path';
import { PolicyPack, PolicyPackManager } from '@unrdf/hooks/src/hooks/policy-pack.mjs';
import { WorkItemSchema, DenialReceiptSchema } from './async-workflow.mjs';
import { WORK_ITEM_STATUS, DENIAL_REASONS } from './constants.mjs';
import { now, toISO } from '@unrdf/kgc-4d';
import { blake3 } from 'hash-wasm';

/**
 * Default policy evaluation timeout (5 seconds in nanoseconds)
 */
const DEFAULT_POLICY_TIMEOUT_NS = 5_000_000_000n;

/**
 * Lifecycle mapping: Hook triggers → WorkItem status
 */
export const HOOK_TO_WORKITEM_LIFECYCLE = {
  'before-add': 'queued',
  'after-add': 'assigned',
  'before-query': 'executing',
  'after-query': 'executing',
  'before-remove': 'executing',
  'after-remove': 'completed',
  'before-commit': 'executing',
  'after-commit': 'completed',
  'before-rollback': 'executing',
  'after-rollback': 'failed',
  'on-error': 'failed',
  'on-validation-fail': 'failed',
  'on-timeout': 'failed',
};

/**
 * Reverse mapping: WorkItem status → Hook triggers
 */
export const WORKITEM_TO_HOOK_TRIGGERS = {
  queued: ['before-add'],
  assigned: ['after-add'],
  executing: ['before-query', 'after-query', 'before-commit', 'before-remove'],
  completed: ['after-commit', 'after-remove'],
  failed: ['on-error', 'on-validation-fail', 'on-timeout', 'after-rollback'],
  cancelled: ['on-error'],
};

/**
 * Generate UUID v4
 * @returns {string}
 */
function generateUUID() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * JSON stringify with BigInt support
 * @param {any} obj
 * @returns {string}
 */
function stringifyWithBigInt(obj) {
  return JSON.stringify(obj, (key, value) => {
    if (typeof value === 'bigint') {
      return value.toString();
    }
    return value;
  });
}

/**
 * Load a policy pack from a manifest path
 *
 * @param {string} manifestPath - Absolute path to manifest.json
 * @param {string} [basePath] - Base path for policy pack resolution
 * @returns {Promise<PolicyPack>}
 * @throws {Error} If manifest not found or invalid
 *
 * @example
 * const pack = await loadPolicyPack('/policy-packs/default/manifest.json');
 */
export async function loadPolicyPack(manifestPath, basePath) {
  if (!existsSync(manifestPath)) {
    throw new Error(`Policy pack manifest not found: ${manifestPath}`);
  }

  // Determine base path from manifest location if not provided
  const resolvedBasePath = basePath || dirname(dirname(manifestPath));

  const manager = new PolicyPackManager(resolvedBasePath);
  const pack = await manager.loadPolicyPack(manifestPath);

  return pack;
}

/**
 * Evaluate hook condition against a WorkItem
 *
 * This function checks if a hook's condition would allow the WorkItem to proceed.
 * Implements deny-by-default: returns false unless hook explicitly allows.
 *
 * @param {Object} workItem - WorkItem to evaluate
 * @param {Object} hook - Hook definition from policy pack
 * @param {Object} [options]
 * @param {bigint} [options.timeout] - Timeout in nanoseconds (default: 5s)
 * @returns {Promise<{ passed: boolean, reason: string }>}
 *
 * @example
 * const result = await evaluateHookCondition(workItem, hook);
 * if (!result.passed) {
 *   console.log('Denied:', result.reason);
 * }
 */
export async function evaluateHookCondition(workItem, hook, options = {}) {
  const timeout = options.timeout || DEFAULT_POLICY_TIMEOUT_NS;
  const startTime = now();

  try {
    // Validate WorkItem schema
    const validatedItem = WorkItemSchema.parse(workItem);

    // Check if hook trigger matches WorkItem lifecycle state
    const expectedStatus = HOOK_TO_WORKITEM_LIFECYCLE[hook.trigger || hook.meta?.trigger];
    if (expectedStatus && validatedItem.status !== expectedStatus) {
      return {
        passed: false,
        reason: `Hook trigger '${hook.trigger}' does not match WorkItem status '${validatedItem.status}' (expected '${expectedStatus}')`,
      };
    }

    // Create execution context for hook
    const context = {
      workItem: validatedItem,
      graph: null, // Would be populated from KGC store if needed
      metadata: {
        evaluatedAt: toISO(startTime),
        hookName: hook.meta?.name || hook.name,
      },
    };

    // Execute hook's run function with timeout guard
    const timeoutMs = Number(timeout / 1_000_000n); // Convert ns to ms
    const timeoutPromise = new Promise((_, reject) =>
      setTimeout(() => reject(new Error('Policy evaluation timeout')), timeoutMs),
    );

    const evaluationPromise = hook.run ? hook.run(context) : Promise.resolve({ success: true });

    const result = await Promise.race([evaluationPromise, timeoutPromise]);

    // Check evaluation time
    const elapsedTime = now() - startTime;
    if (elapsedTime > timeout) {
      return {
        passed: false,
        reason: `Policy evaluation exceeded timeout (${elapsedTime}ns > ${timeout}ns)`,
      };
    }

    // Interpret result (deny-by-default)
    if (result && result.success === true) {
      return {
        passed: true,
        reason: 'Policy evaluation passed',
      };
    } else {
      return {
        passed: false,
        reason: result?.message || result?.result?.message || 'Policy evaluation failed',
      };
    }
  } catch (error) {
    return {
      passed: false,
      reason: `Policy evaluation error: ${error.message}`,
    };
  }
}

/**
 * Create a denial receipt for a WorkItem
 *
 * @param {Object} workItem - WorkItem that was denied
 * @param {string} reason - Denial reason
 * @param {Object} [context] - Additional context
 * @returns {Promise<Object>} DenialReceipt
 */
async function createPolicyDenialReceipt(workItem, reason, context = {}) {
  const id = generateUUID();
  const t_ns = now();

  const receiptContent = {
    id,
    t_ns: t_ns.toString(),
    reason: 'invariant_violated', // Policy violations are invariant violations
    requested: {
      workItemId: workItem.id,
      workItemType: workItem.type,
      workItemStatus: workItem.status,
    },
    budget: context.budget || {},
    usage: context.usage || {},
    policyContext: {
      policyPack: context.policyPackName,
      hookName: context.hookName,
      failureReason: reason,
    },
  };

  const receiptHash = await blake3(stringifyWithBigInt(receiptContent));

  return DenialReceiptSchema.parse({
    id,
    t_ns,
    timestamp_iso: toISO(t_ns),
    reason: 'invariant_violated',
    requested: receiptContent.requested,
    budget: receiptContent.budget,
    usage: receiptContent.usage,
    receiptHash,
  });
}

/**
 * Apply policy pack to a WorkItem
 *
 * Evaluates all relevant hooks from the policy pack against the WorkItem.
 * Returns either:
 * - The WorkItem (if all policies pass) → admitted
 * - A DenialReceipt (if any policy fails) → denied
 *
 * Implements deny-by-default security model.
 *
 * @param {Object} workItem - WorkItem to evaluate
 * @param {PolicyPack} policyPack - Policy pack to apply
 * @param {Object} [options]
 * @param {bigint} [options.timeout] - Per-hook timeout (default: 5s)
 * @returns {Promise<Object>} WorkItem (admitted) or DenialReceipt (denied)
 *
 * @example
 * const result = await applyPolicy(workItem, policyPack);
 * if (result.receiptHash) {
 *   // Denied
 *   console.log('WorkItem denied:', result.reason);
 * } else {
 *   // Admitted
 *   processWorkItem(result);
 * }
 */
export async function applyPolicy(workItem, policyPack, options = {}) {
  try {
    // Validate inputs
    const validatedItem = WorkItemSchema.parse(workItem);

    if (!policyPack.loaded) {
      throw new Error('Policy pack not loaded. Call load() first.');
    }

    // Check if policy pack is enabled
    if (!policyPack.manifest.config.enabled) {
      // If policy pack is disabled, deny by default (safer than allowing)
      return await createPolicyDenialReceipt(
        validatedItem,
        'Policy pack is disabled',
        {
          policyPackName: policyPack.manifest.meta.name,
        },
      );
    }

    // Get hooks that apply to this WorkItem's status
    const applicableTriggers = WORKITEM_TO_HOOK_TRIGGERS[validatedItem.status] || [];
    const hooks = policyPack.getHooks();

    // Filter hooks by applicable triggers and sort by priority
    const applicableHooks = hooks
      .filter((hook) => {
        const trigger = hook.trigger || hook.meta?.trigger;
        return applicableTriggers.includes(trigger);
      })
      .sort((a, b) => (b.priority || 50) - (a.priority || 50));

    // If no applicable hooks, deny by default (explicit allow required)
    if (applicableHooks.length === 0) {
      return await createPolicyDenialReceipt(
        validatedItem,
        `No applicable hooks found for status '${validatedItem.status}' (deny-by-default)`,
        {
          policyPackName: policyPack.manifest.meta.name,
        },
      );
    }

    // Evaluate hooks in priority order (first failure → immediate denial)
    for (const hook of applicableHooks) {
      const evaluation = await evaluateHookCondition(validatedItem, hook, options);

      if (!evaluation.passed) {
        // First failure → immediate denial
        return await createPolicyDenialReceipt(
          validatedItem,
          evaluation.reason,
          {
            policyPackName: policyPack.manifest.meta.name,
            hookName: hook.meta?.name || hook.name,
          },
        );
      }
    }

    // All hooks passed → admit WorkItem
    return validatedItem;
  } catch (error) {
    // Any error → denial
    return await createPolicyDenialReceipt(
      workItem,
      `Policy application error: ${error.message}`,
      {},
    );
  }
}

/**
 * Check if a result is a DenialReceipt
 *
 * @param {Object} result - Result from applyPolicy
 * @returns {boolean}
 */
export function isDenialReceipt(result) {
  return result && typeof result.receiptHash === 'string' && typeof result.reason === 'string';
}

/**
 * Create a PolicyBridge instance with cached policy packs
 *
 * @param {Object} [options]
 * @param {string} [options.basePath] - Base path for policy pack resolution
 * @returns {PolicyBridge}
 *
 * @example
 * const bridge = createPolicyBridge({ basePath: '/home/user/unrdf' });
 * const pack = await bridge.loadPack('/policy-packs/default/manifest.json');
 * const result = await bridge.evaluate(workItem, pack);
 */
export function createPolicyBridge(options = {}) {
  const basePath = options.basePath || process.cwd();
  const policyCache = new Map();

  return {
    /**
     * Load a policy pack (with caching)
     * @param {string} manifestPath
     * @returns {Promise<PolicyPack>}
     */
    async loadPack(manifestPath) {
      const normalizedPath = join(basePath, manifestPath);

      if (policyCache.has(normalizedPath)) {
        return policyCache.get(normalizedPath);
      }

      const pack = await loadPolicyPack(normalizedPath, basePath);
      policyCache.set(normalizedPath, pack);
      return pack;
    },

    /**
     * Evaluate a WorkItem against a policy pack
     * @param {Object} workItem
     * @param {PolicyPack} policyPack
     * @param {Object} [evalOptions]
     * @returns {Promise<Object>}
     */
    async evaluate(workItem, policyPack, evalOptions = {}) {
      return applyPolicy(workItem, policyPack, evalOptions);
    },

    /**
     * Clear policy pack cache
     */
    clearCache() {
      policyCache.clear();
    },

    /**
     * Get cache statistics
     * @returns {{ size: number, paths: string[] }}
     */
    getCacheStats() {
      return {
        size: policyCache.size,
        paths: Array.from(policyCache.keys()),
      };
    },
  };
}

/**
 * @typedef {ReturnType<typeof createPolicyBridge>} PolicyBridge
 */
