/**
 * @file Enhanced Bounds - Soft limits, quotas, and rate limiting
 * @module @unrdf/kgc-runtime/enhanced-bounds
 *
 * @description
 * Advanced bounds enforcement with:
 * - Soft limits with warning receipts at 80% threshold
 * - Per-agent/per-user quotas
 * - Sliding window rate limiting
 * - Integration with existing receipt system
 */

import { z } from 'zod';
import { randomUUID } from 'node:crypto';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Enhanced bounds schema with soft limits
 */
const EnhancedBoundsSchema = z.object({
  // Hard limits (100%)
  max_files_touched: z.number().int().positive().optional(),
  max_bytes_changed: z.number().int().positive().optional(),
  max_tool_ops: z.number().int().positive().optional(),
  max_runtime_ms: z.number().int().positive().optional(),
  max_graph_rewrites: z.number().int().positive().optional(),

  // Warning thresholds (0.0-1.0 as fraction of limit)
  warnings: z
    .object({
      filesThreshold: z.number().min(0).max(1).default(0.8),
      bytesThreshold: z.number().min(0).max(1).default(0.9),
      opsThreshold: z.number().min(0).max(1).default(0.75),
      runtimeThreshold: z.number().min(0).max(1).default(0.9),
      graphRewritesThreshold: z.number().min(0).max(1).default(0.8),
    })
    .optional(),
});

/**
 * Per-agent quota schema
 */
const AgentQuotaSchema = z.object({
  agentId: z.string(),
  max_files: z.number().int().positive().optional(),
  max_bytes: z.number().int().positive().optional(),
  max_ops: z.number().int().positive().optional(),
  max_runtime_ms: z.number().int().positive().optional(),
});

/**
 * Rate limit schema
 */
const RateLimitSchema = z.object({
  maxOpsPerSecond: z.number().positive(),
  windowSizeMs: z.number().int().positive().default(1000),
});

// =============================================================================
// Enhanced Bounds Checker
// =============================================================================

/**
 * Enhanced bounds checker with soft limits, quotas, and rate limiting
 *
 * @class
 *
 * @example
 * const checker = new EnhancedBoundsChecker({
 *   max_files_touched: 100,
 *   max_bytes_changed: 10485760,
 *   warnings: { filesThreshold: 0.8, bytesThreshold: 0.9 }
 * });
 *
 * // Set per-agent quota
 * checker.setAgentQuota('agent:backend-dev', {
 *   max_files: 50,
 *   max_bytes: 5242880
 * });
 *
 * // Enable rate limiting
 * checker.setRateLimit({ maxOpsPerSecond: 10 });
 *
 * // Check operation
 * const receipt = await checker.checkAndRecord({
 *   type: 'file_write',
 *   files: 1,
 *   bytes: 1024
 * }, 'agent:backend-dev');
 */
export class EnhancedBoundsChecker {
  /**
   * @param {Object} bounds - Bounds configuration
   */
  constructor(bounds = {}) {
    this.bounds = EnhancedBoundsSchema.parse(bounds);

    // Global usage tracking
    this.usage = {
      files_touched: 0,
      bytes_changed: 0,
      tool_ops: 0,
      runtime_ms: 0,
      graph_rewrites: 0,
    };

    // Per-agent usage tracking
    /** @type {Map<string, Object>} */
    this.agentUsage = new Map();

    // Per-agent quotas
    /** @type {Map<string, Object>} */
    this.agentQuotas = new Map();

    // Rate limiting state
    /** @type {Map<string, Array<number>>} */
    this.rateLimitWindows = new Map();

    /** @type {Object|null} */
    this.rateLimit = null;

    // Receipt log
    this.receipts = [];

    // Warning thresholds (defaults)
    this.warnings = this.bounds.warnings || {
      filesThreshold: 0.8,
      bytesThreshold: 0.9,
      opsThreshold: 0.75,
      runtimeThreshold: 0.9,
      graphRewritesThreshold: 0.8,
    };
  }

  /**
   * Set per-agent quota
   *
   * @param {string} agentId - Agent identifier
   * @param {Object} quota - Quota limits
   */
  setAgentQuota(agentId, quota) {
    const validatedQuota = AgentQuotaSchema.parse({ agentId, ...quota });
    this.agentQuotas.set(agentId, validatedQuota);

    // Initialize usage if not exists
    if (!this.agentUsage.has(agentId)) {
      this.agentUsage.set(agentId, {
        files_touched: 0,
        bytes_changed: 0,
        tool_ops: 0,
        runtime_ms: 0,
      });
    }
  }

  /**
   * Set rate limit
   *
   * @param {Object} rateLimit - Rate limit configuration
   */
  setRateLimit(rateLimit) {
    this.rateLimit = RateLimitSchema.parse(rateLimit);
  }

  /**
   * Check if operation exceeds soft limit (warning threshold)
   *
   * @param {string} metric - Metric name (files, bytes, ops, runtime, rewrites)
   * @param {number} current - Current usage
   * @param {number} delta - Operation delta
   * @param {number} limit - Hard limit
   * @param {number} threshold - Warning threshold (0.0-1.0)
   * @returns {Object|null} Warning receipt if threshold exceeded
   * @private
   */
  _checkSoftLimit(metric, current, delta, limit, threshold) {
    if (!limit || !threshold) {
      return null;
    }

    const newUsage = current + delta;
    const percentage = newUsage / limit;

    // Error when exceeding limit (>100%)
    if (newUsage > limit) {
      return {
        type: 'error',
        metric,
        current: newUsage,
        limit,
        percentage: Math.round(percentage * 100),
        message: `Exceeded ${metric} limit: ${newUsage}/${limit}`,
      };
    }

    // Warning at threshold (default 80%) but strictly below 100%
    if (percentage >= threshold && percentage < 1.0) {
      return {
        type: 'warning',
        metric,
        current: newUsage,
        limit,
        percentage: Math.round(percentage * 100),
        message: `Approaching ${metric} limit: ${Math.round(percentage * 100)}% used (${newUsage}/${limit})`,
      };
    }

    return null;
  }

  /**
   * Check rate limit for agent
   *
   * @param {string} agentId - Agent identifier
   * @returns {Object|null} Error if rate limit exceeded
   * @private
   */
  _checkRateLimit(agentId) {
    if (!this.rateLimit) {
      return null;
    }

    const now = Date.now();
    const windowSize = this.rateLimit.windowSizeMs;
    const maxOps = this.rateLimit.maxOpsPerSecond;

    // Get or create window for agent
    if (!this.rateLimitWindows.has(agentId)) {
      this.rateLimitWindows.set(agentId, []);
    }

    const window = this.rateLimitWindows.get(agentId);

    // Remove timestamps outside window
    const cutoff = now - windowSize;
    const activeOps = window.filter((timestamp) => timestamp > cutoff);
    this.rateLimitWindows.set(agentId, activeOps);

    // Check if rate limit exceeded
    if (activeOps.length >= maxOps) {
      return {
        type: 'error',
        metric: 'rate_limit',
        current: activeOps.length,
        limit: maxOps,
        message: `Rate limit exceeded: ${activeOps.length} ops in ${windowSize}ms (max ${maxOps})`,
      };
    }

    // Add current operation
    activeOps.push(now);
    this.rateLimitWindows.set(agentId, activeOps);

    return null;
  }

  /**
   * Check per-agent quota
   *
   * @param {string} agentId - Agent identifier
   * @param {Object} operation - Operation to check
   * @returns {Object|null} Error if quota exceeded
   * @private
   */
  _checkAgentQuota(agentId, operation) {
    const quota = this.agentQuotas.get(agentId);
    if (!quota) {
      return null; // No quota set for this agent
    }

    const usage = this.agentUsage.get(agentId) || {
      files_touched: 0,
      bytes_changed: 0,
      tool_ops: 0,
      runtime_ms: 0,
    };

    // Check files quota
    if (quota.max_files && operation.files) {
      if (usage.files_touched + operation.files > quota.max_files) {
        return {
          type: 'error',
          metric: 'agent_quota_files',
          agentId,
          current: usage.files_touched + operation.files,
          limit: quota.max_files,
          message: `Agent ${agentId} exceeded files quota: ${usage.files_touched + operation.files}/${quota.max_files}`,
        };
      }
    }

    // Check bytes quota
    if (quota.max_bytes && operation.bytes) {
      if (usage.bytes_changed + operation.bytes > quota.max_bytes) {
        return {
          type: 'error',
          metric: 'agent_quota_bytes',
          agentId,
          current: usage.bytes_changed + operation.bytes,
          limit: quota.max_bytes,
          message: `Agent ${agentId} exceeded bytes quota: ${usage.bytes_changed + operation.bytes}/${quota.max_bytes}`,
        };
      }
    }

    // Check ops quota
    if (quota.max_ops && operation.ops) {
      if (usage.tool_ops + operation.ops > quota.max_ops) {
        return {
          type: 'error',
          metric: 'agent_quota_ops',
          agentId,
          current: usage.tool_ops + operation.ops,
          limit: quota.max_ops,
          message: `Agent ${agentId} exceeded ops quota: ${usage.tool_ops + operation.ops}/${quota.max_ops}`,
        };
      }
    }

    // Check runtime quota
    if (quota.max_runtime_ms && operation.ms) {
      if (usage.runtime_ms + operation.ms > quota.max_runtime_ms) {
        return {
          type: 'error',
          metric: 'agent_quota_runtime',
          agentId,
          current: usage.runtime_ms + operation.ms,
          limit: quota.max_runtime_ms,
          message: `Agent ${agentId} exceeded runtime quota: ${usage.runtime_ms + operation.ms}/${quota.max_runtime_ms}`,
        };
      }
    }

    return null;
  }

  /**
   * Check and record operation with comprehensive validation
   *
   * @param {Object} operation - Operation to check
   * @param {string} [agentId] - Agent identifier
   * @returns {Object} Receipt with admission decision and warnings
   *
   * @example
   * const receipt = await checker.checkAndRecord({
   *   type: 'file_write',
   *   files: 1,
   *   bytes: 1024,
   *   ops: 1
   * }, 'agent:backend-dev');
   *
   * // Receipt structure:
   * // {
   * //   admit: true/false,
   * //   receipt_id: 'uuid',
   * //   timestamp: 123456789,
   * //   warnings: [...],
   * //   errors: [...]
   * // }
   */
  async checkAndRecord(operation, agentId = 'system') {
    const receipt = {
      receipt_id: randomUUID(),
      timestamp: Date.now(),
      agentId,
      operation: operation.type,
      warnings: [],
      errors: [],
      admit: true,
    };

    // Check rate limit
    const rateLimitError = this._checkRateLimit(agentId);
    if (rateLimitError) {
      receipt.errors.push(rateLimitError);
      receipt.admit = false;
    }

    // Check agent quota
    const quotaError = this._checkAgentQuota(agentId, operation);
    if (quotaError) {
      receipt.errors.push(quotaError);
      receipt.admit = false;
    }

    // Check soft limits for each metric
    if (operation.files !== undefined) {
      const check = this._checkSoftLimit(
        'files',
        this.usage.files_touched,
        operation.files,
        this.bounds.max_files_touched,
        this.warnings.filesThreshold
      );
      if (check) {
        if (check.type === 'warning') {
          receipt.warnings.push(check);
        } else {
          receipt.errors.push(check);
          receipt.admit = false;
        }
      }
    }

    if (operation.bytes !== undefined) {
      const check = this._checkSoftLimit(
        'bytes',
        this.usage.bytes_changed,
        operation.bytes,
        this.bounds.max_bytes_changed,
        this.warnings.bytesThreshold
      );
      if (check) {
        if (check.type === 'warning') {
          receipt.warnings.push(check);
        } else {
          receipt.errors.push(check);
          receipt.admit = false;
        }
      }
    }

    if (operation.ops !== undefined) {
      const check = this._checkSoftLimit(
        'ops',
        this.usage.tool_ops,
        operation.ops,
        this.bounds.max_tool_ops,
        this.warnings.opsThreshold
      );
      if (check) {
        if (check.type === 'warning') {
          receipt.warnings.push(check);
        } else {
          receipt.errors.push(check);
          receipt.admit = false;
        }
      }
    }

    if (operation.ms !== undefined) {
      const check = this._checkSoftLimit(
        'runtime',
        this.usage.runtime_ms,
        operation.ms,
        this.bounds.max_runtime_ms,
        this.warnings.runtimeThreshold
      );
      if (check) {
        if (check.type === 'warning') {
          receipt.warnings.push(check);
        } else {
          receipt.errors.push(check);
          receipt.admit = false;
        }
      }
    }

    if (operation.rewrites !== undefined) {
      const check = this._checkSoftLimit(
        'rewrites',
        this.usage.graph_rewrites,
        operation.rewrites,
        this.bounds.max_graph_rewrites,
        this.warnings.graphRewritesThreshold
      );
      if (check) {
        if (check.type === 'warning') {
          receipt.warnings.push(check);
        } else {
          receipt.errors.push(check);
          receipt.admit = false;
        }
      }
    }

    // If admitted, record usage
    if (receipt.admit) {
      this._recordUsage(operation, agentId);
    }

    // Store receipt
    this.receipts.push(receipt);

    return receipt;
  }

  /**
   * Record operation usage
   *
   * @param {Object} operation - Operation to record
   * @param {string} agentId - Agent identifier
   * @private
   */
  _recordUsage(operation, agentId) {
    // Update global usage
    if (operation.files !== undefined) {
      this.usage.files_touched += operation.files;
    }
    if (operation.bytes !== undefined) {
      this.usage.bytes_changed += operation.bytes;
    }
    if (operation.ops !== undefined) {
      this.usage.tool_ops += operation.ops;
    }
    if (operation.ms !== undefined) {
      this.usage.runtime_ms += operation.ms;
    }
    if (operation.rewrites !== undefined) {
      this.usage.graph_rewrites += operation.rewrites;
    }

    // Update agent usage
    if (!this.agentUsage.has(agentId)) {
      this.agentUsage.set(agentId, {
        files_touched: 0,
        bytes_changed: 0,
        tool_ops: 0,
        runtime_ms: 0,
      });
    }

    const agentUsageData = this.agentUsage.get(agentId);
    if (operation.files !== undefined) {
      agentUsageData.files_touched += operation.files;
    }
    if (operation.bytes !== undefined) {
      agentUsageData.bytes_changed += operation.bytes;
    }
    if (operation.ops !== undefined) {
      agentUsageData.tool_ops += operation.ops;
    }
    if (operation.ms !== undefined) {
      agentUsageData.runtime_ms += operation.ms;
    }
  }

  /**
   * Get current usage
   *
   * @returns {Object} Global usage
   */
  getUsage() {
    return { ...this.usage };
  }

  /**
   * Get agent usage
   *
   * @param {string} agentId - Agent identifier
   * @returns {Object|null} Agent usage or null
   */
  getAgentUsage(agentId) {
    const usage = this.agentUsage.get(agentId);
    return usage ? { ...usage } : null;
  }

  /**
   * Get all receipts
   *
   * @returns {Array<Object>} Receipt history
   */
  getReceipts() {
    return [...this.receipts];
  }

  /**
   * Get receipts by type
   *
   * @param {string} type - Receipt type ('warning' or 'error')
   * @returns {Array<Object>} Filtered receipts
   */
  getReceiptsByType(type) {
    return this.receipts.filter((receipt) => {
      if (type === 'warning') {
        return receipt.warnings.length > 0;
      }
      if (type === 'error') {
        return receipt.errors.length > 0;
      }
      return false;
    });
  }

  /**
   * Reset usage (for testing)
   */
  reset() {
    this.usage = {
      files_touched: 0,
      bytes_changed: 0,
      tool_ops: 0,
      runtime_ms: 0,
      graph_rewrites: 0,
    };
    this.agentUsage.clear();
    this.rateLimitWindows.clear();
    this.receipts = [];
  }
}

// =============================================================================
// Module Exports
// =============================================================================

export default {
  EnhancedBoundsChecker,
  EnhancedBoundsSchema,
  AgentQuotaSchema,
  RateLimitSchema,
};
