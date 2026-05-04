/**
 * Poka-Yoke Guards - Deny-by-construction enforcement
 *
 * Σ_guard (poka-yoke / deny-by-construction):
 *   H ≔ { secret, out-of-root, non-allowlisted-net, privileged-escalation, model-internals }
 *   μ ⊣ H  // partial: unlawful ⇒ receipt-only
 *
 * Silence rule:
 *   unlawful(o) ⇒ emit(Receipt(o)) ∧ ¬emit(payload(o))
 *
 * @module @unrdf/kgc-claude/poka-yoke-guards
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now } from '@unrdf/kgc-4d';

/**
 * Forbidden operation types (H set)
 */
export const ForbiddenOps = {
  SECRET: 'secret',
  OUT_OF_ROOT: 'out-of-root',
  NON_ALLOWLISTED_NET: 'non-allowlisted-net',
  PRIVILEGED_ESCALATION: 'privileged-escalation',
  MODEL_INTERNALS: 'model-internals',
};

/**
 * Guard configuration schema
 */
export const GuardConfigSchema = z.object({
  /** Allowed root directories */
  root_allow: z.array(z.string()).default([]),
  /** Network allowlist (hostnames or patterns) */
  net_allow: z.array(z.string()).default([]),
  /** Secret patterns to detect (regex) */
  secret_patterns: z.array(z.string()).default([
    'password',
    'api_key',
    'secret',
    'token',
    'credential',
    'private_key',
  ]),
  /** Model internal patterns to deny */
  model_internal_patterns: z.array(z.string()).default([
    'system_prompt',
    'training_data',
    'weights',
    'model_config',
  ]),
});

/**
 * @typedef {z.infer<typeof GuardConfigSchema>} GuardConfig
 */

/**
 * Guard violation receipt schema
 */
export const ViolationReceiptSchema = z.object({
  id: z.string(),
  t_ns: z.bigint(),
  violation_type: z.enum([
    ForbiddenOps.SECRET,
    ForbiddenOps.OUT_OF_ROOT,
    ForbiddenOps.NON_ALLOWLISTED_NET,
    ForbiddenOps.PRIVILEGED_ESCALATION,
    ForbiddenOps.MODEL_INTERNALS,
  ]),
  operation: z.string(),
  target: z.string(),
  agent_id: z.string().optional(),
  hash: z.string(),
  /** Payload is NOT emitted - only receipt */
  payload_suppressed: z.literal(true),
});

/**
 * @typedef {z.infer<typeof ViolationReceiptSchema>} ViolationReceipt
 */

/**
 * Check result schema
 */
export const CheckResultSchema = z.object({
  allowed: z.boolean(),
  violation: z.enum([
    ForbiddenOps.SECRET,
    ForbiddenOps.OUT_OF_ROOT,
    ForbiddenOps.NON_ALLOWLISTED_NET,
    ForbiddenOps.PRIVILEGED_ESCALATION,
    ForbiddenOps.MODEL_INTERNALS,
  ]).optional(),
  receipt: ViolationReceiptSchema.optional(),
});

/**
 * @typedef {z.infer<typeof CheckResultSchema>} CheckResult
 */

/**
 * PokaYokeGuard - Enforces deny-by-construction rules
 */
export class PokaYokeGuard {
  /**
   * @param {GuardConfig} config
   */
  constructor(config = {}) {
    this.config = GuardConfigSchema.parse(config);
    this.violations = [];
  }

  /**
   * Check if path is within allowed roots
   * @param {string} path
   * @returns {boolean}
   */
  isWithinRoot(path) {
    if (this.config.root_allow.length === 0) return true;

    const normalizedPath = path.replace(/\\/g, '/');
    return this.config.root_allow.some(root => {
      const normalizedRoot = root.replace(/\\/g, '/');
      return normalizedPath.startsWith(normalizedRoot);
    });
  }

  /**
   * Check if network target is allowlisted
   * @param {string} target
   * @returns {boolean}
   */
  isNetworkAllowed(target) {
    if (this.config.net_allow.length === 0) return false;

    return this.config.net_allow.some(allowed => {
      if (allowed.includes('*')) {
        const regex = new RegExp('^' + allowed.replace(/\*/g, '.*') + '$');
        return regex.test(target);
      }
      return target === allowed || target.endsWith('.' + allowed);
    });
  }

  /**
   * Check if content contains secrets
   * @param {string} content
   * @returns {boolean}
   */
  containsSecret(content) {
    const lower = content.toLowerCase();
    return this.config.secret_patterns.some(pattern =>
      lower.includes(pattern.toLowerCase())
    );
  }

  /**
   * Check if content accesses model internals
   * @param {string} content
   * @returns {boolean}
   */
  accessesModelInternals(content) {
    const lower = content.toLowerCase();
    return this.config.model_internal_patterns.some(pattern =>
      lower.includes(pattern.toLowerCase())
    );
  }

  /**
   * Check if operation is privileged escalation
   * @param {string} operation
   * @returns {boolean}
   */
  isPrivilegedEscalation(operation) {
    const privilegedOps = [
      'sudo',
      'admin',
      'root',
      'chmod 777',
      'setuid',
      'exec',
      'eval',
      'spawn',
    ];
    const lower = operation.toLowerCase();
    return privilegedOps.some(op => lower.includes(op));
  }

  /**
   * Generate violation receipt
   * Silence rule: unlawful(o) ⇒ emit(Receipt(o)) ∧ ¬emit(payload(o))
   * @param {string} violationType
   * @param {string} operation
   * @param {string} target
   * @param {string} [agentId]
   * @returns {Promise<ViolationReceipt>}
   */
  async generateViolationReceipt(violationType, operation, target, agentId) {
    const t_ns = now();
    const id = `violation-${violationType}-${t_ns}`;

    const receiptData = JSON.stringify({
      id,
      t_ns: t_ns.toString(),
      violation_type: violationType,
      operation,
      target,
      agent_id: agentId,
    });

    const hash = await blake3(receiptData);

    const receipt = ViolationReceiptSchema.parse({
      id,
      t_ns,
      violation_type: violationType,
      operation,
      target,
      agent_id: agentId,
      hash,
      payload_suppressed: true,
    });

    this.violations.push(receipt);
    return receipt;
  }

  /**
   * Check file operation
   * @param {string} path
   * @param {string} operation
   * @param {string} [agentId]
   * @returns {Promise<CheckResult>}
   */
  async checkFileOp(path, operation, agentId) {
    // Check out-of-root
    if (!this.isWithinRoot(path)) {
      const receipt = await this.generateViolationReceipt(
        ForbiddenOps.OUT_OF_ROOT,
        operation,
        path,
        agentId
      );
      return { allowed: false, violation: ForbiddenOps.OUT_OF_ROOT, receipt };
    }

    return { allowed: true };
  }

  /**
   * Check network operation
   * @param {string} target
   * @param {string} operation
   * @param {string} [agentId]
   * @returns {Promise<CheckResult>}
   */
  async checkNetworkOp(target, operation, agentId) {
    if (!this.isNetworkAllowed(target)) {
      const receipt = await this.generateViolationReceipt(
        ForbiddenOps.NON_ALLOWLISTED_NET,
        operation,
        target,
        agentId
      );
      return { allowed: false, violation: ForbiddenOps.NON_ALLOWLISTED_NET, receipt };
    }

    return { allowed: true };
  }

  /**
   * Check content for secrets
   * @param {string} content
   * @param {string} operation
   * @param {string} [agentId]
   * @returns {Promise<CheckResult>}
   */
  async checkContent(content, operation, agentId) {
    // Check secrets
    if (this.containsSecret(content)) {
      const receipt = await this.generateViolationReceipt(
        ForbiddenOps.SECRET,
        operation,
        '[content]',
        agentId
      );
      return { allowed: false, violation: ForbiddenOps.SECRET, receipt };
    }

    // Check model internals
    if (this.accessesModelInternals(content)) {
      const receipt = await this.generateViolationReceipt(
        ForbiddenOps.MODEL_INTERNALS,
        operation,
        '[content]',
        agentId
      );
      return { allowed: false, violation: ForbiddenOps.MODEL_INTERNALS, receipt };
    }

    return { allowed: true };
  }

  /**
   * Check operation for privilege escalation
   * @param {string} operation
   * @param {string} [agentId]
   * @returns {Promise<CheckResult>}
   */
  async checkOperation(operation, agentId) {
    if (this.isPrivilegedEscalation(operation)) {
      const receipt = await this.generateViolationReceipt(
        ForbiddenOps.PRIVILEGED_ESCALATION,
        operation,
        '[operation]',
        agentId
      );
      return { allowed: false, violation: ForbiddenOps.PRIVILEGED_ESCALATION, receipt };
    }

    return { allowed: true };
  }

  /**
   * Comprehensive check for an operation
   * μ ⊣ H (partial function - returns receipt-only for unlawful)
   * @param {Object} op
   * @param {string} op.type - Operation type
   * @param {string} [op.path] - File path
   * @param {string} [op.network] - Network target
   * @param {string} [op.content] - Content to check
   * @param {string} [op.agentId] - Agent ID
   * @returns {Promise<CheckResult>}
   */
  async check(op) {
    // Check operation type
    let result = await this.checkOperation(op.type, op.agentId);
    if (!result.allowed) return result;

    // Check file path if present
    if (op.path) {
      result = await this.checkFileOp(op.path, op.type, op.agentId);
      if (!result.allowed) return result;
    }

    // Check network if present
    if (op.network) {
      result = await this.checkNetworkOp(op.network, op.type, op.agentId);
      if (!result.allowed) return result;
    }

    // Check content if present
    if (op.content) {
      result = await this.checkContent(op.content, op.type, op.agentId);
      if (!result.allowed) return result;
    }

    return { allowed: true };
  }

  /**
   * Get all violations
   * @returns {ViolationReceipt[]}
   */
  getViolations() {
    return [...this.violations];
  }

  /**
   * Clear violations
   */
  clearViolations() {
    this.violations = [];
  }
}

/**
 * Create guard with common defaults
 * @param {string[]} rootAllow
 * @param {string[]} netAllow
 * @returns {PokaYokeGuard}
 */
export function createGuard(rootAllow = [], netAllow = []) {
  return new PokaYokeGuard({
    root_allow: rootAllow,
    net_allow: netAllow,
  });
}

/**
 * Guard middleware for wrapping operations
 * @param {PokaYokeGuard} guard
 * @returns {Function}
 */
export function withGuard(guard) {
  /**
   * @param {Function} fn - Function to wrap
   * @param {Object} op - Operation descriptor
   * @returns {Function}
   */
  return function guardedOp(fn, op) {
    return async function(...args) {
      const result = await guard.check(op);

      if (!result.allowed) {
        // Silence rule: emit receipt only, not payload
        return {
          success: false,
          violation: result.violation,
          receipt: result.receipt,
          payload: null, // suppressed
        };
      }

      return fn(...args);
    };
  };
}

export default PokaYokeGuard;
