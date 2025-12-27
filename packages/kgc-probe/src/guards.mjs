#!/usr/bin/env node
/**
 * @fileoverview Poka-yoke guards for KGC Probe
 *
 * Guards = fail-safe enforcement of security and operational boundaries.
 * Deny operations that would violate constraints, emit receipt-only denial records.
 *
 * Design principles:
 * - Deny by default: Explicit allowlists only
 * - Fail loudly: Guards always log denials
 * - Zero trust: Validate every operation
 * - Receipt-driven: Denials are observable via receipts
 */

import { resolve, normalize, relative } from 'node:path';
import { createGuardDenial } from './observation.mjs';

/**
 * Guard configuration
 *
 * @typedef {Object} GuardConfig
 * @property {string[]} allowedRoots - Allowed filesystem root paths
 * @property {string[]} allowedHosts - Allowed network hosts (for future network guards)
 * @property {RegExp[]} secretPatterns - Patterns that indicate secrets
 * @property {boolean} enforceTimeouts - Enforce operation timeouts
 * @property {number} defaultTimeoutMs - Default timeout (ms)
 */

/**
 * Default secret patterns (environment variables, dotfiles, etc.)
 *
 * Note: Using non-word-boundary patterns to catch variations like
 * SECRET_KEY, API_TOKEN, etc.
 */
const DEFAULT_SECRET_PATTERNS = [
  /password/i,
  /token/i,
  /secret/i,
  /api[_-]?key/i,
  /auth/i,
  /credential/i,
  /private[_-]?key/i,
  /\.env$/,
  /\.env\./,
  /\.pem$/,
  /\.key$/,
  /id_rsa/,
  /id_dsa/,
  /\.ssh\//,
  /\.gnupg\//
];

/**
 * Guard manager - enforces all poka-yoke constraints
 */
export class GuardManager {
  /**
   * @param {GuardConfig} config - Guard configuration
   */
  constructor(config = {}) {
    this.config = {
      allowedRoots: config.allowedRoots || [],
      allowedHosts: config.allowedHosts || [],
      secretPatterns: config.secretPatterns || DEFAULT_SECRET_PATTERNS,
      enforceTimeouts: config.enforceTimeouts !== false,
      defaultTimeoutMs: config.defaultTimeoutMs || 5000
    };

    // Normalize allowed roots to absolute paths
    this.config.allowedRoots = this.config.allowedRoots.map(root =>
      resolve(normalize(root))
    );

    this.denials = [];
  }

  /**
   * Check if path is within allowed roots
   *
   * @param {string} targetPath - Path to check
   * @returns {boolean}
   */
  isPathAllowed(targetPath) {
    if (this.config.allowedRoots.length === 0) {
      // No roots specified = deny all (fail-safe)
      return false;
    }

    const normalized = resolve(normalize(targetPath));

    // Check if path is within any allowed root
    for (const root of this.config.allowedRoots) {
      const rel = relative(root, normalized);
      // If relative path doesn't start with '..', it's within root
      if (rel && !rel.startsWith('..') && !resolve(root, rel).startsWith('..')) {
        return true;
      }
    }

    return false;
  }

  /**
   * Check if path matches secret patterns
   *
   * @param {string} targetPath - Path to check
   * @returns {boolean}
   */
  isSecretPath(targetPath) {
    const normalized = normalize(targetPath);

    for (const pattern of this.config.secretPatterns) {
      if (pattern.test(normalized)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Guard filesystem read access
   *
   * @param {string} targetPath - Path to read
   * @param {string} agentId - Agent requesting access
   * @returns {{ allowed: boolean, denial?: import('./observation.mjs').Observation }}
   */
  guardFileRead(targetPath, agentId) {
    // Check secret patterns first (highest priority)
    if (this.isSecretPath(targetPath)) {
      const denial = createGuardDenial({
        guardName: 'secret-pattern',
        reason: `Path matches secret pattern: ${targetPath}`,
        agentId,
        context: { path: targetPath }
      });
      this.denials.push(denial);
      return { allowed: false, denial };
    }

    // Check allowed roots
    if (!this.isPathAllowed(targetPath)) {
      const denial = createGuardDenial({
        guardName: 'filesystem-boundary',
        reason: `Path outside allowed roots: ${targetPath}`,
        agentId,
        context: {
          path: targetPath,
          allowedRoots: this.config.allowedRoots
        }
      });
      this.denials.push(denial);
      return { allowed: false, denial };
    }

    return { allowed: true };
  }

  /**
   * Guard filesystem write access (stricter than read)
   *
   * @param {string} targetPath - Path to write
   * @param {string} agentId - Agent requesting access
   * @returns {{ allowed: boolean, denial?: import('./observation.mjs').Observation }}
   */
  guardFileWrite(targetPath, agentId) {
    // For now, deny all writes (probes are read-only)
    const denial = createGuardDenial({
      guardName: 'readonly-probe',
      reason: `Probe operations are read-only: attempted write to ${targetPath}`,
      agentId,
      context: { path: targetPath }
    });
    this.denials.push(denial);
    return { allowed: false, denial };
  }

  /**
   * Guard network access
   *
   * @param {string} host - Target host
   * @param {string} agentId - Agent requesting access
   * @returns {{ allowed: boolean, denial?: import('./observation.mjs').Observation }}
   */
  guardNetworkAccess(host, agentId) {
    if (this.config.allowedHosts.length === 0) {
      const denial = createGuardDenial({
        guardName: 'network-deny-default',
        reason: `No network hosts allowed (deny by default): ${host}`,
        agentId,
        context: { host }
      });
      this.denials.push(denial);
      return { allowed: false, denial };
    }

    if (!this.config.allowedHosts.includes(host)) {
      const denial = createGuardDenial({
        guardName: 'network-allowlist',
        reason: `Host not in allowlist: ${host}`,
        agentId,
        context: {
          host,
          allowedHosts: this.config.allowedHosts
        }
      });
      this.denials.push(denial);
      return { allowed: false, denial };
    }

    return { allowed: true };
  }

  /**
   * Guard operation timeout
   *
   * @param {number} budgetMs - Requested time budget
   * @param {string} agentId - Agent requesting budget
   * @returns {{ allowed: boolean, denial?: import('./observation.mjs').Observation }}
   */
  guardTimeout(budgetMs, agentId) {
    if (!this.config.enforceTimeouts) {
      return { allowed: true };
    }

    // Maximum timeout: 10x default (50 seconds if default is 5s)
    const maxTimeoutMs = this.config.defaultTimeoutMs * 10;

    if (budgetMs > maxTimeoutMs) {
      const denial = createGuardDenial({
        guardName: 'timeout-limit',
        reason: `Requested timeout ${budgetMs}ms exceeds limit ${maxTimeoutMs}ms`,
        agentId,
        context: {
          budgetMs,
          maxTimeoutMs
        }
      });
      this.denials.push(denial);
      return { allowed: false, denial };
    }

    return { allowed: true };
  }

  /**
   * Get all denials recorded by guards
   *
   * @returns {import('./observation.mjs').Observation[]}
   */
  getDenials() {
    return [...this.denials];
  }

  /**
   * Clear denial log
   */
  clearDenials() {
    this.denials = [];
  }

  /**
   * Get guard statistics
   *
   * @returns {{ totalDenials: number, byGuard: Record<string, number> }}
   */
  getStats() {
    const byGuard = {};
    for (const denial of this.denials) {
      const guardName = denial.data.guardName;
      byGuard[guardName] = (byGuard[guardName] || 0) + 1;
    }

    return {
      totalDenials: this.denials.length,
      byGuard
    };
  }
}

/**
 * Create guard manager from CLI options
 *
 * @param {Object} options - CLI options
 * @param {string[]} options.root - Allowed root paths
 * @param {string[]} options.netAllow - Allowed network hosts
 * @param {number} options.budgetMs - Default timeout
 * @returns {GuardManager}
 */
export function createGuardManager(options) {
  return new GuardManager({
    allowedRoots: options.root || [],
    allowedHosts: options.netAllow || [],
    defaultTimeoutMs: options.budgetMs || 5000
  });
}

export default {
  GuardManager,
  createGuardManager,
  DEFAULT_SECRET_PATTERNS
};
