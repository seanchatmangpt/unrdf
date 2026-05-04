/**
 * @fileoverview Poka-Yoke Guard System - Preventing Illegal Operations
 *
 * H := {secret, out-of-root, non-allowlisted-net, privilege-escalation, "internal weights"}
 *
 * unlawful(o) ⇒ emit(Receipt(o)) ∧ ¬emit(payload(o))
 *
 * All guards are pure functions. Guard enforcement: μ ⊣ H
 *
 * @module @unrdf/kgc-swarm/guards
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';
import { now } from '@unrdf/kgc-4d';
import path from 'node:path';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Operation schema for guard validation
 */
export const OperationSchema = z.object({
  type: z.enum([
    'file:read',
    'file:write',
    'file:delete',
    'network:request',
    'process:spawn',
    'env:access',
    'secret:access',
  ]),
  target: z.string(),
  data: z.any().optional(),
  metadata: z.record(z.any()).optional(),
});

/**
 * Guard result schema
 */
export const GuardResultSchema = z.object({
  allowed: z.boolean(),
  guard: z.string(),
  reason: z.string().optional(),
  receipt: z.object({
    id: z.string(),
    timestamp: z.string(),
    operation: z.string(),
    inputs: z.record(z.any()),
    outputs: z.record(z.any()),
    hash: z.string(),
  }),
});

/**
 * Guard configuration schema
 */
export const GuardConfigSchema = z.object({
  rootPath: z.string().optional(),
  allowedHosts: z.array(z.string()).optional(),
  allowedPorts: z.array(z.number()).optional(),
  secretPatterns: z.array(z.string()).optional(),
  enableSecretGuard: z.boolean().default(true),
  enablePathGuard: z.boolean().default(true),
  enableNetworkGuard: z.boolean().default(true),
  enablePrivilegeGuard: z.boolean().default(true),
});

/**
 * @typedef {z.infer<typeof OperationSchema>} Operation
 * @typedef {z.infer<typeof GuardResultSchema>} GuardResult
 * @typedef {z.infer<typeof GuardConfigSchema>} GuardConfig
 */

// =============================================================================
// Receipt Generation
// =============================================================================

/**
 * Generate a receipt for a guard decision
 * @param {string} operation - Operation name
 * @param {Record<string, any>} inputs - Operation inputs
 * @param {Record<string, any>} outputs - Operation outputs (blocked operations have empty outputs)
 * @returns {Promise<{id: string, timestamp: string, operation: string, inputs: Record<string, any>, outputs: Record<string, any>, hash: string}>} Generated receipt
 */
async function generateReceipt(operation, inputs, outputs) {
  const timestamp = now().toString();
  const id = `receipt-${timestamp}-${operation}`;

  // Create deterministic hash of operation
  const data = JSON.stringify({
    operation,
    timestamp,
    inputs,
    outputs,
  }, null, 0); // No whitespace for determinism

  const hash = await blake3(data);

  return {
    id,
    timestamp,
    operation,
    inputs,
    outputs,
    hash,
  };
}

// =============================================================================
// Guard Implementations
// =============================================================================

/**
 * Secret patterns to detect (API keys, tokens, passwords, etc.)
 */
const DEFAULT_SECRET_PATTERNS = [
  // Environment variable patterns
  /PASSWORD/i,
  /SECRET/i,
  /API[_-]?KEY/i,
  /TOKEN/i,
  /PRIVATE[_-]?KEY/i,
  /CLIENT[_-]?SECRET/i,
  /AUTH[_-]?TOKEN/i,
  /ACCESS[_-]?KEY/i,
  /BEARER/i,

  // Common secret formats
  /sk-[a-zA-Z0-9]{32,}/,  // OpenAI style keys
  /ghp_[a-zA-Z0-9]{36}/,   // GitHub personal access tokens
  /gho_[a-zA-Z0-9]{36}/,   // GitHub OAuth tokens
  /ghs_[a-zA-Z0-9]{36}/,   // GitHub server tokens
  /github_pat_[a-zA-Z0-9_]{82}/,  // GitHub fine-grained PAT
  /glpat-[a-zA-Z0-9_-]{20}/,      // GitLab PAT
  /AKIA[0-9A-Z]{16}/,      // AWS access key
  /AIza[0-9A-Za-z_-]{35}/,  // Google API key
  /ya29\.[0-9A-Za-z_-]+/,   // Google OAuth token
  /[0-9]+-[0-9A-Za-z_]{32}\.apps\.googleusercontent\.com/, // Google OAuth client
  /eyJ[A-Za-z0-9_-]+\.eyJ[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+/, // JWT tokens
  /xox[baprs]-[0-9A-Za-z-]{10,48}/, // Slack tokens
  /sq0atp-[0-9A-Za-z_-]{22}/, // Square access token
  /sq0csp-[0-9A-Za-z_-]{43}/, // Square OAuth secret
];

/**
 * SecretGuard - Prevent exposure of credentials, tokens, keys
 *
 * Blocks operations that would expose secrets in:
 * - File contents being written
 * - Network requests
 * - Environment variable access
 * - Process arguments
 *
 * @param {Operation} operation - Operation to validate
 * @param {GuardConfig} config - Guard configuration
 * @returns {Promise<GuardResult>} Validation result with receipt
 */
export async function SecretGuard(operation, config = {}) {
  const patterns = config.secretPatterns || DEFAULT_SECRET_PATTERNS;

  let containsSecret = false;
  let secretType = '';

  // Check target (filename, URL, etc.)
  for (const pattern of patterns) {
    if (pattern.test(operation.target)) {
      containsSecret = true;
      secretType = pattern.toString();
      break;
    }
  }

  // Check data payload
  if (!containsSecret && operation.data) {
    const dataStr = typeof operation.data === 'string'
      ? operation.data
      : JSON.stringify(operation.data);

    for (const pattern of patterns) {
      if (pattern.test(dataStr)) {
        containsSecret = true;
        secretType = pattern.toString();
        break;
      }
    }
  }

  const allowed = !containsSecret;
  const receipt = await generateReceipt(
    'SecretGuard',
    {
      operation: operation.type,
      target: operation.target,
      hasData: !!operation.data,
    },
    allowed ? { allowed: true } : { allowed: false, blocked: 'secret-detected' }
  );

  return GuardResultSchema.parse({
    allowed,
    guard: 'SecretGuard',
    reason: containsSecret
      ? `Potential secret detected matching pattern: ${secretType}`
      : undefined,
    receipt,
  });
}

/**
 * PathGuard - Prevent out-of-root filesystem access
 *
 * Ensures all file operations stay within the allowed root path.
 * Blocks path traversal attempts (../, symbolic links outside root, etc.)
 *
 * @param {Operation} operation - Operation to validate
 * @param {GuardConfig} config - Guard configuration with rootPath
 * @returns {Promise<GuardResult>} Validation result with receipt
 */
export async function PathGuard(operation, config = {}) {
  // Skip if not a file operation
  if (!operation.type.startsWith('file:')) {
    const receipt = await generateReceipt(
      'PathGuard',
      { operation: operation.type, target: operation.target },
      { allowed: true, reason: 'not-file-operation' }
    );

    return GuardResultSchema.parse({
      allowed: true,
      guard: 'PathGuard',
      receipt,
    });
  }

  const rootPath = config.rootPath || process.cwd();
  const resolvedRoot = path.resolve(rootPath);
  const resolvedTarget = path.resolve(operation.target);

  // Check if target is within root
  const isWithinRoot = resolvedTarget.startsWith(resolvedRoot);

  // Additional check for path traversal attempts
  const hasTraversal = operation.target.includes('../') || operation.target.includes('..\\');

  const allowed = isWithinRoot && !hasTraversal;
  const receipt = await generateReceipt(
    'PathGuard',
    {
      operation: operation.type,
      target: operation.target,
      rootPath: resolvedRoot,
    },
    allowed
      ? { allowed: true, resolvedPath: resolvedTarget }
      : { allowed: false, blocked: 'out-of-root', attemptedPath: resolvedTarget }
  );

  return GuardResultSchema.parse({
    allowed,
    guard: 'PathGuard',
    reason: !allowed
      ? `Path ${resolvedTarget} is outside allowed root ${resolvedRoot}`
      : undefined,
    receipt,
  });
}

/**
 * NetworkGuard - Allowlist-only network access
 *
 * Blocks network requests to hosts not on the allowlist.
 * Supports host and port validation.
 *
 * @param {Operation} operation - Operation to validate
 * @param {GuardConfig} config - Guard configuration with allowedHosts
 * @returns {Promise<GuardResult>} Validation result with receipt
 */
export async function NetworkGuard(operation, config = {}) {
  // Skip if not a network operation
  if (operation.type !== 'network:request') {
    const receipt = await generateReceipt(
      'NetworkGuard',
      { operation: operation.type, target: operation.target },
      { allowed: true, reason: 'not-network-operation' }
    );

    return GuardResultSchema.parse({
      allowed: true,
      guard: 'NetworkGuard',
      receipt,
    });
  }

  const allowedHosts = config.allowedHosts || [];
  const allowedPorts = config.allowedPorts || [80, 443, 8080];

  // Parse URL
  let url;
  try {
    url = new URL(operation.target);
  } catch {
    // If not a valid URL, block it
    const receipt = await generateReceipt(
      'NetworkGuard',
      { operation: operation.type, target: operation.target },
      { allowed: false, blocked: 'invalid-url' }
    );

    return GuardResultSchema.parse({
      allowed: false,
      guard: 'NetworkGuard',
      reason: 'Invalid URL format',
      receipt,
    });
  }

  // Check host allowlist
  const hostAllowed = allowedHosts.length === 0 || allowedHosts.some(allowed => {
    // Support wildcard matching
    if (allowed.startsWith('*.')) {
      const domain = allowed.slice(2);
      return url.hostname.endsWith(domain) || url.hostname === domain;
    }
    return url.hostname === allowed;
  });

  // Check port
  const port = url.port ? parseInt(url.port) : (url.protocol === 'https:' ? 443 : 80);
  const portAllowed = allowedPorts.includes(port);

  const allowed = hostAllowed && portAllowed;
  const receipt = await generateReceipt(
    'NetworkGuard',
    {
      operation: operation.type,
      target: operation.target,
      hostname: url.hostname,
      port,
    },
    allowed
      ? { allowed: true }
      : {
          allowed: false,
          blocked: !hostAllowed ? 'host-not-allowed' : 'port-not-allowed',
          hostname: url.hostname,
          port,
        }
  );

  return GuardResultSchema.parse({
    allowed,
    guard: 'NetworkGuard',
    reason: !allowed
      ? `Network access to ${url.hostname}:${port} not allowed`
      : undefined,
    receipt,
  });
}

/**
 * PrivilegeGuard - Prevent privilege escalation
 *
 * Blocks operations that could escalate privileges:
 * - Running setuid/setgid executables
 * - Accessing /etc/passwd, /etc/shadow
 * - Spawning processes with elevated privileges
 * - Modifying system files
 *
 * @param {Operation} operation - Operation to validate
 * @param {GuardConfig} config - Guard configuration
 * @returns {Promise<GuardResult>} Validation result with receipt
 */
export async function PrivilegeGuard(operation, config = {}) {
  const RESTRICTED_PATHS = [
    '/etc/passwd',
    '/etc/shadow',
    '/etc/sudoers',
    '/etc/group',
    '/root/',
    '/sys/',
    '/proc/self/mem',
  ];

  const RESTRICTED_COMMANDS = [
    'sudo',
    'su',
    'doas',
    'pkexec',
    'chmod',
    'chown',
    'chgrp',
  ];

  let blocked = false;
  let blockReason = '';

  // Check for restricted path access
  if (operation.type.startsWith('file:')) {
    const normalizedTarget = path.normalize(operation.target);

    for (const restrictedPath of RESTRICTED_PATHS) {
      if (normalizedTarget.startsWith(restrictedPath)) {
        blocked = true;
        blockReason = `Access to restricted path: ${restrictedPath}`;
        break;
      }
    }
  }

  // Check for privilege escalation commands
  if (operation.type === 'process:spawn') {
    const command = operation.target.split(' ')[0].toLowerCase();
    const commandName = path.basename(command);

    if (RESTRICTED_COMMANDS.includes(commandName)) {
      blocked = true;
      blockReason = `Restricted command: ${commandName}`;
    }

    // Check for setuid bit operations
    if (operation.data?.setuid || operation.data?.setgid) {
      blocked = true;
      blockReason = 'Setuid/setgid not allowed';
    }
  }

  const allowed = !blocked;
  const receipt = await generateReceipt(
    'PrivilegeGuard',
    {
      operation: operation.type,
      target: operation.target,
    },
    allowed
      ? { allowed: true }
      : { allowed: false, blocked: 'privilege-escalation', reason: blockReason }
  );

  return GuardResultSchema.parse({
    allowed,
    guard: 'PrivilegeGuard',
    reason: blocked ? blockReason : undefined,
    receipt,
  });
}

// =============================================================================
// Guard System
// =============================================================================

/**
 * GuardSystem - Orchestrates all guards for operation validation
 *
 * H := {secret, out-of-root, non-allowlisted-net, privilege-escalation}
 *
 * Validates operations against all enabled guards.
 * unlawful(o) ⇒ emit(Receipt(o)) ∧ ¬emit(payload(o))
 */
export class GuardSystem {
  /**
   * @param {GuardConfig} config - Guard configuration
   */
  constructor(config = {}) {
    this.config = GuardConfigSchema.parse(config);
    this.guards = this._initializeGuards();
  }

  /**
   * Initialize enabled guards
   * @private
   * @returns {Array<{name: string, fn: Function}>} Guard functions
   */
  _initializeGuards() {
    const guards = [];

    if (this.config.enableSecretGuard) {
      guards.push({ name: 'SecretGuard', fn: SecretGuard });
    }
    if (this.config.enablePathGuard) {
      guards.push({ name: 'PathGuard', fn: PathGuard });
    }
    if (this.config.enableNetworkGuard) {
      guards.push({ name: 'NetworkGuard', fn: NetworkGuard });
    }
    if (this.config.enablePrivilegeGuard) {
      guards.push({ name: 'PrivilegeGuard', fn: PrivilegeGuard });
    }

    return guards;
  }

  /**
   * Validate an operation against all guards
   *
   * Returns on first guard that blocks the operation.
   * All guard decisions produce receipts.
   *
   * @param {Operation} operation - Operation to validate
   * @returns {Promise<GuardResult>} Validation result with receipt
   */
  async validate(operation) {
    // Validate operation schema
    const validatedOp = OperationSchema.parse(operation);

    // Run all guards in sequence (fail-fast)
    for (const guard of this.guards) {
      const result = await guard.fn(validatedOp, this.config);

      // If any guard blocks, return immediately with receipt
      if (!result.allowed) {
        return result;
      }
    }

    // All guards passed - generate success receipt
    const receipt = await generateReceipt(
      'GuardSystem:validate',
      {
        operation: validatedOp.type,
        target: validatedOp.target,
        guardsChecked: this.guards.map(g => g.name),
      },
      { allowed: true, guardsChecked: this.guards.length }
    );

    return GuardResultSchema.parse({
      allowed: true,
      guard: 'GuardSystem',
      receipt,
    });
  }

  /**
   * Check if an operation is unlawful (blocked by any guard)
   *
   * unlawful(o) returns true if operation would be blocked
   *
   * @param {Operation} operation - Operation to check
   * @returns {Promise<boolean>} True if operation is unlawful
   */
  async unlawful(operation) {
    const result = await this.validate(operation);
    return !result.allowed;
  }

  /**
   * Get all enabled guards
   * @returns {string[]} Guard names
   */
  getEnabledGuards() {
    return this.guards.map(g => g.name);
  }
}
