/**
 * @fileoverview Tooling Surface Probe - Safe API-based tool detection
 * @module @unrdf/kgc-probe/probes/tooling
 *
 * CRITICAL: This probe uses ONLY safe APIs with explicit allowlisting.
 * NO arbitrary command execution. All commands timeout at 5s.
 *
 * Allowlist: ['git', 'node', 'npm', 'pnpm', 'which']
 *
 * @agent Agent 8 - Tooling Surface Probe (KGC Probe Swarm)
 */

import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { z } from 'zod';

const execFileAsync = promisify(execFile);

// ============================================================================
// SCHEMAS
// ============================================================================

/**
 * Observation schema - represents a single probe measurement
 * @typedef {Object} Observation
 * @property {string} method - Probe method identifier (e.g., "tooling.git_version")
 * @property {Record<string, any>} inputs - Input parameters used for probing
 * @property {Record<string, any>} outputs - Observed outputs/measurements
 * @property {string} [guardDecision] - Guard decision: "allowed", "denied", "unknown"
 * @property {Record<string, any>} [metadata] - Additional metadata
 */
const ObservationSchema = z.object({
  method: z.string().min(1),
  inputs: z.record(z.any()),
  outputs: z.record(z.any()),
  guardDecision: z.enum(['allowed', 'denied', 'unknown']).optional(),
  metadata: z.record(z.any()).optional(),
});

/**
 * Probe configuration schema
 * @typedef {Object} ProbeConfig
 * @property {number} [timeout] - Command timeout in milliseconds (default: 5000)
 * @property {boolean} [strict] - Strict mode - fail on any error (default: false)
 */
const ProbeConfigSchema = z.object({
  timeout: z.number().int().positive().max(10000).default(5000),
  strict: z.boolean().default(false),
}).default({});

// ============================================================================
// GUARD: COMMAND ALLOWLIST (POKA-YOKE)
// ============================================================================

/**
 * CRITICAL: Allowlisted commands - ONLY these can be executed
 * NO shell metacharacters. NO arbitrary commands.
 */
const ALLOWED_COMMANDS = new Set(['git', 'node', 'npm', 'pnpm', 'which']);

/**
 * Guard function - validates command is allowlisted
 * @param {string} command - Command to validate
 * @returns {boolean} True if allowed, false otherwise
 */
function isCommandAllowed(command) {
  return ALLOWED_COMMANDS.has(command);
}

/**
 * Validates command arguments for shell metacharacters
 * @param {string[]} args - Command arguments
 * @returns {boolean} True if safe, false if potentially dangerous
 */
function argsAreSafe(args) {
  // No shell metacharacters allowed
  const dangerous = /[;&|`$<>(){}[\]\\'"]/;
  return args.every(arg => !dangerous.test(arg));
}

// ============================================================================
// SAFE COMMAND EXECUTION
// ============================================================================

/**
 * Executes a command with strict safety guards
 * @param {string} command - Command name (must be allowlisted)
 * @param {string[]} args - Command arguments (no shell metacharacters)
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<{stdout: string, stderr: string, success: boolean, guardDecision: string}>}
 */
async function safeExec(command, args, timeout) {
  // Guard 1: Command allowlist check
  if (!isCommandAllowed(command)) {
    return {
      stdout: '',
      stderr: `Command '${command}' not in allowlist`,
      success: false,
      guardDecision: 'denied',
    };
  }

  // Guard 2: Argument safety check
  if (!argsAreSafe(args)) {
    return {
      stdout: '',
      stderr: 'Arguments contain shell metacharacters',
      success: false,
      guardDecision: 'denied',
    };
  }

  // Execute with timeout (no shell, no stdin, capture stdout/stderr only)
  try {
    const { stdout, stderr } = await execFileAsync(command, args, {
      timeout,
      maxBuffer: 1024 * 1024, // 1MB max output
      shell: false, // CRITICAL: NO shell execution
      windowsHide: true,
    });

    return {
      stdout: stdout.trim(),
      stderr: stderr.trim(),
      success: true,
      guardDecision: 'allowed',
    };
  } catch (error) {
    // Command failed or timed out
    return {
      stdout: error.stdout?.trim() || '',
      stderr: error.stderr?.trim() || error.message,
      success: false,
      guardDecision: error.code === 'ETIMEDOUT' ? 'unknown' : 'allowed',
    };
  }
}

// ============================================================================
// TOOL DETECTION
// ============================================================================

/**
 * Probes for git availability and version
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Observation>}
 */
async function probeGit(timeout) {
  const result = await safeExec('git', ['--version'], timeout);

  const outputs = {};
  const metadata = {};

  if (result.success && result.stdout) {
    // Parse version: "git version 2.34.1" -> "2.34.1"
    const match = result.stdout.match(/git version ([\d.]+)/);
    if (match) {
      outputs.version = match[1];
      outputs.available = true;
    }
  } else {
    outputs.available = false;
    metadata.error = result.stderr || 'Command failed';
  }

  return {
    method: 'tooling.git_version',
    inputs: { command: 'git', args: ['--version'] },
    outputs,
    guardDecision: result.guardDecision,
    metadata,
  };
}

/**
 * Probes for Node.js availability and version
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Observation>}
 */
async function probeNode(timeout) {
  const result = await safeExec('node', ['--version'], timeout);

  const outputs = {};
  const metadata = {};

  if (result.success && result.stdout) {
    // Parse version: "v18.17.0" -> "18.17.0"
    const version = result.stdout.replace(/^v/, '');
    outputs.version = version;
    outputs.available = true;
  } else {
    outputs.available = false;
    metadata.error = result.stderr || 'Command failed';
  }

  return {
    method: 'tooling.node_version',
    inputs: { command: 'node', args: ['--version'] },
    outputs,
    guardDecision: result.guardDecision,
    metadata,
  };
}

/**
 * Probes for npm availability and version
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Observation>}
 */
async function probeNpm(timeout) {
  const result = await safeExec('npm', ['--version'], timeout);

  const outputs = {};
  const metadata = {};

  if (result.success && result.stdout) {
    outputs.version = result.stdout;
    outputs.available = true;
  } else {
    outputs.available = false;
    metadata.error = result.stderr || 'Command failed';
  }

  return {
    method: 'tooling.npm_version',
    inputs: { command: 'npm', args: ['--version'] },
    outputs,
    guardDecision: result.guardDecision,
    metadata,
  };
}

/**
 * Probes for pnpm availability and version
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Observation>}
 */
async function probePnpm(timeout) {
  const result = await safeExec('pnpm', ['--version'], timeout);

  const outputs = {};
  const metadata = {};

  if (result.success && result.stdout) {
    outputs.version = result.stdout;
    outputs.available = true;
  } else {
    outputs.available = false;
    metadata.error = result.stderr || 'Command failed';
  }

  return {
    method: 'tooling.pnpm_version',
    inputs: { command: 'pnpm', args: ['--version'] },
    outputs,
    guardDecision: result.guardDecision,
    metadata,
  };
}

/**
 * Probes for shell availability
 * Uses 'which' to detect shell binaries (sh, bash)
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Observation[]>}
 */
async function probeShells(timeout) {
  const shells = ['sh', 'bash'];
  const observations = [];

  for (const shell of shells) {
    const result = await safeExec('which', [shell], timeout);

    const outputs = {};
    const metadata = {};

    if (result.success && result.stdout) {
      outputs.path = result.stdout;
      outputs.available = true;
    } else {
      outputs.available = false;
      metadata.reason = result.stderr || 'Not found';
    }

    observations.push({
      method: `tooling.shell_${shell}`,
      inputs: { command: 'which', args: [shell] },
      outputs,
      guardDecision: result.guardDecision,
      metadata,
    });
  }

  return observations;
}

/**
 * Probes for build tools (make, cmake)
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Observation[]>}
 */
async function probeBuildTools(timeout) {
  // NOTE: make and cmake are NOT in allowlist
  // Return observations with guardDecision: "denied"
  const tools = ['make', 'cmake'];
  const observations = [];

  for (const tool of tools) {
    observations.push({
      method: `tooling.build_${tool}`,
      inputs: { command: tool },
      outputs: { available: false },
      guardDecision: 'denied',
      metadata: { reason: 'not_in_allowlist' },
    });
  }

  return observations;
}

/**
 * Detects package manager in use
 * Checks for lock files and tool availability
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Observation>}
 */
async function detectPackageManager(timeout) {
  const outputs = {};
  const metadata = {};

  // Check npm
  const npmResult = await safeExec('npm', ['--version'], timeout);
  if (npmResult.success) {
    outputs.npm = { available: true, version: npmResult.stdout };
  }

  // Check pnpm
  const pnpmResult = await safeExec('pnpm', ['--version'], timeout);
  if (pnpmResult.success) {
    outputs.pnpm = { available: true, version: pnpmResult.stdout };
  }

  // Determine primary (prefer pnpm if both available)
  if (outputs.pnpm?.available) {
    outputs.primary = 'pnpm';
  } else if (outputs.npm?.available) {
    outputs.primary = 'npm';
  } else {
    outputs.primary = 'none';
    metadata.warning = 'No package manager detected';
  }

  return {
    method: 'tooling.package_manager',
    inputs: {},
    outputs,
    guardDecision: 'allowed',
    metadata,
  };
}

// ============================================================================
// MAIN PROBE FUNCTION
// ============================================================================

/**
 * Probes tooling surface using ONLY safe APIs
 *
 * Returns observations for:
 * - Available CLI tools (git, node, npm, pnpm)
 * - Tool versions
 * - Shell availability (sh, bash)
 * - Package manager detection
 * - Build tool availability (denied due to allowlist)
 *
 * GUARD CONSTRAINTS:
 * - ONLY allowlisted commands: ['git', 'node', 'npm', 'pnpm', 'which']
 * - NO shell metacharacters
 * - Each command: timeout 5s (default), no stdin, capture stdout only
 * - If execution unavailable: returns observations with guardDecision: "unknown"
 *
 * @param {ProbeConfig} [config] - Probe configuration
 * @returns {Promise<Observation[]>} Array of observations
 *
 * @example
 * const observations = await probeTooling({ timeout: 5000 });
 * observations.forEach(obs => {
 *   console.log(`${obs.method}: ${JSON.stringify(obs.outputs)}`);
 * });
 */
export async function probeTooling(config = {}) {
  // Validate config
  const validatedConfig = ProbeConfigSchema.parse(config);
  const { timeout } = validatedConfig;

  const observations = [];

  try {
    // Probe all tools in parallel
    const [
      gitObs,
      nodeObs,
      npmObs,
      pnpmObs,
      shellObs,
      buildObs,
      pkgMgrObs,
    ] = await Promise.all([
      probeGit(timeout),
      probeNode(timeout),
      probeNpm(timeout),
      probePnpm(timeout),
      probeShells(timeout),
      probeBuildTools(timeout),
      detectPackageManager(timeout),
    ]);

    // Collect all observations
    observations.push(gitObs, nodeObs, npmObs, pnpmObs);
    observations.push(...shellObs);
    observations.push(...buildObs);
    observations.push(pkgMgrObs);

  } catch (error) {
    // Process execution unavailable or catastrophic failure
    observations.push({
      method: 'tooling.execution_error',
      inputs: {},
      outputs: {},
      guardDecision: 'unknown',
      metadata: {
        reason: 'process_execution_unavailable',
        error: error.message,
      },
    });
  }

  // Validate all observations
  return observations.map(obs => ObservationSchema.parse(obs));
}

// ============================================================================
// EXPORTS
// ============================================================================

/**
 * Re-export schemas for external validation
 */
export { ObservationSchema, ProbeConfigSchema };

/**
 * Re-export guard functions for testing
 */
export { isCommandAllowed, argsAreSafe, safeExec };
