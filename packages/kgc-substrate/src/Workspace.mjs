/**
 * Workspace Isolation & IO Contracts
 *
 * Provides secure, isolated workspaces for multi-agent execution with:
 * - Per-agent work directories under /tmp/kgc-workspace/{agentId}/{namespace}/
 * - Declared input/output file sets with enforcement
 * - Security guards preventing unauthorized access
 * - Symlink escape protection
 * - Cross-agent interference prevention
 *
 * @module @unrdf/kgc-substrate/workspace
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import * as fs from 'fs/promises';
import * as path from 'path';
import { existsSync } from 'fs';

/**
 * IO operation type
 */
export const IOOperationType = z.enum(['read', 'write', 'delete', 'list']);

/**
 * IO operation schema
 */
export const IOOperationSchema = z.object({
  type: IOOperationType,
  path: z.string(),
});

/**
 * @typedef {z.infer<typeof IOOperationSchema>} IOOperation
 */

/**
 * Workspace constraints schema
 */
export const WorkspaceConstraintsSchema = z.object({
  /** Set of allowed input file paths (relative to workspace root) */
  inputs: z.set(z.string()).default(() => new Set()),
  /** Set of allowed output file paths (relative to workspace root) */
  outputs: z.set(z.string()).default(() => new Set()),
  /** Set of paths accessible for both read and write */
  readWrite: z.set(z.string()).default(() => new Set()),
  /** Optional namespace for workspace organization */
  namespace: z.string().default('default'),
  /** Prevent access to sensitive system paths */
  allowSystemPaths: z.boolean().default(false),
});

/**
 * @typedef {z.infer<typeof WorkspaceConstraintsSchema>} WorkspaceConstraints
 */

/**
 * Workspace base path - isolated under /tmp
 */
const WORKSPACE_BASE = '/tmp/kgc-workspace';

/**
 * Blocked system paths that should never be accessible
 */
const BLOCKED_PATHS = [
  '/etc',
  '/home',
  '/root',
  '/sys',
  '/proc',
  '/dev',
  '/boot',
  '/var/lib',
  '/usr/bin',
  '/usr/sbin',
  '/bin',
  '/sbin',
];

/**
 * Error class for workspace violations
 */
export class WorkspaceViolationError extends Error {
  /**
   * @param {string} message - Error message
   * @param {Object} details - Violation details
   */
  constructor(message, details = {}) {
    super(message);
    this.name = 'WorkspaceViolationError';
    this.details = details;
  }
}

/**
 * Resolve path and verify it doesn't escape workspace via symlinks
 *
 * @param {string} targetPath - Path to resolve
 * @param {string} workspaceRoot - Workspace root directory
 * @returns {Promise<string>} Resolved absolute path
 * @throws {WorkspaceViolationError} If path escapes workspace
 */
async function resolveAndValidatePath(targetPath, workspaceRoot) {
  try {
    // Convert to absolute path relative to workspace
    const absolutePath = path.isAbsolute(targetPath)
      ? targetPath
      : path.join(workspaceRoot, targetPath);

    // Resolve symlinks
    let resolvedPath;
    try {
      resolvedPath = await fs.realpath(absolutePath);
    } catch (err) {
      // If file doesn't exist yet, resolve parent directory
      const parentDir = path.dirname(absolutePath);
      if (existsSync(parentDir)) {
        const resolvedParent = await fs.realpath(parentDir);
        resolvedPath = path.join(resolvedParent, path.basename(absolutePath));
      } else {
        // Parent doesn't exist, validate the path structure
        resolvedPath = path.resolve(absolutePath);
      }
    }

    // Ensure resolved path is within workspace
    if (!resolvedPath.startsWith(workspaceRoot + path.sep) && resolvedPath !== workspaceRoot) {
      throw new WorkspaceViolationError('Path escapes workspace boundary', {
        requestedPath: targetPath,
        resolvedPath,
        workspaceRoot,
      });
    }

    return resolvedPath;
  } catch (error) {
    if (error instanceof WorkspaceViolationError) {
      throw error;
    }
    throw new WorkspaceViolationError('Path validation failed', {
      requestedPath: targetPath,
      error: error.message,
    });
  }
}

/**
 * Check if path matches any blocked system paths
 *
 * @param {string} targetPath - Path to check
 * @returns {boolean} True if path is blocked
 */
function isBlockedPath(targetPath) {
  const normalized = path.resolve(targetPath);
  return BLOCKED_PATHS.some((blocked) => normalized.startsWith(blocked));
}

/**
 * Normalize path for comparison (remove trailing slashes, resolve ..)
 *
 * @param {string} filePath - Path to normalize
 * @returns {string} Normalized path
 */
function normalizePath(filePath) {
  return path.normalize(filePath).replace(/\/$/, '');
}

/**
 * Check if a file path matches any pattern in the allowed set
 *
 * @param {string} filePath - File path to check
 * @param {Set<string>} allowedSet - Set of allowed patterns
 * @returns {boolean} True if path is allowed
 */
function isPathAllowed(filePath, allowedSet) {
  const normalized = normalizePath(filePath);

  for (const pattern of allowedSet) {
    const normalizedPattern = normalizePath(pattern);

    // Exact match
    if (normalized === normalizedPattern) {
      return true;
    }

    // Directory prefix match (allows files within directory)
    if (normalized.startsWith(normalizedPattern + path.sep)) {
      return true;
    }

    // Wildcard pattern support (simple * and ** patterns)
    if (pattern.includes('*')) {
      const regexPattern = pattern
        .replace(/\*\*/g, '§DOUBLESTAR§')
        .replace(/\*/g, '[^/]*')
        .replace(/§DOUBLESTAR§/g, '.*');
      const regex = new RegExp(`^${regexPattern}$`);
      if (regex.test(normalized)) {
        return true;
      }
    }
  }

  return false;
}

/**
 * Create an isolated workspace for an agent
 *
 * @param {string} agentId - Unique agent identifier
 * @param {Partial<WorkspaceConstraints>} [constraints={}] - IO constraints
 * @returns {Promise<Workspace>} Workspace instance
 *
 * @example
 * const workspace = await createWorkspace('agent-1', {
 *   inputs: new Set(['data/input.json']),
 *   outputs: new Set(['results/output.json']),
 *   namespace: 'experiment-1',
 * });
 *
 * // Check if operation is allowed
 * await workspace.enforceIOContract({ type: 'write', path: 'results/output.json' });
 *
 * // Get workspace paths
 * const inputPath = workspace.resolvePath('data/input.json');
 */
export async function createWorkspace(agentId, constraints = {}) {
  const config = WorkspaceConstraintsSchema.parse(constraints);

  // Create workspace directory
  const workspaceRoot = path.join(WORKSPACE_BASE, agentId, config.namespace);

  try {
    await fs.mkdir(workspaceRoot, { recursive: true });
  } catch (error) {
    throw new WorkspaceViolationError('Failed to create workspace directory', {
      workspaceRoot,
      error: error.message,
    });
  }

  /**
   * Enforcement log for audit trail
   * @type {Array<{timestamp: string, operation: IOOperation, allowed: boolean, reason?: string}>}
   */
  const enforcementLog = [];

  /**
   * Track files accessed for violation detection
   * @type {Set<string>}
   */
  const accessedFiles = new Set();

  return {
    /**
     * Get agent ID
     * @returns {string}
     */
    getAgentId() {
      return agentId;
    },

    /**
     * Get workspace root path
     * @returns {string}
     */
    getRoot() {
      return workspaceRoot;
    },

    /**
     * Get workspace configuration
     * @returns {WorkspaceConstraints}
     */
    getConstraints() {
      return { ...config };
    },

    /**
     * Resolve a relative path to absolute workspace path
     *
     * @param {string} relativePath - Relative path within workspace
     * @returns {string} Absolute path
     */
    resolvePath(relativePath) {
      return path.join(workspaceRoot, relativePath);
    },

    /**
     * Enforce IO contract for an operation
     *
     * @param {IOOperation} operation - IO operation to validate
     * @returns {Promise<void>}
     * @throws {WorkspaceViolationError} If operation violates contract
     */
    async enforceIOContract(operation) {
      const validated = IOOperationSchema.parse(operation);
      const { type, path: targetPath } = validated;

      // Block absolute system paths early (before resolution)
      if (!config.allowSystemPaths && path.isAbsolute(targetPath) && isBlockedPath(targetPath)) {
        const violation = {
          timestamp: new Date().toISOString(),
          operation: validated,
          allowed: false,
          reason: 'Blocked system path',
        };
        enforcementLog.push(violation);
        throw new WorkspaceViolationError('Access to system path denied', {
          path: targetPath,
          operation: type,
        });
      }

      // Resolve path and check for symlink escapes
      const resolvedPath = await resolveAndValidatePath(targetPath, workspaceRoot);

      // Block system paths after resolution (catches symlinks and relative paths)
      if (!config.allowSystemPaths && isBlockedPath(resolvedPath)) {
        const violation = {
          timestamp: new Date().toISOString(),
          operation: validated,
          allowed: false,
          reason: 'Blocked system path (via symlink or resolution)',
        };
        enforcementLog.push(violation);
        throw new WorkspaceViolationError('Access to system path denied', {
          path: targetPath,
          resolvedPath,
          operation: type,
        });
      }

      // Get relative path for contract checking
      const relativePath = path.relative(workspaceRoot, resolvedPath);

      // Track file access
      accessedFiles.add(relativePath);

      // Check operation against declared constraints
      let allowed = false;
      let reason = '';

      switch (type) {
        case 'read':
        case 'list':
          // Allow if in inputs or readWrite sets
          allowed =
            isPathAllowed(relativePath, config.inputs) ||
            isPathAllowed(relativePath, config.readWrite);
          reason = allowed ? '' : 'Path not in declared inputs';
          break;

        case 'write':
        case 'delete':
          // Allow if in outputs or readWrite sets
          allowed =
            isPathAllowed(relativePath, config.outputs) ||
            isPathAllowed(relativePath, config.readWrite);
          reason = allowed ? '' : 'Path not in declared outputs';
          break;

        default:
          reason = 'Unknown operation type';
      }

      const logEntry = {
        timestamp: new Date().toISOString(),
        operation: validated,
        allowed,
        reason,
      };
      enforcementLog.push(logEntry);

      if (!allowed) {
        throw new WorkspaceViolationError(`IO contract violation: ${reason}`, {
          operation: type,
          path: relativePath,
          resolvedPath,
          constraints: {
            inputs: Array.from(config.inputs),
            outputs: Array.from(config.outputs),
            readWrite: Array.from(config.readWrite),
          },
        });
      }
    },

    /**
     * Read a file within workspace
     *
     * @param {string} filePath - File path relative to workspace
     * @returns {Promise<string>} File contents
     */
    async readFile(filePath) {
      await this.enforceIOContract({ type: 'read', path: filePath });
      const absolutePath = this.resolvePath(filePath);
      return await fs.readFile(absolutePath, 'utf-8');
    },

    /**
     * Write a file within workspace
     *
     * @param {string} filePath - File path relative to workspace
     * @param {string} content - File content
     * @returns {Promise<void>}
     */
    async writeFile(filePath, content) {
      await this.enforceIOContract({ type: 'write', path: filePath });
      const absolutePath = this.resolvePath(filePath);

      // Ensure parent directory exists
      const parentDir = path.dirname(absolutePath);
      await fs.mkdir(parentDir, { recursive: true });

      await fs.writeFile(absolutePath, content, 'utf-8');
    },

    /**
     * Delete a file within workspace
     *
     * @param {string} filePath - File path relative to workspace
     * @returns {Promise<void>}
     */
    async deleteFile(filePath) {
      await this.enforceIOContract({ type: 'delete', path: filePath });
      const absolutePath = this.resolvePath(filePath);
      await fs.unlink(absolutePath);
    },

    /**
     * List files in a directory within workspace
     *
     * @param {string} dirPath - Directory path relative to workspace
     * @returns {Promise<string[]>} List of file names
     */
    async listFiles(dirPath) {
      await this.enforceIOContract({ type: 'list', path: dirPath });
      const absolutePath = this.resolvePath(dirPath);
      return await fs.readdir(absolutePath);
    },

    /**
     * Get enforcement log for audit
     *
     * @returns {Array} Enforcement log entries
     */
    getEnforcementLog() {
      return [...enforcementLog];
    },

    /**
     * Get set of accessed files
     *
     * @returns {Set<string>} Accessed file paths
     */
    getAccessedFiles() {
      return new Set(accessedFiles);
    },

    /**
     * Clean up workspace (delete all files)
     *
     * @returns {Promise<void>}
     */
    async cleanup() {
      try {
        await fs.rm(workspaceRoot, { recursive: true, force: true });
      } catch (error) {
        throw new WorkspaceViolationError('Failed to cleanup workspace', {
          workspaceRoot,
          error: error.message,
        });
      }
    },

    /**
     * Generate workspace manifest with hash
     *
     * @returns {Promise<Object>} Workspace manifest
     */
    async getManifest() {
      const manifest = {
        agentId,
        namespace: config.namespace,
        workspaceRoot,
        constraints: {
          inputs: Array.from(config.inputs),
          outputs: Array.from(config.outputs),
          readWrite: Array.from(config.readWrite),
        },
        accessedFiles: Array.from(accessedFiles),
        enforcementLog,
        timestamp: new Date().toISOString(),
      };

      const manifestHash = await blake3(JSON.stringify(manifest));

      return {
        ...manifest,
        manifestHash,
      };
    },
  };
}

/**
 * @typedef {Awaited<ReturnType<typeof createWorkspace>>} Workspace
 */

/**
 * Verify two workspaces don't interfere with each other
 *
 * @param {Workspace} workspace1 - First workspace
 * @param {Workspace} workspace2 - Second workspace
 * @returns {boolean} True if workspaces are isolated
 */
export function verifyWorkspaceIsolation(workspace1, workspace2) {
  const root1 = workspace1.getRoot();
  const root2 = workspace2.getRoot();

  // Ensure paths don't overlap
  return !root1.startsWith(root2) && !root2.startsWith(root1) && root1 !== root2;
}

/**
 * Create a workspace with read-only access to specific paths
 *
 * @param {string} agentId - Unique agent identifier
 * @param {string[]} readOnlyPaths - Paths accessible for reading
 * @param {string} [namespace='default'] - Workspace namespace
 * @returns {Promise<Workspace>}
 */
export async function createReadOnlyWorkspace(agentId, readOnlyPaths, namespace = 'default') {
  return createWorkspace(agentId, {
    inputs: new Set(readOnlyPaths),
    outputs: new Set(),
    namespace,
  });
}

/**
 * Create a workspace with write-only access to specific paths
 *
 * @param {string} agentId - Unique agent identifier
 * @param {string[]} writeOnlyPaths - Paths accessible for writing
 * @param {string} [namespace='default'] - Workspace namespace
 * @returns {Promise<Workspace>}
 */
export async function createWriteOnlyWorkspace(agentId, writeOnlyPaths, namespace = 'default') {
  return createWorkspace(agentId, {
    inputs: new Set(),
    outputs: new Set(writeOnlyPaths),
    namespace,
  });
}
