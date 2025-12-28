/**
 * Filesystem Surface Probe - KGC Probe Swarm Agent 3
 *
 * Probes filesystem behaviors within declared --root paths only.
 * Implements guard constraints (poka-yoke) to prevent access to sensitive paths.
 *
 * @module kgc-probe/probes/filesystem
 */

import { z } from 'zod';
import { createHash } from 'crypto';
import fs from 'fs/promises';
import { constants as fsConstants } from 'fs';
import path from 'path';
import os from 'os';

/**
 * Observation Schema - matches KGC architecture receipts
 */
const ObservationSchema = z.object({
  method: z.string().describe('Filesystem operation tested'),
  inputs: z.record(z.any()).describe('Input parameters'),
  outputs: z.record(z.any()).optional().describe('Results of operation'),
  timestamp: z.string().describe('ISO 8601 timestamp'),
  hash: z.string().describe('SHA256 hash of observation'),
  guardDecision: z.enum(['allowed', 'denied']).describe('Guard evaluation result'),
  guardReason: z.string().optional().describe('Reason for denial'),
  error: z.string().optional().describe('Error message if operation failed')
});

/**
 * Config Schema
 */
const ConfigSchema = z.object({
  roots: z.array(z.string()).min(1).describe('Allowed root paths'),
  out: z.string().describe('Output directory for temp files'),
  budgetMs: z.number().optional().default(5000).describe('Timeout budget in milliseconds')
});

/**
 * Forbidden path patterns (CRITICAL SECURITY)
 */
const FORBIDDEN_PATTERNS = [
  /^\/etc\//,
  /^\/root\//,
  /\/\.ssh\//,
  /\/\.env$/,
  /\/\.config\//,
  /\/\.npmrc$/,
  /\/\.gitconfig$/,
  /\/\.aws\//,
  /\/\.kube\//,
  /\/\.docker\//,
  // Secret-like files
  /credentials\.json$/,
  /secrets\.json$/,
  /\.pem$/,
  /\.key$/,
  /id_rsa/,
  /id_ed25519/
];

/**
 * Guard: Check if path is within allowed roots and not forbidden
 *
 * @param {string} targetPath - Path to check
 * @param {string[]} allowedRoots - Allowed root paths
 * @returns {{ allowed: boolean, reason?: string }}
 */
function guardPath(targetPath, allowedRoots) {
  const normalizedPath = path.resolve(targetPath);

  // Check forbidden patterns first
  for (const pattern of FORBIDDEN_PATTERNS) {
    if (pattern.test(normalizedPath)) {
      return {
        allowed: false,
        reason: `Path matches forbidden pattern: ${pattern}`
      };
    }
  }

  // Check if within allowed roots
  const withinRoot = allowedRoots.some(root => {
    const normalizedRoot = path.resolve(root);
    return normalizedPath.startsWith(normalizedRoot);
  });

  if (!withinRoot) {
    return {
      allowed: false,
      reason: `Path outside allowed roots: ${allowedRoots.join(', ')}`
    };
  }

  return { allowed: true };
}

/**
 * Compute deterministic hash of observation data
 *
 * @param {object} data - Observation data
 * @returns {string} SHA256 hex hash
 */
function computeHash(data) {
  const json = JSON.stringify(data, Object.keys(data).sort());
  return createHash('sha256').update(json).digest('hex');
}

/**
 * Create observation object
 *
 * @param {string} method - Method name
 * @param {object} inputs - Input parameters
 * @param {object} outputs - Output data
 * @param {string} guardDecision - 'allowed' or 'denied'
 * @param {string} [guardReason] - Reason for denial
 * @param {string} [error] - Error message
 * @returns {object} Observation
 */
function createObservation(method, inputs, outputs, guardDecision, guardReason, error) {
  const obs = {
    method,
    inputs,
    outputs: guardDecision === 'allowed' ? outputs : undefined,
    timestamp: new Date().toISOString(),
    guardDecision,
    guardReason,
    error
  };

  // Compute hash
  obs.hash = computeHash({
    method: obs.method,
    inputs: obs.inputs,
    outputs: obs.outputs,
    guardDecision: obs.guardDecision
  });

  return ObservationSchema.parse(obs);
}

/**
 * Probe: Read capability
 *
 * @param {string} targetPath - Path to test
 * @param {string[]} allowedRoots - Allowed roots
 * @returns {Promise<object>} Observation
 */
async function probeReadCapability(targetPath, allowedRoots) {
  const guard = guardPath(targetPath, allowedRoots);

  if (!guard.allowed) {
    return createObservation(
      'fs.access(R_OK)',
      { path: targetPath },
      {},
      'denied',
      guard.reason
    );
  }

  try {
    await fs.access(targetPath, fsConstants.R_OK);
    return createObservation(
      'fs.access(R_OK)',
      { path: targetPath },
      { readable: true },
      'allowed'
    );
  } catch (err) {
    return createObservation(
      'fs.access(R_OK)',
      { path: targetPath },
      { readable: false },
      'allowed',
      undefined,
      err.message
    );
  }
}

/**
 * Probe: Write capability
 *
 * @param {string} targetPath - Path to test
 * @param {string[]} allowedRoots - Allowed roots
 * @returns {Promise<object>} Observation
 */
async function probeWriteCapability(targetPath, allowedRoots) {
  const guard = guardPath(targetPath, allowedRoots);

  if (!guard.allowed) {
    return createObservation(
      'fs.access(W_OK)',
      { path: targetPath },
      {},
      'denied',
      guard.reason
    );
  }

  try {
    await fs.access(targetPath, fsConstants.W_OK);
    return createObservation(
      'fs.access(W_OK)',
      { path: targetPath },
      { writable: true },
      'allowed'
    );
  } catch (err) {
    return createObservation(
      'fs.access(W_OK)',
      { path: targetPath },
      { writable: false },
      'allowed',
      undefined,
      err.message
    );
  }
}

/**
 * Probe: Symlink creation and following
 *
 * @param {string} outDir - Output directory
 * @param {string[]} allowedRoots - Allowed roots
 * @returns {Promise<object>} Observation
 */
async function probeSymlinkBehavior(outDir, allowedRoots) {
  const guard = guardPath(outDir, allowedRoots);

  if (!guard.allowed) {
    return createObservation(
      'fs.symlink',
      { outDir },
      {},
      'denied',
      guard.reason
    );
  }

  const target = path.join(outDir, 'probe-symlink-target.txt');
  const link = path.join(outDir, 'probe-symlink-link.txt');

  try {
    // Create target file
    await fs.writeFile(target, 'symlink test', 'utf8');

    // Create symlink
    await fs.symlink(target, link);

    // Check if can follow symlink
    const stats = await fs.stat(link);
    const linkStats = await fs.lstat(link);

    const outputs = {
      canCreateSymlink: true,
      canFollowSymlink: stats.isFile(),
      isSymbolicLink: linkStats.isSymbolicLink()
    };

    // Cleanup
    await fs.unlink(link);
    await fs.unlink(target);

    return createObservation(
      'fs.symlink',
      { target, link },
      outputs,
      'allowed'
    );
  } catch (err) {
    // Cleanup on error
    try {
      await fs.unlink(link).catch(() => {});
      await fs.unlink(target).catch(() => {});
    } catch {}

    return createObservation(
      'fs.symlink',
      { target, link },
      { canCreateSymlink: false },
      'allowed',
      undefined,
      err.message
    );
  }
}

/**
 * Probe: Directory traversal
 *
 * @param {string} rootPath - Root to traverse
 * @param {string[]} allowedRoots - Allowed roots
 * @param {number} maxDepth - Maximum depth
 * @returns {Promise<object>} Observation
 */
async function probeDirectoryTraversal(rootPath, allowedRoots, maxDepth = 3) {
  const guard = guardPath(rootPath, allowedRoots);

  if (!guard.allowed) {
    return createObservation(
      'fs.readdir(recursive)',
      { path: rootPath, maxDepth },
      {},
      'denied',
      guard.reason
    );
  }

  try {
    let fileCount = 0;
    let dirCount = 0;
    let maxDepthReached = 0;

    async function traverse(dir, depth) {
      if (depth > maxDepth) return;
      maxDepthReached = Math.max(maxDepthReached, depth);

      const entries = await fs.readdir(dir, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = path.join(dir, entry.name);

        // Check guard for each entry
        const entryGuard = guardPath(fullPath, allowedRoots);
        if (!entryGuard.allowed) continue;

        if (entry.isDirectory()) {
          dirCount++;
          await traverse(fullPath, depth + 1);
        } else if (entry.isFile()) {
          fileCount++;
        }
      }
    }

    await traverse(rootPath, 0);

    return createObservation(
      'fs.readdir(recursive)',
      { path: rootPath, maxDepth },
      { fileCount, dirCount, maxDepthReached, canTraverse: true },
      'allowed'
    );
  } catch (err) {
    return createObservation(
      'fs.readdir(recursive)',
      { path: rootPath, maxDepth },
      { canTraverse: false },
      'allowed',
      undefined,
      err.message
    );
  }
}

/**
 * Probe: File size and count constraints (quota detection)
 *
 * @param {string} outDir - Output directory
 * @param {string[]} allowedRoots - Allowed roots
 * @returns {Promise<object>} Observation
 */
async function probeQuotaConstraints(outDir, allowedRoots) {
  const guard = guardPath(outDir, allowedRoots);

  if (!guard.allowed) {
    return createObservation(
      'quota.detect',
      { outDir },
      {},
      'denied',
      guard.reason
    );
  }

  const sizes = [1024, 10240, 102400]; // 1KB, 10KB, 100KB
  const results = {};

  for (const size of sizes) {
    const testFile = path.join(outDir, `probe-quota-${size}.bin`);
    const buffer = Buffer.alloc(size);

    try {
      const start = Date.now();
      await fs.writeFile(testFile, buffer);
      const writeTime = Date.now() - start;

      const stats = await fs.stat(testFile);
      results[`${size}B`] = {
        success: true,
        writeTimeMs: writeTime,
        actualSize: stats.size
      };

      await fs.unlink(testFile);
    } catch (err) {
      results[`${size}B`] = {
        success: false,
        error: err.message
      };
    }
  }

  return createObservation(
    'quota.detect',
    { outDir, testSizes: sizes },
    results,
    'allowed'
  );
}

/**
 * Probe: Atomic operations (rename, unlink)
 *
 * @param {string} outDir - Output directory
 * @param {string[]} allowedRoots - Allowed roots
 * @returns {Promise<object>} Observation
 */
async function probeAtomicOperations(outDir, allowedRoots) {
  const guard = guardPath(outDir, allowedRoots);

  if (!guard.allowed) {
    return createObservation(
      'atomic.operations',
      { outDir },
      {},
      'denied',
      guard.reason
    );
  }

  const source = path.join(outDir, 'probe-atomic-source.txt');
  const dest = path.join(outDir, 'probe-atomic-dest.txt');

  try {
    // Write source file
    await fs.writeFile(source, 'atomic test', 'utf8');

    // Test atomic rename
    const renameStart = Date.now();
    await fs.rename(source, dest);
    const renameTime = Date.now() - renameStart;

    // Verify source gone, dest exists
    const sourceExists = await fs.access(source).then(() => true).catch(() => false);
    const destExists = await fs.access(dest).then(() => true).catch(() => false);

    // Test atomic unlink
    const unlinkStart = Date.now();
    await fs.unlink(dest);
    const unlinkTime = Date.now() - unlinkStart;

    const destExistsAfter = await fs.access(dest).then(() => true).catch(() => false);

    return createObservation(
      'atomic.operations',
      { source, dest },
      {
        renameAtomic: !sourceExists && destExists,
        renameTimeMs: renameTime,
        unlinkAtomic: !destExistsAfter,
        unlinkTimeMs: unlinkTime
      },
      'allowed'
    );
  } catch (err) {
    // Cleanup
    try {
      await fs.unlink(source).catch(() => {});
      await fs.unlink(dest).catch(() => {});
    } catch {}

    return createObservation(
      'atomic.operations',
      { source, dest },
      { atomicSupport: false },
      'allowed',
      undefined,
      err.message
    );
  }
}

/**
 * Probe: Path normalization behavior
 *
 * @param {string} rootPath - Root path
 * @param {string[]} allowedRoots - Allowed roots
 * @returns {Promise<object>} Observation
 */
async function probePathNormalization(rootPath, allowedRoots) {
  const guard = guardPath(rootPath, allowedRoots);

  if (!guard.allowed) {
    return createObservation(
      'path.normalization',
      { rootPath },
      {},
      'denied',
      guard.reason
    );
  }

  const testCases = {
    absolute: path.resolve(rootPath),
    relative: path.relative(process.cwd(), rootPath),
    normalized: path.normalize(rootPath),
    withDots: path.normalize(path.join(rootPath, '..', path.basename(rootPath))),
    withDoubleSlash: rootPath.replace(/\//, '//')
  };

  const results = {};

  for (const [name, testPath] of Object.entries(testCases)) {
    try {
      const resolved = path.resolve(testPath);
      const exists = await fs.access(resolved).then(() => true).catch(() => false);

      results[name] = {
        input: testPath,
        resolved,
        exists,
        sameAsRoot: resolved === path.resolve(rootPath)
      };
    } catch (err) {
      results[name] = {
        input: testPath,
        error: err.message
      };
    }
  }

  return createObservation(
    'path.normalization',
    { rootPath, testCases: Object.keys(testCases) },
    results,
    'allowed'
  );
}

/**
 * Probe: Temp directory access
 *
 * @param {string[]} allowedRoots - Allowed roots
 * @returns {Promise<object>} Observation
 */
async function probeTempDirectory(allowedRoots) {
  const tmpdir = os.tmpdir();
  const guard = guardPath(tmpdir, allowedRoots);

  // Special case: temp directory is usually allowed
  // But we still check if it's in roots or add special permission
  const effectiveGuard = guard.allowed || allowedRoots.length === 0
    ? { allowed: true }
    : guard;

  if (!effectiveGuard.allowed) {
    return createObservation(
      'tmpdir.access',
      { tmpdir },
      {},
      'denied',
      effectiveGuard.reason
    );
  }

  const testFile = path.join(tmpdir, `kgc-probe-${Date.now()}.txt`);

  try {
    await fs.writeFile(testFile, 'temp test', 'utf8');
    const stats = await fs.stat(testFile);
    await fs.unlink(testFile);

    return createObservation(
      'tmpdir.access',
      { tmpdir },
      {
        tmpdir,
        writable: true,
        testFileSize: stats.size
      },
      'allowed'
    );
  } catch (err) {
    return createObservation(
      'tmpdir.access',
      { tmpdir },
      { writable: false },
      'allowed',
      undefined,
      err.message
    );
  }
}

/**
 * Probe: File locking semantics (if observable)
 *
 * @param {string} outDir - Output directory
 * @param {string[]} allowedRoots - Allowed roots
 * @returns {Promise<object>} Observation
 */
async function probeFileLocking(outDir, allowedRoots) {
  const guard = guardPath(outDir, allowedRoots);

  if (!guard.allowed) {
    return createObservation(
      'file.locking',
      { outDir },
      {},
      'denied',
      guard.reason
    );
  }

  const testFile = path.join(outDir, 'probe-lock-test.txt');

  try {
    // Create file
    await fs.writeFile(testFile, 'lock test', 'utf8');

    // Try to open file exclusively (if supported)
    // Node.js doesn't have direct locking, but we can test concurrent access
    const handle1 = await fs.open(testFile, 'r+');
    const handle2 = await fs.open(testFile, 'r+');

    // Both handles open - no exclusive locking
    await handle1.close();
    await handle2.close();

    await fs.unlink(testFile);

    return createObservation(
      'file.locking',
      { testFile },
      {
        exclusiveLocking: false,
        concurrentAccess: true
      },
      'allowed'
    );
  } catch (err) {
    try {
      await fs.unlink(testFile).catch(() => {});
    } catch {}

    return createObservation(
      'file.locking',
      { testFile },
      { lockingObservable: false },
      'allowed',
      undefined,
      err.message
    );
  }
}

/**
 * Main probe function: Filesystem capabilities
 *
 * @param {object} config - Configuration
 * @param {string[]} config.roots - Allowed root paths
 * @param {string} config.out - Output directory for temp files
 * @param {number} [config.budgetMs=5000] - Timeout budget in milliseconds
 * @returns {Promise<object[]>} Array of observations
 */
export async function probeFilesystem(config) {
  // Validate config
  const validConfig = ConfigSchema.parse(config);

  const { roots, out, budgetMs } = validConfig;

  // Create timeout promise
  const timeout = new Promise((_, reject) =>
    setTimeout(() => reject(new Error(`Probe timeout after ${budgetMs}ms`)), budgetMs)
  );

  try {
    // Ensure output directory exists and is allowed
    const outGuard = guardPath(out, roots);
    if (!outGuard.allowed) {
      return [
        createObservation(
          'probeFilesystem',
          { config: validConfig },
          {},
          'denied',
          `Output directory not in allowed roots: ${outGuard.reason}`
        )
      ];
    }

    await fs.mkdir(out, { recursive: true });

    // Run all probes
    const probes = Promise.all([
      probeReadCapability(roots[0], roots),
      probeWriteCapability(out, roots),
      probeSymlinkBehavior(out, roots),
      probeDirectoryTraversal(roots[0], roots, 2),
      probeQuotaConstraints(out, roots),
      probeAtomicOperations(out, roots),
      probePathNormalization(roots[0], roots),
      probeTempDirectory(roots),
      probeFileLocking(out, roots)
    ]);

    const observations = await Promise.race([probes, timeout]);

    return observations;
  } catch (err) {
    return [
      createObservation(
        'probeFilesystem',
        { config: validConfig },
        {},
        'allowed',
        undefined,
        err.message
      )
    ];
  }
}

/**
 * Export guard function for external use
 */
export { guardPath };
