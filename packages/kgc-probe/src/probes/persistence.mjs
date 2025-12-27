/**
 * @file Persistence Probe - Output persistence and storage characteristics
 * @module @unrdf/kgc-probe/probes/persistence
 *
 * @description
 * Probes output persistence and storage characteristics:
 * - Output persistence across runs (write, read, verify)
 * - Quota behavior (write until failure, record limit)
 * - Temp directory behavior (location, cleanup, permissions)
 * - File locking semantics (if observable via flock or exclusive open)
 * - Directory permissions (can create, rename, delete)
 * - Atomic operations (rename, link)
 * - Storage type detection (in-memory, disk, network if detectable)
 *
 * GUARD CONSTRAINTS:
 * - ONLY write within config.out directory
 * - NO access to system directories
 * - Clean up test files after probing
 * - Limit quota test to 100MB max
 * - Timeout operations (5s per operation)
 */

import { promises as fs } from 'node:fs';
import { join, resolve, dirname } from 'node:path';
import { tmpdir } from 'node:os';
import { fileURLToPath } from 'node:url';
import { ObservationSchema, ProbeConfigSchema } from '../types.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// =============================================================================
// Guard Functions
// =============================================================================

/**
 * Check if path is within allowed output directory
 * @param {string} path - Path to check
 * @param {string} outDir - Allowed output directory
 * @returns {Object} Guard decision
 */
function guardPathAccess(path, outDir) {
  const resolvedPath = resolve(path);
  const resolvedOutDir = resolve(outDir);

  const allowed = resolvedPath.startsWith(resolvedOutDir);

  return {
    path: resolvedPath,
    allowed,
    reason: allowed
      ? 'Within config.out directory'
      : `Outside config.out directory (${resolvedOutDir})`,
    policy: 'output-only',
    timestamp: Date.now(),
  };
}

/**
 * Create observation with guard decision
 * @param {string} probeName - Name of probe
 * @param {string} category - Observation category
 * @param {string} observation - Observation description
 * @param {any} value - Observed value
 * @param {Object} guardDecision - Guard decision
 * @param {Object} metadata - Additional metadata
 * @returns {Object} Validated observation
 */
function createObservation(probeName, category, observation, value, guardDecision, metadata = {}) {
  const obs = {
    probeName,
    timestamp: Date.now(),
    category,
    observation,
    value,
    guardDecision,
    metadata,
  };

  // Validate before returning
  const validated = ObservationSchema.parse(obs);
  return validated;
}

/**
 * Create error observation
 * @param {string} probeName - Name of probe
 * @param {string} category - Observation category
 * @param {string} observation - Observation description
 * @param {Error} error - Error object
 * @param {Object} guardDecision - Guard decision
 * @returns {Object} Validated observation
 */
function createErrorObservation(probeName, category, observation, error, guardDecision) {
  return createObservation(
    probeName,
    category,
    observation,
    null,
    guardDecision,
    {
      error: {
        message: error.message,
        code: error.code,
        stack: error.stack,
      },
    }
  );
}

// =============================================================================
// Persistence Probe Functions
// =============================================================================

/**
 * Test basic write/read/verify persistence
 * @param {Object} config - Probe configuration
 * @returns {Promise<Object[]>} Observations
 */
async function probeBasicPersistence(config) {
  const observations = [];
  const testFile = join(config.out, 'persistence-test.txt');
  const testContent = `Persistence test at ${Date.now()}`;

  const guardDecision = guardPathAccess(testFile, config.out);

  if (!guardDecision.allowed) {
    observations.push(
      createObservation(
        'persistence',
        'security',
        'Basic persistence test blocked by guard',
        false,
        guardDecision
      )
    );
    return observations;
  }

  try {
    // Write
    const writeStart = Date.now();
    await fs.writeFile(testFile, testContent, 'utf8');
    const writeTime = Date.now() - writeStart;

    observations.push(
      createObservation(
        'persistence',
        'storage',
        'Write operation successful',
        true,
        guardDecision,
        { writeTime, fileSize: testContent.length }
      )
    );

    // Read
    const readStart = Date.now();
    const readContent = await fs.readFile(testFile, 'utf8');
    const readTime = Date.now() - readStart;

    const contentMatches = readContent === testContent;
    observations.push(
      createObservation(
        'persistence',
        'storage',
        'Read operation and content verification',
        contentMatches,
        guardDecision,
        { readTime, contentMatches }
      )
    );

    // Check file stats
    const stats = await fs.stat(testFile);
    observations.push(
      createObservation(
        'persistence',
        'filesystem',
        'File metadata accessible',
        true,
        guardDecision,
        {
          size: stats.size,
          mode: stats.mode.toString(8),
          created: stats.birthtime.getTime(),
          modified: stats.mtime.getTime(),
        }
      )
    );

    // Clean up
    await fs.unlink(testFile);
    observations.push(
      createObservation(
        'persistence',
        'filesystem',
        'File deletion successful',
        true,
        guardDecision
      )
    );
  } catch (error) {
    observations.push(createErrorObservation('persistence', 'storage', 'Basic persistence test failed', error, guardDecision));
  }

  return observations;
}

/**
 * Test persistence across runs by checking for marker file
 * @param {Object} config - Probe configuration
 * @returns {Promise<Object[]>} Observations
 */
async function probeCrossRunPersistence(config) {
  const observations = [];
  const markerFile = join(config.out, '.persistence-marker');

  const guardDecision = guardPathAccess(markerFile, config.out);

  if (!guardDecision.allowed) {
    observations.push(
      createObservation(
        'persistence',
        'security',
        'Cross-run persistence test blocked by guard',
        false,
        guardDecision
      )
    );
    return observations;
  }

  try {
    let markerExists = false;
    let previousTimestamp = null;

    try {
      const content = await fs.readFile(markerFile, 'utf8');
      previousTimestamp = parseInt(content, 10);
      markerExists = true;
    } catch (error) {
      if (error.code !== 'ENOENT') {
        throw error;
      }
    }

    observations.push(
      createObservation(
        'persistence',
        'storage',
        'Persistence marker from previous run',
        markerExists,
        guardDecision,
        { previousTimestamp, markerExists }
      )
    );

    // Write new marker
    const currentTimestamp = Date.now();
    await fs.writeFile(markerFile, currentTimestamp.toString(), 'utf8');

    observations.push(
      createObservation(
        'persistence',
        'storage',
        'Persistence marker written for next run',
        true,
        guardDecision,
        { currentTimestamp }
      )
    );
  } catch (error) {
    observations.push(
      createErrorObservation('persistence', 'storage', 'Cross-run persistence test failed', error, guardDecision)
    );
  }

  return observations;
}

/**
 * Test quota limits by writing incrementally
 * @param {Object} config - Probe configuration
 * @returns {Promise<Object[]>} Observations
 */
async function probeQuotaLimits(config) {
  const observations = [];
  const quotaTestFile = join(config.out, 'quota-test.bin');

  const guardDecision = guardPathAccess(quotaTestFile, config.out);

  if (!guardDecision.allowed) {
    observations.push(
      createObservation(
        'persistence',
        'security',
        'Quota test blocked by guard',
        false,
        guardDecision
      )
    );
    return observations;
  }

  try {
    const chunkSize = config.chunkSize || 1024 * 1024; // 1 MB
    const maxQuota = config.maxQuota || 100 * 1024 * 1024; // 100 MB
    const chunk = Buffer.alloc(chunkSize, 'A');

    let bytesWritten = 0;
    let quotaReached = false;
    let quotaError = null;

    const startTime = Date.now();

    // Write chunks until we hit quota or max limit
    while (bytesWritten < maxQuota && !quotaReached) {
      try {
        await fs.appendFile(quotaTestFile, chunk);
        bytesWritten += chunkSize;

        // Check if file size matches expected
        const stats = await fs.stat(quotaTestFile);
        if (stats.size !== bytesWritten) {
          quotaReached = true;
          quotaError = `Size mismatch: expected ${bytesWritten}, got ${stats.size}`;
          break;
        }

        // Timeout check (don't spend more than config.timeout on quota test)
        if (Date.now() - startTime > config.timeout) {
          break;
        }
      } catch (error) {
        quotaReached = true;
        quotaError = error.message;
        break;
      }
    }

    const duration = Date.now() - startTime;

    observations.push(
      createObservation(
        'persistence',
        'quota',
        quotaReached ? 'Quota limit reached' : 'Quota test completed without hitting limit',
        bytesWritten,
        guardDecision,
        {
          bytesWritten,
          quotaReached,
          quotaError,
          duration,
          throughputMBps: bytesWritten / duration / 1024,
        }
      )
    );

    // Clean up
    try {
      await fs.unlink(quotaTestFile);
    } catch (error) {
      // Ignore cleanup errors
    }
  } catch (error) {
    observations.push(createErrorObservation('persistence', 'quota', 'Quota test failed', error, guardDecision));
  }

  return observations;
}

/**
 * Test directory permissions (create, rename, delete)
 * @param {Object} config - Probe configuration
 * @returns {Promise<Object[]>} Observations
 */
async function probeDirectoryPermissions(config) {
  const observations = [];
  const testDir = join(config.out, 'perm-test-dir');
  const renamedDir = join(config.out, 'perm-test-dir-renamed');

  const guardDecision = guardPathAccess(testDir, config.out);
  const guardDecisionRenamed = guardPathAccess(renamedDir, config.out);

  if (!guardDecision.allowed || !guardDecisionRenamed.allowed) {
    observations.push(
      createObservation(
        'persistence',
        'security',
        'Directory permissions test blocked by guard',
        false,
        guardDecision
      )
    );
    return observations;
  }

  try {
    // Create directory
    await fs.mkdir(testDir, { recursive: true });
    observations.push(
      createObservation(
        'persistence',
        'permissions',
        'Directory creation successful',
        true,
        guardDecision
      )
    );

    // Check directory stats
    const stats = await fs.stat(testDir);
    observations.push(
      createObservation(
        'persistence',
        'permissions',
        'Directory is accessible',
        stats.isDirectory(),
        guardDecision,
        { mode: stats.mode.toString(8) }
      )
    );

    // Rename directory
    await fs.rename(testDir, renamedDir);
    observations.push(
      createObservation(
        'persistence',
        'permissions',
        'Directory rename successful',
        true,
        guardDecisionRenamed
      )
    );

    // Delete directory
    await fs.rmdir(renamedDir);
    observations.push(
      createObservation(
        'persistence',
        'permissions',
        'Directory deletion successful',
        true,
        guardDecisionRenamed
      )
    );
  } catch (error) {
    observations.push(
      createErrorObservation('persistence', 'permissions', 'Directory permissions test failed', error, guardDecision)
    );

    // Cleanup on error
    try {
      await fs.rmdir(testDir).catch(() => {});
      await fs.rmdir(renamedDir).catch(() => {});
    } catch (e) {
      // Ignore cleanup errors
    }
  }

  return observations;
}

/**
 * Test atomic operations (rename, link if supported)
 * @param {Object} config - Probe configuration
 * @returns {Promise<Object[]>} Observations
 */
async function probeAtomicOperations(config) {
  const observations = [];
  const sourceFile = join(config.out, 'atomic-source.txt');
  const targetFile = join(config.out, 'atomic-target.txt');
  const linkFile = join(config.out, 'atomic-link.txt');

  const guardDecisionSource = guardPathAccess(sourceFile, config.out);
  const guardDecisionTarget = guardPathAccess(targetFile, config.out);
  const guardDecisionLink = guardPathAccess(linkFile, config.out);

  if (!guardDecisionSource.allowed || !guardDecisionTarget.allowed || !guardDecisionLink.allowed) {
    observations.push(
      createObservation(
        'persistence',
        'security',
        'Atomic operations test blocked by guard',
        false,
        guardDecisionSource
      )
    );
    return observations;
  }

  try {
    // Create source file
    await fs.writeFile(sourceFile, 'atomic test content', 'utf8');

    // Test atomic rename
    const renameStart = Date.now();
    await fs.rename(sourceFile, targetFile);
    const renameTime = Date.now() - renameStart;

    observations.push(
      createObservation(
        'persistence',
        'filesystem',
        'Atomic rename successful',
        true,
        guardDecisionTarget,
        { renameTime }
      )
    );

    // Test hard link (may not be supported on all filesystems)
    try {
      await fs.link(targetFile, linkFile);
      const linkStats = await fs.stat(linkFile);

      observations.push(
        createObservation(
          'persistence',
          'filesystem',
          'Hard link creation successful',
          true,
          guardDecisionLink,
          { nlink: linkStats.nlink }
        )
      );

      // Clean up link
      await fs.unlink(linkFile);
    } catch (error) {
      observations.push(
        createObservation(
          'persistence',
          'filesystem',
          'Hard link not supported or failed',
          false,
          guardDecisionLink,
          { error: error.message }
        )
      );
    }

    // Clean up
    await fs.unlink(targetFile);
  } catch (error) {
    observations.push(
      createErrorObservation('persistence', 'filesystem', 'Atomic operations test failed', error, guardDecisionSource)
    );

    // Cleanup on error
    try {
      await fs.unlink(sourceFile).catch(() => {});
      await fs.unlink(targetFile).catch(() => {});
      await fs.unlink(linkFile).catch(() => {});
    } catch (e) {
      // Ignore cleanup errors
    }
  }

  return observations;
}

/**
 * Test temp directory behavior
 * @param {Object} config - Probe configuration
 * @returns {Promise<Object[]>} Observations
 */
async function probeTempDirectory(config) {
  const observations = [];

  try {
    const tempDir = tmpdir();

    // We don't write to temp dir (guard constraint), just observe it
    const guardDecision = {
      path: tempDir,
      allowed: false,
      reason: 'System temp directory - read-only observation',
      policy: 'observe-only',
      timestamp: Date.now(),
    };

    observations.push(
      createObservation(
        'persistence',
        'filesystem',
        'System temp directory location',
        tempDir,
        guardDecision
      )
    );

    // Try to check if temp dir is accessible (read-only)
    try {
      const stats = await fs.stat(tempDir);
      observations.push(
        createObservation(
          'persistence',
          'filesystem',
          'System temp directory accessible for reading',
          true,
          guardDecision,
          { mode: stats.mode.toString(8) }
        )
      );
    } catch (error) {
      observations.push(
        createObservation(
          'persistence',
          'filesystem',
          'System temp directory not accessible',
          false,
          guardDecision,
          { error: error.message }
        )
      );
    }
  } catch (error) {
    const guardDecision = {
      path: 'tmpdir()',
      allowed: false,
      reason: 'System temp directory access failed',
      policy: 'observe-only',
      timestamp: Date.now(),
    };

    observations.push(
      createErrorObservation('persistence', 'filesystem', 'Temp directory probe failed', error, guardDecision)
    );
  }

  return observations;
}

/**
 * Detect storage type (heuristic based on performance and behavior)
 * @param {Object} config - Probe configuration
 * @returns {Promise<Object[]>} Observations
 */
async function probeStorageType(config) {
  const observations = [];
  const testFile = join(config.out, 'storage-type-test.bin');

  const guardDecision = guardPathAccess(testFile, config.out);

  if (!guardDecision.allowed) {
    observations.push(
      createObservation(
        'persistence',
        'security',
        'Storage type detection blocked by guard',
        false,
        guardDecision
      )
    );
    return observations;
  }

  try {
    // Write a small file and measure latency
    const testData = Buffer.alloc(4096, 'X');
    const iterations = 10;
    const latencies = [];

    for (let i = 0; i < iterations; i++) {
      const start = Date.now();
      await fs.writeFile(testFile, testData);
      const handle = await fs.open(testFile, 'r+');
      try {
        await handle.sync();
      } finally {
        await handle.close();
      }
      latencies.push(Date.now() - start);
    }

    const avgLatency = latencies.reduce((a, b) => a + b, 0) / latencies.length;
    const minLatency = Math.min(...latencies);
    const maxLatency = Math.max(...latencies);

    // Heuristic: in-memory < 1ms, SSD < 5ms, HDD > 5ms
    let storageType = 'unknown';
    if (avgLatency < 1) {
      storageType = 'in-memory (likely tmpfs or ramfs)';
    } else if (avgLatency < 5) {
      storageType = 'fast-storage (likely SSD or cached)';
    } else {
      storageType = 'slow-storage (likely HDD or network)';
    }

    observations.push(
      createObservation(
        'persistence',
        'performance',
        'Storage type detection (heuristic)',
        storageType,
        guardDecision,
        {
          avgLatency,
          minLatency,
          maxLatency,
          latencies,
        }
      )
    );

    // Clean up
    await fs.unlink(testFile);
  } catch (error) {
    observations.push(
      createErrorObservation('persistence', 'performance', 'Storage type detection failed', error, guardDecision)
    );
  }

  return observations;
}

// =============================================================================
// Main Probe Function
// =============================================================================

/**
 * Probe output persistence and storage characteristics
 *
 * @param {Object} config - Probe configuration
 * @param {string} config.out - Output directory (REQUIRED)
 * @param {number} [config.timeout=5000] - Timeout per operation (ms)
 * @param {number} [config.maxQuota=104857600] - Max quota to test (bytes, default 100MB)
 * @param {number} [config.chunkSize=1048576] - Chunk size for quota test (bytes, default 1MB)
 * @returns {Promise<Object[]>} Array of observations
 *
 * @example
 * const observations = await probePersistence({
 *   out: '/home/user/output',
 *   timeout: 5000,
 *   maxQuota: 100 * 1024 * 1024
 * });
 */
export async function probePersistence(config) {
  // Validate config
  const validatedConfig = ProbeConfigSchema.parse(config);

  const observations = [];

  try {
    // Ensure output directory exists
    await fs.mkdir(validatedConfig.out, { recursive: true });

    // Run all persistence probes
    const results = await Promise.all([
      probeBasicPersistence(validatedConfig),
      probeCrossRunPersistence(validatedConfig),
      probeQuotaLimits(validatedConfig),
      probeDirectoryPermissions(validatedConfig),
      probeAtomicOperations(validatedConfig),
      probeTempDirectory(validatedConfig),
      probeStorageType(validatedConfig),
    ]);

    // Flatten results
    results.forEach(result => observations.push(...result));
  } catch (error) {
    const guardDecision = {
      path: validatedConfig.out,
      allowed: false,
      reason: 'Probe execution failed',
      policy: 'error',
      timestamp: Date.now(),
    };

    observations.push(
      createErrorObservation('persistence', 'storage', 'Persistence probe failed', error, guardDecision)
    );
  }

  return observations;
}

// =============================================================================
// Module Exports
// =============================================================================

export default probePersistence;
