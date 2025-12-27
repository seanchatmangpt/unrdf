/**
 * @file Materialization applier - execute file write plans with transactions
 * @module project-engine/materialize-apply
 */

import { z } from 'zod';
import { promises as fs } from 'fs';
import path from 'path';
import { createHash } from 'crypto';
import { scanFileSystemToStore } from './fs-scan.mjs';
import { diffProjectStructure } from './project-diff.mjs';
import { createStore } from '@unrdf/oxigraph';

/**
 * @typedef {import('./materialize-plan.mjs').MaterializationPlan} MaterializationPlan
 * @typedef {import('./materialize-plan.mjs').WriteOperation} WriteOperation
 * @typedef {import('./materialize-plan.mjs').UpdateOperation} UpdateOperation
 * @typedef {import('./materialize-plan.mjs').DeleteOperation} DeleteOperation
 */

/**
 * @typedef {Object} ApplyResult
 * @property {number} appliedCount - Number of operations applied
 * @property {number} skippedCount - Number of operations skipped
 * @property {string[]} writtenPaths - Paths that were written
 * @property {string[]} updatedPaths - Paths that were updated
 * @property {string[]} deletedPaths - Paths that were deleted
 * @property {string[]} errors - Any errors encountered
 */

/**
 * @typedef {Object} ApplyReceipt
 * @property {string} planHash - Hash of the plan that was applied
 * @property {string} beforeHash - Hash of FS state before
 * @property {string} afterHash - Hash of FS state after
 * @property {Object} fsDiff - Diff between before/after FS graphs
 * @property {string} timestamp - ISO timestamp
 * @property {boolean} success - Whether all operations succeeded
 */

/**
 * @typedef {Object} ApplyOutput
 * @property {ApplyResult} result
 * @property {ApplyReceipt} receipt
 */

const ApplyOptionsSchema = z.object({
  dryRun: z.boolean().default(false),
  createDirectories: z.boolean().default(true),
  validateHashes: z.boolean().default(true),
  outputRoot: z.string().default('.'),
  snapshotBefore: z.boolean().default(true),
  snapshotAfter: z.boolean().default(true),
});

const WriteOperationSchema = z.object({
  path: z.string(),
  content: z.string(),
  hash: z.string(),
  templateIri: z.string(),
  entityIri: z.string(),
  entityType: z.string(),
});

const UpdateOperationSchema = z.object({
  path: z.string(),
  content: z.string(),
  oldHash: z.string(),
  newHash: z.string(),
  templateIri: z.string(),
  entityIri: z.string(),
});

const DeleteOperationSchema = z.object({
  path: z.string(),
  hash: z.string(),
  reason: z.string(),
});

const MaterializationPlanSchema = z.object({
  writes: z.array(WriteOperationSchema),
  updates: z.array(UpdateOperationSchema),
  deletes: z.array(DeleteOperationSchema),
});

/**
 * Compute SHA256 hash of content
 *
 * @param {string} content
 * @returns {string}
 */
function hashContent(content) {
  return createHash('sha256').update(content).digest('hex');
}

/**
 * Ensure directory exists for a file path
 *
 * @param {string} filePath
 * @returns {Promise<void>}
 */
async function ensureDirectoryExists(filePath) {
  const dir = path.dirname(filePath);
  await fs.mkdir(dir, { recursive: true });
}

/**
 * Read file and compute hash, returns null if file doesn't exist
 *
 * @param {string} filePath
 * @returns {Promise<{content: string, hash: string} | null>}
 */
async function readFileWithHash(filePath) {
  try {
    const content = await fs.readFile(filePath, 'utf-8');
    return {
      content,
      hash: hashContent(content),
    };
  } catch (err) {
    if (err.code === 'ENOENT') return null;
    throw err;
  }
}

/**
 * Apply a single write operation
 *
 * @param {WriteOperation} op
 * @param {Object} options
 * @param {boolean} options.dryRun
 * @param {boolean} options.createDirectories
 * @param {string} options.outputRoot
 * @returns {Promise<{success: boolean, error?: string}>}
 */
async function applyWrite(op, options) {
  const fullPath = path.resolve(options.outputRoot, op.path);

  // Check if file already exists
  const existing = await readFileWithHash(fullPath);
  if (existing !== null) {
    return {
      success: false,
      error: `File already exists: ${op.path}`,
    };
  }

  if (options.dryRun) {
    return { success: true };
  }

  if (options.createDirectories) {
    await ensureDirectoryExists(fullPath);
  }

  await fs.writeFile(fullPath, op.content, 'utf-8');
  return { success: true };
}

/**
 * Apply a single update operation
 *
 * @param {UpdateOperation} op
 * @param {Object} options
 * @param {boolean} options.dryRun
 * @param {boolean} options.validateHashes
 * @param {string} options.outputRoot
 * @returns {Promise<{success: boolean, error?: string}>}
 */
async function applyUpdate(op, options) {
  const fullPath = path.resolve(options.outputRoot, op.path);

  // Read existing file
  const existing = await readFileWithHash(fullPath);

  if (existing === null) {
    return {
      success: false,
      error: `File not found for update: ${op.path}`,
    };
  }

  // Validate hash if required
  if (options.validateHashes && existing.hash !== op.oldHash) {
    return {
      success: false,
      error: `Hash mismatch for ${op.path}: expected ${op.oldHash}, got ${existing.hash}`,
    };
  }

  if (options.dryRun) {
    return { success: true };
  }

  await fs.writeFile(fullPath, op.content, 'utf-8');
  return { success: true };
}

/**
 * Apply a single delete operation
 *
 * @param {DeleteOperation} op
 * @param {Object} options
 * @param {boolean} options.dryRun
 * @param {boolean} options.validateHashes
 * @param {string} options.outputRoot
 * @returns {Promise<{success: boolean, error?: string}>}
 */
async function applyDelete(op, options) {
  const fullPath = path.resolve(options.outputRoot, op.path);

  // Read existing file
  const existing = await readFileWithHash(fullPath);

  if (existing === null) {
    // File already doesn't exist - skip
    return { success: true };
  }

  // Validate hash if required
  if (options.validateHashes && existing.hash !== op.hash) {
    return {
      success: false,
      error: `Hash mismatch for delete ${op.path}: expected ${op.hash}, got ${existing.hash}`,
    };
  }

  if (options.dryRun) {
    return { success: true };
  }

  await fs.unlink(fullPath);
  return { success: true };
}

/**
 * Create FS snapshot using scanFileSystemToStore
 *
 * @param {string} root
 * @returns {Promise<{store: Store, hash: string}>}
 */
async function snapshotFileSystem(root) {
  try {
    const { store, summary } = await scanFileSystemToStore({
      root,
      baseIri: 'http://example.org/unrdf/materialize#',
    });

    const hash = createHash('sha256')
      .update(`files:${summary.fileCount}|folders:${summary.folderCount}`)
      .digest('hex')
      .substring(0, 16);

    return { store, hash };
  } catch (err) {
    // If directory doesn't exist, return empty store
    const { Store } = await import('n3');
    return {
      store: await createStore(),
      hash: 'empty',
    };
  }
}

/**
 * Apply a materialization plan
 *
 * Execution flow:
 * 1. Validate plan (check hashes match current state)
 * 2. Snapshot before with scanFileSystemToStore
 * 3. For each write in plan:
 *    - Check file doesn't exist (or is in updates list)
 *    - Write file with template content
 * 4. For each update in plan:
 *    - Validate existing hash matches
 *    - Write new content
 * 5. For each delete in plan:
 *    - Validate hash matches
 *    - Delete file
 * 6. Snapshot after with scanFileSystemToStore
 * 7. Compute diff using diffProjectStructure
 * 8. Return result + receipt
 *
 * @param {MaterializationPlan} plan - The plan to apply
 * @param {Object} [options] - Apply options
 * @param {boolean} [options.dryRun] - If true, don't actually write files
 * @param {boolean} [options.createDirectories] - Create parent directories if needed
 * @param {boolean} [options.validateHashes] - Validate file hashes before updates/deletes
 * @param {string} [options.outputRoot] - Base directory for file operations
 * @param {boolean} [options.snapshotBefore] - Take FS snapshot before applying
 * @param {boolean} [options.snapshotAfter] - Take FS snapshot after applying
 * @returns {Promise<ApplyOutput>}
 */
export async function applyMaterializationPlan(plan, options = {}) {
  const opts = ApplyOptionsSchema.parse(options);
  const validatedPlan = MaterializationPlanSchema.parse(plan);

  /** @type {ApplyResult} */
  const result = {
    appliedCount: 0,
    skippedCount: 0,
    writtenPaths: [],
    updatedPaths: [],
    deletedPaths: [],
    errors: [],
  };

  // Snapshot before
  let beforeSnapshot = null;
  if (opts.snapshotBefore) {
    beforeSnapshot = await snapshotFileSystem(opts.outputRoot);
  }

  // Apply writes
  for (const op of validatedPlan.writes) {
    const writeResult = await applyWrite(op, opts);
    if (writeResult.success) {
      result.appliedCount++;
      result.writtenPaths.push(op.path);
    } else {
      result.skippedCount++;
      result.errors.push(writeResult.error);
    }
  }

  // Apply updates
  for (const op of validatedPlan.updates) {
    const updateResult = await applyUpdate(op, opts);
    if (updateResult.success) {
      result.appliedCount++;
      result.updatedPaths.push(op.path);
    } else {
      result.skippedCount++;
      result.errors.push(updateResult.error);
    }
  }

  // Apply deletes
  for (const op of validatedPlan.deletes) {
    const deleteResult = await applyDelete(op, opts);
    if (deleteResult.success) {
      result.appliedCount++;
      result.deletedPaths.push(op.path);
    } else {
      result.skippedCount++;
      result.errors.push(deleteResult.error);
    }
  }

  // Snapshot after
  let afterSnapshot = null;
  let fsDiff = null;
  if (opts.snapshotAfter && !opts.dryRun) {
    afterSnapshot = await snapshotFileSystem(opts.outputRoot);

    // Compute diff if we have both snapshots
    if (beforeSnapshot && afterSnapshot) {
      try {
        fsDiff = diffProjectStructure({
          actualStore: afterSnapshot.store,
          goldenStore: beforeSnapshot.store,
        });
      } catch (err) {
        // Diff failed, but apply succeeded
        fsDiff = { error: err.message };
      }
    }
  }

  // Create plan hash
  const planHash = createHash('sha256')
    .update(
      JSON.stringify({
        writes: validatedPlan.writes.length,
        updates: validatedPlan.updates.length,
        deletes: validatedPlan.deletes.length,
      })
    )
    .digest('hex')
    .substring(0, 16);

  /** @type {ApplyReceipt} */
  const receipt = {
    planHash,
    beforeHash: beforeSnapshot?.hash || 'none',
    afterHash: afterSnapshot?.hash || 'none',
    fsDiff: fsDiff || {},
    timestamp: new Date().toISOString(),
    success: result.errors.length === 0,
  };

  return { result, receipt };
}

/**
 * Rollback a materialization by deleting written files
 *
 * @param {ApplyResult} result - Result from a previous apply
 * @param {Object} [options]
 * @param {string} [options.outputRoot] - Base directory
 * @returns {Promise<{rolledBack: string[], errors: string[]}>}
 */
export async function rollbackMaterialization(result, options = {}) {
  const outputRoot = options.outputRoot || '.';
  const rolledBack = [];
  const errors = [];

  // Delete written files
  for (const filePath of result.writtenPaths) {
    const fullPath = path.resolve(outputRoot, filePath);
    try {
      await fs.unlink(fullPath);
      rolledBack.push(filePath);
    } catch (err) {
      if (err.code !== 'ENOENT') {
        errors.push(`Failed to rollback ${filePath}: ${err.message}`);
      }
    }
  }

  // Note: We can't easily rollback updates or restore deleted files
  // without having stored the original content

  return { rolledBack, errors };
}

/**
 * Preview a plan without applying
 *
 * @param {MaterializationPlan} plan
 * @returns {Object} Summary of what would be done
 */
export function previewPlan(plan) {
  const validatedPlan = MaterializationPlanSchema.parse(plan);

  return {
    totalOperations:
      validatedPlan.writes.length + validatedPlan.updates.length + validatedPlan.deletes.length,
    writes: validatedPlan.writes.map(op => ({
      path: op.path,
      templateIri: op.templateIri,
      entityIri: op.entityIri,
    })),
    updates: validatedPlan.updates.map(op => ({
      path: op.path,
      templateIri: op.templateIri,
    })),
    deletes: validatedPlan.deletes.map(op => ({
      path: op.path,
      reason: op.reason,
    })),
  };
}

/**
 * Check if a plan can be safely applied
 *
 * Validates:
 * - No files exist for write operations
 * - All files exist for update operations
 * - Hash matches for update/delete operations
 *
 * @param {MaterializationPlan} plan
 * @param {Object} [options]
 * @param {string} [options.outputRoot] - Base directory
 * @returns {Promise<{canApply: boolean, issues: string[]}>}
 */
export async function checkPlanApplicability(plan, options = {}) {
  const outputRoot = options.outputRoot || '.';
  const validatedPlan = MaterializationPlanSchema.parse(plan);
  const issues = [];

  // Check writes don't conflict
  for (const op of validatedPlan.writes) {
    const fullPath = path.resolve(outputRoot, op.path);
    const existing = await readFileWithHash(fullPath);
    if (existing !== null) {
      issues.push(`Write conflict: ${op.path} already exists`);
    }
  }

  // Check updates exist and match
  for (const op of validatedPlan.updates) {
    const fullPath = path.resolve(outputRoot, op.path);
    const existing = await readFileWithHash(fullPath);
    if (existing === null) {
      issues.push(`Update target missing: ${op.path}`);
    } else if (existing.hash !== op.oldHash) {
      issues.push(`Update hash mismatch: ${op.path}`);
    }
  }

  // Check deletes exist and match
  for (const op of validatedPlan.deletes) {
    const fullPath = path.resolve(outputRoot, op.path);
    const existing = await readFileWithHash(fullPath);
    if (existing !== null && existing.hash !== op.hash) {
      issues.push(`Delete hash mismatch: ${op.path}`);
    }
  }

  return {
    canApply: issues.length === 0,
    issues,
  };
}
