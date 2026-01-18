/**
 * Impact Set Computation for Capsule Deltas
 *
 * Analyzes what resources are affected by a delta operation.
 * Computes reads, writes, and deletes with transitive impacts.
 *
 * @module agent-6/impact-set
 */

/**
 * @typedef {Object} ImpactSet
 * @property {Set<string>} reads - Resources that are read
 * @property {Set<string>} writes - Resources that are written
 * @property {Set<string>} deletes - Resources that are deleted
 */

/**
 * @typedef {Object} Delta
 * @property {string} type - Delta operation type
 * @property {string} [path] - Resource path
 * @property {string[]} [fields] - Fields affected
 * @property {Object} [data] - Delta data
 * @property {Delta[]} [deltas] - Nested deltas (for composite operations)
 */

/**
 * Normalize a path by removing trailing slashes and ensuring leading slash
 * @param {string} path - Path to normalize
 * @returns {string} Normalized path
 */
function normalizePath(path) {
  if (!path) return '/';
  let normalized = path.trim();
  if (!normalized.startsWith('/')) normalized = '/' + normalized;
  if (normalized.endsWith('/') && normalized.length > 1) {
    normalized = normalized.slice(0, -1);
  }
  return normalized;
}

/**
 * Get all parent paths for a given path
 * @param {string} path - Resource path
 * @returns {string[]} Array of parent paths
 */
function getParentPaths(path) {
  const normalized = normalizePath(path);
  const parts = normalized.split('/').filter(Boolean);
  const parents = [];

  for (let i = 0; i < parts.length; i++) {
    parents.push('/' + parts.slice(0, i + 1).join('/'));
  }

  return parents;
}

/**
 * Get all child paths for fields under a base path
 * @param {string} basePath - Base resource path
 * @param {string[]} fields - Field names
 * @returns {string[]} Array of field paths
 */
function getFieldPaths(basePath, fields) {
  const normalized = normalizePath(basePath);
  if (!fields || fields.length === 0) return [normalized];

  return fields.map(field => `${normalized}/${field}`);
}

/**
 * Compute impact set for a CREATE operation
 * @param {Delta} delta - Delta object
 * @returns {ImpactSet} Impact set
 */
function computeCreateImpact(delta) {
  const path = normalizePath(delta.path || '/');
  const writes = new Set([path]);
  const reads = new Set();

  // Parent paths are read to check existence/permissions
  const parents = getParentPaths(path);
  parents.slice(0, -1).forEach(p => reads.add(p));

  // If fields specified, add field paths as writes
  if (delta.fields && delta.fields.length > 0) {
    getFieldPaths(path, delta.fields).forEach(fp => writes.add(fp));
  }

  return {
    reads,
    writes,
    deletes: new Set()
  };
}

/**
 * Compute impact set for an UPDATE operation
 * @param {Delta} delta - Delta object
 * @returns {ImpactSet} Impact set
 */
function computeUpdateImpact(delta) {
  const path = normalizePath(delta.path || '/');
  const reads = new Set([path]); // Read current state
  const writes = new Set();

  // If fields specified, write those field paths
  if (delta.fields && delta.fields.length > 0) {
    getFieldPaths(path, delta.fields).forEach(fp => writes.add(fp));
  } else {
    // No fields = update entire resource
    writes.add(path);
  }

  return {
    reads,
    writes,
    deletes: new Set()
  };
}

/**
 * Compute impact set for a DELETE operation
 * @param {Delta} delta - Delta object
 * @returns {ImpactSet} Impact set
 */
function computeDeleteImpact(delta) {
  const path = normalizePath(delta.path || '/');
  const reads = new Set([path]); // Read to verify existence
  const deletes = new Set([path]);

  // Parent paths read for permission checks
  const parents = getParentPaths(path);
  parents.slice(0, -1).forEach(p => reads.add(p));

  // If fields specified, only delete those fields
  if (delta.fields && delta.fields.length > 0) {
    deletes.clear();
    getFieldPaths(path, delta.fields).forEach(fp => deletes.add(fp));
  }

  return {
    reads,
    writes: new Set(),
    deletes
  };
}

/**
 * Compute impact set for a MOVE/RENAME operation
 * @param {Delta} delta - Delta object
 * @returns {ImpactSet} Impact set
 */
function computeMoveImpact(delta) {
  const sourcePath = normalizePath(delta.path || delta.source || '/');
  const targetPath = normalizePath(delta.target || delta.newPath || '/');

  const reads = new Set([sourcePath]);
  const writes = new Set([targetPath]);
  const deletes = new Set([sourcePath]);

  // Read parent paths
  const sourceParents = getParentPaths(sourcePath);
  const targetParents = getParentPaths(targetPath);

  sourceParents.slice(0, -1).forEach(p => reads.add(p));
  targetParents.slice(0, -1).forEach(p => reads.add(p));

  return { reads, writes, deletes };
}

/**
 * Compute impact set for a COPY operation
 * @param {Delta} delta - Delta object
 * @returns {ImpactSet} Impact set
 */
function computeCopyImpact(delta) {
  const sourcePath = normalizePath(delta.path || delta.source || '/');
  const targetPath = normalizePath(delta.target || delta.newPath || '/');

  const reads = new Set([sourcePath]);
  const writes = new Set([targetPath]);

  // Read parent paths
  const sourceParents = getParentPaths(sourcePath);
  const targetParents = getParentPaths(targetPath);

  sourceParents.slice(0, -1).forEach(p => reads.add(p));
  targetParents.slice(0, -1).forEach(p => reads.add(p));

  return {
    reads,
    writes,
    deletes: new Set()
  };
}

/**
 * Compute impact set for a COMPOSITE operation (multiple deltas)
 * @param {Delta} delta - Delta object with nested deltas
 * @returns {ImpactSet} Merged impact set
 */
function computeCompositeImpact(delta) {
  if (!delta.deltas || delta.deltas.length === 0) {
    return { reads: new Set(), writes: new Set(), deletes: new Set() };
  }

  const merged = {
    reads: new Set(),
    writes: new Set(),
    deletes: new Set()
  };

  for (const nestedDelta of delta.deltas) {
    const impact = computeImpactSet(nestedDelta);
    impact.reads.forEach(r => merged.reads.add(r));
    impact.writes.forEach(w => merged.writes.add(w));
    impact.deletes.forEach(d => merged.deletes.add(d));
  }

  return merged;
}

/**
 * Compute impact set for a delta operation
 *
 * Analyzes a delta to determine:
 * - Which resources are read
 * - Which resources are written
 * - Which resources are deleted
 *
 * Includes transitive impacts (parent path reads, field-level granularity).
 *
 * @param {Delta} delta - Delta operation to analyze
 * @returns {ImpactSet} Impact set with reads, writes, and deletes
 * @throws {Error} If delta is invalid or missing required fields
 *
 * @example
 * computeImpactSet({
 *   type: 'UPDATE_USER',
 *   path: '/users/123',
 *   fields: ['name', 'email']
 * })
 * // Returns: {
 * //   reads: Set(['/users/123']),
 * //   writes: Set(['/users/123/name', '/users/123/email']),
 * //   deletes: Set()
 * // }
 */
export function computeImpactSet(delta) {
  if (!delta || typeof delta !== 'object') {
    throw new Error('Delta must be a non-null object');
  }

  if (!delta.type || typeof delta.type !== 'string') {
    throw new Error('Delta must have a string type field');
  }

  const type = delta.type.toUpperCase();

  // Map operation types to impact computation functions
  if (type.includes('CREATE') || type.includes('INSERT') || type.includes('ADD')) {
    return computeCreateImpact(delta);
  }

  if (type.includes('UPDATE') || type.includes('MODIFY') || type.includes('SET')) {
    return computeUpdateImpact(delta);
  }

  if (type.includes('DELETE') || type.includes('REMOVE')) {
    return computeDeleteImpact(delta);
  }

  if (type.includes('MOVE') || type.includes('RENAME')) {
    return computeMoveImpact(delta);
  }

  if (type.includes('COPY') || type.includes('DUPLICATE')) {
    return computeCopyImpact(delta);
  }

  if (type.includes('COMPOSITE') || type.includes('BATCH') || type.includes('MULTI')) {
    return computeCompositeImpact(delta);
  }

  // Default: treat as read+write of specified path
  const path = normalizePath(delta.path || '/');
  return {
    reads: new Set([path]),
    writes: new Set([path]),
    deletes: new Set()
  };
}

/**
 * Check if two impact sets overlap (have any common resources)
 * @param {ImpactSet} impactA - First impact set
 * @param {ImpactSet} impactB - Second impact set
 * @returns {boolean} True if impacts overlap
 */
export function hasOverlap(impactA, impactB) {
  // Check writes vs writes
  for (const write of impactA.writes) {
    if (impactB.writes.has(write)) return true;
  }

  // Check writes vs reads
  for (const write of impactA.writes) {
    if (impactB.reads.has(write)) return true;
  }
  for (const write of impactB.writes) {
    if (impactA.reads.has(write)) return true;
  }

  // Check deletes vs any access
  for (const del of impactA.deletes) {
    if (impactB.reads.has(del) || impactB.writes.has(del) || impactB.deletes.has(del)) {
      return true;
    }
  }
  for (const del of impactB.deletes) {
    if (impactA.reads.has(del) || impactA.writes.has(del)) {
      return true;
    }
  }

  return false;
}

/**
 * Merge multiple impact sets into one
 * @param {...ImpactSet} impacts - Impact sets to merge
 * @returns {ImpactSet} Merged impact set
 */
export function mergeImpactSets(...impacts) {
  const merged = {
    reads: new Set(),
    writes: new Set(),
    deletes: new Set()
  };

  for (const impact of impacts) {
    if (!impact) continue;
    impact.reads?.forEach(r => merged.reads.add(r));
    impact.writes?.forEach(w => merged.writes.add(w));
    impact.deletes?.forEach(d => merged.deletes.add(d));
  }

  return merged;
}

/**
 * Convert impact set to JSON-serializable format
 * @param {ImpactSet} impact - Impact set
 * @returns {Object} Serializable object
 */
export function serializeImpactSet(impact) {
  return {
    reads: Array.from(impact.reads).sort(),
    writes: Array.from(impact.writes).sort(),
    deletes: Array.from(impact.deletes).sort()
  };
}

/**
 * Restore impact set from serialized format
 * @param {Object} serialized - Serialized impact set
 * @returns {ImpactSet} Impact set
 */
export function deserializeImpactSet(serialized) {
  return {
    reads: new Set(serialized.reads || []),
    writes: new Set(serialized.writes || []),
    deletes: new Set(serialized.deletes || [])
  };
}
