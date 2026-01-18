/**
 * @fileoverview RDF Diff Engine - Graph comparison and change tracking
 * @module diff
 * @description Computes differences between RDF graphs and tracks changes
 */

/**
 * Converts RDF Quad to Triple (removes graph component)
 * @param {Object} quad - RDF Quad with subject, predicate, object, graph
 * @returns {Object} Triple with subject, predicate, object
 */
export function quadToTriple(quad) {
  return {
    subject: quad.subject?.value || quad.subject,
    predicate: quad.predicate?.value || quad.predicate,
    object: quad.object?.value || quad.object,
  };
}

/**
 * Generates unique key for a triple
 * @param {Object} triple - Triple to key
 * @returns {string} Unique key string
 */
export function tripleKey(triple) {
  return `${triple.subject}|${triple.predicate}|${triple.object}`;
}

/**
 * Computes diff between two RDF graphs
 * @param {Array} before - Before state triples
 * @param {Array} after - After state triples
 * @returns {Object} Diff with added, removed, updated arrays
 * @example
 * const diff = diffGraphs(beforeTriples, afterTriples);
 * console.log(diff); // { added: [...], removed: [...], updated: [...] }
 */
export function diffGraphs(before, after) {
  const beforeKeys = new Set(before.map(tripleKey));
  const afterKeys = new Set(after.map(tripleKey));

  const added = after.filter(t => !beforeKeys.has(tripleKey(t)));
  const removed = before.filter(t => !afterKeys.has(tripleKey(t)));
  const updated = [];

  // Find updates (same subject+predicate, different object)
  const beforeBySubjectPred = new Map();
  const afterBySubjectPred = new Map();

  for (const t of before) {
    const key = `${t.subject}|${t.predicate}`;
    beforeBySubjectPred.set(key, t);
  }

  for (const t of after) {
    const key = `${t.subject}|${t.predicate}`;
    afterBySubjectPred.set(key, t);
  }

  for (const [key, beforeTriple] of beforeBySubjectPred) {
    const afterTriple = afterBySubjectPred.get(key);
    if (afterTriple && beforeTriple.object !== afterTriple.object) {
      updated.push({
        from: beforeTriple,
        to: afterTriple,
      });
    }
  }

  return {
    added,
    removed,
    updated,
    stats: {
      addedCount: added.length,
      removedCount: removed.length,
      updatedCount: updated.length,
    },
  };
}

/**
 * Applies a diff to generate new graph state
 * @param {Array} current - Current state triples
 * @param {Object} diff - Diff to apply
 * @returns {Array} New state after applying diff
 */
export function applyDiff(current, diff) {
  const result = [...current];

  // Remove triples
  if (diff.removed) {
    const removedKeys = new Set(diff.removed.map(tripleKey));
    return result.filter(t => !removedKeys.has(tripleKey(t)));
  }

  // Apply updates
  if (diff.updated) {
    for (const update of diff.updated) {
      const oldKey = tripleKey(update.from);
      const idx = result.findIndex(t => tripleKey(t) === oldKey);
      if (idx >= 0) {
        result[idx] = update.to;
      }
    }
  }

  // Add new triples
  if (diff.added) {
    result.push(...diff.added);
  }

  return result;
}

/**
 * Merges multiple diffs in order
 * @param {Array<Object>} diffs - Array of diffs to merge
 * @returns {Object} Combined diff
 */
export function mergeDiffs(diffs) {
  const merged = {
    added: [],
    removed: [],
    updated: [],
  };

  for (const diff of diffs) {
    if (diff.added) merged.added.push(...diff.added);
    if (diff.removed) merged.removed.push(...diff.removed);
    if (diff.updated) merged.updated.push(...diff.updated);
  }

  return merged;
}

export default {
  quadToTriple,
  tripleKey,
  diffGraphs,
  applyDiff,
  mergeDiffs,
};
