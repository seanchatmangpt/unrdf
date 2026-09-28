/**
 * @file Path-level diff between two file system snapshots (used by materialize-apply receipts)
 * @module project-engine/fs-path-diff
 */

import { FS } from './namespaces.mjs';

const REL_PATH = FS.relativePath;

/**
 * Collect the relative paths recorded in a snapshot store.
 * @param {Object} store
 * @returns {Set<string>}
 */
function collectPaths(store) {
  const paths = new Set();
  for (const q of store.match(null, { termType: 'NamedNode', value: REL_PATH }, null, null)) {
    if (q.object.value !== '.') paths.add(q.object.value); // '.' is the scan root itself
  }
  return paths;
}

/**
 * Diff two snapshots produced by scanFileSystemToStore.
 *
 * `added` are paths present in `actualStore` but not in `goldenStore`;
 * `removed` are paths present in `goldenStore` but not in `actualStore`.
 *
 * @param {Object} options
 * @param {Object} options.actualStore - Snapshot of the current state
 * @param {Object} options.goldenStore - Snapshot to compare against
 * @returns {{added: string[], removed: string[], unchanged: number}}
 */
export function diffFsPaths({ actualStore, goldenStore }) {
  const actual = collectPaths(actualStore);
  const golden = collectPaths(goldenStore);
  const added = [...actual].filter(p => !golden.has(p)).sort();
  const removed = [...golden].filter(p => !actual.has(p)).sort();
  return { added, removed, unchanged: actual.size - added.length };
}
