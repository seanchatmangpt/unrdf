/**
 * @file Project structure diff between two file system snapshots
 * @module project-engine/project-diff
 */

const REL_PATH = 'http://example.org/unrdf/fs#relativePath';

/**
 * Collect the relative paths recorded in a snapshot store.
 * @param {Object} store
 * @returns {Set<string>}
 */
function collectPaths(store) {
  const paths = new Set();
  for (const q of store.match(null, { termType: 'NamedNode', value: REL_PATH }, null, null)) {
    paths.add(q.object.value);
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
export function diffProjectStructure({ actualStore, goldenStore }) {
  const actual = collectPaths(actualStore);
  const golden = collectPaths(goldenStore);
  const added = [...actual].filter(p => !golden.has(p)).sort();
  const removed = [...golden].filter(p => !actual.has(p)).sort();
  return { added, removed, unchanged: actual.size - added.length };
}
