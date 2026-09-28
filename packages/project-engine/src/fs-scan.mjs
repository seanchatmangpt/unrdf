/**
 * @file File system scanner - snapshot a directory tree as an RDF store
 * @module project-engine/fs-scan
 */

import { promises as fs } from 'fs';
import path from 'path';
import { createStore, dataFactory } from '@unrdf/oxigraph';

const RDF_TYPE = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';
const FS_NS = 'http://example.org/unrdf/fs#';

const DEFAULT_IGNORE = new Set(['node_modules', '.git']);

/**
 * Percent-encode a relative path into an IRI-safe suffix (keeps `/` separators).
 * @param {string} rel
 * @returns {string}
 */
function encodePath(rel) {
  return rel.split('/').map(encodeURIComponent).join('/');
}

/**
 * Scan a directory tree into an RDF store.
 *
 * Each folder is `fs:Folder`, each file `fs:File`, both with an `fs:relativePath` literal.
 * A missing root throws (callers decide how to treat "no directory yet").
 *
 * @param {Object} options
 * @param {string} options.root - Directory to scan
 * @param {string} [options.baseIri] - Base IRI for resource nodes
 * @param {string[]} [options.ignore] - Directory/file names to skip
 * @returns {Promise<{store: Object, summary: {fileCount: number, folderCount: number}}>}
 */
export async function scanFileSystemToStore({
  root,
  baseIri = 'http://example.org/unrdf/fs/',
  ignore = [],
}) {
  const skip = new Set([...DEFAULT_IGNORE, ...ignore]);
  const store = createStore();
  const { namedNode, literal, quad } = dataFactory;
  const summary = { fileCount: 0, folderCount: 0 };

  const rootStat = await fs.stat(root);
  if (!rootStat.isDirectory()) {
    throw new Error(`Not a directory: ${root}`);
  }

  /**
   * @param {string} dir
   * @param {string} rel
   */
  async function walk(dir, rel) {
    const entries = (await fs.readdir(dir, { withFileTypes: true })).sort((a, b) =>
      a.name < b.name ? -1 : a.name > b.name ? 1 : 0
    );
    for (const entry of entries) {
      if (skip.has(entry.name)) continue;
      const childRel = rel ? `${rel}/${entry.name}` : entry.name;
      const node = namedNode(`${baseIri}${encodePath(childRel)}`);
      if (entry.isDirectory()) {
        summary.folderCount++;
        store.add(quad(node, namedNode(RDF_TYPE), namedNode(`${FS_NS}Folder`)));
        store.add(quad(node, namedNode(`${FS_NS}relativePath`), literal(childRel)));
        await walk(path.join(dir, entry.name), childRel);
      } else if (entry.isFile()) {
        summary.fileCount++;
        store.add(quad(node, namedNode(RDF_TYPE), namedNode(`${FS_NS}File`)));
        store.add(quad(node, namedNode(`${FS_NS}relativePath`), literal(childRel)));
      }
    }
  }

  await walk(root, '');
  return { store, summary };
}
