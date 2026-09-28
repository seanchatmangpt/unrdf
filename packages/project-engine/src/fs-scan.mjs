/**
 * @file Filesystem scanner - walk a directory tree and emit an RDF graph
 * @module project-engine/fs-scan
 */

import { promises as fs } from 'fs';
import path from 'path';
import { z } from 'zod';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { RDF_TYPE, XSD, NFO, FS } from './namespaces.mjs';

const { namedNode, literal } = dataFactory;

/**
 * Default ignore patterns for a filesystem scan.
 * `.unrdf` is the init output directory; ignoring it keeps `init` idempotent.
 */
export const DEFAULT_IGNORE_PATTERNS = [
  'node_modules',
  '.git',
  '.gitignore',
  '.unrdf',
  'dist',
  'build',
  '.next',
  '.turbo',
  'coverage',
  '.cache',
  '.venv',
  'env',
  '*.swp',
  '.DS_Store',
  '.env.local',
  '.env.*.local',
];

const ScanOptionsSchema = z.object({
  root: z.string().min(1),
  ignorePatterns: z.array(z.string()).optional(),
  baseIri: z.string().default('http://example.org/unrdf/fs#'),
});

const integer = value => literal(String(value), namedNode(`${XSD}integer`));
const boolean = value => literal(String(value), namedNode(`${XSD}boolean`));

/**
 * Scan a directory and build an RDF graph (NFO + UNRDF filesystem ontology).
 *
 * @param {Object} options
 * @param {string} options.root - Directory to scan
 * @param {string[]} [options.ignorePatterns] - Names/globs to ignore
 * @param {string} [options.baseIri] - Base IRI for resource identifiers
 * @returns {Promise<{store: import('@unrdf/oxigraph').OxigraphStore, summary: {fileCount: number, folderCount: number, ignoredCount: number, rootIri: string}}>}
 */
export async function scanFileSystemToStore(options) {
  const {
    root,
    baseIri,
    ignorePatterns = DEFAULT_IGNORE_PATTERNS,
  } = ScanOptionsSchema.parse(options);

  const rootStat = await fs.stat(root);
  if (!rootStat.isDirectory()) {
    throw new Error(`Scan root is not a directory: ${root}`);
  }

  const store = createStore();
  const stats = { fileCount: 0, folderCount: 0, ignoredCount: 0, rootIri: `${baseIri}root` };
  const matchers = ignorePatterns.map(compileIgnore);

  const rootIri = namedNode(stats.rootIri);
  store.addQuad(rootIri, namedNode(RDF_TYPE), namedNode(FS.ProjectRoot));
  store.addQuad(rootIri, namedNode(FS.relativePath), literal('.'));
  store.addQuad(rootIri, namedNode(FS.depth), integer(0));

  await walkDirectory(root, '.', rootIri, store, matchers, baseIri, stats);
  return { store, summary: stats };
}

async function walkDirectory(diskPath, relativePath, parentIri, store, matchers, baseIri, stats) {
  let entries;
  try {
    entries = await fs.readdir(diskPath, { withFileTypes: true });
  } catch {
    // Unreadable directory: not part of the project graph
    stats.ignoredCount++;
    return;
  }
  entries.sort((a, b) => (a.name < b.name ? -1 : a.name > b.name ? 1 : 0));

  for (const entry of entries) {
    if (matchers.some(match => match(entry.name))) {
      stats.ignoredCount++;
      continue;
    }
    const entryRelPath = relativePath === '.' ? entry.name : `${relativePath}/${entry.name}`;
    const entryDiskPath = path.join(diskPath, entry.name);
    const entryIri = namedNode(`${baseIri}${encodeURIComponent(entryRelPath)}`);

    if (entry.isDirectory()) {
      addDirectory(entryRelPath, entryIri, parentIri, store);
      stats.folderCount++;
      await walkDirectory(entryDiskPath, entryRelPath, entryIri, store, matchers, baseIri, stats);
    } else if (entry.isFile()) {
      if (await addFile(entryDiskPath, entryRelPath, entryIri, parentIri, store)) {
        stats.fileCount++;
      } else {
        stats.ignoredCount++;
      }
    }
  }
}

function folderTypeFor(relativePath) {
  const segments = relativePath.split('/');
  const name = segments[segments.length - 1];
  if (name === 'src') return FS.SourceFolder;
  if (['dist', 'build', '.next'].includes(name)) return FS.BuildFolder;
  if (name === 'config' || name.startsWith('.')) return FS.ConfigFolder;
  return `${NFO}Folder`;
}

function addDirectory(relativePath, dirIri, parentIri, store) {
  const depth = relativePath.split('/').length - 1;
  store.addQuad(dirIri, namedNode(RDF_TYPE), namedNode(folderTypeFor(relativePath)));
  store.addQuad(dirIri, namedNode(FS.relativePath), literal(relativePath));
  store.addQuad(dirIri, namedNode(FS.depth), integer(depth));
  store.addQuad(dirIri, namedNode(FS.containedIn), parentIri);
}

async function addFile(diskPath, relativePath, fileIri, parentIri, store) {
  let stat;
  try {
    stat = await fs.stat(diskPath);
  } catch {
    return false;
  }
  const depth = relativePath.split('/').length - 1;
  const ext = path.extname(relativePath).replace(/^\./, '');
  const isHidden = path.basename(relativePath).startsWith('.');

  store.addQuad(fileIri, namedNode(RDF_TYPE), namedNode(`${NFO}FileDataObject`));
  store.addQuad(fileIri, namedNode(FS.relativePath), literal(relativePath));
  store.addQuad(fileIri, namedNode(FS.depth), integer(depth));
  store.addQuad(fileIri, namedNode(FS.byteSize), integer(stat.size));
  store.addQuad(
    fileIri,
    namedNode(FS.lastModified),
    literal(stat.mtime.toISOString(), namedNode(`${XSD}dateTime`))
  );
  store.addQuad(fileIri, namedNode(FS.isHidden), boolean(isHidden));
  if (ext) store.addQuad(fileIri, namedNode(FS.extension), literal(ext));
  store.addQuad(fileIri, namedNode(FS.containedIn), parentIri);
  return true;
}

/**
 * Compile an ignore pattern (exact name or `*` glob) to a predicate.
 * Regex metacharacters other than `*` are escaped.
 * @param {string} pattern
 * @returns {(name: string) => boolean}
 */
function compileIgnore(pattern) {
  if (!pattern.includes('*')) return name => name === pattern;
  const source = pattern
    .split('*')
    .map(part => part.replace(/[.+?^${}()|[\]\\]/g, '\\$&'))
    .join('.*');
  const regex = new RegExp(`^${source}$`);
  return name => regex.test(name);
}
