/**
 * @file Filesystem scanner - walk directory tree and emit RDF graph
 * @module project-engine/fs-scan
 */

import { promises as fs } from 'fs';
import path from 'path';
import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';
import { Store } from 'n3'; // TODO: Replace with Oxigraph Store
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { z } from 'zod';

const tracer = trace.getTracer('unrdf/fs-scan');
const { namedNode, literal } = DataFactory;

/**
 * Default ignore patterns for filesystem scan
 */
const DEFAULT_IGNORE_PATTERNS = [
  'node_modules',
  '.git',
  '.gitignore',
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
  root: z.string(),
  ignorePatterns: z.array(z.string()).optional(),
  baseIri: z.string().default('http://example.org/unrdf/fs#'),
});

/**
 * Scan a filesystem directory and create RDF graph with NFO + UNRDF FS ontology
 *
 * @param {Object} options
 * @param {string} options.root - Directory to scan
 * @param {string[]} [options.ignorePatterns] - Glob patterns to ignore
 * @param {string} [options.baseIri] - Base IRI for resource identifiers
 * @returns {Promise<{store: Store, summary: {fileCount: number, folderCount: number, ignoredCount: number, rootIri: string}}>}
 */
export async function scanFileSystemToStore(options) {
  const validated = ScanOptionsSchema.parse(options);
  const { root, baseIri } = validated;
  const ignorePatterns = validated.ignorePatterns || DEFAULT_IGNORE_PATTERNS;

  return tracer.startActiveSpan('fs.scan', async span => {
    try {
      span.setAttributes({
        'fs.root': root,
        'fs.ignore_count': ignorePatterns.length,
      });

      const store = new Store();
      const stats = {
        fileCount: 0,
        folderCount: 0,
        ignoredCount: 0,
        rootIri: `${baseIri}root`,
      };

      // Add root as ProjectRoot
      const rootIri = namedNode(`${baseIri}root`);
      store.addQuad(
        rootIri,
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/unrdf/filesystem#ProjectRoot')
      );
      store.addQuad(
        rootIri,
        namedNode('http://example.org/unrdf/filesystem#relativePath'),
        literal('.')
      );
      store.addQuad(
        rootIri,
        namedNode('http://example.org/unrdf/filesystem#depth'),
        literal(0, namedNode('http://www.w3.org/2001/XMLSchema#integer'))
      );

      // Recursive walk
      await walkDirectory(root, '.', rootIri, store, ignorePatterns, baseIri, stats);

      span.setAttributes({
        'fs.file_count': stats.fileCount,
        'fs.folder_count': stats.folderCount,
        'fs.ignored_count': stats.ignoredCount,
      });
      span.setStatus({ code: SpanStatusCode.OK });

      return { store, summary: stats };
    } catch (error) {
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message,
      });
      throw error;
    }
  });
}

/**
 * Recursively walk directory and add quads to store
 *
 * @private
 */
async function walkDirectory(
  diskPath,
  relativePath,
  parentIri,
  store,
  ignorePatterns,
  baseIri,
  stats
) {
  try {
    const entries = await fs.readdir(diskPath, { withFileTypes: true });

    for (const entry of entries) {
      // Check ignore patterns
      if (shouldIgnore(entry.name, ignorePatterns)) {
        stats.ignoredCount++;
        continue;
      }

      const entryRelPath = relativePath === '.' ? entry.name : `${relativePath}/${entry.name}`;
      const entryDiskPath = path.join(diskPath, entry.name);
      const entryIri = namedNode(`${baseIri}${encodeURIComponent(entryRelPath)}`);

      // Determine type
      if (entry.isDirectory()) {
        await handleDirectory(
          entryDiskPath,
          entryRelPath,
          entryIri,
          parentIri,
          store,
          ignorePatterns,
          baseIri,
          stats
        );
      } else if (entry.isFile()) {
        await handleFile(entryDiskPath, entryRelPath, entryIri, parentIri, store, stats);
      }
    }
  } catch (error) {
    // Skip unreadable directories
    console.warn(`Skipped: ${diskPath} (${error.message})`);
  }
}

/**
 * Handle a directory entry
 *
 * @private
 */
async function handleDirectory(
  diskPath,
  relativePath,
  dirIri,
  parentIri,
  store,
  ignorePatterns,
  baseIri,
  stats
) {
  const depth = (relativePath.match(/\//g) || []).length;

  // Determine folder type
  let folderType = 'http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#Folder';
  if (relativePath.includes('/src') || relativePath === 'src') {
    folderType = 'http://example.org/unrdf/filesystem#SourceFolder';
  } else if (
    relativePath.includes('/dist') ||
    relativePath === 'dist' ||
    relativePath.includes('/.next') ||
    relativePath === '.next' ||
    relativePath.includes('/build') ||
    relativePath === 'build'
  ) {
    folderType = 'http://example.org/unrdf/filesystem#BuildFolder';
  } else if (
    relativePath.includes('/config') ||
    relativePath === 'config' ||
    relativePath.includes('/.') ||
    relativePath.startsWith('.')
  ) {
    folderType = 'http://example.org/unrdf/filesystem#ConfigFolder';
  }

  // Add folder quads
  store.addQuad(
    dirIri,
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode(folderType)
  );
  store.addQuad(
    dirIri,
    namedNode('http://example.org/unrdf/filesystem#relativePath'),
    literal(relativePath)
  );
  store.addQuad(
    dirIri,
    namedNode('http://example.org/unrdf/filesystem#depth'),
    literal(depth, namedNode('http://www.w3.org/2001/XMLSchema#integer'))
  );
  store.addQuad(
    dirIri,
    namedNode('http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#containedFolders'),
    namedNode('http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject')
  );

  // Link to parent
  store.addQuad(dirIri, namedNode('http://example.org/unrdf/filesystem#containedIn'), parentIri);

  stats.folderCount++;

  // Recurse
  await walkDirectory(diskPath, relativePath, dirIri, store, ignorePatterns, baseIri, stats);
}

/**
 * Handle a file entry
 *
 * @private
 */
async function handleFile(diskPath, relativePath, fileIri, parentIri, store, stats) {
  try {
    const stat = await fs.stat(diskPath);
    const depth = (relativePath.match(/\//g) || []).length;
    const ext = path.extname(relativePath).replace(/^\./, '');
    const isHidden = path.basename(relativePath).startsWith('.');

    store.addQuad(
      fileIri,
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode('http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject')
    );
    store.addQuad(
      fileIri,
      namedNode('http://example.org/unrdf/filesystem#relativePath'),
      literal(relativePath)
    );
    store.addQuad(
      fileIri,
      namedNode('http://example.org/unrdf/filesystem#depth'),
      literal(depth, namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    );
    store.addQuad(
      fileIri,
      namedNode('http://example.org/unrdf/filesystem#byteSize'),
      literal(stat.size, namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    );
    store.addQuad(
      fileIri,
      namedNode('http://example.org/unrdf/filesystem#lastModified'),
      literal(stat.mtime.toISOString(), namedNode('http://www.w3.org/2001/XMLSchema#dateTime'))
    );
    store.addQuad(
      fileIri,
      namedNode('http://example.org/unrdf/filesystem#isHidden'),
      literal(isHidden, namedNode('http://www.w3.org/2001/XMLSchema#boolean'))
    );

    if (ext) {
      store.addQuad(
        fileIri,
        namedNode('http://example.org/unrdf/filesystem#extension'),
        literal(ext)
      );
    }

    // Link to parent
    store.addQuad(fileIri, namedNode('http://example.org/unrdf/filesystem#containedIn'), parentIri);

    stats.fileCount++;
  } catch (error) {
    console.warn(`Skipped file: ${diskPath} (${error.message})`);
  }
}

/**
 * Check if path matches ignore patterns
 *
 * @private
 */
function shouldIgnore(name, patterns) {
  return patterns.some(pattern => {
    if (pattern.includes('*')) {
      // Simple glob: *.swp, .*, etc.
      const regex = new RegExp(`^${pattern.replace(/\*/g, '.*')}$`);
      return regex.test(name);
    }
    return name === pattern;
  });
}
