/**
 * @fileoverview Store import implementation with OTEL instrumentation
 *
 * @description
 * Bulk imports RDF files into stores with support for multiple formats,
 * named graphs, and error recovery. Fully instrumented with OpenTelemetry.
 *
 * @module cli/store-import
 * @version 2.1.1
 * @license MIT
 */

import { readFile, writeFile, readdir, stat } from 'node:fs/promises';
import { join } from 'node:path';
import { z } from 'zod';
import { OxigraphStore } from '@unrdf/oxigraph';
import { defaultObservabilityManager } from '@unrdf/knowledge-engine';
import { fileExists, ensureDir, detectRDFFormat } from '../utils/io-utils.mjs';

/**
 * Import options schema
 */
const ImportOptionsSchema = z.object({
  storePath: z.string(),
  format: z.enum(['turtle', 'n-quads', 'n-triples', 'json-ld', 'auto']).default('auto'),
  graph: z.string().optional(),
  skipErrors: z.boolean().default(false),
  batchSize: z.number().default(1000),
});

/**
 * Import result schema
 */
const ImportResultSchema = z.object({
  storePath: z.string(),
  totalFiles: z.number(),
  filesImported: z.number(),
  quadCount: z.number(),
  graphCount: z.number(),
  duration: z.number(),
  errors: z.array(
    z.object({
      file: z.string(),
      message: z.string(),
    })
  ),
});

/**
 * Bulk import RDF files into store
 *
 * @param {Array<string>} files - File paths or glob patterns
 * @param {Object} options - Import options
 * @param {string} options.storePath - Target store directory path
 * @param {string} [options.format='auto'] - RDF format (auto-detected if not specified)
 * @param {string} [options.graph] - Target named graph IRI
 * @param {boolean} [options.skipErrors=false] - Continue on file errors
 * @param {number} [options.batchSize=1000] - Batch size for processing
 * @returns {Promise<Object>} Import result with statistics
 *
 * @throws {Error} If no files found or import fails
 *
 * @example
 * const result = await importStore(['data/*.ttl'], {
 *   storePath: './my-store',
 *   format: 'turtle',
 *   graph: 'http://example.org/graph1'
 * });
 * console.log(`Imported ${result.quadCount} quads from ${result.filesImported} files`);
 */
export async function importStore(files, options) {
  // Initialize observability
  await defaultObservabilityManager.initialize();

  const transactionId = `import-${Date.now()}`;
  const _spanContext = defaultObservabilityManager.startTransactionSpan(transactionId, {
    operation: 'store.import',
    'store.path': options.storePath,
  });

  const startTime = Date.now();

  try {
    // Validate options
    const opts = ImportOptionsSchema.parse(options);

    // Ensure store directory exists
    await ensureDir(opts.storePath);

    // Expand file patterns
    const expandedFiles = await expandFilePatterns(files);

    if (expandedFiles.length === 0) {
      throw new Error('No files found matching the provided patterns');
    }

    console.log(`📥 Importing ${expandedFiles.length} files into ${opts.storePath}...`);

    // Import files
    const stats = await importFiles(expandedFiles, opts);

    const duration = Date.now() - startTime;

    const result = ImportResultSchema.parse({
      storePath: opts.storePath,
      totalFiles: expandedFiles.length,
      filesImported: stats.filesImported,
      quadCount: stats.quadCount,
      graphCount: stats.graphCount,
      duration,
      errors: stats.errors,
    });

    // End span successfully
    defaultObservabilityManager.endTransactionSpan(transactionId, {
      'import.files': stats.filesImported,
      'import.quads': stats.quadCount,
      'import.graphs': stats.graphCount,
      'import.duration_ms': duration,
      'import.errors': stats.errors.length,
    });

    return result;
  } catch (error) {
    // Record error
    defaultObservabilityManager.recordError(error, {
      operation: 'store.import',
      'store.path': options.storePath,
    });

    // End span with error
    defaultObservabilityManager.endTransactionSpan(transactionId, {}, error);

    throw error;
  }
}

/**
 * Expand file patterns to actual file paths
 *
 * @param {Array<string>} patterns - File patterns
 * @returns {Promise<Array<string>>} Expanded file paths
 * @private
 */
async function expandFilePatterns(patterns) {
  // Ensure patterns is always an array
  const patternArray = Array.isArray(patterns) ? patterns : [patterns];
  const files = [];

  for (const pattern of patternArray) {
    // Simple glob expansion (supports * and **)
    if (pattern.includes('*')) {
      const dir = pattern.substring(0, pattern.lastIndexOf('/') + 1) || '.';
      const filePattern = pattern.substring(pattern.lastIndexOf('/') + 1);

      const dirExists = await fileExists(dir);
      if (dirExists) {
        const dirFiles = await readdir(dir);
        const regex = new RegExp('^' + filePattern.replace(/\*/g, '.*') + '$');

        for (const file of dirFiles) {
          if (regex.test(file)) {
            const fullPath = join(dir, file);

            // Check if entry is actually a file
            try {
              const stats = await stat(fullPath);
              if (stats.isFile()) {
                files.push(fullPath);
              }
              // Skip directories silently
            } catch (error) {
              // Skip entries we can't stat
              continue;
            }
          }
        }
      }
    } else {
      // Direct file path
      const exists = await fileExists(pattern);
      if (exists) {
        files.push(pattern);
      }
    }
  }

  return files;
}

/**
 * Import files into store
 *
 * @param {Array<string>} files - File paths to import
 * @param {Object} options - Import options
 * @returns {Promise<Object>} Import statistics
 * @private
 */
async function importFiles(files, options) {
  const store = new OxigraphStore();
  const graphs = new Set();
  const errors = [];
  let filesImported = 0;
  let quadCount = 0;

  for (const filePath of files) {
    try {
      console.log(`  Processing ${filePath}...`);

      // Detect format
      const format = options.format === 'auto' ? detectRDFFormat(filePath) : options.format;

      // Read file
      const content = await readFile(filePath, 'utf-8');

      // Parse RDF using Oxigraph
      const mimeTypes = {
        turtle: 'text/turtle',
        'n-triples': 'application/n-triples',
        'n-quads': 'application/n-quads',
        trig: 'application/trig',
        n3: 'text/n3',
        rdf: 'application/rdf+xml',
        'json-ld': 'application/ld+json',
      };

      const mimeType = mimeTypes[format] || 'text/turtle';
      const quadsAdded = store.load(content, { format: mimeType, baseIRI: options.graph });

      // Count quads and track graphs
      const allQuads = store.getQuads();
      quadCount = allQuads.length;
      allQuads.forEach(quad => {
        if (quad.graph && quad.graph.value) {
          graphs.add(quad.graph.value);
        }
      });

      filesImported++;

      console.log(`    ✅ Imported ${quadsAdded} quads`);
    } catch (error) {
      if (options.skipErrors) {
        console.warn(`    ⚠️  Error: ${error.message}`);
        errors.push({
          file: filePath,
          message: error.message,
        });
      } else {
        throw new Error(`Failed to import ${filePath}: ${error.message}`);
      }
    }
  }

  // Write store to disk using Oxigraph dump
  const storeFile = join(options.storePath, 'store.nq');
  const serialized = store.dump({ format: 'application/n-quads' });
  await writeFile(storeFile, serialized, 'utf-8');

  return {
    filesImported,
    quadCount,
    graphCount: graphs.size,
    errors,
  };
}
