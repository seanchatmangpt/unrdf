/**
 * @file Ontology Loader
 * @module cli/commands/sync/ontology-loader
 * @description Loads RDF ontology into store for SPARQL queries
 */
import { readFile, access } from 'fs/promises';
import { existsSync, constants, readdirSync, statSync } from 'fs';
import { resolve, extname, join } from 'path';
import { createStore, COMMON_PREFIXES } from '@unrdf/core';

const c = {
  reset: '\x1b[0m',
  bold: '\x1b[1m',
  dim: '\x1b[2m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
  red: '\x1b[31m',
};

const EXT_TO_FORMAT = {
  '.ttl': 'turtle',
  '.turtle': 'turtle',
  '.nt': 'ntriples',
  '.nq': 'nquads',
  '.jsonld': 'jsonld',
  '.rdf': 'rdfxml',
  '.owl': 'rdfxml',
  '.xml': 'rdfxml',
  '.trig': 'trig',
};

const FORMAT_TO_MIME = {
  turtle: 'text/turtle',
  ntriples: 'application/n-triples',
  nquads: 'application/n-quads',
  jsonld: 'application/ld+json',
  rdfxml: 'application/rdf+xml',
  trig: 'application/trig',
};

/**
 * Find all RDF files in a directory recursively
 * @param {string} dirPath - Directory path
 * @returns {string[]} Array of file paths
 */
function findAllRdfFiles(dirPath) {
  const files = [];
  const entries = readdirSync(dirPath);

  for (const entry of entries) {
    const fullPath = join(dirPath, entry);
    const stat = statSync(fullPath);

    if (stat.isDirectory()) {
      files.push(...findAllRdfFiles(fullPath));
    } else {
      const ext = extname(entry).toLowerCase();
      if (EXT_TO_FORMAT[ext]) {
        files.push(fullPath);
      }
    }
  }

  return files;
}

/**
 * Load RDF ontology from file or directory into UnrdfStore
 * @param {Object} ontologyConfig - Ontology configuration from unrdf.toml
 * @param {string} baseDir - Base directory for resolving relative paths
 * @returns {Promise<Object>} Object with store, tripleCount, prefixes, source, format
 */
export async function loadOntology(ontologyConfig, baseDir = process.cwd()) {
  const { source, format, base_iri, prefixes: customPrefixes = {} } = ontologyConfig;

  if (!source) {
    throw new Error('Ontology source is required in configuration');
  }

  const absolutePath = resolve(baseDir, source);

  if (!existsSync(absolutePath)) {
    throw new Error(
      `Ontology source not found: ${absolutePath}\n` +
        `  Configured path: ${source}\n` +
        `  Base directory: ${baseDir}\n` +
        `  Fix: Check that the source path is correct and the source exists`
    );
  }

  const isDirectory = statSync(absolutePath).isDirectory();
  const filesToLoad = isDirectory ? findAllRdfFiles(absolutePath) : [absolutePath];

  if (filesToLoad.length === 0) {
    throw new Error(
      `No RDF files found in: ${absolutePath}\n` +
        `  Source: ${source}\n` +
        `  Fix: Ensure the directory contains .ttl, .nt, .rdf, or .owl files`
    );
  }

  if (isDirectory) {
    console.log(`   Found ${filesToLoad.length} RDF file(s) in directory`);
  }

  // Check file(s) are readable
  for (const filePath of filesToLoad) {
    try {
      await access(filePath, constants.R_OK);
    } catch (accessErr) {
      throw new Error(
        `Ontology file is not readable: ${filePath}\n` +
          `  Fix: Check file permissions (should be readable)`
      );
    }
  }

  const store = createStore();
  const extractedPrefixes = {};
  let tripleCount = 0;
  let finalFormat = format;

  // Load each file
  for (const filePath of filesToLoad) {
    // Read file content
    let content;
    try {
      content = await readFile(filePath, 'utf-8');
    } catch (readErr) {
      throw new Error(
        `Failed to read ontology file: ${filePath}\n` +
          `  Error: ${readErr.message}\n` +
          `  Fix: Ensure file is a valid text file with UTF-8 encoding`
      );
    }

    // Auto-detect format if not specified (per file in directory)
    let fileFormat = format;
    if (!fileFormat) {
      const ext = extname(filePath).toLowerCase();
      fileFormat = EXT_TO_FORMAT[ext] || 'turtle';
      if (filesToLoad.length === 1) {
        console.log(`Auto-detected format: ${fileFormat} (from extension ${ext})`);
      }
    }
    finalFormat = fileFormat;

    // Extract prefixes from Turtle/TriG content
    if (fileFormat === 'turtle' || fileFormat === 'trig') {
      const prefixRegex = /@prefix\s+([^:]*):?\s*<([^>]+)>\s*\./g;
      let match;
      while ((match = prefixRegex.exec(content)) !== null) {
        extractedPrefixes[match[1]] = match[2];
      }
    }

    // Load content into store
    const mimeType = FORMAT_TO_MIME[fileFormat] || fileFormat;

    if (typeof store.load === 'function') {
      try {
        const previousSize = store.size || 0;
        await store.load(content, { format: mimeType, baseIri: base_iri });
        const newSize = store.size || 0;
        const loadedCount = newSize - previousSize;

        if (filesToLoad.length > 1) {
          console.log(`   Loaded ${loadedCount} triples from ${filePath}`);
        }
      } catch (loadErr) {
        // Extract helpful information from parse error
        const errorMsg = loadErr.message || String(loadErr);
        const lineMatch = errorMsg.match(/line[:\s]+(\d+)/i);
        const lineInfo = lineMatch ? `\n  Line: ${lineMatch[1]}` : '';

        // Suggest possible fixes based on format
        const formatSuggestions = [
          `\nPossible fixes:`,
          `  1. Check ${fileFormat.toUpperCase()} syntax is valid`,
          `  2. Verify all prefixes are declared with @prefix`,
          `  3. Check for unescaped special characters in literals`,
          `  4. Ensure all statements end with a period (.)`,
        ];

        if (!format) {
          formatSuggestions.push(
            `  5. Try specifying format explicitly in config:`,
            `     "format": "turtle"  (or "ntriples", "rdfxml", etc.)`
          );
        }

        const errorResult = `Failed to parse ontology file: ${filePath}\n` +
            `  Format: ${fileFormat}${lineInfo}\n` +
            `  Parse error: ${errorMsg}` +
            formatSuggestions.join('\n');

        if (filesToLoad.length > 1) {
          console.error(`   ${c.red}ERROR${c.reset} ${errorResult}`);
          continue;
        } else {
          throw new Error(errorResult);
        }
      }
    } else {
      // Fallback: Count triples from content (estimate)
      const tripleLines = content
        .split('\n')
        .filter(l => l.trim() && !l.trim().startsWith('@') && !l.trim().startsWith('#'));
      tripleCount += tripleLines.length;
    }
  }

  tripleCount = store.size || tripleCount;

  if (ontologyConfig.additional && Array.isArray(ontologyConfig.additional)) {
    for (const add of ontologyConfig.additional) {
      if (!add.source) continue;
      const addPath = resolve(baseDir, add.source);
      if (existsSync(addPath)) {
        try {
          const { tripleCount: addCount } = await loadOntology(add, baseDir);
          tripleCount += addCount;
        } catch (err) {
          console.error(`   ${c.yellow}Warning:${c.reset} Failed to load additional source: ${add.source}`);
        }
      }
    }
  }

  const prefixes = { ...COMMON_PREFIXES, ...extractedPrefixes, ...customPrefixes };

  return { store, tripleCount, prefixes, source: absolutePath, format: finalFormat };
}

/**
 * Extract prefixes from store
 * @param {Object} store - RDF store
 * @returns {Object} Prefix map
 */
export function extractPrefixes(store) {
  const prefixes = { ...COMMON_PREFIXES };
  if (typeof store.getPrefixes === 'function') {
    Object.assign(prefixes, store.getPrefixes());
  }
  return prefixes;
}

export default { loadOntology, extractPrefixes };
