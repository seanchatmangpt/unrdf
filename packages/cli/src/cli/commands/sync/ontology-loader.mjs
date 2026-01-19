/**
 * @file Ontology Loader
 * @module cli/commands/sync/ontology-loader
 * @description Loads RDF ontology into store for SPARQL queries
 */
import { readFile, access } from 'fs/promises';
import { existsSync, constants } from 'fs';
import { resolve, dirname, extname } from 'path';
import { createStore, COMMON_PREFIXES } from '@unrdf/core';

const FORMAT_TO_EXT = { turtle: 'ttl', 'text/turtle': 'ttl', ntriples: 'nt', nquads: 'nq', trig: 'trig', rdfxml: 'rdf' };
const EXT_TO_FORMAT = { '.ttl': 'turtle', '.nt': 'ntriples', '.nq': 'nquads', '.trig': 'trig', '.rdf': 'rdfxml', '.owl': 'rdfxml' };

/**
 * Load ontology from file into RDF store
 * @param {Object} ontologyConfig - Ontology configuration
 * @param {string} baseDir - Base directory for path resolution
 * @returns {Promise<Object>} Object with store and metadata
 * @throws {Error} If file not found, not readable, or has invalid syntax
 */
export async function loadOntology(ontologyConfig, baseDir = process.cwd()) {
  const { source, format, base_iri, prefixes: customPrefixes } = ontologyConfig;

  if (!source) {
    throw new Error(
      'Ontology configuration missing required "source" field\n' +
      '  Fix: Add "source" field to ontology config pointing to your RDF file'
    );
  }

  const absolutePath = resolve(baseDir, source);

  // Check file exists
  if (!existsSync(absolutePath)) {
    throw new Error(
      `Ontology file not found: ${absolutePath}\n` +
      `  Configured path: ${source}\n` +
      `  Base directory: ${baseDir}\n` +
      `  Fix: Check that the file path is correct and the file exists`
    );
  }

  // Check file is readable
  try {
    await access(absolutePath, constants.R_OK);
  } catch (accessErr) {
    throw new Error(
      `Ontology file is not readable: ${absolutePath}\n` +
      `  Fix: Check file permissions (should be readable)`
    );
  }

  // Read file content
  let content;
  try {
    content = await readFile(absolutePath, 'utf-8');
  } catch (readErr) {
    throw new Error(
      `Failed to read ontology file: ${absolutePath}\n` +
      `  Error: ${readErr.message}\n` +
      `  Fix: Ensure file is a valid text file with UTF-8 encoding`
    );
  }

  // Auto-detect format if not specified
  let detectedFormat = format;
  if (!detectedFormat) {
    const ext = extname(absolutePath).toLowerCase();
    detectedFormat = EXT_TO_FORMAT[ext] || 'turtle';
    console.log(`Auto-detected format: ${detectedFormat} (from extension ${ext})`);
  }

  const store = createStore();

  // Extract prefixes from Turtle/TriG content
  const extractedPrefixes = {};
  if (detectedFormat === 'turtle' || detectedFormat === 'trig') {
    const prefixRegex = /@prefix\s+([^:]*):?\s*<([^>]+)>\s*\./g;
    let match;
    while ((match = prefixRegex.exec(content)) !== null) {
      extractedPrefixes[match[1]] = match[2];
    }
  }

  // Load content into store
  let tripleCount = 0;
  const ext = FORMAT_TO_EXT[detectedFormat] || detectedFormat;

  if (typeof store.load === 'function') {
    try {
      await store.load(content, { format: ext, baseIRI: base_iri });
      tripleCount = store.size || 0;
    } catch (loadErr) {
      // Extract helpful information from parse error
      const errorMsg = loadErr.message || String(loadErr);
      const lineMatch = errorMsg.match(/line[:\s]+(\d+)/i);
      const lineInfo = lineMatch ? `\n  Line: ${lineMatch[1]}` : '';

      // Suggest possible fixes based on format
      const formatSuggestions = [
        `\nPossible fixes:`,
        `  1. Check ${detectedFormat.toUpperCase()} syntax is valid`,
        `  2. Verify all prefixes are declared with @prefix`,
        `  3. Check for unescaped special characters in literals`,
        `  4. Ensure all statements end with a period (.)`
      ];

      if (!format) {
        formatSuggestions.push(
          `  5. Try specifying format explicitly in config:`,
          `     "format": "turtle"  (or "ntriples", "rdfxml", etc.)`
        );
      }

      throw new Error(
        `Failed to parse ontology file: ${absolutePath}\n` +
        `  Format: ${detectedFormat}${lineInfo}\n` +
        `  Parse error: ${errorMsg}` +
        formatSuggestions.join('\n')
      );
    }
  } else {
    // Fallback: Count triples from content (estimate)
    const tripleLines = content.split('\n').filter(l =>
      l.trim() && !l.trim().startsWith('@') && !l.trim().startsWith('#')
    );
    tripleCount = tripleLines.length;
  }

  const prefixes = { ...COMMON_PREFIXES, ...extractedPrefixes, ...customPrefixes };

  return { store, tripleCount, prefixes, source: absolutePath, format: detectedFormat };
}

/**
 * Extract prefixes from store
 */
export function extractPrefixes(store) {
  const prefixes = { ...COMMON_PREFIXES };
  if (typeof store.getPrefixes === 'function') {
    Object.assign(prefixes, store.getPrefixes());
  }
  return prefixes;
}

export default { loadOntology, extractPrefixes };
