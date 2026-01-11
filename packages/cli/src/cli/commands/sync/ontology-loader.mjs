/**
 * @file Ontology Loader
 * @module cli/commands/sync/ontology-loader
 * @description Loads RDF ontology into store for SPARQL queries
 */
import { readFile } from 'fs/promises';
import { existsSync } from 'fs';
import { resolve, dirname } from 'path';
import { createStore, COMMON_PREFIXES } from '@unrdf/core';

/**
 * Load ontology from file into RDF store
 * @param {Object} ontologyConfig - Ontology configuration
 * @param {string} baseDir - Base directory for path resolution
 * @returns {Promise<Object>} Object with store and metadata
 */
export async function loadOntology(ontologyConfig, baseDir = process.cwd()) {
  const { source, format = 'turtle', base_iri, prefixes: customPrefixes } = ontologyConfig;
  
  const absolutePath = resolve(baseDir, source);
  if (!existsSync(absolutePath)) {
    throw new Error(`Ontology file not found: ${absolutePath}`);
  }
  
  const content = await readFile(absolutePath, 'utf-8');
  const store = createStore();
  
  // Extract prefixes from Turtle content
  const extractedPrefixes = {};
  const prefixRegex = /@prefix\s+([^:]*):?\s*<([^>]+)>\s*\./g;
  let match;
  while ((match = prefixRegex.exec(content)) !== null) {
    extractedPrefixes[match[1]] = match[2];
  }
  
  // Load content into store
  let tripleCount = 0;
  if (typeof store.load === 'function') {
    await store.load(content, { format, baseIRI: base_iri });
    tripleCount = store.size || 0;
  } else {
    // Count triples from content (estimate)
    const tripleLines = content.split('\n').filter(l => 
      l.trim() && !l.trim().startsWith('@') && !l.trim().startsWith('#')
    );
    tripleCount = tripleLines.length;
  }
  
  const prefixes = { ...COMMON_PREFIXES, ...extractedPrefixes, ...customPrefixes };
  
  return { store, tripleCount, prefixes, source: absolutePath, format };
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
