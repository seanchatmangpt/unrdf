/**
 * @file SPARQL Query Executor
 * @module cli/commands/sync/sparql-executor
 * @description Executes SPARQL queries against RDF ontology store
 */
import { COMMON_PREFIXES, executeQuery } from '@unrdf/core';

/**
 * Execute SPARQL query and return results
 * @param {Object} store - RDF store
 * @param {string} query - SPARQL query string
 * @param {Object} prefixes - Prefix mappings
 * @param {Object} options - Query options
 * @returns {Promise<Array>} Query results as array of objects
 */
export async function executeSparqlQuery(store, query, prefixes = {}, options = {}) {
  const { timeout = 5000 } = options;
  
  const prefixDeclarations = buildPrefixDeclarations(prefixes);
  const fullQuery = prefixDeclarations + '\n' + query.trim();
  
  try {
    const results = await Promise.race([
      executeQueryInternal(store, fullQuery),
      new Promise((_, reject) => setTimeout(() => reject(new Error('Query timeout')), timeout)),
    ]);
    return transformResults(results);
  } catch (err) {
    throw new Error(`SPARQL query failed: ${err.message}`);
  }
}

async function executeQueryInternal(store, query) {
  if (typeof executeQuery === 'function') {
    try { return await executeQuery(store, query); } catch (e) { /* fallthrough */ }
  }
  if (typeof store.query === 'function') return store.query(query);
  if (typeof store.execute === 'function') return store.execute(query);
  throw new Error('Store does not support SPARQL queries');
}

/**
 * Build PREFIX declarations string
 */
export function buildPrefixDeclarations(prefixes) {
  const allPrefixes = { ...COMMON_PREFIXES, ...prefixes };
  return Object.entries(allPrefixes)
    .map(([prefix, uri]) => `PREFIX ${prefix}: <${uri}>`)
    .join('\n');
}

/**
 * Transform SPARQL results to template-friendly format
 */
export function transformResults(results) {
  let bindings = [];
  if (Array.isArray(results)) bindings = results;
  else if (results?.results?.bindings) bindings = results.results.bindings;
  else if (results?.bindings) bindings = results.bindings;
  else if (typeof results?.[Symbol.iterator] === 'function') bindings = [...results];
  
  return bindings.map((binding, index) => {
    const row = { _index: index };
    for (const [key, value] of Object.entries(binding)) {
      const k = key.startsWith('?') ? key : `?${key}`;
      if (value === null || value === undefined) row[k] = null;
      else if (typeof value === 'object') {
        row[k] = value.value ?? value.id ?? String(value);
        if (value.termType === 'NamedNode' || value.type === 'uri') {
          const uri = value.value || value.id;
          row[`${k}_localName`] = uri.split(/[#/]/).pop();
        }
      } else row[k] = value;
    }
    return row;
  });
}

export default { executeSparqlQuery, buildPrefixDeclarations, transformResults };
