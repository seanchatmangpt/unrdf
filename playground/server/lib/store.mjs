/**
 * @fileoverview Per-request RDF store helpers for the playground (Oxigraph)
 *
 * @module playground/store
 */

import { createStore } from '@unrdf/oxigraph';

/**
 * Create a store loaded with Turtle data.
 *
 * @param {string} [turtle] - Turtle document to load
 * @returns {object} Store
 * @throws {Error} If the Turtle is invalid
 */
export function createTurtleStore(turtle) {
  const store = createStore();
  if (turtle) {
    store.load(turtle, { format: 'text/turtle' });
  }
  return store;
}

/**
 * Serialise an RDF term into a plain JSON object.
 *
 * @param {object} term - RDF/JS term
 * @returns {{type: string, value: string, datatype?: string, language?: string}} JSON term
 */
export function termToJSON(term) {
  const json = { type: term.termType, value: term.value };
  if (term.termType === 'Literal') {
    if (term.language) json.language = term.language;
    else if (term.datatype) json.datatype = term.datatype.value;
  }
  return json;
}

/**
 * @param {object} term - RDF/JS term
 * @returns {string} Term value
 */
export function termValue(term) {
  return term.value;
}

/**
 * Run a SELECT query and return rows as plain objects of RDF/JS terms.
 *
 * @param {object} store - Store
 * @param {string} sparql - SPARQL SELECT query
 * @returns {Array<Record<string, object>>} Rows keyed by variable name
 * @throws {Error} If the query is not a SELECT query
 */
export function selectRows(store, sparql) {
  const result = store.query(sparql);
  if (!Array.isArray(result)) {
    throw new Error('Hook select must be a SPARQL SELECT query');
  }
  return result.map(binding => {
    const row = {};
    for (const [name, term] of binding.entries()) {
      row[name] = term;
    }
    return row;
  });
}
