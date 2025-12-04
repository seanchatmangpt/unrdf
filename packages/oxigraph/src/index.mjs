import { OxigraphStore } from './store.mjs';
import oxigraph from 'oxigraph';

/**
 * Create a new Oxigraph-backed RDF store
 * @param {Array} [quads] - Initial quads
 * @returns {OxigraphStore}
 */
export function createStore(quads) {
  return new OxigraphStore(quads);
}

/**
 * Export Oxigraph data model functions for creating RDF terms
 */
export const dataFactory = {
  namedNode: oxigraph.namedNode,
  blankNode: oxigraph.blankNode,
  literal: oxigraph.literal,
  defaultGraph: oxigraph.defaultGraph,
  quad: oxigraph.quad,
  triple: oxigraph.triple,
};

/**
 * Re-export types for documentation
 */
export { OxigraphStore };

export default {
  createStore,
  dataFactory,
  OxigraphStore,
};
