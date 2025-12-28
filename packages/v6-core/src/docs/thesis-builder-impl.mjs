/**
 * Load ontology into RDF store
 * @param {string} ontologyPath - Path to ontology file (.ttl, .rdf, .owl)
 * @returns {Promise<Object>} RDF store with loaded ontology
 */
export async function loadOntology(ontologyPath) {
  const { readFile } = await import('node:fs/promises');
  const { createStore } = await import('@unrdf/oxigraph');
  
  const data = await readFile(ontologyPath, 'utf8');
  const ext = ontologyPath.toLowerCase().split('.').pop();
  const formatMap = {
    'ttl': 'text/turtle',
    'turtle': 'text/turtle',
    'rdf': 'application/rdf+xml',
    'owl': 'application/rdf+xml',
    'nt': 'application/n-triples'
  };
  const format = formatMap[ext] || 'text/turtle';
  
  const store = createStore();
  store.load(data, { format });
  
  return {
    store,
    loaded: true,
    path: ontologyPath,
    tripleCount: store.match().length
  };
}
