/**
 * @typedef {Object} OxigraphStoreOptions
 * @property {Array} [quads] - Initial quads to populate the store
 */

/**
 * @typedef {Object} QueryOptions
 * @property {string} [baseIri] - Base IRI for resolving relative IRIs
 * @property {boolean} [useDefaultGraphAsUnion] - Use default graph as union of all graphs
 * @property {Array} [defaultGraph] - Specify default graphs
 * @property {Array} [namedGraphs] - Restrict available named graphs
 * @property {string} [resultsFormat] - Format for query results (json, etc)
 */

/**
 * @typedef {Object} UpdateOptions
 * @property {string} [baseIri] - Base IRI for resolving relative IRIs
 */

/**
 * @typedef {Object} LoadOptions
 * @property {string} format - RDF serialization format (ttl, nt, nq, trig, jsonld, rdf)
 * @property {string} [baseIri] - Base IRI for resolving relative IRIs
 * @property {Object} [toNamedGraph] - Named graph for loaded triples
 * @property {boolean} [unchecked] - Disable validation
 * @property {boolean} [noTransaction] - Disable transactional guarantees
 */

/**
 * @typedef {Object} DumpOptions
 * @property {string} format - Serialization format
 * @property {Object} [fromNamedGraph] - Named graph to dump from
 */

/**
 * @typedef {Object} BenchmarkResult
 * @property {string} operation - Operation name
 * @property {string} engine - Engine name (current-engine or oxigraph)
 * @property {number} duration - Duration in milliseconds
 * @property {number} tripleCount - Number of triples processed
 * @property {number} throughput - Operations per second
 * @property {Error} [error] - Error if operation failed
 */

export {};
