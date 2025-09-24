/**
 * @fileoverview useJSONLD composable - JSON-LD processing with jsonld library
 * 
 * This composable provides comprehensive JSON-LD processing capabilities
 * using the jsonld library for compaction, expansion, framing, and more.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import jsonld from "jsonld";
import { useStoreContext } from "../context/index.mjs";

/**
 * Create a JSONLD composable for JSON-LD processing
 * 
 * @param {Object} [options] - JSONLD options
 * @param {Object} [options.defaultContext] - Default JSON-LD context
 * @param {boolean} [options.strict=true] - Enable strict processing
 * @returns {Object} JSONLD interface
 * 
 * @example
 * // Initialize store context first
 * const runApp = initStore();
 * 
 * runApp(() => {
 *   const jsonld = useJSONLD();
 * 
 *   // Compact JSON-LD
 *   const compacted = await jsonld.compact(doc, context);
 * 
 *   // Expand JSON-LD
 *   const expanded = await jsonld.expand(doc);
 * 
 *   // Frame JSON-LD
 *   const framed = await jsonld.frame(doc, frame);
 * 
 *   // Convert RDF to JSON-LD
 *   const jsonldDoc = await jsonld.fromRDF(store);
 * });
 * 
 * @throws {Error} If store context is not initialized
 */
export function useJSONLD(options = {}) {
  // Get the engine from context
  const storeContext = useStoreContext();
  const engine = storeContext.engine;
  
  const { 
    defaultContext = {},
    strict = true 
  } = options;

  return {
    /**
     * The underlying RDF engine
     * @type {RdfEngine}
     */
    get engine() {
      return engine;
    },

    /**
     * The underlying store
     * @type {Store}
     */
    get store() {
      return storeContext.store;
    },

    /**
     * Compact a JSON-LD document
     * @param {Object|Array} doc - JSON-LD document to compact
     * @param {Object|string} context - Context to use for compaction
     * @param {Object} [options] - Compaction options
     * @returns {Promise<Object|Array>} Compacted JSON-LD document
     * 
     * @example
     * const compacted = await jsonld.compact({
     *   "@id": "http://example.org/person",
     *   "name": "John Doe"
     * }, {
     *   "@context": {
     *     "name": "http://xmlns.com/foaf/0.1/name"
     *   }
     * });
     */
    async compact(doc, context, options = {}) {
      try {
        return await jsonld.compact(doc, context, {
          base: engine.baseIRI,
          ...options
        });
      } catch (error) {
        if (strict) {
          throw new Error(`JSON-LD compaction failed: ${error.message}`);
        }
        return doc;
      }
    },

    /**
     * Expand a JSON-LD document
     * @param {Object|Array} doc - JSON-LD document to expand
     * @param {Object} [options] - Expansion options
     * @returns {Promise<Object|Array>} Expanded JSON-LD document
     * 
     * @example
     * const expanded = await jsonld.expand({
     *   "@context": {
     *     "name": "http://xmlns.com/foaf/0.1/name"
     *   },
     *   "@id": "http://example.org/person",
     *   "name": "John Doe"
     * });
     */
    async expand(doc, options = {}) {
      try {
        return await jsonld.expand(doc, {
          base: engine.baseIRI,
          ...options
        });
      } catch (error) {
        if (strict) {
          throw new Error(`JSON-LD expansion failed: ${error.message}`);
        }
        return doc;
      }
    },

    /**
     * Frame a JSON-LD document
     * @param {Object|Array} doc - JSON-LD document to frame
     * @param {Object} frame - Frame to use
     * @param {Object} [options] - Framing options
     * @returns {Promise<Object|Array>} Framed JSON-LD document
     * 
     * @example
     * const framed = await jsonld.frame(doc, {
     *   "@type": "Person",
     *   "name": {}
     * });
     */
    async frame(doc, frame, options = {}) {
      try {
        return await jsonld.frame(doc, frame, {
          base: engine.baseIRI,
          ...options
        });
      } catch (error) {
        if (strict) {
          throw new Error(`JSON-LD framing failed: ${error.message}`);
        }
        return doc;
      }
    },

    /**
     * Flatten a JSON-LD document
     * @param {Object|Array} doc - JSON-LD document to flatten
     * @param {Object|string} [context] - Context to use for flattening
     * @param {Object} [options] - Flattening options
     * @returns {Promise<Object|Array>} Flattened JSON-LD document
     * 
     * @example
     * const flattened = await jsonld.flatten(doc, context);
     */
    async flatten(doc, context, options = {}) {
      try {
        return await jsonld.flatten(doc, context, {
          base: engine.baseIRI,
          ...options
        });
      } catch (error) {
        if (strict) {
          throw new Error(`JSON-LD flattening failed: ${error.message}`);
        }
        return doc;
      }
    },

    /**
     * Convert RDF store to JSON-LD
     * @param {Store} [store] - Store to convert (uses context store if not provided)
     * @param {Object} [options] - Conversion options
     * @param {Object} [options.context] - JSON-LD context
     * @param {string} [options.format] - Input format (nquads, turtle, etc.)
     * @returns {Promise<Object|Array>} JSON-LD document
     * 
     * @example
     * const jsonldDoc = await jsonld.fromRDF(store, {
     *   context: { "@vocab": "http://example.org/" }
     * });
     */
    async fromRDF(store, options = {}) {
      const targetStore = store || storeContext.store;
      const { context = {}, format = 'nquads' } = options;

      try {
        // Convert store to N-Quads
        const nquads = await engine.serializeNQuads(targetStore);
        
        // Convert N-Quads to JSON-LD
        const jsonldDoc = await jsonld.fromRDF(nquads, {
          format: format,
          base: engine.baseIRI,
          ...options
        });

        // Apply context if provided
        if (Object.keys(context).length > 0) {
          return await this.compact(jsonldDoc, context);
        }

        return jsonldDoc;
      } catch (error) {
        if (strict) {
          throw new Error(`RDF to JSON-LD conversion failed: ${error.message}`);
        }
        return [];
      }
    },

    /**
     * Convert JSON-LD to RDF store
     * @param {Object|Array} doc - JSON-LD document to convert
     * @param {Object} [options] - Conversion options
     * @param {string} [options.format] - Output format (nquads, turtle, etc.)
     * @returns {Promise<Store>} RDF store
     * 
     * @example
     * const store = await jsonld.toRDF({
     *   "@id": "http://example.org/person",
     *   "name": "John Doe"
     * });
     */
    async toRDF(doc, options = {}) {
      const { format = 'nquads' } = options;

      try {
        // Convert JSON-LD to N-Quads
        const nquads = await jsonld.toRDF(doc, {
          format: 'application/n-quads',
          base: engine.baseIRI,
          ...options
        });

        // Parse N-Quads into store
        return engine.parseNQuads(nquads);
      } catch (error) {
        if (strict) {
          throw new Error(`JSON-LD to RDF conversion failed: ${error.message}`);
        }
        return engine.createStore();
      }
    },

    /**
     * Normalize a JSON-LD document
     * @param {Object|Array} doc - JSON-LD document to normalize
     * @param {Object} [options] - Normalization options
     * @returns {Promise<string>} Normalized JSON-LD as N-Quads
     * 
     * @example
     * const normalized = await jsonld.normalize(doc);
     */
    async normalize(doc, options = {}) {
      try {
        return await jsonld.normalize(doc, {
          base: engine.baseIRI,
          ...options
        });
      } catch (error) {
        if (strict) {
          throw new Error(`JSON-LD normalization failed: ${error.message}`);
        }
        return '';
      }
    },

    /**
     * Check if a document is valid JSON-LD
     * @param {Object|Array} doc - Document to validate
     * @param {Object} [options] - Validation options
     * @returns {Promise<Object>} Validation result
     * 
     * @example
     * const validation = await jsonld.validate(doc);
     * if (!validation.valid) {
     *   console.log("Validation errors:", validation.errors);
     * }
     */
    async validate(doc, options = {}) {
      try {
        const result = await jsonld.validate(doc, {
          base: engine.baseIRI,
          ...options
        });

        return {
          valid: result.length === 0,
          errors: result,
          warnings: []
        };
      } catch (error) {
        return {
          valid: false,
          errors: [error.message],
          warnings: []
        };
      }
    },

    /**
     * Create a JSON-LD context
     * @param {Object} mappings - Prefix to IRI mappings
     * @returns {Object} JSON-LD context
     * 
     * @example
     * const context = jsonld.createContext({
     *   "foaf": "http://xmlns.com/foaf/0.1/",
     *   "ex": "http://example.org/"
     * });
     */
    createContext(mappings) {
      const context = {
        "@context": {}
      };

      for (const [prefix, iri] of Object.entries(mappings)) {
        context["@context"][prefix] = iri;
      }

      return context;
    },

    /**
     * Merge multiple JSON-LD documents
     * @param {...Object|Array} docs - JSON-LD documents to merge
     * @returns {Promise<Object|Array>} Merged JSON-LD document
     * 
     * @example
     * const merged = await jsonld.merge(doc1, doc2, doc3);
     */
    async merge(...docs) {
      try {
        // Expand all documents first
        const expandedDocs = await Promise.all(
          docs.map(doc => this.expand(doc))
        );

        // Merge expanded documents
        const merged = [];
        for (const doc of expandedDocs) {
          if (Array.isArray(doc)) {
            merged.push(...doc);
          } else {
            merged.push(doc);
          }
        }

        return merged;
      } catch (error) {
        if (strict) {
          throw new Error(`JSON-LD merge failed: ${error.message}`);
        }
        return [];
      }
    },

    /**
     * Extract specific properties from JSON-LD document
     * @param {Object|Array} doc - JSON-LD document
     * @param {Array<string>} properties - Properties to extract
     * @returns {Promise<Object>} Extracted properties
     * 
     * @example
     * const extracted = await jsonld.extract(doc, ['name', 'email']);
     */
    async extract(doc, properties) {
      try {
        const expanded = await this.expand(doc);
        const result = {};

        const processNode = (node) => {
          if (typeof node === 'object' && node !== null) {
            for (const prop of properties) {
              if (node[prop]) {
                if (!result[prop]) {
                  result[prop] = [];
                }
                if (Array.isArray(node[prop])) {
                  result[prop].push(...node[prop]);
                } else {
                  result[prop].push(node[prop]);
                }
              }
            }
          }
        };

        if (Array.isArray(expanded)) {
          expanded.forEach(processNode);
        } else {
          processNode(expanded);
        }

        return result;
      } catch (error) {
        if (strict) {
          throw new Error(`JSON-LD extraction failed: ${error.message}`);
        }
        return {};
      }
    },

    /**
     * Get statistics about JSON-LD document
     * @param {Object|Array} doc - JSON-LD document to analyze
     * @returns {Object} Document statistics
     * 
     * @example
     * const stats = jsonld.getStats(doc);
     * console.log(`Nodes: ${stats.nodes}, Properties: ${stats.properties}`);
     */
    getStats(doc) {
      const stats = {
        nodes: 0,
        properties: 0,
        literals: 0,
        iris: 0,
        blankNodes: 0,
        contexts: 0
      };

      const processNode = (node) => {
        if (typeof node === 'object' && node !== null) {
          stats.nodes++;

          if (node['@context']) {
            stats.contexts++;
          }

          if (node['@id']) {
            if (node['@id'].startsWith('_:')) {
              stats.blankNodes++;
            } else {
              stats.iris++;
            }
          }

          for (const [key, value] of Object.entries(node)) {
            if (key.startsWith('@')) continue;

            stats.properties++;

            if (typeof value === 'string') {
              stats.literals++;
            } else if (Array.isArray(value)) {
              value.forEach(processNode);
            } else if (typeof value === 'object') {
              processNode(value);
            }
          }
        }
      };

      if (Array.isArray(doc)) {
        doc.forEach(processNode);
      } else {
        processNode(doc);
      }

      return stats;
    }
  };
}
