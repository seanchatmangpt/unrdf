/**
 * @fileoverview useJsonLd composable - JSON-LD operations
 * 
 * This composable provides JSON-LD format operations for RDF data.
 * It enforces the "One JSON-LD Rule" - standard JSON-LD format only.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { RdfEngine } from "../engines/rdf-engine.mjs";

/**
 * Create a JSON-LD composable for JSON-LD operations
 * 
 * @param {Object} [options] - JSON-LD options
 * @param {string} [options.baseIRI] - Base IRI for operations
 * @param {boolean} [options.strict=true] - Enable strict processing
 * @returns {Object} JSON-LD composable interface
 * 
 * @example
 * const jsonld = useJsonLd();
 * 
 * // Convert store to JSON-LD
 * const doc = await jsonld.toJSONLD(store, { context: { ex: "http://example.org/" } });
 * 
 * // Convert JSON-LD to store
 * const store = await jsonld.fromJSONLD(doc);
 */
export function useJsonLd(options = {}) {
  const {
    baseIRI = "http://example.org/",
    strict = true
  } = options || {};

  const engine = new RdfEngine({ baseIRI });

  return {
    /**
     * Convert a store to JSON-LD
     * @param {Store} store - N3.Store to convert
     * @param {Object} [options] - Conversion options
     * @param {Object} [options.context] - JSON-LD context
     * @param {Object} [options.frame] - JSON-LD frame
     * @param {boolean} [options.compact=true] - Enable compaction
     * @returns {Promise<Object>} JSON-LD document
     * 
     * @example
     * const doc = await jsonld.toJSONLD(store, {
     *   context: {
     *     "@vocab": "http://example.org/",
     *     "name": "http://xmlns.com/foaf/0.1/name"
     *   }
     * });
     */
    async toJSONLD(store, options = {}) {
      if (strict && (!store || typeof store.getQuads !== "function")) {
        throw new Error("[useJsonLd] Store is required");
      }
      
      if (!store || typeof store.getQuads !== "function") {
        return { "@context": {}, "@graph": [] };
      }

      const {
        context = {},
        frame = null,
        compact = true
      } = options;

      try {
        if (frame) {
          return await engine.toJSONLD(store, { frame });
        }
        
        const result = await engine.toJSONLD(store, { context });
        
        // Ensure @context is present
        if (compact && !result["@context"]) {
          result["@context"] = context;
        }
        
        return result;
      } catch (error) {
        if (strict) {
          throw new Error(`[useJsonLd] Conversion error: ${error.message}`);
        }
        return { "@context": context, "@graph": [] };
      }
    },

    /**
     * Convert JSON-LD to a store
     * @param {Object} jsonldDoc - JSON-LD document
     * @param {Object} [options] - Conversion options
     * @param {string} [options.baseIRI] - Base IRI for conversion
     * @returns {Promise<Store>} N3.Store containing converted data
     * 
     * @example
     * const store = await jsonld.fromJSONLD({
     *   "@context": { "ex": "http://example.org/" },
     *   "@id": "ex:person",
     *   "name": "John Doe"
     * });
     */
    async fromJSONLD(jsonldDoc, options = {}) {
      if (!jsonldDoc || typeof jsonldDoc !== "object") {
        throw new Error("[useJsonLd] JSON-LD document is required");
      }

      const { baseIRI: conversionBaseIRI } = options;

      try {
        return await engine.fromJSONLD(jsonldDoc);
      } catch (error) {
        if (strict) {
          throw new Error(`[useJsonLd] Conversion error: ${error.message}`);
        }
        return engine.createStore();
      }
    },

    /**
     * Compact a JSON-LD document
     * @param {Object} jsonldDoc - JSON-LD document to compact
     * @param {Object} context - Compaction context
     * @param {Object} [options] - Compaction options
     * @returns {Promise<Object>} Compacted JSON-LD document
     * 
     * @example
     * const compacted = await jsonld.compact(doc, {
     *   "@vocab": "http://example.org/",
     *   "name": "http://xmlns.com/foaf/0.1/name"
     * });
     */
    async compact(jsonldDoc, context, options = {}) {
      if (!jsonldDoc || typeof jsonldDoc !== "object") {
        throw new Error("[useJsonLd] JSON-LD document is required");
      }
      
      if (!context || typeof context !== "object") {
        throw new Error("[useJsonLd] Context is required");
      }

      try {
        const jsonld = await import("jsonld");
        return await jsonld.compact(jsonldDoc, context, options);
      } catch (error) {
        if (strict) {
          throw new Error(`[useJsonLd] Compaction error: ${error.message}`);
        }
        return { "@context": context, "@graph": [] };
      }
    },

    /**
     * Expand a JSON-LD document
     * @param {Object} jsonldDoc - JSON-LD document to expand
     * @param {Object} [options] - Expansion options
     * @returns {Promise<Object>} Expanded JSON-LD document
     * 
     * @example
     * const expanded = await jsonld.expand(doc);
     */
    async expand(jsonldDoc, options = {}) {
      if (!jsonldDoc || typeof jsonldDoc !== "object") {
        throw new Error("[useJsonLd] JSON-LD document is required");
      }

      try {
        const jsonld = await import("jsonld");
        return await jsonld.expand(jsonldDoc, options);
      } catch (error) {
        if (strict) {
          throw new Error(`[useJsonLd] Expansion error: ${error.message}`);
        }
        return { "@graph": [] };
      }
    },

    /**
     * Frame a JSON-LD document
     * @param {Object} jsonldDoc - JSON-LD document to frame
     * @param {Object} frame - JSON-LD frame
     * @param {Object} [options] - Framing options
     * @returns {Promise<Object>} Framed JSON-LD document
     * 
     * @example
     * const framed = await jsonld.frame(doc, {
     *   "@type": "Person",
     *   "name": {}
     * });
     */
    async frame(jsonldDoc, frame, options = {}) {
      if (!jsonldDoc || typeof jsonldDoc !== "object") {
        throw new Error("[useJsonLd] JSON-LD document is required");
      }
      
      if (!frame || typeof frame !== "object") {
        throw new Error("[useJsonLd] Frame is required");
      }

      try {
        const jsonld = await import("jsonld");
        return await jsonld.frame(jsonldDoc, frame, options);
      } catch (error) {
        if (strict) {
          throw new Error(`[useJsonLd] Framing error: ${error.message}`);
        }
        return { "@context": {}, "@graph": [] };
      }
    },

    /**
     * Flatten a JSON-LD document
     * @param {Object} jsonldDoc - JSON-LD document to flatten
     * @param {Object} [options] - Flattening options
     * @returns {Promise<Object>} Flattened JSON-LD document
     * 
     * @example
     * const flattened = await jsonld.flatten(doc);
     */
    async flatten(jsonldDoc, options = {}) {
      if (!jsonldDoc || typeof jsonldDoc !== "object") {
        throw new Error("[useJsonLd] JSON-LD document is required");
      }

      try {
        const jsonld = await import("jsonld");
        return await jsonld.flatten(jsonldDoc, options);
      } catch (error) {
        if (strict) {
          throw new Error(`[useJsonLd] Flattening error: ${error.message}`);
        }
        return { "@context": {}, "@graph": [] };
      }
    },

    /**
     * Validate a JSON-LD document
     * @param {Object} jsonldDoc - JSON-LD document to validate
     * @returns {Object} Validation result
     * 
     * @example
     * const validation = await jsonld.validate(doc);
     * if (validation.valid) {
     *   console.log("Valid JSON-LD");
     * } else {
     *   console.log(`Invalid: ${validation.error}`);
     * }
     */
    async validate(jsonldDoc) {
      if (!jsonldDoc || typeof jsonldDoc !== "object") {
        return {
          valid: false,
          error: "JSON-LD document must be an object",
          warnings: []
        };
      }

      try {
        // Try to expand the document to check for errors
        await this.expand(jsonldDoc);
        
        return {
          valid: true,
          error: null,
          warnings: []
        };
      } catch (error) {
        return {
          valid: false,
          error: error.message,
          warnings: []
        };
      }
    },

    /**
     * Get statistics about a JSON-LD document
     * @param {Object} jsonldDoc - JSON-LD document to analyze
     * @returns {Object} Statistics object
     * 
     * @example
     * const stats = jsonld.getStats(doc);
     * console.log(`Nodes: ${stats.nodes}, Properties: ${stats.properties}`);
     */
    getStats(jsonldDoc) {
      if (!jsonldDoc || typeof jsonldDoc !== "object") {
        return {
          nodes: 0,
          properties: 0,
          contexts: 0,
          hasContext: false,
          hasGraph: false,
          hasId: false
        };
      }

      const stats = {
        nodes: 0,
        properties: 0,
        contexts: 0,
        hasContext: false,
        hasGraph: false,
        hasId: false
      };

      // Check for context
      if (jsonldDoc["@context"]) {
        stats.hasContext = true;
        stats.contexts = 1;
      }

      // Check for graph
      if (jsonldDoc["@graph"]) {
        stats.hasGraph = true;
        if (Array.isArray(jsonldDoc["@graph"])) {
          stats.nodes += jsonldDoc["@graph"].length;
        }
      }

      // Check for @id
      if (jsonldDoc["@id"]) {
        stats.hasId = true;
        stats.nodes += 1;
      }

      // Count properties
      for (const [key, value] of Object.entries(jsonldDoc)) {
        if (!key.startsWith("@")) {
          stats.properties += 1;
          if (Array.isArray(value)) {
            stats.nodes += value.length;
          } else if (typeof value === "object" && value !== null) {
            stats.nodes += 1;
          }
        }
      }

      return stats;
    },

    /**
     * Convert JSON-LD to Turtle
     * @param {Object} jsonldDoc - JSON-LD document
     * @param {Object} [options] - Conversion options
     * @param {Object} [options.prefixes] - Prefix mappings
     * @returns {Promise<string>} Turtle string
     * 
     * @example
     * const turtle = await jsonld.toTurtle(doc, {
     *   prefixes: { ex: "http://example.org/" }
     * });
     */
    async toTurtle(jsonldDoc, options = {}) {
      const { prefixes = {} } = options;
      
      const store = await this.fromJSONLD(jsonldDoc);
      return await engine.serializeTurtle(store, { prefixes });
    },

    /**
     * Convert Turtle to JSON-LD
     * @param {string} turtle - Turtle string
     * @param {Object} [options] - Conversion options
     * @param {Object} [options.context] - JSON-LD context
     * @returns {Promise<Object>} JSON-LD document
     * 
     * @example
     * const doc = await jsonld.fromTurtle(turtleString, {
     *   context: { ex: "http://example.org/" }
     * });
     */
    async fromTurtle(turtle, options = {}) {
      const { context = {} } = options;
      
      const store = engine.parseTurtle(turtle);
      return await this.toJSONLD(store, { context });
    },

    /**
     * Get the base IRI used for operations
     * @returns {string} Base IRI
     */
    getBaseIRI() {
      return baseIRI;
    },

    /**
     * Check if content is valid JSON-LD
     * @param {Object} content - Content to check
     * @returns {Promise<boolean>} True if valid JSON-LD
     * 
     * @example
     * const isValid = await jsonld.isValid(doc);
     */
    async isValid(content) {
      const validation = await this.validate(content);
      return validation.valid;
    }
  };
}