/**
 * @fileoverview useZod composable - Dynamic Zod schema generation from RDF data
 *
 * This composable generates Zod schemas dynamically from RDF data patterns.
 * Focuses on 80/20 use cases: generating schemas from SPARQL results and RDF shapes.
 *
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { z } from 'zod';
import { useStoreContext } from '../context/index.mjs';

/**
 * Create a Zod schema generation composable
 *
 * @param {Object} [options] - Zod options
 * @param {boolean} [options.strict=true] - Enable strict validation
 * @returns {Object} Zod schema generation interface
 *
 * @example
 * // Initialize store context first
 * const runApp = initStore();
 *
 * runApp(() => {
 *   const zod = useZod();
 *
 *   // Generate schema from SPARQL results
 *   const results = graph.select(`
 *     PREFIX foaf: <http://xmlns.com/foaf/0.1/>
 *     SELECT ?id ?name ?age WHERE {
 *       ?id a foaf:Person ;
 *           foaf:name ?name ;
 *           foaf:age ?age .
 *     }
 *   `);
 *
 *   const schema = zod.generateFromResults(results);
 *   const validated = schema.parse(results[0]);
 * });
 *
 * @throws {Error} If store context is not initialized
 */
export function useZod(options = {}) {
  const storeContext = useStoreContext();
  const engine = storeContext.engine;
  const store = storeContext.store;

  const { strict = true } = options;

  return {
    /**
     * Generate Zod schema from SPARQL SELECT results
     * @param {Array<Object>} results - SPARQL SELECT results
     * @param {Object} [options] - Generation options
     * @param {boolean} [options.inferTypes=true] - Infer types from values
     * @param {boolean} [options.allowOptional=true] - Allow optional fields
     * @returns {z.ZodSchema} Generated Zod schema
     *
     * @example
     * const results = [
     *   { id: "http://example.org/alice", name: "Alice", age: "30" },
     *   { id: "http://example.org/bob", name: "Bob", age: "25" }
     * ];
     * const schema = zod.generateFromResults(results);
     * // Returns: z.object({ id: z.string().url(), name: z.string(), age: z.string() })
     */
    generateFromResults(results, options = {}) {
      const { inferTypes = true, allowOptional = true } = options;

      if (!results || results.length === 0) {
        return z.object({});
      }

      const shape = {};
      const sample = results[0];

      for (const [key, value] of Object.entries(sample)) {
        if (value === null || value === undefined) {
          if (allowOptional) {
            shape[key] = z.any().optional();
          }
          continue;
        }

        if (inferTypes) {
          shape[key] = this._inferZodType(value, key);
        } else {
          shape[key] = z.any();
        }
      }

      return strict ? z.object(shape) : z.object(shape).partial();
    },

    /**
     * Generate Zod schema from RDF store patterns
     * @param {string} subjectPattern - Subject pattern to analyze
     * @param {Object} [options] - Generation options
     * @returns {z.ZodSchema} Generated Zod schema
     *
     * @example
     * const schema = zod.generateFromPattern("http://example.org/person");
     * // Analyzes store for patterns and generates schema
     */
    generateFromPattern(subjectPattern, _options = {}) {
      const quads = store.getQuads ? store.getQuads(null, null, null, null) : Array.from(store);
      const patternQuads = quads.filter(
        q => q.subject.value === subjectPattern || q.subject.value.includes(subjectPattern)
      );

      if (patternQuads.length === 0) {
        return z.object({});
      }

      const shape = {};
      const predicates = new Set();

      for (const quad of patternQuads) {
        const pred = quad.predicate.value;
        if (predicates.has(pred)) continue;
        predicates.add(pred);

        const objectQuads = patternQuads.filter(q => q.predicate.value === pred);
        const values = objectQuads.map(q => q.object.value);

        shape[this._predicateToKey(pred)] = this._inferZodTypeFromValues(values);
      }

      return strict ? z.object(shape) : z.object(shape).partial();
    },

    /**
     * Generate Zod schema from SHACL shapes
     * @param {string} shapeIRI - SHACL shape IRI
     * @param {Object} [options] - Generation options
     * @returns {z.ZodSchema} Generated Zod schema
     *
     * @example
     * const schema = zod.generateFromSHACL("http://example.org/PersonShape");
     */
    generateFromSHACL(shapeIRI, _options = {}) {
      const quads = store.getQuads ? store.getQuads(null, null, null, null) : Array.from(store);
      const shapeQuads = quads.filter(
        q => q.subject.value === shapeIRI || q.subject.value.includes(shapeIRI)
      );

      if (shapeQuads.length === 0) {
        return z.object({});
      }

      const shape = {};
      const propertyPaths = new Set();

      // Find property paths
      for (const quad of shapeQuads) {
        if (quad.predicate.value === 'http://www.w3.org/ns/shacl#path') {
          propertyPaths.add(quad.object.value);
        }
      }

      // Generate schema for each property
      for (const path of propertyPaths) {
        const key = this._predicateToKey(path);
        const propertyQuads = shapeQuads.filter(
          q => q.predicate.value === 'http://www.w3.org/ns/shacl#path' && q.object.value === path
        );

        shape[key] = this._generateFromSHACLProperty(propertyQuads);
      }

      return strict ? z.object(shape) : z.object(shape).partial();
    },

    /**
     * Validate data against generated schema
     * @param {Object|Array} data - Data to validate
     * @param {z.ZodSchema} schema - Schema to validate against
     * @param {Object} [options] - Validation options
     * @returns {Object} Validation result
     *
     * @example
     * const schema = zod.generateFromResults(results);
     * const validation = zod.validate(data, schema);
     * if (validation.success) {
     *   console.log("Valid:", validation.data);
     * }
     */
    validate(data, schema, _options = {}) {
      try {
        if (strict) {
          const validated = schema.parse(data);
          return { success: true, data: validated, error: null };
        } else {
          const result = schema.safeParse(data);
          return {
            success: result.success,
            data: result.success ? result.data : null,
            error: result.success ? null : result.error,
          };
        }
      } catch (error) {
        return { success: false, data: null, error };
      }
    },

    /**
     * Get the underlying engine
     * @returns {RdfEngine} RdfEngine instance
     */
    get engine() {
      return engine;
    },

    /**
     * Get the underlying store
     * @returns {Store} N3.Store instance
     */
    get store() {
      return store;
    },

    /**
     * Infer Zod type from a single value
     * @param {any} value - Value to analyze
     * @param {string} key - Key name for context
     * @returns {z.ZodType} Inferred Zod type
     * @private
     */
    _inferZodType(value, key) {
      if (typeof value === 'string') {
        // Check for common patterns
        if (key.toLowerCase().includes('id') || key.toLowerCase().includes('uri')) {
          return z.string().url();
        }
        if (key.toLowerCase().includes('email')) {
          return z.string().email();
        }
        if (key.toLowerCase().includes('date')) {
          return z.string().datetime();
        }
        return z.string();
      }

      if (typeof value === 'number') {
        return z.number();
      }

      if (typeof value === 'boolean') {
        return z.boolean();
      }

      if (Array.isArray(value)) {
        return z.array(z.any());
      }

      if (typeof value === 'object' && value !== null) {
        return z.object({});
      }

      return z.any();
    },

    /**
     * Infer Zod type from multiple values
     * @param {Array} values - Values to analyze
     * @returns {z.ZodType} Inferred Zod type
     * @private
     */
    _inferZodTypeFromValues(values) {
      if (values.length === 0) return z.any();

      const types = new Set(values.map(v => typeof v));

      if (types.size === 1) {
        const type = types.values().next().value;
        if (type === 'string') {
          // Check for common patterns
          if (values.some(v => v.startsWith('http'))) {
            return z.string().url();
          }
          if (values.some(v => v.includes('@') && v.includes('.'))) {
            return z.string().email();
          }
          return z.string();
        }
        if (type === 'number') return z.number();
        if (type === 'boolean') return z.boolean();
      }

      return z.union([z.string(), z.number(), z.boolean()]);
    },

    /**
     * Convert predicate IRI to schema key
     * @param {string} predicate - Predicate IRI
     * @returns {string} Schema key
     * @private
     */
    _predicateToKey(predicate) {
      // Extract local name from IRI
      const parts = predicate.split('/');
      const localName = parts[parts.length - 1];

      // Convert to camelCase
      return localName.replace(/([A-Z])/g, (match, p1, offset) =>
        offset === 0 ? p1.toLowerCase() : p1
      );
    },

    /**
     * Generate Zod type from SHACL property constraints
     * @param {Array} propertyQuads - SHACL property quads
     * @returns {z.ZodType} Generated Zod type
     * @private
     */
    _generateFromSHACLProperty(propertyQuads) {
      let baseType = z.string();

      for (const quad of propertyQuads) {
        const pred = quad.predicate.value;
        const obj = quad.object.value;

        switch (pred) {
          case 'http://www.w3.org/ns/shacl#datatype':
            if (obj === 'http://www.w3.org/2001/XMLSchema#string') {
              baseType = z.string();
            } else if (obj === 'http://www.w3.org/2001/XMLSchema#integer') {
              baseType = z.number().int();
            } else if (obj === 'http://www.w3.org/2001/XMLSchema#boolean') {
              baseType = z.boolean();
            }
            break;
          case 'http://www.w3.org/ns/shacl#minCount':
            if (obj === '0') {
              baseType = baseType.optional();
            }
            break;
          case 'http://www.w3.org/ns/shacl#maxCount':
            if (obj === '1') {
              // Already single value
            } else if (obj !== '1') {
              baseType = z.array(baseType);
            }
            break;
        }
      }

      return baseType;
    },
  };
}
