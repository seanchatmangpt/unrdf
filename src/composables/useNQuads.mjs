/**
 * @fileoverview useNQuads composable - N-Quads parsing and serialization
 * 
 * This composable provides N-Quads format operations for RDF data.
 * It enforces the "One N-Quads Rule" - standard N-Quads format only.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { RdfEngine } from "../engines/rdf-engine.mjs";

/**
 * Create an N-Quads composable for N-Quads operations
 * 
 * @param {Object} [options] - N-Quads options
 * @param {string} [options.baseIRI] - Base IRI for parsing
 * @param {boolean} [options.strict=true] - Enable strict parsing
 * @returns {Object} N-Quads composable interface
 * 
 * @example
 * const nquads = useNQuads();
 * 
 * // Parse N-Quads
 * const store = nquads.parse(nquadsString);
 * 
 * // Serialize to N-Quads
 * const nquadsString = await nquads.serialize(store);
 */
export function useNQuads(options = {}) {
  let {
    baseIRI = "http://example.org/",
    strict = true
  } = options;

  let engine = new RdfEngine({ baseIRI });

  return {
    /**
     * Parse N-Quads string into a store
     * @param {string} nquads - N-Quads string to parse
     * @param {Object} [parseOptions] - Parse options
     * @param {string} [parseOptions.baseIRI] - Base IRI for parsing
     * @returns {Store} N3.Store containing parsed quads
     * 
     * @example
     * const store = nquads.parse(`
     *   <http://example.org/s> <http://example.org/p> <http://example.org/o> .
     *   <http://example.org/s> <http://example.org/p> "literal" .
     * `);
     */
    parse(nquads, parseOptions = {}) {
      if (typeof nquads !== "string") {
        throw new TypeError("[useNQuads] N-Quads content must be a string");
      }

      try {
        return engine.parseNQuads(nquads);
      } catch (error) {
        if (strict) {
          throw new Error(`[useNQuads] Parse error: ${error.message}`);
        }
        // Return empty store if not strict
        return engine.createStore();
      }
    },

    /**
     * Serialize a store to N-Quads string
     * @param {Store} store - N3.Store to serialize
     * @param {Object} [serializeOptions] - Serialization options
     * @param {boolean} [serializeOptions.deterministic=true] - Enable deterministic output
     * @returns {Promise<string>} N-Quads string
     * 
     * @example
     * const nquadsString = await nquads.serialize(store);
     */
    async serialize(store, serializeOptions = {}) {
      if (!store || typeof store.getQuads !== "function") {
        throw new Error("[useNQuads] Store is required");
      }

      const { deterministic = true } = serializeOptions;
      
      try {
        return await engine.serializeNQuads(store);
      } catch (error) {
        throw new Error(`[useNQuads] Serialization error: ${error.message}`);
      }
    },

    /**
     * Parse N-Quads from a file
     * @param {string} filePath - Path to N-Quads file
     * @param {Object} [options] - File options
     * @param {string} [options.encoding='utf8'] - File encoding
     * @returns {Promise<Store>} N3.Store containing parsed quads
     * 
     * @example
     * const store = await nquads.parseFile('./data.nq');
     */
    async parseFile(filePath, options = {}) {
      const { encoding = 'utf8' } = options;
      
      if (typeof filePath !== "string") {
        throw new TypeError("[useNQuads] File path must be a string");
      }

      try {
        const { readFile } = await import('node:fs/promises');
        const content = await readFile(filePath, encoding);
        return this.parse(content);
      } catch (error) {
        throw new Error(`[useNQuads] File read error: ${error.message}`);
      }
    },

    /**
     * Write a store to an N-Quads file
     * @param {Store} store - N3.Store to write
     * @param {string} filePath - Path to write to
     * @param {Object} [options] - Write options
     * @param {string} [options.encoding='utf8'] - File encoding
     * @param {boolean} [options.createBackup=false] - Create backup of existing file
     * @returns {Promise<Object>} Write result
     * 
     * @example
     * const result = await nquads.writeFile(store, './output.nq');
     */
    async writeFile(store, filePath, options = {}) {
      const { 
        encoding = 'utf8', 
        createBackup = false 
      } = options;
      
      if (!store || typeof store.getQuads !== "function") {
        throw new Error("[useNQuads] Store is required");
      }
      
      if (typeof filePath !== "string") {
        throw new TypeError("[useNQuads] File path must be a string");
      }

      try {
        const { writeFile, access } = await import('node:fs/promises');
        const { join, dirname } = await import('node:path');
        
        // Create backup if requested and file exists
        if (createBackup) {
          try {
            await access(filePath);
            const backupPath = `${filePath}.backup`;
            const { copyFile } = await import('node:fs/promises');
            await copyFile(filePath, backupPath);
          } catch {
            // File doesn't exist, no backup needed
          }
        }
        
        const nquadsContent = await this.serialize(store);
        await writeFile(filePath, nquadsContent, encoding);
        
        return {
          path: filePath,
          bytes: Buffer.byteLength(nquadsContent, encoding),
          quads: store.size
        };
      } catch (error) {
        throw new Error(`[useNQuads] File write error: ${error.message}`);
      }
    },

    /**
     * Validate N-Quads string format
     * @param {string} nquads - N-Quads string to validate
     * @returns {Object} Validation result
     * 
     * @example
     * const validation = nquads.validate(nquadsString);
     * if (validation.valid) {
     *   console.log(`Valid N-Quads with ${validation.quads} quads`);
     * } else {
     *   console.log(`Invalid: ${validation.error}`);
     * }
     */
    validate(nquads) {
      if (typeof nquads !== "string") {
        return {
          valid: false,
          error: "N-Quads content must be a string",
          quads: 0
        };
      }

      try {
        const store = this.parse(nquads, { strict: true });
        return {
          valid: true,
          error: null,
          quads: store.size
        };
      } catch (error) {
        return {
          valid: false,
          error: error.message,
          quads: 0
        };
      }
    },

    /**
     * Get statistics about N-Quads content
     * @param {string} nquads - N-Quads string to analyze
     * @returns {Object} Statistics object
     * 
     * @example
     * const stats = nquads.getStats(nquadsString);
     * console.log(`Quads: ${stats.quads}, Lines: ${stats.lines}`);
     */
    getStats(nquads) {
      if (typeof nquads !== "string") {
        throw new TypeError("[useNQuads] N-Quads content must be a string");
      }

      const lines = nquads.split('\n').filter(line => line.trim().length > 0);
      const quadLines = lines.filter(line => line.trim().endsWith('.'));
      
      try {
        const store = this.parse(nquads);
        const engineStats = engine.getStats(store);
        
        return {
          ...engineStats,
          lines: lines.length,
          quadLines: quadLines.length,
          emptyLines: lines.length - quadLines.length
        };
      } catch (error) {
        return {
          quads: 0,
          subjects: 0,
          predicates: 0,
          objects: 0,
          graphs: 0,
          lines: lines.length,
          quadLines: quadLines.length,
          emptyLines: lines.length - quadLines.length,
          error: error.message
        };
      }
    },

    /**
     * Convert N-Quads to Turtle format
     * @param {string} nquads - N-Quads string
     * @param {Object} [options] - Conversion options
     * @param {Object} [options.prefixes] - Prefix mappings
     * @returns {Promise<string>} Turtle string
     * 
     * @example
     * const turtle = await nquads.toTurtle(nquadsString, {
     *   prefixes: { ex: "http://example.org/" }
     * });
     */
    async toTurtle(nquads, options = {}) {
      const { prefixes = {} } = options;
      
      const store = this.parse(nquads);
      return await engine.serializeTurtle(store, { prefixes });
    },

    /**
     * Convert Turtle to N-Quads format
     * @param {string} turtle - Turtle string
     * @returns {Promise<string>} N-Quads string
     * 
     * @example
     * const nquads = await nquads.fromTurtle(turtleString);
     */
    async fromTurtle(turtle) {
      const store = engine.parseTurtle(turtle);
      return await this.serialize(store);
    },

    /**
     * Get the base IRI used for parsing
     * @returns {string} Base IRI
     */
    getBaseIRI() {
      return baseIRI;
    },

    /**
     * Set the base IRI for parsing
     * @param {string} newBaseIRI - New base IRI
     * @returns {Object} This composable instance
     */
    setBaseIRI(newBaseIRI) {
      if (typeof newBaseIRI !== "string") {
        throw new TypeError("[useNQuads] Base IRI must be a string");
      }
      baseIRI = newBaseIRI;
      engine = new RdfEngine({ baseIRI });
      return this;
    },

    /**
     * Check if content is N-Quads format
     * @param {string} content - Content to check
     * @returns {boolean} True if content appears to be N-Quads
     */
    isNQuads(content) {
      if (typeof content !== "string") {
        return false;
      }
      
      // Basic heuristic: check for N-Quads patterns
      const lines = content.split('\n').filter(line => line.trim().length > 0);
      if (lines.length === 0) return false;
      
      // Check if lines end with '.' (N-Quads characteristic)
      const quadLines = lines.filter(line => line.trim().endsWith('.'));
      return quadLines.length > 0;
    },

    /**
     * Check if content is valid N-Quads
     * @param {string} content - Content to check
     * @returns {boolean} True if valid N-Quads
     * 
     * @example
     * const isValid = nquads.isValid(nquadsString);
     */
    isValid(content) {
      const validation = this.validate(content);
      return validation.valid;
    }
  };
}