/**
 * @fileoverview usePrefixes composable - prefix management and CURIE operations
 * 
 * This composable provides prefix management capabilities for RDF operations.
 * It enforces the "One Prefix Rule" - centralized prefix management.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

/**
 * Create a prefixes composable for prefix management
 * 
 * @param {Object} [initialPrefixes] - Initial prefix mappings
 * @param {Object} [options] - Prefix options
 * @param {boolean} [options.caseSensitive=false] - Case sensitive prefix matching
 * @returns {Object} Prefix management interface
 * 
 * @example
 * const prefixes = usePrefixes({
 *   "ex": "http://example.org/",
 *   "foaf": "http://xmlns.com/foaf/0.1/"
 * });
 * 
 * // Register new prefixes
 * prefixes.register({ "dc": "http://purl.org/dc/terms/" });
 * 
 * // Expand CURIEs
 * const fullIRI = prefixes.expand("ex:Person");
 * 
 * // Shrink IRIs
 * const curie = prefixes.shrink("http://example.org/Person");
 * 
 * // List all prefixes
 * const allPrefixes = prefixes.list();
 */
export function usePrefixes(initialPrefixes = {}, options = {}) {
  const { caseSensitive = false } = options;
  
  // Internal prefix map
  const prefixes = new Map();
  
  // Initialize with provided prefixes
  Object.entries(initialPrefixes).forEach(([prefix, uri]) => {
    prefixes.set(prefix, uri);
  });

  return {
    /**
     * Register prefix mappings
     * @param {Object} prefixMap - Object mapping prefixes to URIs
     * @returns {Object} This composable instance
     */
    register(prefixMap) {
      if (typeof prefixMap !== "object" || prefixMap === null) {
        throw new Error("[usePrefixes] Prefix map must be an object");
      }
      
      Object.entries(prefixMap).forEach(([prefix, uri]) => {
        if (typeof prefix !== "string" || typeof uri !== "string") {
          throw new Error("[usePrefixes] Prefix and URI must be strings");
        }
        
        if (!uri.endsWith("/") && !uri.endsWith("#")) {
          throw new Error("[usePrefixes] URI should end with '/' or '#'");
        }
        
        prefixes.set(prefix, uri);
      });
      
      return this;
    },

    /**
     * Expand a CURIE to a full IRI
     * @param {string} curie - The CURIE to expand
     * @returns {string} The expanded IRI
     */
    expand(curie) {
      if (typeof curie !== "string") {
        throw new Error("[usePrefixes] CURIE must be a string");
      }
      
      const colonIndex = curie.indexOf(":");
      if (colonIndex === -1) {
        // Not a CURIE, return as-is
        return curie;
      }
      
      const prefix = curie.substring(0, colonIndex);
      const localName = curie.substring(colonIndex + 1);
      
      if (!prefix || !localName) {
        throw new Error("[usePrefixes] Invalid CURIE format");
      }
      
      const uri = prefixes.get(prefix);
      if (!uri) {
        throw new Error(`[usePrefixes] Unknown prefix: ${prefix}`);
      }
      
      return uri + localName;
    },

    /**
     * Shrink a full IRI to a CURIE
     * @param {string} iri - The IRI to shrink
     * @returns {string} The CURIE or original IRI if no prefix matches
     */
    shrink(iri) {
      if (typeof iri !== "string") {
        throw new Error("[usePrefixes] IRI must be a string");
      }
      
      // Find the longest matching prefix
      let bestMatch = { prefix: null, uri: null, length: 0 };
      
      for (const [prefix, uri] of prefixes.entries()) {
        if (iri.startsWith(uri) && uri.length > bestMatch.length) {
          bestMatch = { prefix, uri, length: uri.length };
        }
      }
      
      if (bestMatch.prefix) {
        const localName = iri.substring(bestMatch.length);
        return `${bestMatch.prefix}:${localName}`;
      }
      
      return iri;
    },

    /**
     * Get all registered prefixes
     * @returns {Object} Object mapping prefixes to URIs
     */
    list() {
      return Object.fromEntries(prefixes);
    },

    /**
     * Check if a prefix is registered
     * @param {string} prefix - The prefix to check
     * @returns {boolean} True if prefix is registered
     */
    has(prefix) {
      return prefixes.has(prefix);
    },

    /**
     * Get the URI for a prefix
     * @param {string} prefix - The prefix
     * @returns {string|undefined} The URI or undefined if not found
     */
    get(prefix) {
      return prefixes.get(prefix);
    },

    /**
     * Remove a prefix
     * @param {string} prefix - The prefix to remove
     * @returns {boolean} True if prefix was removed
     */
    remove(prefix) {
      return prefixes.delete(prefix);
    },

    /**
     * Clear all prefixes
     * @returns {Object} This composable instance
     */
    clear() {
      prefixes.clear();
      return this;
    },

    /**
     * Get the number of registered prefixes
     * @returns {number} Number of prefixes
     */
    size() {
      return prefixes.size;
    }
  };
}