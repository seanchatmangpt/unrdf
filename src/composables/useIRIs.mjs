/**
 * @fileoverview useIRIs composable - IRI resolution and management
 * 
 * This composable provides IRI resolution capabilities with support for
 * custom URI schemes like graph:// and prefix-based resolution.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

/**
 * Create an IRIs composable for IRI resolution
 * 
 * @param {Object} [options] - IRI options
 * @param {Object} [options.uriMaps] - Initial URI mappings
 * @param {string} [options.baseIRI] - Base IRI for resolution
 * @returns {Object} IRI resolution interface
 * 
 * @example
 * const iris = useIRIs({
 *   uriMaps: {
 *     'graph://': './graph/',
 *     'data://': './data/'
 *   },
 *   baseIRI: 'http://example.org/'
 * });
 * 
 * // Resolve a URI
 * const resolved = iris.resolve('graph://my-graph.ttl');
 * 
 * // Map a prefix to a path
 * iris.map('ex', './examples/');
 * 
 * // Get all mappings
 * const maps = iris.maps();
 */
export function useIRIs(options = {}) {
  let {
    uriMaps = {},
    baseIRI = "http://example.org/"
  } = options;

  // Internal URI mappings
  const mappings = new Map(Object.entries(uriMaps));

  return {
    /**
     * Resolve a URI to its actual path or IRI
     * @param {string} uri - The URI to resolve
     * @returns {string} Resolved URI or path
     */
    resolve(uri) {
      if (typeof uri !== "string") {
        throw new Error("[useIRIs] URI must be a string");
      }

      // Check for custom URI schemes
      for (const [scheme, path] of mappings.entries()) {
        if (uri.startsWith(scheme)) {
          return path + uri.substring(scheme.length);
        }
      }

      // Handle relative IRIs
      if (uri.startsWith("#") || uri.startsWith("/")) {
        return baseIRI + uri;
      }

      // Return as-is if it's already an absolute IRI
      if (uri.startsWith("http://") || uri.startsWith("https://")) {
        return uri;
      }

      // Default to base IRI
      return baseIRI + uri;
    },

    /**
     * Map a prefix to a path
     * @param {string} prefix - The prefix to map
     * @param {string} path - The path to map to
     * @returns {Object} This composable instance
     */
    map(prefix, path) {
      if (typeof prefix !== "string" || typeof path !== "string") {
        throw new Error("[useIRIs] Prefix and path must be strings");
      }

      // Ensure prefix ends with ://
      const normalizedPrefix = prefix.endsWith("://") ? prefix : prefix + "://";
      
      // Ensure path ends with /
      const normalizedPath = path.endsWith("/") ? path : path + "/";

      mappings.set(normalizedPrefix, normalizedPath);
      return this;
    },

    /**
     * Get all URI mappings
     * @returns {Object} Object mapping prefixes to paths
     */
    maps() {
      return Object.fromEntries(mappings);
    },

    /**
     * Check if a prefix is mapped
     * @param {string} prefix - The prefix to check
     * @returns {boolean} True if prefix is mapped
     */
    hasMapping(prefix) {
      const normalizedPrefix = prefix.endsWith("://") ? prefix : prefix + "://";
      return mappings.has(normalizedPrefix);
    },

    /**
     * Get the path for a prefix
     * @param {string} prefix - The prefix
     * @returns {string|undefined} The mapped path or undefined
     */
    getMapping(prefix) {
      const normalizedPrefix = prefix.endsWith("://") ? prefix : prefix + "://";
      return mappings.get(normalizedPrefix);
    },

    /**
     * Remove a prefix mapping
     * @param {string} prefix - The prefix to remove
     * @returns {boolean} True if prefix was removed
     */
    removeMapping(prefix) {
      const normalizedPrefix = prefix.endsWith("://") ? prefix : prefix + "://";
      return mappings.delete(normalizedPrefix);
    },

    /**
     * Clear all mappings
     * @returns {Object} This composable instance
     */
    clearMappings() {
      mappings.clear();
      return this;
    },

    /**
     * Get the base IRI
     * @returns {string} Base IRI
     */
    getBaseIRI() {
      return baseIRI;
    },

    /**
     * Set the base IRI
     * @param {string} newBaseIRI - New base IRI
     * @returns {Object} This composable instance
     */
    setBaseIRI(newBaseIRI) {
      if (typeof newBaseIRI !== "string") {
        throw new Error("[useIRIs] Base IRI must be a string");
      }

      if (!newBaseIRI.endsWith("/") && !newBaseIRI.endsWith("#")) {
        throw new Error("[useIRIs] Base IRI should end with '/' or '#'");
      }

      baseIRI = newBaseIRI;
      return this;
    },

    /**
     * Check if a URI is absolute
     * @param {string} uri - The URI to check
     * @returns {boolean} True if URI is absolute
     */
    isAbsolute(uri) {
      if (typeof uri !== "string") {
        return false;
      }

      return uri.startsWith("http://") || 
             uri.startsWith("https://") || 
             uri.startsWith("file://") ||
             mappings.has(uri.split("://")[0] + "://");
    },

    /**
     * Check if a URI is relative
     * @param {string} uri - The URI to check
     * @returns {boolean} True if URI is relative
     */
    isRelative(uri) {
      return !this.isAbsolute(uri);
    },

    /**
     * Get the scheme of a URI
     * @param {string} uri - The URI
     * @returns {string|undefined} The scheme or undefined
     */
    getScheme(uri) {
      if (typeof uri !== "string") {
        return undefined;
      }

      const colonIndex = uri.indexOf(":");
      if (colonIndex === -1) {
        return undefined;
      }

      return uri.substring(0, colonIndex);
    },

    /**
     * Get the path part of a URI
     * @param {string} uri - The URI
     * @returns {string} The path part
     */
    getPath(uri) {
      if (typeof uri !== "string") {
        return "";
      }

      const colonIndex = uri.indexOf(":");
      if (colonIndex === -1) {
        return uri;
      }

      return uri.substring(colonIndex + 1);
    },

    /**
     * Get the number of mappings
     * @returns {number} Number of mappings
     */
    size() {
      return mappings.size;
    }
  };
}
