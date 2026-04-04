/**
 * @file File URI resolver with content-addressed verification.
 * @module file-resolver
 *
 * @description
 * Production-ready file resolver that loads SPARQL/SHACL files from URIs
 * with SHA-256 hash verification for content integrity and provenance.
 */

import { readFile } from 'fs/promises';
import { createHash } from 'crypto';
import { fileURLToPath as _fileURLToPath } from 'url';
import { dirname as _dirname, join as _join, resolve as _resolve } from 'path';
import { createPathValidator } from './security/path-validator.mjs';

/**
 * Resolve a file URI to an absolute path.
 * @param {string} uri - The file URI (e.g., "file://hooks/compliance/largeTx.ask.rq")
 * @param {string} [basePath] - Base path for relative resolution
 * @returns {string} Absolute file path
 *
 * @throws {Error} If URI is invalid or file doesn't exist
 */
export function resolveFileUri(uri, basePath = process.cwd()) {
  if (!uri || typeof uri !== 'string') {
    throw new TypeError('resolveFileUri: uri must be a non-empty string');
  }

  if (!uri.startsWith('file://')) {
    throw new Error(`resolveFileUri: URI must start with 'file://', got: ${uri}`);
  }

  // Validate path for security vulnerabilities
  const pathValidator = createPathValidator({ basePath });
  const validation = pathValidator.validateFileUri(uri);

  if (!validation.valid) {
    throw new Error(`Security validation failed: ${validation.violations.join(', ')}`);
  }

  // Use sanitized path from validator
  return validation.sanitizedPath;
}

/**
 * Calculate SHA-256 hash of file content.
 * @param {string} filePath - Path to the file
 * @returns {Promise<string>} Hexadecimal SHA-256 hash
 *
 * @throws {Error} If file cannot be read
 */
export async function calculateFileHash(filePath) {
  try {
    const content = await readFile(filePath, 'utf-8');
    const hash = createHash('sha256');
    hash.update(content, 'utf-8');
    return hash.digest('hex');
  } catch (error) {
    throw new Error(`Failed to calculate hash for ${filePath}: ${error.message}`);
  }
}

/**
 * Load file content with hash verification.
 * @param {string} uri - The file URI
 * @param {string} expectedHash - Expected SHA-256 hash
 * @param {string} [basePath] - Base path for resolution
 * @returns {Promise<{content: string, hash: string, path: string}>} File content and metadata
 *
 * @throws {Error} If file cannot be loaded or hash doesn't match
 */
export async function loadFileWithHash(uri, expectedHash, basePath = process.cwd()) {
  if (!expectedHash || typeof expectedHash !== 'string') {
    throw new TypeError('loadFileWithHash: expectedHash must be a non-empty string');
  }

  const filePath = resolveFileUri(uri, basePath);

  try {
    // Load file content
    const content = await readFile(filePath, 'utf-8');

    // Calculate actual hash
    const actualHash = await calculateFileHash(filePath);

    // Verify hash matches
    if (actualHash !== expectedHash) {
      throw new Error(
        `Hash verification failed for ${uri}\n` +
          `Expected: ${expectedHash}\n` +
          `Actual:   ${actualHash}\n` +
          `File:     ${filePath}`
      );
    }

    return {
      content,
      hash: actualHash,
      path: filePath,
      uri,
    };
  } catch (error) {
    if (error.code === 'ENOENT') {
      throw new Error(`File not found: ${uri} (resolved to: ${filePath})`);
    }
    throw error;
  }
}

/**
 * Load and parse a SPARQL query file.
 * @param {string} uri - The file URI
 * @param {string} expectedHash - Expected SHA-256 hash
 * @param {string} [basePath] - Base path for resolution
 * @returns {Promise<{sparql: string, hash: string, path: string}>} Parsed SPARQL query
 *
 * @throws {Error} If file cannot be loaded or is not valid SPARQL
 */
export async function loadSparqlFile(uri, expectedHash, basePath = process.cwd()) {
  const fileData = await loadFileWithHash(uri, expectedHash, basePath);

  // Basic SPARQL syntax validation
  const sparql = fileData.content.trim();
  if (!sparql) {
    throw new Error(`Empty SPARQL file: ${uri}`);
  }

  // Check for common SPARQL keywords
  const sparqlKeywords = ['SELECT', 'ASK', 'CONSTRUCT', 'DESCRIBE', 'INSERT', 'DELETE', 'PREFIX'];
  const hasKeyword = sparqlKeywords.some(keyword => sparql.toUpperCase().includes(keyword));

  if (!hasKeyword) {
    throw new Error(`Invalid SPARQL syntax in ${uri}: No recognized SPARQL keywords found`);
  }

  return {
    sparql,
    hash: fileData.hash,
    path: fileData.path,
    uri: fileData.uri,
  };
}

/**
 * Load and parse a SHACL shapes file.
 * @param {string} uri - The file URI
 * @param {string} expectedHash - Expected SHA-256 hash
 * @param {string} [basePath] - Base path for resolution
 * @returns {Promise<{turtle: string, hash: string, path: string}>} Parsed SHACL shapes
 *
 * @throws {Error} If file cannot be loaded or is not valid Turtle
 */
export async function loadShaclFile(uri, expectedHash, basePath = process.cwd()) {
  const fileData = await loadFileWithHash(uri, expectedHash, basePath);

  // Basic Turtle syntax validation
  const turtle = fileData.content.trim();
  if (!turtle) {
    throw new Error(`Empty SHACL file: ${uri}`);
  }

  // Check for SHACL namespace
  if (!turtle.includes('http://www.w3.org/ns/shacl#')) {
    throw new Error(`Invalid SHACL file ${uri}: Missing SHACL namespace`);
  }

  // Check for common SHACL terms
  const shaclTerms = ['sh:NodeShape', 'sh:PropertyShape', 'sh:targetClass', 'sh:path'];
  const hasShaclTerm = shaclTerms.some(term => turtle.includes(term));

  if (!hasShaclTerm) {
    throw new Error(`Invalid SHACL file ${uri}: No recognized SHACL terms found`);
  }

  return {
    turtle,
    hash: fileData.hash,
    path: fileData.path,
    uri: fileData.uri,
  };
}

/**
 * Create a file resolver with caching.
 * @param {Object} [options] - Resolver options
 * @param {string} [options.basePath] - Base path for file resolution
 * @param {boolean} [options.enableCache] - Enable file content caching
 * @param {number} [options.cacheMaxAge] - Cache max age in milliseconds
 * @param {number} [options.cacheSize] - Maximum cache size
 * @param {boolean} [options.allowAbsolutePaths] - Allow absolute paths (default: true)
 * @param {boolean} [options.enablePathValidation] - Enforce strict path validation
 * @param {Array<string>} [options.allowedExtensions] - Whitelist of allowed extensions
 * @returns {Object} File resolver instance
 */
export function createFileResolver(options = {}) {
  const {
    basePath = process.cwd(),
    enableCache = true,
    cacheMaxAge = 300000, // 5 minutes
    cacheSize = 1000,
    allowAbsolutePaths = true,
    enablePathValidation = false,
    allowedExtensions = null,
  } = options;

  const cache = new Map();
  const pathResolveCache = new Map(); // Cache for resolve() results
  let cacheHits = 0;
  let cacheMisses = 0;

  /**
   * Ensure cache doesn't exceed size limit by removing oldest entries
   */
  function enforceMaxCacheSize() {
    if (cache.size > cacheSize) {
      const keysToDelete = Array.from(cache.keys()).slice(0, cache.size - cacheSize);
      keysToDelete.forEach(key => cache.delete(key));
    }
    if (pathResolveCache.size > cacheSize) {
      const keysToDelete = Array.from(pathResolveCache.keys()).slice(0, pathResolveCache.size - cacheSize);
      keysToDelete.forEach(key => pathResolveCache.delete(key));
    }
  }

  return {
    /**
     * Sanitize a file path to remove dangerous patterns.
     * @param {string} filePath - The file path to sanitize
     * @returns {string} Sanitized file path
     */
    sanitizePath(filePath) {
      if (!filePath || typeof filePath !== 'string') {
        return '';
      }
      // Remove parent directory traversal attempts
      return filePath.split(/[/\\]+/).filter(part => part && part !== '..').join('/');
    },

    /**
     * Validate hash format.
     * @param {string} hash - The hash string to validate
     * @param {string} algorithm - Hash algorithm ('sha256', 'sha512', etc.)
     * @returns {boolean} True if hash is valid
     */
    isValidHash(hash, algorithm = 'sha256') {
      if (!hash || typeof hash !== 'string') {
        return false;
      }

      const expectedLength = algorithm === 'sha512' ? 128 : algorithm === 'sha256' ? 64 : 0;
      if (expectedLength === 0 || hash.length !== expectedLength) {
        return false;
      }

      // Check if all characters are valid hex (0-9, a-f, A-F)
      return /^[0-9a-fA-F]+$/.test(hash);
    },

    /**
     * Resolve a file path relative to basePath.
     * @param {string} filePath - The file path to resolve
     * @returns {string} Absolute file path
     * @throws {Error} If path is invalid or not allowed
     */
    resolve(filePath) {
      if (!filePath || typeof filePath !== 'string') {
        throw new TypeError('resolve: path must be a non-empty string');
      }

      // Check cache first
      if (enableCache && pathResolveCache.has(filePath)) {
        cacheHits++;
        return pathResolveCache.get(filePath);
      }
      cacheMisses++;

      // Pass through HTTP/HTTPS URIs unchanged
      if (filePath.startsWith('http://') || filePath.startsWith('https://')) {
        if (enableCache) {
          pathResolveCache.set(filePath, filePath);
          enforceMaxCacheSize();
        }
        return filePath;
      }

      // Pass through file:// URIs with special handling
      if (filePath.startsWith('file://')) {
        if (enableCache) {
          pathResolveCache.set(filePath, filePath);
          enforceMaxCacheSize();
        }
        return filePath;
      }

      // Reject unknown URI schemes
      if (filePath.includes('://')) {
        throw new Error(`resolve: unsupported URI scheme in ${filePath}`);
      }

      // Path traversal validation if enabled
      if (enablePathValidation && filePath.includes('..')) {
        throw new Error(`resolve: path traversal detected: ${filePath}`);
      }

      // Check for absolute paths - only reject if explicitly disabled
      const isAbsolute = filePath.startsWith('/') || /^[a-zA-Z]:/.test(filePath);
      if (!allowAbsolutePaths && isAbsolute) {
        throw new Error(`resolve: absolute paths are not allowed: ${filePath}`);
      }

      // If absolute path and allowed, return as-is
      if (isAbsolute) {
        const resolved = _resolve(filePath);
        if (enableCache) {
          pathResolveCache.set(filePath, resolved);
          enforceMaxCacheSize();
        }
        return resolved;
      }

      // Check file extensions if whitelist provided
      if (allowedExtensions && Array.isArray(allowedExtensions)) {
        const ext = filePath.slice(filePath.lastIndexOf('.'));
        if (ext && !allowedExtensions.includes(ext)) {
          throw new Error(`resolve: file extension '${ext}' is not allowed. Allowed: ${allowedExtensions.join(', ')}`);
        }
      }

      // Resolve relative paths against basePath
      const resolved = _resolve(_join(basePath, filePath));
      if (enableCache) {
        pathResolveCache.set(filePath, resolved);
        enforceMaxCacheSize();
      }
      return resolved;
    },

    /**
     * Load a file with hash verification.
     * @param {string} uri - The file URI
     * @param {string} expectedHash - Expected SHA-256 hash
     * @returns {Promise<Object>} File content and metadata
     */
    async loadFile(uri, expectedHash) {
      const cacheKey = `${uri}:${expectedHash}`;

      if (enableCache && cache.has(cacheKey)) {
        const cached = cache.get(cacheKey);
        if (Date.now() - cached.timestamp < cacheMaxAge) {
          return cached.data;
        }
        cache.delete(cacheKey);
      }

      const data = await loadFileWithHash(uri, expectedHash, basePath);

      if (enableCache) {
        cache.set(cacheKey, {
          data,
          timestamp: Date.now(),
        });
        enforceMaxCacheSize();
      }

      return data;
    },

    /**
     * Load a SPARQL file.
     * @param {string} uri - The file URI
     * @param {string} expectedHash - Expected SHA-256 hash
     * @returns {Promise<Object>} Parsed SPARQL query
     */
    async loadSparql(uri, expectedHash) {
      const cacheKey = `sparql:${uri}:${expectedHash}`;

      if (enableCache && cache.has(cacheKey)) {
        const cached = cache.get(cacheKey);
        if (Date.now() - cached.timestamp < cacheMaxAge) {
          return cached.data;
        }
        cache.delete(cacheKey);
      }

      const data = await loadSparqlFile(uri, expectedHash, basePath);

      if (enableCache) {
        cache.set(cacheKey, {
          data,
          timestamp: Date.now(),
        });
        enforceMaxCacheSize();
      }

      return data;
    },

    /**
     * Load a SHACL file.
     * @param {string} uri - The file URI
     * @param {string} expectedHash - Expected SHA-256 hash
     * @returns {Promise<Object>} Parsed SHACL shapes
     */
    async loadShacl(uri, expectedHash) {
      const cacheKey = `shacl:${uri}:${expectedHash}`;

      if (enableCache && cache.has(cacheKey)) {
        const cached = cache.get(cacheKey);
        if (Date.now() - cached.timestamp < cacheMaxAge) {
          return cached.data;
        }
        cache.delete(cacheKey);
      }

      const data = await loadShaclFile(uri, expectedHash, basePath);

      if (enableCache) {
        cache.set(cacheKey, {
          data,
          timestamp: Date.now(),
        });
        enforceMaxCacheSize();
      }

      return data;
    },

    /**
     * Clear the cache.
     */
    clearCache() {
      cache.clear();
      pathResolveCache.clear();
    },

    /**
     * Pre-load a file at startup (compute hash once, avoid I/O in hot path)
     * @param {string} uri - The file URI
     * @returns {Promise<Object>} File content, computed hash, and metadata
     *
     * This method loads a file and computes its SHA-256 hash once at startup,
     * then caches the result. Useful for "warming" the cache before transaction execution.
     */
    async preload(uri) {
      try {
        const filePath = resolveFileUri(uri, basePath);
        const content = await readFile(filePath, 'utf-8');
        const hash = await calculateFileHash(filePath);

        const data = {
          content,
          hash,
          path: filePath,
          uri,
        };

        // Cache with preloaded marker (very long TTL)
        const cacheKey = `preloaded:${uri}`;
        if (enableCache) {
          cache.set(cacheKey, {
            data,
            timestamp: Date.now(),
          });
        }

        return data;
      } catch (error) {
        throw new Error(`Failed to preload ${uri}: ${error.message}`);
      }
    },

    /**
     * Collect all file URIs referenced in hook conditions
     * @param {Array<Object>} hooks - Array of hook definitions
     * @returns {Set<string>} Set of unique file URIs
     *
     * This method analyzes hook conditions to find all file references
     * that should be preloaded at startup to eliminate File I/O from hot path.
     */
    collectFileUris(hooks) {
      const uris = new Set();

      if (!Array.isArray(hooks)) {
        return uris;
      }

      for (const hook of hooks) {
        if (hook.condition && hook.condition.file) {
          uris.add(hook.condition.file);
        }
        if (hook.conditions && Array.isArray(hook.conditions)) {
          for (const cond of hook.conditions) {
            if (cond.file) {
              uris.add(cond.file);
            }
          }
        }
      }

      return uris;
    },

    /**
     * Get cache statistics.
     * @returns {Object} Cache statistics
     */
    getCacheStats() {
      const now = Date.now();
      let validEntries = 0;
      let expiredEntries = 0;

      for (const [_key, value] of cache.entries()) {
        if (now - value.timestamp < cacheMaxAge) {
          validEntries++;
        } else {
          expiredEntries++;
        }
      }

      return {
        size: pathResolveCache.size,
        hits: cacheHits,
        misses: cacheMisses,
        totalEntries: cache.size + pathResolveCache.size,
        validEntries,
        expiredEntries,
        cacheMaxAge,
        cacheSize,
        pathCacheSize: pathResolveCache.size,
      };
    },
  };
}
