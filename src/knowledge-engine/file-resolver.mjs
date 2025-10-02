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
import { fileURLToPath } from 'url';
import { dirname, join, resolve } from 'path';
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
      uri
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
  const hasKeyword = sparqlKeywords.some(keyword => 
    sparql.toUpperCase().includes(keyword)
  );
  
  if (!hasKeyword) {
    throw new Error(`Invalid SPARQL syntax in ${uri}: No recognized SPARQL keywords found`);
  }
  
  return {
    sparql,
    hash: fileData.hash,
    path: fileData.path,
    uri: fileData.uri
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
    uri: fileData.uri
  };
}

/**
 * Create a file resolver with caching.
 * @param {Object} [options] - Resolver options
 * @param {string} [options.basePath] - Base path for file resolution
 * @param {boolean} [options.enableCache] - Enable file content caching
 * @param {number} [options.cacheMaxAge] - Cache max age in milliseconds
 * @returns {Object} File resolver instance
 */
export function createFileResolver(options = {}) {
  const {
    basePath = process.cwd(),
    enableCache = true,
    cacheMaxAge = 300000 // 5 minutes
  } = options;
  
  const cache = new Map();
  
  return {
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
          timestamp: Date.now()
        });
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
          timestamp: Date.now()
        });
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
          timestamp: Date.now()
        });
      }
      
      return data;
    },
    
    /**
     * Clear the cache.
     */
    clearCache() {
      cache.clear();
    },
    
    /**
     * Get cache statistics.
     * @returns {Object} Cache statistics
     */
    getCacheStats() {
      const now = Date.now();
      let validEntries = 0;
      let expiredEntries = 0;
      
      for (const [key, value] of cache.entries()) {
        if (now - value.timestamp < cacheMaxAge) {
          validEntries++;
        } else {
          expiredEntries++;
        }
      }
      
      return {
        totalEntries: cache.size,
        validEntries,
        expiredEntries,
        cacheMaxAge
      };
    }
  };
}
