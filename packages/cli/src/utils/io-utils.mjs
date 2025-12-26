/**
 * @fileoverview File system utilities for CLI operations
 * @module cli/utils/io-utils
 */

import { access, mkdir } from 'node:fs/promises';
import { constants } from 'node:fs';
import { dirname as _dirname } from 'node:path';

/**
 * Check if a file exists
 * @param {string} filePath - Path to check
 * @returns {Promise<boolean>} True if file exists
 */
export async function fileExists(filePath) {
  try {
    await access(filePath, constants.F_OK);
    return true;
  } catch {
    return false;
  }
}

/**
 * Ensure directory exists, create if not
 * @param {string} dirPath - Directory path
 * @returns {Promise<void>}
 */
export async function ensureDir(dirPath) {
  try {
    await mkdir(dirPath, { recursive: true });
  } catch (error) {
    if (error.code !== 'EEXIST') {
      throw error;
    }
  }
}

/**
 * Detect RDF format from file extension
 * @param {string} filePath - File path
 * @returns {string} Format name
 */
export function detectRDFFormat(filePath) {
  const ext = filePath.toLowerCase().split('.').pop();
  const formatMap = {
    'ttl': 'turtle',
    'turtle': 'turtle',
    'nt': 'n-triples',
    'ntriples': 'n-triples',
    'nq': 'n-quads',
    'nquads': 'n-quads',
    'jsonld': 'json-ld',
    'json': 'json-ld',
    'rdf': 'rdf/xml',
    'xml': 'rdf/xml',
  };
  return formatMap[ext] || 'turtle';
}
