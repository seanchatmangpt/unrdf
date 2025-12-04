/**
 * Format Conversion Utilities
 * @module converter
 */

import { OxigraphStore } from '@unrdf/oxigraph';
import { z } from 'zod';

/**
 * Supported RDF formats
 */
export const RDF_FORMATS = {
  turtle: ['ttl', 'turtle'],
  ntriples: ['nt', 'ntriples'],
  nquads: ['nq', 'nquads'],
  trig: ['trig'],
  jsonld: ['jsonld', 'json']
};

/**
 * Map format name to Oxigraph format string
 * @param {string} format - Format name
 * @returns {string} Oxigraph format string
 */
function toOxigraphFormat(format) {
  const oxFormatMap = {
    'turtle': 'text/turtle',
    'ttl': 'text/turtle',
    'ntriples': 'application/n-triples',
    'nt': 'application/n-triples',
    'nquads': 'application/n-quads',
    'nq': 'application/n-quads',
    'trig': 'application/trig',
    'n3': 'text/n3',
    'rdf': 'application/rdf+xml',
    'jsonld': 'application/ld+json',
    'json': 'application/ld+json',
  };
  return oxFormatMap[format] || 'text/turtle';
}

/**
 * Auto-detect RDF format from file extension
 * @param {string} filepath - File path
 * @returns {string} Format name
 */
export function detectFormat(filepath) {
  const inputSchema = z.string().min(1);
  const validPath = inputSchema.parse(filepath);

  const ext = validPath.split('.').pop().toLowerCase();

  for (const [format, extensions] of Object.entries(RDF_FORMATS)) {
    if (extensions.includes(ext)) {
      return format;
    }
  }

  return 'turtle';
}

/**
 * Parse RDF content using Oxigraph
 * @param {string} content - RDF content
 * @param {string} format - Input format
 * @returns {Promise<Object>} Oxigraph Store
 */
export async function parseRDF(content, format = 'turtle') {
  const inputSchema = z.object({
    content: z.string(),
    format: z.string()
  });

  inputSchema.parse({ content, format });

  const oxFormat = toOxigraphFormat(format);
  const store = new OxigraphStore();
  store.load(content, { format: oxFormat });
  return store;
}

/**
 * Serialize store to RDF format using Oxigraph
 * @param {Object} store - Oxigraph Store
 * @param {string} format - Output format
 * @returns {Promise<string>} Serialized RDF
 */
export async function serializeRDF(store, format = 'turtle') {
  const inputSchema = z.object({
    store: z.any(),
    format: z.string()
  });

  inputSchema.parse({ store, format });

  const oxFormat = toOxigraphFormat(format);
  return store.dump({ format: oxFormat });
}

/**
 * Convert RDF from one format to another
 * @param {string} content - Input RDF content
 * @param {string} inputFormat - Input format
 * @param {string} outputFormat - Output format
 * @returns {Promise<string>} Converted RDF
 */
export async function convertFormat(content, inputFormat, outputFormat) {
  const inputSchema = z.object({
    content: z.string(),
    inputFormat: z.string(),
    outputFormat: z.string()
  });

  inputSchema.parse({ content, inputFormat, outputFormat });

  const store = await parseRDF(content, inputFormat);
  return serializeRDF(store, outputFormat);
}

/**
 * Validate RDF content
 * @param {string} content - RDF content
 * @param {string} format - Format to validate
 * @returns {Promise<boolean>} True if valid
 */
export async function validateRDF(content, format = 'turtle') {
  try {
    await parseRDF(content, format);
    return true;
  } catch (error) {
    return false;
  }
}

/**
 * Get format info
 * @param {string} format - Format name
 * @returns {object} Format information
 */
export function getFormatInfo(format) {
  const extensions = RDF_FORMATS[format] || [];

  return {
    format,
    extensions,
    supported: extensions.length > 0
  };
}
