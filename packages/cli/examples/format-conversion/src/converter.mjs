/**
 * Format Conversion Utilities
 * @module converter
 */

import { Parser, Writer } from 'n3';
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
 * Parse RDF content
 * @param {string} content - RDF content
 * @param {string} format - Input format
 * @returns {Promise<Array>} Array of quads
 */
export async function parseRDF(content, format = 'turtle') {
  const inputSchema = z.object({
    content: z.string(),
    format: z.string()
  });

  inputSchema.parse({ content, format });

  const parser = new Parser({ format });
  const quads = [];

  return new Promise((resolve, reject) => {
    parser.parse(content, (error, quad, prefixes) => {
      if (error) {
        reject(error);
      } else if (quad) {
        quads.push(quad);
      } else {
        resolve(quads);
      }
    });
  });
}

/**
 * Serialize quads to RDF format
 * @param {Array} quads - Array of quads
 * @param {string} format - Output format
 * @returns {Promise<string>} Serialized RDF
 */
export async function serializeRDF(quads, format = 'turtle') {
  const inputSchema = z.object({
    quads: z.array(z.any()),
    format: z.string()
  });

  inputSchema.parse({ quads, format });

  const writer = new Writer({ format });

  return new Promise((resolve, reject) => {
    writer.addQuads(quads);
    writer.end((error, result) => {
      if (error) {
        reject(error);
      } else {
        resolve(result);
      }
    });
  });
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

  const quads = await parseRDF(content, inputFormat);
  return serializeRDF(quads, outputFormat);
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
