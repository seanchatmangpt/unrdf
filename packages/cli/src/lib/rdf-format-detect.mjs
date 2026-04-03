/**
 * @file RDF serialization format detection for CLI loaders
 * @module @unrdf/cli/lib/rdf-format-detect
 * @description Maps file extensions to N3 Parser format strings (aligned with query/convert).
 */

/**
 * Detect N3 parser format from file path extension.
 * @param {string} filename
 * @returns {string} Parser format e.g. Turtle, N-Triples, N-Quads, JSON-LD
 */
export function detectRdfParserFormat(filename) {
  const ext = filename.split('.').pop()?.toLowerCase() ?? '';
  switch (ext) {
    case 'ttl':
      return 'Turtle';
    case 'nt':
      return 'N-Triples';
    case 'nq':
      return 'N-Quads';
    case 'jsonld':
    case 'json-ld':
      return 'JSON-LD';
    case 'trig':
      return 'TriG';
    default:
      return 'Turtle';
  }
}
