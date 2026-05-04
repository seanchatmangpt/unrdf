/**
 * @fileoverview Main orchestrator for JSDoc→RDF→MDX documentation generator
 */

export { parseFile, parseFiles } from './parser.mjs';
export { buildRDFGraph, buildRDFGraphs, buildJSONLD, PREFIXES } from './rdf-builder.mjs';
export { generateModuleMDX, generateModulesMDX } from './mdx-generator.mjs';

/**
 * Generate documentation for a single file
 * @param {string} filePath - Source file path
 * @param {Object} options - Generation options
 * @returns {Object} Generated documentation
 */
export async function generateDocs(filePath, options = {}) {
  const { parseFile } = await import('./parser.mjs');
  const { buildRDFGraph, buildJSONLD } = await import('./rdf-builder.mjs');
  const { generateModuleMDX } = await import('./mdx-generator.mjs');

  const parsed = parseFile(filePath, options.rootDir);
  const rdf = buildRDFGraph(parsed);
  const jsonld = buildJSONLD(parsed);
  const mdx = generateModuleMDX(parsed);

  return { parsed, rdf, jsonld, mdx };
}

export default { generateDocs };
