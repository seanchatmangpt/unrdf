/**
 * Parse SPARQL query to AST
 * @param {string} input - SPARQL query
 * @returns {Object} Parse result
 */
export function parseSPARQL(input: string): any;
/**
 * Estimate SPARQL query complexity
 * @param {Object} ast - SPARQL AST
 * @returns {Object} Complexity bounds
 */
export function estimateSPARQLComplexity(ast: any): any;
