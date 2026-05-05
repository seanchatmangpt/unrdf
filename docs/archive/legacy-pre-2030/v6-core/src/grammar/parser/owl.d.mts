/**
 * Parse OWL ontology to AST
 * @param {string} input - OWL (RDF/XML, Turtle, etc.)
 * @returns {Object} Parse result
 */
export function parseOWL(input: string): any;
/**
 * Estimate OWL reasoning complexity
 * @param {Object} ast - OWL AST
 * @returns {Object} Complexity bounds
 */
export function estimateOWLComplexity(ast: any): any;
