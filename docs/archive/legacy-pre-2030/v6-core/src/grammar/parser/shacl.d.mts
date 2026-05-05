/**
 * Parse SHACL shapes to AST
 * @param {string} input - SHACL shapes (Turtle/JSON-LD)
 * @returns {Object} Parse result
 */
export function parseSHACL(input: string): any;
/**
 * Estimate SHACL validation complexity
 * @param {Object} ast - SHACL AST
 * @returns {Object} Complexity bounds
 */
export function estimateSHACLComplexity(ast: any): any;
