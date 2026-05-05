/**
 * Parse N3 logic to AST
 * @param {string} input - N3 notation
 * @returns {Object} Parse result
 */
export function parseN3(input: string): any;
/**
 * Estimate N3 reasoning complexity
 * @param {Object} ast - N3 AST
 * @returns {Object} Complexity bounds
 */
export function estimateN3Complexity(ast: any): any;
