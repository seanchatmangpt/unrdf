/**
 * Parse ShEx schema to AST
 * @param {string} input - ShEx schema
 * @returns {Object} Parse result
 */
export function parseShEx(input: string): any;
/**
 * Estimate ShEx validation complexity
 * @param {Object} ast - ShEx AST
 * @returns {Object} Complexity bounds
 */
export function estimateShExComplexity(ast: any): any;
