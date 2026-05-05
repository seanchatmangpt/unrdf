/**
 * Parse any grammar to AST with complexity annotations
 *
 * **Guarantee**: Never throws on syntactically valid grammar.
 * Invalid syntax returns { success: false, errors: [...] }
 *
 * @param {string} input - Grammar input text
 * @param {string} grammarType - Grammar type (sparql|shacl|n3|owl|shex)
 * @returns {Object} Parse result with AST and complexity
 *
 * @example
 * const result = parseGrammar(sparqlQuery, 'sparql');
 * if (result.success) {
 *   console.log('Complexity:', result.complexity);
 * }
 */
export function parseGrammar(input: string, grammarType: string): any;
/**
 * Get complexity bounds for parsed AST
 *
 * @param {Object} ast - Parsed AST from parseGrammar()
 * @returns {Object} Complexity bounds
 *
 * @example
 * const bounds = getComplexityBounds(ast);
 * console.log('Estimated time:', bounds.estimatedTimeMs);
 */
export function getComplexityBounds(ast: any): any;
export namespace GRAMMAR_TYPES {
    let SPARQL: string;
    let SHACL: string;
    let N3: string;
    let OWL: string;
    let SHEX: string;
}
/**
 * Parse result schema
 */
export const ParseResultSchema: z.ZodObject<{
    success: z.ZodBoolean;
    grammarType: z.ZodEnum<{
        [x: string]: string;
    }>;
    ast: z.ZodAny;
    complexity: z.ZodObject<{
        estimatedTimeMs: z.ZodNumber;
        astNodeCount: z.ZodNumber;
        maxDepth: z.ZodNumber;
        triplePatterns: z.ZodOptional<z.ZodNumber>;
        joinDepth: z.ZodOptional<z.ZodNumber>;
        filterComplexity: z.ZodOptional<z.ZodNumber>;
        shapesDepth: z.ZodOptional<z.ZodNumber>;
        ruleDepth: z.ZodOptional<z.ZodNumber>;
    }, z.core.$strip>;
    parseReceipt: z.ZodObject<{
        timestamp: z.ZodString;
        grammarVersion: z.ZodString;
        parser: z.ZodString;
    }, z.core.$strip>;
    errors: z.ZodOptional<z.ZodArray<z.ZodObject<{
        message: z.ZodString;
        line: z.ZodOptional<z.ZodNumber>;
        column: z.ZodOptional<z.ZodNumber>;
    }, z.core.$strip>>>;
}, z.core.$strip>;
import { z } from 'zod';
import { parseSPARQL } from './parser/sparql.mjs';
import { parseSHACL } from './parser/shacl.mjs';
import { parseN3 } from './parser/n3.mjs';
import { parseOWL } from './parser/owl.mjs';
import { parseShEx } from './parser/shex.mjs';
import { estimateSPARQLComplexity } from './parser/sparql.mjs';
import { estimateSHACLComplexity } from './parser/shacl.mjs';
import { estimateN3Complexity } from './parser/n3.mjs';
import { estimateOWLComplexity } from './parser/owl.mjs';
import { estimateShExComplexity } from './parser/shex.mjs';
export { parseSPARQL, parseSHACL, parseN3, parseOWL, parseShEx, estimateSPARQLComplexity, estimateSHACLComplexity, estimateN3Complexity, estimateOWLComplexity, estimateShExComplexity };
export { createEmptyComplexity, createParseReceipt } from "./parser/utils.mjs";
