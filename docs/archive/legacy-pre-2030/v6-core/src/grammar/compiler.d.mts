/**
 * Compile grammar with AOT complexity gating
 *
 * **Guarantee**: Returns { denial, denialReceipt } instead of throwing.
 *
 * @param {Object} ast - Parsed AST from parser.parseGrammar()
 * @param {Object} [options] - Compile options
 * @param {boolean} [options.strict=true] - Strict bounds enforcement
 * @param {Object} [options.customBounds] - Override default bounds
 * @param {boolean} [options.emitReceipts=true] - Emit receipts
 * @returns {Object} Compile result { compiled, compileReceipt } OR { denial, denialReceipt }
 *
 * @example
 * const result = compileGrammar(ast, { strict: true });
 * if (result.success) {
 *   // Execute result.compiled
 * } else {
 *   // Handle result.denialReceipt
 * }
 */
export function compileGrammar(ast: any, options?: {
    strict?: boolean;
    customBounds?: any;
    emitReceipts?: boolean;
}): any;
/**
 * Check if AST complexity exceeds bounds - returns denial if so
 *
 * @param {Object} ast - Parsed AST
 * @param {Object} bounds - Complexity bounds
 * @param {Object} opts - Compile options
 * @returns {Object|null} Denial result or null if acceptable
 */
export function rejectIfTooComplex(ast: any, bounds: any, opts?: any): any | null;
/**
 * Emit denial receipt with Merkle proof
 *
 * @param {Object} ast - Denied AST
 * @param {Object} details - Denial details
 * @returns {Object} Denial receipt
 */
export function emitDenialReceipt(ast: any, details: any): any;
/**
 * Emit compile receipt with metadata
 *
 * @param {Object} ast - Compiled AST
 * @param {Object} compiled - Compiled output
 * @param {string} grammarType - Grammar type
 * @returns {Object} Compile receipt
 */
export function emitCompileReceipt(ast: any, compiled: any, grammarType: string): any;
export namespace COMPLEXITY_BOUNDS {
    namespace sparql {
        let maxTriplePatterns: number;
        let maxJoinDepth: number;
        let maxFilterComplexity: number;
        let maxAggregations: number;
        let maxSubqueries: number;
        let estimatedTimeMs: number;
    }
    namespace shacl {
        export let maxShapesDepth: number;
        export let maxPropertyPaths: number;
        export let maxTargetNodes: number;
        export let maxValidationRules: number;
        let estimatedTimeMs_1: number;
        export { estimatedTimeMs_1 as estimatedTimeMs };
    }
    namespace n3 {
        export let maxRuleDepth: number;
        export let maxFormulaSize: number;
        export let maxLogicQuantifiers: number;
        export let maxBuiltinCalls: number;
        let estimatedTimeMs_2: number;
        export { estimatedTimeMs_2 as estimatedTimeMs };
    }
    namespace owl {
        export let maxOWLAxioms: number;
        export let maxClassHierarchyDepth: number;
        export let maxPropertyChainLength: number;
        export let maxReasoningIterations: number;
        let estimatedTimeMs_3: number;
        export { estimatedTimeMs_3 as estimatedTimeMs };
    }
    namespace shex {
        export let maxShapeDepth: number;
        export let maxTripleConstraints: number;
        export let maxShapeReferences: number;
        export let maxRegexComplexity: number;
        let estimatedTimeMs_4: number;
        export { estimatedTimeMs_4 as estimatedTimeMs };
    }
}
/**
 * Compile result schema (exported for type inference)
 */
export const CompileResultSchema: z.ZodObject<{
    success: z.ZodBoolean;
    compiled: z.ZodOptional<z.ZodAny>;
    denial: z.ZodOptional<z.ZodAny>;
    compileReceipt: z.ZodObject<{
        timestamp: z.ZodString;
        compileTimeMs: z.ZodNumber;
        grammarType: z.ZodString;
        decision: z.ZodEnum<{
            ACCEPT: "ACCEPT";
            REJECT: "REJECT";
        }>;
        reason: z.ZodOptional<z.ZodString>;
    }, z.core.$strip>;
    denialReceipt: z.ZodOptional<z.ZodObject<{
        timestamp: z.ZodString;
        merkleProof: z.ZodString;
        deniedInput: z.ZodString;
        reason: z.ZodString;
        details: z.ZodRecord<z.ZodAny, z.core.SomeType>;
    }, z.core.$strip>>;
}, z.core.$strip>;
import { z } from 'zod';
