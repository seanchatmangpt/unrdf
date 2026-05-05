/**
 * Full grammar closure pipeline: parse → compile → execute
 *
 * @param {string} input - Grammar input (SPARQL/SHACL/N3/OWL/ShEx)
 * @param {string} grammarType - Grammar type
 * @param {Function} executeFn - Execution function (compiled) => result
 * @param {Object} store - RDF store
 * @param {Object} [options] - Options
 * @returns {Promise<Object>} Execution result with receipts
 *
 * @example
 * const result = await grammarClosurePipeline(
 *   sparqlQuery,
 *   'sparql',
 *   (compiled) => store.query(compiled.ast.queryString),
 *   myStore
 * );
 *
 * if (result.success) {
 *   console.log('Result:', result.result);
 * } else if (result.denialReceipt) {
 *   console.error('Denied:', result.denialReceipt);
 * }
 */
export function grammarClosurePipeline(input: string, grammarType: string, executeFn: Function, store: any, options?: any): Promise<any>;
/**
 * Get grammar definition for a specific grammar type
 * @deprecated Legacy compatibility function
 * @param {string} grammarType - Grammar type to query
 * @returns {Object} JSON Schema definition with type and required fields
 * @example
 * const def = getGrammarDefinition('receipt');
 * // { type: 'object', required: ['id', 'type', 'timestamp', 'payload'] }
 */
export function getGrammarDefinition(grammarType: string): any;
/**
 * Validate data against specified grammar type
 * @deprecated Legacy compatibility function
 * @param {string} grammarType - Grammar type for validation
 * @param {any} data - Data to validate
 * @returns {boolean} True if valid, false otherwise
 * @example
 * const isValid = validateAgainstGrammar('receipt', myData);
 * // true or false
 */
export function validateAgainstGrammar(grammarType: string, data: any): boolean;
/**
 * v6 Grammar version identifier
 * @constant {string}
 */
export const GRAMMAR_VERSION: "6.0.0-alpha.1";
export namespace V6_GRAMMAR {
    export { GRAMMAR_VERSION as version };
    export namespace definitions {
        namespace receipt {
            let type: string;
            let required: string[];
        }
        namespace delta {
            let type_1: string;
            export { type_1 as type };
            let required_1: string[];
            export { required_1 as required };
        }
        namespace operation {
            let type_2: string;
            export { type_2 as type };
            let required_2: string[];
            export { required_2 as required };
        }
    }
    export let types: string[];
    export { grammarClosurePipeline as pipeline };
}
export { parseGrammar, getComplexityBounds, GRAMMAR_TYPES } from "./parser.mjs";
export { compileGrammar, rejectIfTooComplex, emitCompileReceipt, emitDenialReceipt, COMPLEXITY_BOUNDS } from "./compiler.mjs";
export { checkRuntimeComplexity, wrapWithTimeout, executeWithGate, getRuntimeBounds, RUNTIME_BOUNDS } from "./runtime-gate.mjs";
