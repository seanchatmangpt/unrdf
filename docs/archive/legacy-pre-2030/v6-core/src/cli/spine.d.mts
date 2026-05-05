/**
 * Build V6 spine for existing registry.
 *
 * Augments registry with V6 canonical noun-verb structure.
 * Validates that registered commands align with V6 ontology.
 *
 * @param {Registry} registry - Existing kgc-cli registry
 * @returns {Object} V6-enhanced command tree
 */
export function buildV6Spine(registry: Registry): any;
/**
 * Generate V6 spine report.
 *
 * Human-readable summary of V6 coverage and validation.
 *
 * @param {Object} spine - Result from buildV6Spine
 * @returns {string} Formatted report
 */
export function generateSpineReport(spine: any): string;
/**
 * Get noun-verb matrix for documentation.
 *
 * Returns complete mapping of all noun-verb combinations.
 *
 * @returns {Object} Matrix with metadata
 */
export function getNounVerbMatrix(): any;
/**
 * Validate that command emits proper receipt.
 *
 * Wraps command handler to ensure receipt emission.
 *
 * @param {string} noun - Noun name
 * @param {string} verb - Verb name
 * @param {Function} handler - Original handler
 * @returns {Function} Wrapped handler
 */
export function wrapWithReceiptValidation(noun: string, verb: string, handler: Function): Function;
/**
 * Create extension definition for V6 commands.
 *
 * Helper to create registry-compatible extensions following V6 patterns.
 *
 * @param {Object} config - Extension configuration
 * @returns {Object} Registry extension
 */
export function createV6Extension({ id, nouns }: any): any;
declare namespace _default {
    export { buildV6Spine };
    export { generateSpineReport };
    export { getNounVerbMatrix };
    export { wrapWithReceiptValidation };
    export { createV6Extension };
}
export default _default;
