/**
 * Validate all verbs against schema.
 *
 * @returns {Array<{verb: string, errors: Array}>} Validation errors
 */
export function validateVerbs(): Array<{
    verb: string;
    errors: any[];
}>;
/**
 * Get verb definition by name.
 *
 * @param {string} name - Verb name
 * @returns {Object|undefined} Verb definition
 */
export function getVerb(name: string): any | undefined;
/**
 * Get verbs applicable to a noun.
 *
 * @param {string} noun - Noun name
 * @returns {Array<Object>} Verb definitions
 */
export function getVerbsForNoun(noun: string): Array<any>;
/**
 * Build noun-verb matrix.
 *
 * @returns {Object} Map of noun -> array of verbs
 */
export function buildNounVerbMatrix(): any;
/**
 * Check if noun-verb combination is valid.
 *
 * @param {string} noun - Noun name
 * @param {string} verb - Verb name
 * @returns {boolean} Whether combination is valid
 */
export function isValidCombination(noun: string, verb: string): boolean;
/**
 * Verb schema - validates verb structure.
 * @type {z.ZodSchema}
 */
export const VerbSchema: z.ZodSchema;
/**
 * Canonical verbs for v6.
 *
 * Each verb represents a distinct operation type.
 * Verbs are reusable across multiple nouns.
 *
 * @type {Array<Object>}
 */
export const CANONICAL_VERBS: Array<any>;
declare namespace _default {
    export { CANONICAL_VERBS };
    export { VerbSchema };
    export { validateVerbs };
    export { getVerb };
    export { getVerbsForNoun };
    export { buildNounVerbMatrix };
    export { isValidCombination };
}
export default _default;
import { z } from 'zod';
