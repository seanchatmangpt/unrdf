/**
 * Validate all canonical nouns against schema.
 *
 * @returns {Array<{noun: string, errors: Array}>} Validation errors
 */
export function validateNouns(): Array<{
    noun: string;
    errors: any[];
}>;
/**
 * Get noun definition by name.
 *
 * @param {string} name - Noun name
 * @returns {Object|undefined} Noun definition
 */
export function getNoun(name: string): any | undefined;
/**
 * Get all noun names (for CLI autocomplete/help).
 *
 * @returns {Array<string>} Noun names
 */
export function getNounNames(): Array<string>;
/**
 * Build noun-to-package mapping.
 *
 * @returns {Object} Map of noun -> package
 */
export function buildNounPackageMap(): any;
/**
 * Noun schema - validates noun structure.
 * @type {z.ZodSchema}
 */
export const NounSchema: z.ZodSchema;
/**
 * Canonical nouns for v6.
 *
 * Order matters for help display but not for execution.
 * Each noun represents a distinct conceptual domain.
 *
 * @type {Array<Object>}
 */
export const CANONICAL_NOUNS: Array<any>;
declare namespace _default {
    export { CANONICAL_NOUNS };
    export { NounSchema };
    export { validateNouns };
    export { getNoun };
    export { getNounNames };
    export { buildNounPackageMap };
}
export default _default;
import { z } from 'zod';
