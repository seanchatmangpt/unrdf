/**
 * L5 Determinism Test
 */
export function testGrammarDeterminism(context: any, iterations?: number): Promise<{
    iterations: number;
    uniqueHashes: number;
    deterministic: boolean;
    expectedHash: any;
}>;
/**
 * Grammar Schema
 */
export const GrammarSchema: z.ZodObject<{
    source: z.ZodString;
    version: z.ZodDefault<z.ZodString>;
    rules: z.ZodOptional<z.ZodArray<z.ZodString>>;
}, z.core.$strip>;
/**
 * Parsed Grammar Schema
 */
export const ParsedGrammarSchema: z.ZodObject<{
    ast: z.ZodAny;
    version: z.ZodString;
    versionHash: z.ZodString;
    ruleCount: z.ZodNumber;
}, z.core.$strip>;
/**
 * Wrapped: Parse grammar with receipt
 */
export const parseGrammar: Function;
import { z } from 'zod';
