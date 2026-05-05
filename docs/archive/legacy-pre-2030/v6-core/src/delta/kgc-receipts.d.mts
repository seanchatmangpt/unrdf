/**
 * L5 Determinism Test
 */
export function testKGCDeterminism(context: any, iterations?: number): Promise<{
    iterations: number;
    uniqueHashes: number;
    deterministic: boolean;
    expectedHash: any;
}>;
/**
 * Delta Schema
 */
export const DeltaSchema: z.ZodObject<{
    type: z.ZodEnum<{
        create: "create";
        update: "update";
        delete: "delete";
        composite: "composite";
    }>;
    target: z.ZodObject<{
        entity: z.ZodString;
        scope: z.ZodOptional<z.ZodString>;
    }, z.core.$strip>;
    changes: z.ZodArray<z.ZodObject<{
        operation: z.ZodEnum<{
            add_triple: "add_triple";
            remove_triple: "remove_triple";
            replace_value: "replace_value";
            execute_sparql: "execute_sparql";
        }>;
        subject: z.ZodOptional<z.ZodAny>;
        predicate: z.ZodOptional<z.ZodAny>;
        object: z.ZodOptional<z.ZodAny>;
        sparql: z.ZodOptional<z.ZodString>;
    }, z.core.$strip>>;
}, z.core.$strip>;
/**
 * Delta Result Schema
 */
export const DeltaResultSchema: z.ZodObject<{
    deltaId: z.ZodString;
    deltaHash: z.ZodString;
    merkleProof: z.ZodObject<{
        root: z.ZodString;
        leaves: z.ZodArray<z.ZodString>;
    }, z.core.$strip>;
    changeCount: z.ZodNumber;
}, z.core.$strip>;
/**
 * Wrapped: Generate delta with receipt
 */
export const generateDelta: Function;
import { z } from 'zod';
