/**
 * L5 Determinism Test
 */
export function testIndexingDeterminism(context: any, iterations?: number): Promise<{
    iterations: number;
    uniqueHashes: number;
    deterministic: boolean;
    expectedHash: any;
}>;
/**
 * Index Config Schema
 */
export const IndexConfigSchema: z.ZodObject<{
    entity: z.ZodString;
    fields: z.ZodArray<z.ZodString>;
    type: z.ZodDefault<z.ZodEnum<{
        hash: "hash";
        btree: "btree";
        fulltext: "fulltext";
    }>>;
}, z.core.$strip>;
/**
 * Index Result Schema
 */
export const IndexResultSchema: z.ZodObject<{
    indexId: z.ZodString;
    indexHash: z.ZodString;
    fieldCount: z.ZodNumber;
    status: z.ZodEnum<{
        created: "created";
        updated: "updated";
        exists: "exists";
    }>;
}, z.core.$strip>;
/**
 * Wrapped: Create index with receipt
 */
export const createIndex: Function;
import { z } from 'zod';
