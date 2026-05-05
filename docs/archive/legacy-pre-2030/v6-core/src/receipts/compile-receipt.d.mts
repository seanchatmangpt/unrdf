/**
 * Compile event types
 */
export type COMPILE_EVENT_TYPES = string;
/**
 * Compile event types
 * @readonly
 * @enum {string}
 */
export const COMPILE_EVENT_TYPES: Readonly<{
    GRAMMAR_COMPILED: "GRAMMAR_COMPILED";
    DOC_GENERATED: "DOC_GENERATED";
    SCHEMA_VALIDATED: "SCHEMA_VALIDATED";
    RULESET_BUILT: "RULESET_BUILT";
    ONTOLOGY_COMPILED: "ONTOLOGY_COMPILED";
}>;
/**
 * Grammar types
 */
export type GRAMMAR_TYPES = string;
/**
 * Grammar types
 * @readonly
 * @enum {string}
 */
export const GRAMMAR_TYPES: Readonly<{
    SPARQL: "SPARQL";
    SHACL: "SHACL";
    N3: "N3";
    OWL: "OWL";
    RDFS: "RDFS";
    CUSTOM: "CUSTOM";
}>;
/**
 * Event type schema
 */
export const CompileEventTypeSchema: z.ZodEnum<{
    GRAMMAR_COMPILED: "GRAMMAR_COMPILED";
    DOC_GENERATED: "DOC_GENERATED";
    SCHEMA_VALIDATED: "SCHEMA_VALIDATED";
    RULESET_BUILT: "RULESET_BUILT";
    ONTOLOGY_COMPILED: "ONTOLOGY_COMPILED";
}>;
/**
 * Grammar type schema
 */
export const GrammarTypeSchema: z.ZodEnum<{
    CUSTOM: "CUSTOM";
    SPARQL: "SPARQL";
    SHACL: "SHACL";
    N3: "N3";
    OWL: "OWL";
    RDFS: "RDFS";
}>;
/**
 * Compilation metadata schema
 */
export const CompilationMetadataSchema: z.ZodObject<{
    inputCount: z.ZodNumber;
    outputCount: z.ZodNumber;
    durationMs: z.ZodOptional<z.ZodNumber>;
    compilerFlags: z.ZodOptional<z.ZodArray<z.ZodString>>;
    warningCount: z.ZodOptional<z.ZodNumber>;
    errorCount: z.ZodOptional<z.ZodNumber>;
}, z.core.$strip>;
/**
 * Compile payload schema
 */
export const CompilePayloadSchema: z.ZodObject<{
    result: z.ZodString;
    metadata: z.ZodObject<{
        inputCount: z.ZodNumber;
        outputCount: z.ZodNumber;
        durationMs: z.ZodOptional<z.ZodNumber>;
        compilerFlags: z.ZodOptional<z.ZodArray<z.ZodString>>;
        warningCount: z.ZodOptional<z.ZodNumber>;
        errorCount: z.ZodOptional<z.ZodNumber>;
    }, z.core.$strip>;
    outputPaths: z.ZodOptional<z.ZodArray<z.ZodString>>;
    warnings: z.ZodOptional<z.ZodArray<z.ZodString>>;
    context: z.ZodOptional<z.ZodAny>;
}, z.core.$loose>;
/**
 * Compile receipt schema - extends base with compilation-specific fields
 */
export const CompileReceiptSchema: z.ZodObject<{
    id: z.ZodString;
    t_ns: z.ZodBigInt;
    timestamp_iso: z.ZodString;
    previousHash: z.ZodNullable<z.ZodString>;
    payloadHash: z.ZodString;
    receiptHash: z.ZodString;
    attestation: z.ZodOptional<z.ZodObject<{
        algorithm: z.ZodString;
        publicKey: z.ZodString;
        signature: z.ZodString;
        signer: z.ZodOptional<z.ZodString>;
    }, z.core.$strip>>;
    vectorClock: z.ZodOptional<z.ZodObject<{
        nodeId: z.ZodString;
        counters: z.ZodRecord<z.ZodString, z.ZodString>;
    }, z.core.$loose>>;
    gitRef: z.ZodOptional<z.ZodString>;
    kgcEventId: z.ZodOptional<z.ZodString>;
    receiptType: z.ZodLiteral<"compile">;
    eventType: z.ZodEnum<{
        GRAMMAR_COMPILED: "GRAMMAR_COMPILED";
        DOC_GENERATED: "DOC_GENERATED";
        SCHEMA_VALIDATED: "SCHEMA_VALIDATED";
        RULESET_BUILT: "RULESET_BUILT";
        ONTOLOGY_COMPILED: "ONTOLOGY_COMPILED";
    }>;
    inputHashes: z.ZodArray<z.ZodString>;
    outputHash: z.ZodString;
    compilerVersion: z.ZodString;
    grammarType: z.ZodEnum<{
        CUSTOM: "CUSTOM";
        SPARQL: "SPARQL";
        SHACL: "SHACL";
        N3: "N3";
        OWL: "OWL";
        RDFS: "RDFS";
    }>;
    payload: z.ZodObject<{
        result: z.ZodString;
        metadata: z.ZodObject<{
            inputCount: z.ZodNumber;
            outputCount: z.ZodNumber;
            durationMs: z.ZodOptional<z.ZodNumber>;
            compilerFlags: z.ZodOptional<z.ZodArray<z.ZodString>>;
            warningCount: z.ZodOptional<z.ZodNumber>;
            errorCount: z.ZodOptional<z.ZodNumber>;
        }, z.core.$strip>;
        outputPaths: z.ZodOptional<z.ZodArray<z.ZodString>>;
        warnings: z.ZodOptional<z.ZodArray<z.ZodString>>;
        context: z.ZodOptional<z.ZodAny>;
    }, z.core.$loose>;
}, z.core.$strip>;
export default CompileReceiptSchema;
export type CompilationMetadata = {
    /**
     * - Input file count
     */
    inputCount: number;
    /**
     * - Output file count
     */
    outputCount: number;
    /**
     * - Compilation duration
     */
    durationMs?: number;
    /**
     * - Compiler flags/options
     */
    compilerFlags?: string[];
    /**
     * - Warning count
     */
    warningCount?: number;
    /**
     * - Error count
     */
    errorCount?: number;
};
export type CompilePayload = {
    /**
     * - Compilation result
     */
    result: string;
    /**
     * - Compilation metadata
     */
    metadata: CompilationMetadata;
    /**
     * - Output file paths
     */
    outputPaths?: string[];
    /**
     * - Compilation warnings
     */
    warnings?: string[];
    /**
     * - Additional context
     */
    context?: any;
};
export type CompileReceipt = {
    /**
     * - UUID of receipt
     */
    id: string;
    /**
     * - Receipt type discriminator
     */
    receiptType: "compile";
    /**
     * - Nanosecond timestamp
     */
    t_ns: bigint;
    /**
     * - ISO timestamp
     */
    timestamp_iso: string;
    /**
     * - Previous receipt hash
     */
    previousHash: string | null;
    /**
     * - Payload hash
     */
    payloadHash: string;
    /**
     * - Receipt hash
     */
    receiptHash: string;
    /**
     * - Compile event type
     */
    eventType: string;
    /**
     * - Input file hashes
     */
    inputHashes: string[];
    /**
     * - Output artifact hash
     */
    outputHash: string;
    /**
     * - Compiler version
     */
    compilerVersion: string;
    /**
     * - Grammar/schema type
     */
    grammarType: string;
    /**
     * - Compile payload
     */
    payload: CompilePayload;
    /**
     * - Signature/attestation
     */
    attestation?: any;
    /**
     * - Vector clock
     */
    vectorClock?: any;
    /**
     * - Git reference
     */
    gitRef?: string;
    /**
     * - KGC event ID
     */
    kgcEventId?: string;
};
import { z } from 'zod';
