/**
 * Create a receipt of the specified type
 *
 * Unified factory function that generates properly typed receipts with
 * cryptographic proofs.
 *
 * @param {'execution'|'allocation'|'compile'|'verification'} type - Receipt type
 * @param {Object} event - Event data specific to receipt type
 * @param {Object|null} [previousReceipt=null] - Previous receipt for chaining
 * @returns {Promise<Object>} Complete receipt with hashes
 *
 * @example
 * // Execution receipt
 * const execReceipt = await createReceipt('execution', {
 *   eventType: 'TASK_COMPLETED',
 *   caseId: 'case-123',
 *   taskId: 'approval',
 *   payload: { decision: 'APPROVE' }
 * });
 *
 * @example
 * // Allocation receipt
 * const allocReceipt = await createReceipt('allocation', {
 *   eventType: 'RESOURCE_ALLOCATED',
 *   resourceId: 'res-456',
 *   poolId: 'pool-789',
 *   allocationPeriod: { start: '2025-01-01', end: '2025-01-02' },
 *   capacity: { total: 100, available: 80, allocated: 20, unit: 'hours' },
 *   payload: { action: 'ALLOCATE' }
 * });
 *
 * @example
 * // Compile receipt
 * const compileReceipt = await createReceipt('compile', {
 *   eventType: 'GRAMMAR_COMPILED',
 *   inputHashes: ['abc...', 'def...'],
 *   outputHash: 'ghi...',
 *   compilerVersion: '1.0.0',
 *   grammarType: 'SPARQL',
 *   payload: { result: 'SUCCESS', metadata: { inputCount: 2, outputCount: 1 } }
 * });
 *
 * @example
 * // Verification receipt
 * const verifyReceipt = await createReceipt('verification', {
 *   eventType: 'MERKLE_PROOF_VERIFIED',
 *   verifiedHash: 'abc...',
 *   merkleRoot: 'def...',
 *   proofPath: [{ hash: 'ghi...', position: 'left' }],
 *   payload: { result: 'VALID', method: 'merkle-tree' }
 * });
 */
export function createReceipt(type: "execution" | "allocation" | "compile" | "verification", event: any, previousReceipt?: any | null): Promise<any>;
/**
 * Verify a receipt of any type
 *
 * Validates schema, hashes, and type-specific constraints.
 *
 * @param {Object} receipt - Receipt to verify
 * @returns {Promise<{valid: boolean, error?: string, checks?: Object}>}
 */
export function verifyReceipt(receipt: any): Promise<{
    valid: boolean;
    error?: string;
    checks?: any;
}>;
/**
 * Verify chain link between two receipts
 *
 * @param {Object} receipt - Current receipt
 * @param {Object} previousReceipt - Previous receipt
 * @returns {Promise<{valid: boolean, error?: string}>}
 */
export function verifyChainLink(receipt: any, previousReceipt: any): Promise<{
    valid: boolean;
    error?: string;
}>;
/**
 * Unified Receipt Schema - Discriminated union of all receipt types
 *
 * Validates any receipt type based on the `receiptType` discriminator field.
 */
export const ReceiptSchema: z.ZodDiscriminatedUnion<[z.ZodObject<{
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
    receiptType: z.ZodLiteral<"execution">;
    eventType: z.ZodEnum<{
        CASE_CREATED: "CASE_CREATED";
        TASK_ENABLED: "TASK_ENABLED";
        TASK_STARTED: "TASK_STARTED";
        TASK_COMPLETED: "TASK_COMPLETED";
        TASK_CANCELLED: "TASK_CANCELLED";
        TASK_FAILED: "TASK_FAILED";
        TASK_TIMEOUT: "TASK_TIMEOUT";
        WORK_ITEM_CREATED: "WORK_ITEM_CREATED";
        CONTROL_FLOW_EVALUATED: "CONTROL_FLOW_EVALUATED";
    }>;
    caseId: z.ZodString;
    taskId: z.ZodString;
    workItemId: z.ZodOptional<z.ZodString>;
    payload: z.ZodObject<{
        decision: z.ZodOptional<z.ZodString>;
        justification: z.ZodOptional<z.ZodObject<{
            hookValidated: z.ZodOptional<z.ZodString>;
            sparqlQuery: z.ZodOptional<z.ZodString>;
            reasoning: z.ZodOptional<z.ZodString>;
            conditionChecked: z.ZodOptional<z.ZodString>;
            approvedBy: z.ZodOptional<z.ZodString>;
        }, z.core.$strip>>;
        actor: z.ZodOptional<z.ZodString>;
        context: z.ZodOptional<z.ZodAny>;
    }, z.core.$loose>;
}, z.core.$strip>, z.ZodObject<{
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
    receiptType: z.ZodLiteral<"allocation">;
    eventType: z.ZodEnum<{
        RESOURCE_ALLOCATED: "RESOURCE_ALLOCATED";
        RESOURCE_RELEASED: "RESOURCE_RELEASED";
        POOL_CREATED: "POOL_CREATED";
        POOL_UPDATED: "POOL_UPDATED";
        CAPACITY_CHANGED: "CAPACITY_CHANGED";
        CALENDAR_UPDATED: "CALENDAR_UPDATED";
    }>;
    resourceId: z.ZodString;
    poolId: z.ZodString;
    allocationPeriod: z.ZodObject<{
        start: z.ZodString;
        end: z.ZodString;
        durationMs: z.ZodOptional<z.ZodNumber>;
    }, z.core.$strip>;
    capacity: z.ZodObject<{
        total: z.ZodNumber;
        available: z.ZodNumber;
        allocated: z.ZodNumber;
        unit: z.ZodString;
    }, z.core.$strip>;
    allocatedTo: z.ZodOptional<z.ZodString>;
    payload: z.ZodObject<{
        action: z.ZodString;
        amount: z.ZodOptional<z.ZodNumber>;
        resourceAttributes: z.ZodOptional<z.ZodRecord<z.ZodString, z.ZodAny>>;
        priority: z.ZodOptional<z.ZodNumber>;
        reason: z.ZodOptional<z.ZodString>;
    }, z.core.$loose>;
}, z.core.$strip>, z.ZodObject<{
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
}, z.core.$strip>, z.ZodObject<{
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
    receiptType: z.ZodLiteral<"verification">;
    eventType: z.ZodEnum<{
        HASH_VERIFIED: "HASH_VERIFIED";
        SIGNATURE_VERIFIED: "SIGNATURE_VERIFIED";
        MERKLE_PROOF_VERIFIED: "MERKLE_PROOF_VERIFIED";
        CHAIN_VERIFIED: "CHAIN_VERIFIED";
        BLOCKCHAIN_ANCHORED: "BLOCKCHAIN_ANCHORED";
    }>;
    verifiedHash: z.ZodString;
    merkleRoot: z.ZodOptional<z.ZodString>;
    proofPath: z.ZodOptional<z.ZodArray<z.ZodObject<{
        hash: z.ZodString;
        position: z.ZodEnum<{
            left: "left";
            right: "right";
        }>;
    }, z.core.$strip>>>;
    signatureValid: z.ZodOptional<z.ZodBoolean>;
    blockchainAnchor: z.ZodOptional<z.ZodObject<{
        network: z.ZodString;
        txHash: z.ZodString;
        blockNumber: z.ZodNumber;
        blockTimestamp: z.ZodNumber;
        contractAddress: z.ZodOptional<z.ZodString>;
    }, z.core.$strip>>;
    payload: z.ZodObject<{
        result: z.ZodString;
        method: z.ZodString;
        details: z.ZodOptional<z.ZodRecord<z.ZodString, z.ZodAny>>;
        errorMessage: z.ZodOptional<z.ZodString>;
    }, z.core.$loose>;
}, z.core.$strip>], "receiptType">;
export { default as MerkleTree } from "./merkle/index.mjs";
declare namespace _default {
    export { createReceipt };
    export { verifyReceipt };
    export { verifyChainLink };
    export { RECEIPT_TYPES };
    export { EXECUTION_EVENT_TYPES };
    export { ALLOCATION_EVENT_TYPES };
    export { COMPILE_EVENT_TYPES };
    export { GRAMMAR_TYPES };
    export { VERIFICATION_EVENT_TYPES };
}
export default _default;
import { z } from 'zod';
import { RECEIPT_TYPES } from './base-receipt.mjs';
import { BLAKE3_HEX_LENGTH } from './base-receipt.mjs';
import { generateUUID } from './base-receipt.mjs';
import { deterministicSerialize } from './base-receipt.mjs';
import { computeBlake3 } from './base-receipt.mjs';
import { computeChainHash } from './base-receipt.mjs';
import { verifyBaseReceipt } from './base-receipt.mjs';
import { BaseReceiptSchema } from './base-receipt.mjs';
import { ReceiptTypeSchema } from './base-receipt.mjs';
import { AttestationSchema } from './base-receipt.mjs';
import { VectorClockSchema } from './base-receipt.mjs';
import ExecutionReceiptSchema from './execution-receipt.mjs';
import AllocationReceiptSchema from './allocation-receipt.mjs';
import CompileReceiptSchema from './compile-receipt.mjs';
import VerificationReceiptSchema from './verification-receipt.mjs';
import { EXECUTION_EVENT_TYPES } from './execution-receipt.mjs';
import { ALLOCATION_EVENT_TYPES } from './allocation-receipt.mjs';
import { COMPILE_EVENT_TYPES } from './compile-receipt.mjs';
import { GRAMMAR_TYPES } from './compile-receipt.mjs';
import { VERIFICATION_EVENT_TYPES } from './verification-receipt.mjs';
import { ExecutionEventTypeSchema } from './execution-receipt.mjs';
import { JustificationSchema } from './execution-receipt.mjs';
import { ExecutionPayloadSchema } from './execution-receipt.mjs';
import { AllocationEventTypeSchema } from './allocation-receipt.mjs';
import { AllocationPeriodSchema } from './allocation-receipt.mjs';
import { CapacitySchema } from './allocation-receipt.mjs';
import { AllocationPayloadSchema } from './allocation-receipt.mjs';
import { CompileEventTypeSchema } from './compile-receipt.mjs';
import { GrammarTypeSchema } from './compile-receipt.mjs';
import { CompilationMetadataSchema } from './compile-receipt.mjs';
import { CompilePayloadSchema } from './compile-receipt.mjs';
import { VerificationEventTypeSchema } from './verification-receipt.mjs';
import { MerkleProofStepSchema } from './verification-receipt.mjs';
import { BlockchainAnchorSchema } from './verification-receipt.mjs';
import { VerificationPayloadSchema } from './verification-receipt.mjs';
export { RECEIPT_TYPES, BLAKE3_HEX_LENGTH, generateUUID, deterministicSerialize, computeBlake3, computeChainHash, verifyBaseReceipt, BaseReceiptSchema, ReceiptTypeSchema, AttestationSchema, VectorClockSchema, ExecutionReceiptSchema, AllocationReceiptSchema, CompileReceiptSchema, VerificationReceiptSchema, EXECUTION_EVENT_TYPES, ALLOCATION_EVENT_TYPES, COMPILE_EVENT_TYPES, GRAMMAR_TYPES, VERIFICATION_EVENT_TYPES, ExecutionEventTypeSchema, JustificationSchema, ExecutionPayloadSchema, AllocationEventTypeSchema, AllocationPeriodSchema, CapacitySchema, AllocationPayloadSchema, CompileEventTypeSchema, GrammarTypeSchema, CompilationMetadataSchema, CompilePayloadSchema, VerificationEventTypeSchema, MerkleProofStepSchema, BlockchainAnchorSchema, VerificationPayloadSchema };
export { withReceipt, createReceiptChain, verifyIdempotency } from "./with-receipt.mjs";
