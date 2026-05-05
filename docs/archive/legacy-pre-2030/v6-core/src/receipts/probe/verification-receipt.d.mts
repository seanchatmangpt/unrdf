/**
 * @typedef {Object} VerificationCheck
 * @property {'observation-hash-recompute'|'chain-integrity'|'merkle-root-recompute'|'temporal-ordering'|'shard-consistency'} checkType
 * @property {string} [agentId] - Agent being verified (null for global)
 * @property {boolean} passed - Whether check passed
 * @property {Record<string, any>} [details] - Check details
 * @property {string} [errorMessage] - Error if failed
 */
/**
 * @typedef {Object} CertificateChainStep
 * @property {string} receiptHash - Receipt hash
 * @property {'probe-observation'|'probe-merge'} receiptType - Receipt type
 * @property {string} relationship - Relationship to next step
 * @property {Object} [context] - Additional context
 * @property {string} [context.agentId] - Agent ID
 * @property {number} [context.observationIndex] - Observation index
 * @property {string} [context.mergeId] - Merge ID
 */
/**
 * @typedef {Object} ProbeVerificationReceipt
 * @property {string} id - UUID of receipt
 * @property {'probe-verification'} receiptType - Receipt type
 * @property {bigint} t_ns - Nanosecond timestamp
 * @property {string} timestamp_iso - ISO timestamp
 * @property {string|null} previousHash - Previous receipt hash
 * @property {string} payloadHash - Payload hash
 * @property {string} receiptHash - Receipt hash
 * @property {string} verificationId - Verification ID
 * @property {string} mergeReceiptHash - Merge receipt hash
 * @property {VerificationCheck[]} verifications - Checks performed
 * @property {boolean} deterministic - Determinism result
 * @property {boolean} conflictFree - Conflict-free result
 * @property {CertificateChainStep[]} certificateChain - Audit chain
 * @property {number} obsCount - Total observations
 * @property {number} agentCount - Total agents
 * @property {string} [verifiedAt] - Verification timestamp
 * @property {string} [verifierId] - Verifier ID
 */
/**
 * Summarize verification results
 *
 * @param {ProbeVerificationReceipt} receipt - Verification receipt
 * @returns {Object} Summary object
 *
 * @example
 * const summary = summarizeVerification(receipt);
 * console.log(summary.passed); // true if all checks passed
 * console.log(summary.passCount); // number of passed checks
 */
export function summarizeVerification(receipt: ProbeVerificationReceipt): any;
/**
 * Get failed checks from verification receipt
 *
 * @param {ProbeVerificationReceipt} receipt - Verification receipt
 * @returns {VerificationCheck[]} Array of failed checks
 */
export function getFailedChecks(receipt: ProbeVerificationReceipt): VerificationCheck[];
/**
 * Get confidence score (0-100)
 *
 * Based on number of passed checks and properties.
 *
 * @param {ProbeVerificationReceipt} receipt - Verification receipt
 * @returns {number} Confidence percentage (0-100)
 */
export function getConfidenceScore(receipt: ProbeVerificationReceipt): number;
/**
 * Probe verification receipt type discriminator
 * @constant {string}
 */
export const PROBE_VERIFICATION_TYPE: "probe-verification";
/**
 * Verification check result schema
 */
export const VerificationCheckSchema: z.ZodObject<{
    checkType: z.ZodEnum<{
        "observation-hash-recompute": "observation-hash-recompute";
        "chain-integrity": "chain-integrity";
        "merkle-root-recompute": "merkle-root-recompute";
        "temporal-ordering": "temporal-ordering";
        "shard-consistency": "shard-consistency";
    }>;
    agentId: z.ZodNullable<z.ZodOptional<z.ZodString>>;
    passed: z.ZodBoolean;
    details: z.ZodOptional<z.ZodRecord<z.ZodAny, z.core.SomeType>>;
    errorMessage: z.ZodOptional<z.ZodString>;
}, z.core.$strip>;
/**
 * Certificate chain step schema
 *
 * Proves relationship between observations, chains, and merge.
 */
export const CertificateChainStepSchema: z.ZodObject<{
    receiptHash: z.ZodString;
    receiptType: z.ZodEnum<{
        "probe-observation": "probe-observation";
        "probe-merge": "probe-merge";
    }>;
    relationship: z.ZodString;
    context: z.ZodOptional<z.ZodObject<{
        agentId: z.ZodOptional<z.ZodString>;
        observationIndex: z.ZodOptional<z.ZodNumber>;
        mergeId: z.ZodOptional<z.ZodString>;
    }, z.core.$strip>>;
}, z.core.$strip>;
/**
 * Probe verification receipt schema
 */
export const ProbeVerificationReceiptSchema: z.ZodObject<{
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
    receiptType: z.ZodLiteral<"probe-verification">;
    verificationId: z.ZodString;
    mergeReceiptHash: z.ZodString;
    verifications: z.ZodArray<z.ZodObject<{
        checkType: z.ZodEnum<{
            "observation-hash-recompute": "observation-hash-recompute";
            "chain-integrity": "chain-integrity";
            "merkle-root-recompute": "merkle-root-recompute";
            "temporal-ordering": "temporal-ordering";
            "shard-consistency": "shard-consistency";
        }>;
        agentId: z.ZodNullable<z.ZodOptional<z.ZodString>>;
        passed: z.ZodBoolean;
        details: z.ZodOptional<z.ZodRecord<z.ZodAny, z.core.SomeType>>;
        errorMessage: z.ZodOptional<z.ZodString>;
    }, z.core.$strip>>;
    deterministic: z.ZodBoolean;
    conflictFree: z.ZodBoolean;
    certificateChain: z.ZodArray<z.ZodObject<{
        receiptHash: z.ZodString;
        receiptType: z.ZodEnum<{
            "probe-observation": "probe-observation";
            "probe-merge": "probe-merge";
        }>;
        relationship: z.ZodString;
        context: z.ZodOptional<z.ZodObject<{
            agentId: z.ZodOptional<z.ZodString>;
            observationIndex: z.ZodOptional<z.ZodNumber>;
            mergeId: z.ZodOptional<z.ZodString>;
        }, z.core.$strip>>;
    }, z.core.$strip>>;
    obsCount: z.ZodNumber;
    agentCount: z.ZodNumber;
    verifiedAt: z.ZodOptional<z.ZodString>;
    verifierId: z.ZodOptional<z.ZodString>;
}, z.core.$strip>;
export default ProbeVerificationReceiptSchema;
export type VerificationCheck = {
    checkType: "observation-hash-recompute" | "chain-integrity" | "merkle-root-recompute" | "temporal-ordering" | "shard-consistency";
    /**
     * - Agent being verified (null for global)
     */
    agentId?: string;
    /**
     * - Whether check passed
     */
    passed: boolean;
    /**
     * - Check details
     */
    details?: Record<string, any>;
    /**
     * - Error if failed
     */
    errorMessage?: string;
};
export type CertificateChainStep = {
    /**
     * - Receipt hash
     */
    receiptHash: string;
    /**
     * - Receipt type
     */
    receiptType: "probe-observation" | "probe-merge";
    /**
     * - Relationship to next step
     */
    relationship: string;
    /**
     * - Additional context
     */
    context?: {
        agentId?: string;
        observationIndex?: number;
        mergeId?: string;
    };
};
export type ProbeVerificationReceipt = {
    /**
     * - UUID of receipt
     */
    id: string;
    /**
     * - Receipt type
     */
    receiptType: "probe-verification";
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
     * - Verification ID
     */
    verificationId: string;
    /**
     * - Merge receipt hash
     */
    mergeReceiptHash: string;
    /**
     * - Checks performed
     */
    verifications: VerificationCheck[];
    /**
     * - Determinism result
     */
    deterministic: boolean;
    /**
     * - Conflict-free result
     */
    conflictFree: boolean;
    /**
     * - Audit chain
     */
    certificateChain: CertificateChainStep[];
    /**
     * - Total observations
     */
    obsCount: number;
    /**
     * - Total agents
     */
    agentCount: number;
    /**
     * - Verification timestamp
     */
    verifiedAt?: string;
    /**
     * - Verifier ID
     */
    verifierId?: string;
};
import { z } from 'zod';
