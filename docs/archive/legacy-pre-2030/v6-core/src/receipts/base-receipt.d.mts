/**
 * @typedef {Object} Attestation
 * @property {string} algorithm - Signature algorithm
 * @property {string} publicKey - Public key hex
 * @property {string} signature - Signature hex
 * @property {string} [signer] - Signer identity
 */
/**
 * @typedef {Object} VectorClock
 * @property {string} nodeId - Node identifier
 * @property {Record<string, string>} counters - Vector clock counters
 */
/**
 * @typedef {Object} BaseReceipt
 * @property {string} id - UUID of receipt
 * @property {'execution'|'allocation'|'compile'|'verification'} receiptType - Receipt type
 * @property {bigint} t_ns - Nanosecond timestamp
 * @property {string} timestamp_iso - ISO timestamp
 * @property {string|null} previousHash - Previous receipt hash
 * @property {string} payloadHash - Payload hash
 * @property {string} receiptHash - Receipt hash
 * @property {Attestation} [attestation] - Signature/attestation
 * @property {VectorClock} [vectorClock] - Vector clock
 * @property {string} [gitRef] - Git reference
 * @property {string} [kgcEventId] - KGC event ID
 */
/**
 * Generate a UUID v4
 * @param {Object} [context={}] - Execution context with uuid for determinism
 * @returns {string} UUID string
 */
export function generateUUID(context?: any): string;
/**
 * Serialize object deterministically for hashing
 * Keys are sorted alphabetically at all levels
 *
 * @param {Object} obj - Object to serialize
 * @returns {string} Deterministic JSON string
 */
export function deterministicSerialize(obj: any): string;
/**
 * Compute BLAKE3 hash of data
 *
 * @param {string|Object} data - Data to hash
 * @returns {Promise<string>} 64-character hex hash
 */
export function computeBlake3(data: string | any): Promise<string>;
/**
 * Compute chained receipt hash from previousHash and payloadHash
 * Chain format: previousHash:payloadHash
 *
 * @param {string|null} previousHash - Previous receipt hash (null for genesis)
 * @param {string} payloadHash - Current payload hash
 * @returns {Promise<string>} 64-character hex hash
 */
export function computeChainHash(previousHash: string | null, payloadHash: string): Promise<string>;
/**
 * Verify base receipt structure and hashes
 *
 * @param {BaseReceipt} receipt - Receipt to verify
 * @returns {Promise<{valid: boolean, error?: string, checks?: Object}>}
 */
export function verifyBaseReceipt(receipt: BaseReceipt): Promise<{
    valid: boolean;
    error?: string;
    checks?: any;
}>;
/**
 * BLAKE3 hash length in hex characters
 * @constant {number}
 */
export const BLAKE3_HEX_LENGTH: 64;
/**
 * Receipt type discriminator values
 */
export type RECEIPT_TYPES = string;
/**
 * Receipt type discriminator values
 * @readonly
 * @enum {string}
 */
export const RECEIPT_TYPES: Readonly<{
    EXECUTION: "execution";
    ALLOCATION: "allocation";
    COMPILE: "compile";
    VERIFICATION: "verification";
}>;
/**
 * Receipt type discriminator schema
 */
export const ReceiptTypeSchema: z.ZodEnum<{
    execution: "execution";
    allocation: "allocation";
    compile: "compile";
    verification: "verification";
}>;
/**
 * Signature/attestation hook schema
 */
export const AttestationSchema: z.ZodOptional<z.ZodObject<{
    algorithm: z.ZodString;
    publicKey: z.ZodString;
    signature: z.ZodString;
    signer: z.ZodOptional<z.ZodString>;
}, z.core.$strip>>;
/**
 * Vector clock schema for causality tracking
 */
export const VectorClockSchema: z.ZodObject<{
    nodeId: z.ZodString;
    counters: z.ZodRecord<z.ZodString, z.ZodString>;
}, z.core.$loose>;
/**
 * Base receipt schema - common fields for all receipt types
 */
export const BaseReceiptSchema: z.ZodObject<{
    id: z.ZodString;
    receiptType: z.ZodEnum<{
        execution: "execution";
        allocation: "allocation";
        compile: "compile";
        verification: "verification";
    }>;
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
}, z.core.$strip>;
export type Attestation = {
    /**
     * - Signature algorithm
     */
    algorithm: string;
    /**
     * - Public key hex
     */
    publicKey: string;
    /**
     * - Signature hex
     */
    signature: string;
    /**
     * - Signer identity
     */
    signer?: string;
};
export type VectorClock = {
    /**
     * - Node identifier
     */
    nodeId: string;
    /**
     * - Vector clock counters
     */
    counters: Record<string, string>;
};
export type BaseReceipt = {
    /**
     * - UUID of receipt
     */
    id: string;
    /**
     * - Receipt type
     */
    receiptType: "execution" | "allocation" | "compile" | "verification";
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
     * - Signature/attestation
     */
    attestation?: Attestation;
    /**
     * - Vector clock
     */
    vectorClock?: VectorClock;
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
