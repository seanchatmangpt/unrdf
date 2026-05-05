/**
 * Verification event types
 */
export type VERIFICATION_EVENT_TYPES = string;
/**
 * Verification event types
 * @readonly
 * @enum {string}
 */
export const VERIFICATION_EVENT_TYPES: Readonly<{
    HASH_VERIFIED: "HASH_VERIFIED";
    SIGNATURE_VERIFIED: "SIGNATURE_VERIFIED";
    MERKLE_PROOF_VERIFIED: "MERKLE_PROOF_VERIFIED";
    CHAIN_VERIFIED: "CHAIN_VERIFIED";
    BLOCKCHAIN_ANCHORED: "BLOCKCHAIN_ANCHORED";
}>;
/**
 * Event type schema
 */
export const VerificationEventTypeSchema: z.ZodEnum<{
    HASH_VERIFIED: "HASH_VERIFIED";
    SIGNATURE_VERIFIED: "SIGNATURE_VERIFIED";
    MERKLE_PROOF_VERIFIED: "MERKLE_PROOF_VERIFIED";
    CHAIN_VERIFIED: "CHAIN_VERIFIED";
    BLOCKCHAIN_ANCHORED: "BLOCKCHAIN_ANCHORED";
}>;
/**
 * Merkle proof step schema
 */
export const MerkleProofStepSchema: z.ZodObject<{
    hash: z.ZodString;
    position: z.ZodEnum<{
        left: "left";
        right: "right";
    }>;
}, z.core.$strip>;
/**
 * Blockchain anchor schema
 */
export const BlockchainAnchorSchema: z.ZodOptional<z.ZodObject<{
    network: z.ZodString;
    txHash: z.ZodString;
    blockNumber: z.ZodNumber;
    blockTimestamp: z.ZodNumber;
    contractAddress: z.ZodOptional<z.ZodString>;
}, z.core.$strip>>;
/**
 * Verification payload schema
 */
export const VerificationPayloadSchema: z.ZodObject<{
    result: z.ZodString;
    method: z.ZodString;
    details: z.ZodOptional<z.ZodRecord<z.ZodString, z.ZodAny>>;
    errorMessage: z.ZodOptional<z.ZodString>;
}, z.core.$loose>;
/**
 * Verification receipt schema - extends base with verification-specific fields
 */
export const VerificationReceiptSchema: z.ZodObject<{
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
}, z.core.$strip>;
export default VerificationReceiptSchema;
export type MerkleProofStep = {
    /**
     * - Sibling hash
     */
    hash: string;
    /**
     * - Position
     */
    position: "left" | "right";
};
export type BlockchainAnchor = {
    /**
     * - Blockchain network
     */
    network: string;
    /**
     * - Transaction hash
     */
    txHash: string;
    /**
     * - Block number
     */
    blockNumber: number;
    /**
     * - Block timestamp
     */
    blockTimestamp: number;
    /**
     * - Contract address
     */
    contractAddress?: string;
};
export type VerificationPayload = {
    /**
     * - Verification result
     */
    result: string;
    /**
     * - Verification method
     */
    method: string;
    /**
     * - Additional details
     */
    details?: Record<string, any>;
    /**
     * - Error message if failed
     */
    errorMessage?: string;
};
export type VerificationReceipt = {
    /**
     * - UUID of receipt
     */
    id: string;
    /**
     * - Receipt type discriminator
     */
    receiptType: "verification";
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
     * - Verification event type
     */
    eventType: string;
    /**
     * - Hash that was verified
     */
    verifiedHash: string;
    /**
     * - Merkle root
     */
    merkleRoot?: string;
    /**
     * - Merkle proof path
     */
    proofPath?: MerkleProofStep[];
    /**
     * - Signature validation result
     */
    signatureValid?: boolean;
    /**
     * - Blockchain anchor info
     */
    blockchainAnchor?: BlockchainAnchor;
    /**
     * - Verification payload
     */
    payload: VerificationPayload;
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
