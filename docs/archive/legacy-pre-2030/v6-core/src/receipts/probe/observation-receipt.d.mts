/**
 * @typedef {Object} ObservationMetadata
 * @property {string} serializationVersion - Version for reproducibility
 * @property {string} encoding - Encoding format used
 * @property {boolean} [deterministic=true] - Whether deterministically serialized
 */
/**
 * @typedef {Object} ObservationData
 * @property {any} payload - The actual observed data
 * @property {bigint} timestamp - Nanosecond timestamp
 * @property {string} hash - BLAKE3 hash of payload
 * @property {ObservationMetadata} [metadata] - Serialization metadata
 */
/**
 * @typedef {Object} DeterminismCheck
 * @property {'hash-recompute'|'serialization-stable'|'payload-integrity'} checkType
 * @property {string} [checkValue] - Check result value
 * @property {boolean} passed - Whether check passed
 * @property {Record<string, any>} [details] - Check details
 */
/**
 * @typedef {Object} ProbeObservationReceipt
 * @property {string} id - UUID of receipt
 * @property {'probe-observation'} receiptType - Receipt type
 * @property {bigint} t_ns - Nanosecond timestamp
 * @property {string} timestamp_iso - ISO timestamp
 * @property {string|null} previousHash - Previous receipt hash (v6-core chain)
 * @property {string} payloadHash - Payload hash
 * @property {string} receiptHash - Receipt chain hash
 * @property {string} agentId - Agent ID
 * @property {number} observationIndex - Sequence number
 * @property {string} obsHash - Observation hash
 * @property {string|null} prevHash - Previous observation hash (agent chain)
 * @property {string} domain - Domain/category
 * @property {ObservationData} observation - Observation record
 * @property {DeterminismCheck[]} checks - Determinism checks
 * @property {Object} [attestation] - Signature/attestation
 */
/**
 * Compute observation hash from payload
 *
 * Uses deterministic serialization to ensure reproducibility.
 *
 * @param {any} payload - Observation payload
 * @returns {Promise<string>} 64-character BLAKE3 hex hash
 */
export function computeObsHash(payload: any): Promise<string>;
/**
 * Check if observation hash is reproducible
 *
 * Recomputes hash from payload, compares with expected.
 *
 * @param {any} payload - Observation payload
 * @param {string} expectedHash - Expected hash
 * @returns {Promise<DeterminismCheck>}
 */
export function checkHashRecompute(payload: any, expectedHash: string): Promise<DeterminismCheck>;
/**
 * Check if observation serialization is stable
 *
 * Serializes and hashes observation 3 times to ensure consistency.
 *
 * @param {any} payload - Observation payload
 * @param {string} expectedHash - Expected final hash
 * @returns {Promise<DeterminismCheck>}
 */
export function checkSerializationStable(payload: any, expectedHash: string): Promise<DeterminismCheck>;
/**
 * Validate observation payload structure
 *
 * @param {any} payload - Payload to validate
 * @returns {DeterminismCheck}
 */
export function checkPayloadIntegrity(payload: any): DeterminismCheck;
/**
 * Probe observation receipt type discriminator
 * @constant {string}
 */
export const PROBE_OBSERVATION_TYPE: "probe-observation";
/**
 * Serialization version for determinism
 * @constant {string}
 */
export const SERIALIZATION_VERSION: "1.0";
/**
 * Encoding format for determinism
 * @constant {string}
 */
export const ENCODING_FORMAT: "json-deterministic";
/**
 * Observation payload metadata schema
 */
export const ObservationMetadataSchema: z.ZodObject<{
    serializationVersion: z.ZodString;
    encoding: z.ZodString;
    deterministic: z.ZodDefault<z.ZodBoolean>;
}, z.core.$loose>;
/**
 * Core observation data schema
 */
export const ObservationDataSchema: z.ZodObject<{
    payload: z.ZodAny;
    timestamp: z.ZodBigInt;
    hash: z.ZodString;
    metadata: z.ZodOptional<z.ZodObject<{
        serializationVersion: z.ZodString;
        encoding: z.ZodString;
        deterministic: z.ZodDefault<z.ZodBoolean>;
    }, z.core.$loose>>;
}, z.core.$strip>;
/**
 * Determinism check result schema
 */
export const DeterminismCheckSchema: z.ZodObject<{
    checkType: z.ZodEnum<{
        "hash-recompute": "hash-recompute";
        "serialization-stable": "serialization-stable";
        "payload-integrity": "payload-integrity";
    }>;
    checkValue: z.ZodOptional<z.ZodString>;
    passed: z.ZodBoolean;
    details: z.ZodOptional<z.ZodRecord<z.ZodAny, z.core.SomeType>>;
}, z.core.$strip>;
/**
 * Probe observation receipt schema - extends base with probe-specific fields
 */
export const ProbeObservationReceiptSchema: z.ZodObject<{
    id: z.ZodString;
    t_ns: z.ZodBigInt;
    timestamp_iso: z.ZodString;
    previousHash: z.ZodNullable<z.ZodString>;
    payloadHash: z.ZodString;
    receiptHash: z.ZodString;
    vectorClock: z.ZodOptional<z.ZodObject<{
        nodeId: z.ZodString;
        counters: z.ZodRecord<z.ZodString, z.ZodString>;
    }, z.core.$loose>>;
    gitRef: z.ZodOptional<z.ZodString>;
    kgcEventId: z.ZodOptional<z.ZodString>;
    receiptType: z.ZodLiteral<"probe-observation">;
    agentId: z.ZodString;
    observationIndex: z.ZodNumber;
    obsHash: z.ZodString;
    prevHash: z.ZodNullable<z.ZodString>;
    domain: z.ZodString;
    observation: z.ZodObject<{
        payload: z.ZodAny;
        timestamp: z.ZodBigInt;
        hash: z.ZodString;
        metadata: z.ZodOptional<z.ZodObject<{
            serializationVersion: z.ZodString;
            encoding: z.ZodString;
            deterministic: z.ZodDefault<z.ZodBoolean>;
        }, z.core.$loose>>;
    }, z.core.$strip>;
    checks: z.ZodArray<z.ZodObject<{
        checkType: z.ZodEnum<{
            "hash-recompute": "hash-recompute";
            "serialization-stable": "serialization-stable";
            "payload-integrity": "payload-integrity";
        }>;
        checkValue: z.ZodOptional<z.ZodString>;
        passed: z.ZodBoolean;
        details: z.ZodOptional<z.ZodRecord<z.ZodAny, z.core.SomeType>>;
    }, z.core.$strip>>;
    attestation: z.ZodOptional<z.ZodObject<{
        algorithm: z.ZodString;
        publicKey: z.ZodString;
        signature: z.ZodString;
        signer: z.ZodOptional<z.ZodString>;
    }, z.core.$strip>>;
}, z.core.$strip>;
export default ProbeObservationReceiptSchema;
export type ObservationMetadata = {
    /**
     * - Version for reproducibility
     */
    serializationVersion: string;
    /**
     * - Encoding format used
     */
    encoding: string;
    /**
     * - Whether deterministically serialized
     */
    deterministic?: boolean;
};
export type ObservationData = {
    /**
     * - The actual observed data
     */
    payload: any;
    /**
     * - Nanosecond timestamp
     */
    timestamp: bigint;
    /**
     * - BLAKE3 hash of payload
     */
    hash: string;
    /**
     * - Serialization metadata
     */
    metadata?: ObservationMetadata;
};
export type DeterminismCheck = {
    checkType: "hash-recompute" | "serialization-stable" | "payload-integrity";
    /**
     * - Check result value
     */
    checkValue?: string;
    /**
     * - Whether check passed
     */
    passed: boolean;
    /**
     * - Check details
     */
    details?: Record<string, any>;
};
export type ProbeObservationReceipt = {
    /**
     * - UUID of receipt
     */
    id: string;
    /**
     * - Receipt type
     */
    receiptType: "probe-observation";
    /**
     * - Nanosecond timestamp
     */
    t_ns: bigint;
    /**
     * - ISO timestamp
     */
    timestamp_iso: string;
    /**
     * - Previous receipt hash (v6-core chain)
     */
    previousHash: string | null;
    /**
     * - Payload hash
     */
    payloadHash: string;
    /**
     * - Receipt chain hash
     */
    receiptHash: string;
    /**
     * - Agent ID
     */
    agentId: string;
    /**
     * - Sequence number
     */
    observationIndex: number;
    /**
     * - Observation hash
     */
    obsHash: string;
    /**
     * - Previous observation hash (agent chain)
     */
    prevHash: string | null;
    /**
     * - Domain/category
     */
    domain: string;
    /**
     * - Observation record
     */
    observation: ObservationData;
    /**
     * - Determinism checks
     */
    checks: DeterminismCheck[];
    /**
     * - Signature/attestation
     */
    attestation?: any;
};
import { z } from 'zod';
