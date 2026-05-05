/**
 * Read delta from file
 *
 * @param {string} filePath - Path to delta JSON file
 * @returns {Promise<Object>} Parsed delta
 * @throws {Error} If file cannot be read or JSON is invalid
 */
export function readDeltaFromFile(filePath: string): Promise<any>;
/**
 * Create delta store
 *
 * @param {Object} [options] - Store options
 * @returns {DeltaStore}
 */
export function createDeltaStore(options?: any): DeltaStore;
/**
 * Get default delta store instance
 *
 * @returns {DeltaStore}
 */
export function getDefaultStore(): DeltaStore;
/**
 * Reset default store (mainly for testing)
 *
 * @returns {void}
 */
export function resetDefaultStore(): void;
export namespace DeltaStatus {
    let PROPOSED: string;
    let APPLIED: string;
    let REJECTED: string;
}
/**
 * Stored delta schema with metadata
 */
export const StoredDeltaSchema: z.ZodObject<{
    id: z.ZodString;
    from: z.ZodString;
    to: z.ZodString;
    operations: z.ZodArray<z.ZodObject<{
        type: z.ZodEnum<{
            add: "add";
            remove: "remove";
            modify: "modify";
        }>;
        subject: z.ZodString;
        predicate: z.ZodString;
        object: z.ZodAny;
        oldValue: z.ZodOptional<z.ZodAny>;
    }, z.core.$strip>>;
    metadata: z.ZodObject<{
        description: z.ZodOptional<z.ZodString>;
        proposedAt: z.ZodString;
        appliedAt: z.ZodOptional<z.ZodString>;
        status: z.ZodEnum<{
            rejected: "rejected";
            proposed: "proposed";
            applied: "applied";
        }>;
        stateHashBefore: z.ZodOptional<z.ZodString>;
        stateHashAfter: z.ZodOptional<z.ZodString>;
    }, z.core.$loose>;
}, z.core.$strip>;
/**
 * Delta Store - In-memory storage for deltas
 *
 * Provides:
 * - CRUD operations for deltas
 * - Status tracking and updates
 * - State hash management
 * - Query capabilities
 *
 * @class
 * @example
 * const store = new DeltaStore();
 * await store.store(deltaId, delta);
 * const delta = await store.get(deltaId);
 * const allDeltas = await store.list();
 */
export class DeltaStore {
    /**
     * @param {Object} [options] - Store options
     * @param {Object} [options.stateStore] - Optional state store for validation
     */
    constructor(options?: {
        stateStore?: any;
    });
    /** @type {Map<string, Object>} */
    deltas: Map<string, any>;
    stateStore: any;
    /**
     * Store a delta
     *
     * @param {string} deltaId - Delta identifier
     * @param {Object} delta - Delta object
     * @returns {Promise<void>}
     * @throws {Error} If delta is invalid
     */
    store(deltaId: string, delta: any): Promise<void>;
    /**
     * Get a delta by ID
     *
     * @param {string} deltaId - Delta identifier
     * @returns {Promise<Object|null>} Delta or null if not found
     */
    get(deltaId: string): Promise<any | null>;
    /**
     * Check if delta exists
     *
     * @param {string} deltaId - Delta identifier
     * @returns {Promise<boolean>}
     */
    has(deltaId: string): Promise<boolean>;
    /**
     * List all deltas
     *
     * @param {Object} [options] - Query options
     * @param {string} [options.status] - Filter by status
     * @param {string} [options.from] - Filter by source state hash
     * @returns {Promise<Array<Object>>} Array of deltas
     */
    list(options?: {
        status?: string;
        from?: string;
    }): Promise<Array<any>>;
    /**
     * Mark delta as applied
     *
     * Updates delta status and records application metadata.
     *
     * @param {string} deltaId - Delta identifier
     * @param {Object} [metadata] - Additional metadata
     * @param {string} [metadata.appliedAt] - Application timestamp
     * @param {string} [metadata.stateHashAfter] - State hash after application
     * @returns {Promise<Object>} Updated delta
     * @throws {Error} If delta not found
     */
    markApplied(deltaId: string, metadata?: {
        appliedAt?: string;
        stateHashAfter?: string;
    }): Promise<any>;
    /**
     * Mark delta as rejected
     *
     * @param {string} deltaId - Delta identifier
     * @param {string} reason - Rejection reason
     * @returns {Promise<Object>} Updated delta
     * @throws {Error} If delta not found
     */
    markRejected(deltaId: string, reason: string): Promise<any>;
    /**
     * Delete a delta
     *
     * @param {string} deltaId - Delta identifier
     * @returns {Promise<boolean>} True if deleted, false if not found
     */
    delete(deltaId: string): Promise<boolean>;
    /**
     * Clear all deltas
     *
     * @returns {Promise<void>}
     */
    clear(): Promise<void>;
    /**
     * Get store statistics
     *
     * @returns {Promise<Object>} Statistics
     */
    getStats(): Promise<any>;
    /**
     * Get current state hash from state store
     *
     * @returns {Promise<string|null>} Current state hash or null if no state store
     */
    getCurrentStateHash(): Promise<string | null>;
    /**
     * Verify operation against state store
     *
     * @param {Object} operation - Operation to verify
     * @returns {Promise<boolean>} True if operation is valid
     */
    verifyOperation(operation: any): Promise<boolean>;
    /**
     * Apply operations to state store
     *
     * @param {Array<Object>} operations - Operations to apply
     * @returns {Promise<Object>} Application result with new state hash
     */
    applyOperations(operations: Array<any>): Promise<any>;
}
import { z } from 'zod';
