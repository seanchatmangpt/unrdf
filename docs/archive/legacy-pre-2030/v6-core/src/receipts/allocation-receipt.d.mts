/**
 * Allocation event types
 */
export type ALLOCATION_EVENT_TYPES = string;
/**
 * Allocation event types
 * @readonly
 * @enum {string}
 */
export const ALLOCATION_EVENT_TYPES: Readonly<{
    RESOURCE_ALLOCATED: "RESOURCE_ALLOCATED";
    RESOURCE_RELEASED: "RESOURCE_RELEASED";
    POOL_CREATED: "POOL_CREATED";
    POOL_UPDATED: "POOL_UPDATED";
    CAPACITY_CHANGED: "CAPACITY_CHANGED";
    CALENDAR_UPDATED: "CALENDAR_UPDATED";
}>;
/**
 * Event type schema
 */
export const AllocationEventTypeSchema: z.ZodEnum<{
    RESOURCE_ALLOCATED: "RESOURCE_ALLOCATED";
    RESOURCE_RELEASED: "RESOURCE_RELEASED";
    POOL_CREATED: "POOL_CREATED";
    POOL_UPDATED: "POOL_UPDATED";
    CAPACITY_CHANGED: "CAPACITY_CHANGED";
    CALENDAR_UPDATED: "CALENDAR_UPDATED";
}>;
/**
 * Time period schema
 */
export const AllocationPeriodSchema: z.ZodObject<{
    start: z.ZodString;
    end: z.ZodString;
    durationMs: z.ZodOptional<z.ZodNumber>;
}, z.core.$strip>;
/**
 * Capacity schema
 */
export const CapacitySchema: z.ZodObject<{
    total: z.ZodNumber;
    available: z.ZodNumber;
    allocated: z.ZodNumber;
    unit: z.ZodString;
}, z.core.$strip>;
/**
 * Allocation payload schema
 */
export const AllocationPayloadSchema: z.ZodObject<{
    action: z.ZodString;
    amount: z.ZodOptional<z.ZodNumber>;
    resourceAttributes: z.ZodOptional<z.ZodRecord<z.ZodString, z.ZodAny>>;
    priority: z.ZodOptional<z.ZodNumber>;
    reason: z.ZodOptional<z.ZodString>;
}, z.core.$loose>;
/**
 * Allocation receipt schema - extends base with resource-specific fields
 */
export const AllocationReceiptSchema: z.ZodObject<{
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
}, z.core.$strip>;
export default AllocationReceiptSchema;
export type AllocationPeriod = {
    /**
     * - Start timestamp (ISO 8601)
     */
    start: string;
    /**
     * - End timestamp (ISO 8601)
     */
    end: string;
    /**
     * - Duration in milliseconds
     */
    durationMs?: number;
};
export type Capacity = {
    /**
     * - Total capacity
     */
    total: number;
    /**
     * - Available capacity
     */
    available: number;
    /**
     * - Allocated capacity
     */
    allocated: number;
    /**
     * - Unit of capacity
     */
    unit: string;
};
export type AllocationPayload = {
    /**
     * - Allocation action
     */
    action: string;
    /**
     * - Amount allocated/released
     */
    amount?: number;
    /**
     * - Resource attributes
     */
    resourceAttributes?: Record<string, any>;
    /**
     * - Allocation priority
     */
    priority?: number;
    /**
     * - Allocation reason
     */
    reason?: string;
};
export type AllocationReceipt = {
    /**
     * - UUID of receipt
     */
    id: string;
    /**
     * - Receipt type discriminator
     */
    receiptType: "allocation";
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
     * - Allocation event type
     */
    eventType: string;
    /**
     * - Resource ID
     */
    resourceId: string;
    /**
     * - Resource pool ID
     */
    poolId: string;
    /**
     * - Allocation period
     */
    allocationPeriod: AllocationPeriod;
    /**
     * - Capacity state
     */
    capacity: Capacity;
    /**
     * - Allocated to entity/task
     */
    allocatedTo?: string;
    /**
     * - Allocation payload
     */
    payload: AllocationPayload;
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
