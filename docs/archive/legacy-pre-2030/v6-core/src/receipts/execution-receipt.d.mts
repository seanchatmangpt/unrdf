/**
 * Execution event types
 */
export type EXECUTION_EVENT_TYPES = string;
/**
 * Execution event types
 * @readonly
 * @enum {string}
 */
export const EXECUTION_EVENT_TYPES: Readonly<{
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
/**
 * Event type schema
 */
export const ExecutionEventTypeSchema: z.ZodEnum<{
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
/**
 * Justification schema - explains why the decision was made
 */
export const JustificationSchema: z.ZodObject<{
    hookValidated: z.ZodOptional<z.ZodString>;
    sparqlQuery: z.ZodOptional<z.ZodString>;
    reasoning: z.ZodOptional<z.ZodString>;
    conditionChecked: z.ZodOptional<z.ZodString>;
    approvedBy: z.ZodOptional<z.ZodString>;
}, z.core.$strip>;
/**
 * Execution payload schema
 */
export const ExecutionPayloadSchema: z.ZodObject<{
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
/**
 * Execution receipt schema - extends base with workflow-specific fields
 */
export const ExecutionReceiptSchema: z.ZodObject<{
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
}, z.core.$strip>;
export default ExecutionReceiptSchema;
export type Justification = {
    /**
     * - Hook that validated the transition
     */
    hookValidated?: string;
    /**
     * - SPARQL query used for evaluation
     */
    sparqlQuery?: string;
    /**
     * - Human-readable reasoning
     */
    reasoning?: string;
    /**
     * - Condition that was checked
     */
    conditionChecked?: string;
    /**
     * - Actor who approved
     */
    approvedBy?: string;
};
export type ExecutionPayload = {
    /**
     * - The decision made
     */
    decision: string;
    /**
     * - Justification for decision
     */
    justification?: Justification;
    /**
     * - Actor who made decision
     */
    actor?: string;
    /**
     * - Additional context
     */
    context?: any;
};
export type ExecutionReceipt = {
    /**
     * - UUID of receipt
     */
    id: string;
    /**
     * - Receipt type discriminator
     */
    receiptType: "execution";
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
     * - Execution event type
     */
    eventType: string;
    /**
     * - Workflow case ID
     */
    caseId: string;
    /**
     * - Task ID
     */
    taskId: string;
    /**
     * - Work item ID
     */
    workItemId?: string;
    /**
     * - Decision payload
     */
    payload: ExecutionPayload;
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
