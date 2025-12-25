/**
 * @file YAWL Workflow API - Validation, Schemas, and Utilities
 * @module @unrdf/yawl/api/workflow-api-validation
 *
 * @description
 * Provides validation schemas, constants, and utility functions for YAWL workflow API.
 * All schemas use Zod for runtime validation.
 * Utility functions are pure (deterministic, no side effects).
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';

// ============================================================================
// YAWL Namespace Constants
// ============================================================================

/**
 * YAWL ontology namespace URIs
 * @constant
 */
export const YAWL_NS = {
  BASE: 'http://yawl.io/',
  WORKFLOW: 'http://yawl.io/workflow/',
  TASK: 'http://yawl.io/task/',
  CASE: 'http://yawl.io/case/',
  WORK_ITEM: 'http://yawl.io/workitem/',
  RESOURCE: 'http://yawl.io/resource/',
  EVENT: 'http://yawl.io/event/',
};

/**
 * YAWL event types for KGC-4D logging
 * @constant
 */
export const YAWL_EVENT_TYPES = {
  WORKFLOW_CREATED: 'YAWL_WORKFLOW_CREATED',
  CASE_CREATED: 'YAWL_CASE_CREATED',
  TASK_ENABLED: 'YAWL_TASK_ENABLED',
  TASK_STARTED: 'YAWL_TASK_STARTED',
  TASK_COMPLETED: 'YAWL_TASK_COMPLETED',
  WORK_ITEM_CANCELLED: 'YAWL_WORK_ITEM_CANCELLED',
  CASE_REPLAYED: 'YAWL_CASE_REPLAYED',
};

/**
 * Work item status enumeration
 * @constant
 */
export const WORK_ITEM_STATUS = {
  PENDING: 'pending',
  ENABLED: 'enabled',
  ACTIVE: 'active',
  COMPLETED: 'completed',
  CANCELLED: 'cancelled',
  SUSPENDED: 'suspended',
};

/**
 * Control flow pattern types
 * @constant
 */
export const CONTROL_FLOW_PATTERNS = {
  SEQUENCE: 'sequence',
  AND_SPLIT: 'and-split',
  AND_JOIN: 'and-join',
  XOR_SPLIT: 'xor-split',
  XOR_JOIN: 'xor-join',
  OR_SPLIT: 'or-split',
  OR_JOIN: 'or-join',
  DEFERRED_CHOICE: 'deferred-choice',
  CANCELLATION_REGION: 'cancellation-region',
};

// ============================================================================
// Zod Schemas
// ============================================================================

/**
 * Schema for task definition
 */
export const TaskSchema = z.object({
  id: z.string().min(1).max(255),
  name: z.string().min(1).max(255),
  type: z.enum(['atomic', 'composite', 'multiple-instance']).default('atomic'),
  description: z.string().max(2000).optional(),
  inputVariables: z.array(z.string()).optional(),
  outputVariables: z.array(z.string()).optional(),
  preConditions: z.array(z.string()).optional(),
  postConditions: z.array(z.string()).optional(),
  timeout: z.number().positive().optional(),
  priority: z.number().int().min(0).max(100).default(50),
  resourcePattern: z.string().optional(),
  cancellationRegion: z.string().optional(),
});

/**
 * Schema for control flow definition
 */
export const ControlFlowSchema = z.object({
  id: z.string().min(1).max(255),
  type: z.enum([
    'sequence',
    'and-split',
    'and-join',
    'xor-split',
    'xor-join',
    'or-split',
    'or-join',
    'deferred-choice',
    'cancellation-region',
  ]),
  from: z.string().min(1),
  to: z.union([z.string(), z.array(z.string())]),
  condition: z.string().optional(),
  weight: z.number().min(0).max(1).optional(),
});

/**
 * Schema for resource definition
 */
export const ResourceSchema = z.object({
  id: z.string().min(1).max(255),
  name: z.string().min(1).max(255),
  type: z.enum(['role', 'participant', 'position', 'capability', 'org-group']),
  qualifications: z.array(z.string()).optional(),
  capabilities: z.array(z.string()).optional(),
  constraints: z.record(z.string(), z.any()).optional(),
});

/**
 * Schema for workflow specification
 */
export const WorkflowSpecSchema = z.object({
  id: z.string().min(1).max(255),
  name: z.string().min(1).max(255).optional(),
  version: z.string().regex(/^\d+\.\d+\.\d+$/).optional(),
  description: z.string().max(5000).optional(),
  tasks: z.array(TaskSchema).min(1),
  controlFlow: z.array(ControlFlowSchema).optional(),
  resources: z.array(ResourceSchema).optional(),
  inputVariables: z.array(z.string()).optional(),
  outputVariables: z.array(z.string()).optional(),
  cancellationRegions: z.record(z.string(), z.array(z.string())).optional(),
});

/**
 * Schema for workflow creation options
 */
export const WorkflowOptionsSchema = z.object({
  store: z.any().optional(),
  gitBackbone: z.any().optional(),
  hookRegistry: z.any().optional(),
  policyPacks: z.array(z.any()).optional(),
  validateSpec: z.boolean().default(true),
  createRDF: z.boolean().default(true),
}).optional();

/**
 * Schema for case creation options
 */
export const CaseOptionsSchema = z.object({
  caseId: z.string().optional(),
  initialVariables: z.record(z.string(), z.any()).optional(),
  priority: z.number().int().min(0).max(100).optional(),
  deadline: z.string().datetime().optional(),
  parent: z.string().optional(),
}).optional();

/**
 * Schema for work item
 */
export const WorkItemSchema = z.object({
  id: z.string().min(1),
  caseId: z.string().min(1),
  taskId: z.string().min(1),
  status: z.enum(['pending', 'enabled', 'active', 'completed', 'cancelled', 'suspended']),
  assignedResource: z.string().optional(),
  startTime: z.string().optional(),
  endTime: z.string().optional(),
  variables: z.record(z.string(), z.any()).optional(),
  result: z.any().optional(),
});

/**
 * Schema for enable task options
 */
export const EnableTaskOptionsSchema = z.object({
  assignTo: z.string().optional(),
  priority: z.number().int().min(0).max(100).optional(),
  deadline: z.string().datetime().optional(),
  policyPack: z.any().optional(),
}).optional();

/**
 * Schema for receipt
 */
export const ReceiptSchema = z.object({
  id: z.string().min(1),
  type: z.string().min(1),
  timestamp: z.string(),
  t_ns: z.string(),
  hash: z.string().min(1),
  payload: z.record(z.string(), z.any()),
  justification: z.object({
    policyPackId: z.string().optional(),
    hookResults: z.array(z.any()).optional(),
    conditionsMet: z.array(z.string()).optional(),
    resourceEligibility: z.boolean().optional(),
  }).optional(),
});

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Generate a unique ID using crypto.randomUUID or fallback
 * @returns {string} Unique identifier
 */
export function generateId() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  // Node.js fallback
  try {
    const cryptoModule = require('crypto');
    return cryptoModule.randomUUID();
  } catch {
    return `${Date.now()}-${Math.random().toString(36).slice(2, 11)}`;
  }
}

/**
 * Get current time in nanoseconds as BigInt
 * @returns {bigint} Nanosecond timestamp
 */
export function now() {
  if (typeof process !== 'undefined' && process.hrtime?.bigint) {
    return process.hrtime.bigint();
  }
  return BigInt(Math.floor(performance.now() * 1_000_000));
}

/**
 * Convert nanosecond BigInt to ISO string
 * @param {bigint} t_ns - Nanosecond timestamp
 * @returns {string} ISO 8601 string
 */
export function toISO(t_ns) {
  const ms = Number(t_ns / 1_000_000n);
  return new Date(ms).toISOString();
}

/**
 * Create cryptographic hash for receipt
 * @param {Object} payload - Data to hash
 * @returns {Promise<string>} BLAKE3 hash
 */
export async function createHash(payload) {
  const data = JSON.stringify(payload, (_, value) =>
    typeof value === 'bigint' ? value.toString() : value
  );
  return blake3(data);
}

/**
 * Create a receipt with cryptographic justification
 * @param {string} type - Receipt type
 * @param {Object} payload - Receipt payload
 * @param {Object} [justification] - Optional justification
 * @returns {Promise<Object>} Receipt object
 */
export async function createReceipt(type, payload, justification) {
  const t_ns = now();
  const id = generateId();

  const receiptData = {
    id,
    type,
    timestamp: toISO(t_ns),
    t_ns: t_ns.toString(),
    payload,
    justification,
  };

  const hash = await createHash(receiptData);

  return ReceiptSchema.parse({
    ...receiptData,
    hash,
  });
}
