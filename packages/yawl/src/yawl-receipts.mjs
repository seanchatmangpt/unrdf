/**
 * YAWL Workflow Execution with L5 Receipt Support
 *
 * Wraps workflow execution with receipts
 *
 * @module @unrdf/yawl/yawl-receipts
 */

import { z } from 'zod';
import {
  withReceipt,
  createContext,
} from '../../v6-core/src/receipt-pattern.mjs';

/**
 * Workflow Schema
 */
export const WorkflowSchema = z.object({
  id: z.string(),
  name: z.string(),
  tasks: z.array(
    z.object({
      id: z.string(),
      name: z.string(),
      type: z.string(),
    })
  ),
  caseId: z.string().optional(),
});

/**
 * Workflow Result Schema
 */
export const WorkflowResultSchema = z.object({
  caseId: z.string(),
  status: z.enum(['completed', 'failed', 'running']),
  tasksExecuted: z.number().int().nonnegative(),
  output: z.any(),
});

/**
 * Pure function: Execute workflow
 *
 * @param {Object} workflow - Workflow specification
 * @returns {Object} Workflow result
 */
async function executeWorkflowImpl(workflow) {
  const validated = WorkflowSchema.parse(workflow);

  // Simulate workflow execution (deterministic)
  return {
    caseId: validated.caseId || `case-${validated.id}`,
    status: 'completed',
    tasksExecuted: validated.tasks.length,
    output: { result: 'success' },
  };
}

/**
 * Wrapped: Execute workflow with receipt
 */
export const executeWorkflow = withReceipt(executeWorkflowImpl, {
  operation: 'executeWorkflow',
  profile: 'workflow',
  inputSchema: z.tuple([WorkflowSchema]),
  outputSchema: WorkflowResultSchema,
});

/**
 * L5 Determinism Test
 */
export async function testWorkflowDeterminism(context, iterations = 100) {
  const workflow = {
    id: 'wf-001',
    name: 'Test Workflow',
    tasks: [
      { id: 'task-1', name: 'Task 1', type: 'automated' },
      { id: 'task-2', name: 'Task 2', type: 'automated' },
    ],
    caseId: 'case-test-001',
  };

  const receipts = [];
  const hashes = new Set();

  for (let i = 0; i < iterations; i++) {
    const { receipt } = await executeWorkflow(context, workflow);
    receipts.push(receipt);
    hashes.add(receipt.receiptHash);
  }

  return {
    iterations,
    uniqueHashes: hashes.size,
    deterministic: hashes.size === 1,
    expectedHash: receipts[0].receiptHash,
  };
}
