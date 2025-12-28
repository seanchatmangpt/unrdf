/**
 * @file YAWL Workflow Serialization - JSON serialization utilities
 * @module @unrdf/yawl/workflow/serialization
 *
 * @description
 * JSON serialization for Workflow instances:
 * - workflowToJSON: Serialize workflow to JSON representation
 *
 * Note: Function references (conditions) are not serialized.
 * Note: This module does NOT import Workflow to avoid circular dependencies.
 *
 * @example
 * import { workflowToJSON } from '@unrdf/yawl/workflow/serialization';
import { WorkflowError } from '../../errors.mjs';
 *
 * const json = workflowToJSON(workflow);
 */

/**
 * Serialize workflow to JSON
 * @param {Object} workflow - Workflow instance
 * @returns {Object} JSON representation
 *
 * @example
 * const json = workflowToJSON(workflow);
 * console.log(JSON.stringify(json, null, 2));
 */
export function workflowToJSON(workflow) {
  return {
    id: workflow.id,
    name: workflow.name,
    version: workflow.version,
    description: workflow.description,
    author: workflow.author,
    startTaskId: workflow._startTaskId,
    endTaskIds: workflow._endTaskIds,
    tasks: workflow.getTasks().map((task) => ({
      ...task,
      // Remove function references
      condition: undefined,
    })),
    flows: workflow.getFlows().map((flow) => ({
      ...flow,
      // Remove function references
      condition: undefined,
    })),
    cancellationRegions: Object.fromEntries(workflow._regionToTasks),
    locked: workflow._locked,
    createdAt: workflow.createdAt?.toISOString(),
    modifiedAt: workflow.modifiedAt?.toISOString(),
  };
}

// =============================================================================
// Module Exports
// =============================================================================

export default {
  workflowToJSON,
};
