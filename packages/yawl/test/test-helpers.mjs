import { randomUUID } from 'node:crypto';

/**
 * Create a test workflow with all required fields
 * @param {object} overrides - Override default values
 * @returns {object} Complete workflow spec
 */
export function createTestWorkflow(overrides = {}) {
  return {
    id: overrides.id || `workflow-${randomUUID()}`,
    name: overrides.name || 'Test Workflow',
    version: overrides.version || '1.0.0',
    tasks: overrides.tasks || [],  // ✅ Required by WorkflowSpecSchema
    controlFlow: overrides.controlFlow || [],
    resources: overrides.resources || [],
    ...overrides
  };
}

/**
 * Create a test task
 * @param {object} overrides - Override default values
 * @returns {object} Task object
 */
export function createTestTask(overrides = {}) {
  return {
    id: overrides.id || `task-${randomUUID()}`,
    name: overrides.name || 'Test Task',
    type: overrides.type || 'atomic',
    ...overrides
  };
}

/**
 * Create a test case
 * @param {object} overrides - Override default values
 * @returns {object} Case object
 */
export function createTestCase(overrides = {}) {
  return {
    id: overrides.id || `case-${randomUUID()}`,
    workflowId: overrides.workflowId || `workflow-${randomUUID()}`,
    status: overrides.status || 'active',
    workItems: overrides.workItems || [],
    ...overrides
  };
}
