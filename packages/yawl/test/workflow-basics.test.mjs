/**
 * @file YAWL Workflow Basics Tests (FAST)
 * @description Minimal fast tests for workflow creation and specification
 * Target: <50ms execution time
 */

import { describe, it, expect } from 'vitest';
import { randomUUID } from 'crypto';
import { createWorkflowAPI } from '../src/index.mjs';

describe('Workflow Basics (Fast)', () => {
  it('should create minimal workflow with single task', async () => {
    const spec = {
      id: `workflow-${randomUUID()}`,
      name: 'Test Workflow',
      tasks: [
        { id: 'task-1', name: 'Task 1', type: 'atomic' },
      ],
      controlFlow: [],
    };

    const workflow = await createWorkflowAPI(spec);

    expect(workflow).toBeDefined();
    expect(workflow.id).toBeDefined();
    expect(workflow.name).toBe('Test Workflow');
  });

  it('should support workflow with multiple tasks', async () => {
    const spec = {
      id: `workflow-${randomUUID()}`,
      name: 'Multi-Task',
      tasks: [
        { id: 'task-1', name: 'Start', type: 'atomic' },
        { id: 'task-2', name: 'End', type: 'atomic' },
      ],
      controlFlow: [
        { id: 'cf-1', type: 'sequence', from: 'task-1', to: 'task-2' },
      ],
    };

    const workflow = await createWorkflowAPI(spec);

    expect(workflow).toBeDefined();
    expect(workflow.id).toBeDefined();
  });
});
