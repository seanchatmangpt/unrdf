/**
 * @file YAWL Task Activation Tests (FAST)
 * @description Minimal fast tests for task operations
 * Target: <50ms execution time
 */

import { describe, it, expect } from 'vitest';
import { randomUUID } from 'crypto';
import { createWorkflowAPI } from '../src/index.mjs';

describe('Task Activation (Fast)', () => {
  it('should support atomic task definitions', async () => {
    const workflow = await createWorkflowAPI({
      id: `workflow-${randomUUID()}`,
      name: 'Task Test',
      tasks: [
        { id: 'task-1', name: 'Atomic Task', type: 'atomic' },
      ],
      controlFlow: [],
    });

    expect(workflow).toBeDefined();
    expect(workflow.id).toBeDefined();
  });

  it('should support multiple task types', async () => {
    const workflow = await createWorkflowAPI({
      id: `workflow-${randomUUID()}`,
      name: 'Mixed Tasks',
      tasks: [
        { id: 'atomic-1', name: 'Atomic', type: 'atomic' },
        { id: 'composite-1', name: 'Composite', type: 'composite' },
        { id: 'multi-1', name: 'Multiple Instance', type: 'multiple-instance' },
      ],
      controlFlow: [
        { id: 'cf-1', type: 'sequence', from: 'atomic-1', to: 'composite-1' },
        { id: 'cf-2', type: 'sequence', from: 'composite-1', to: 'multi-1' },
      ],
    });

    expect(workflow).toBeDefined();
    expect(workflow.id).toBeDefined();
  });
});
