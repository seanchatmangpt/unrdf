/**
 * WorkItem Tests - Ultra-fast
 * Enqueue/poll smoke test only
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { WorkItemExecutor } from '../src/work-item.mjs';

describe('WorkItemExecutor', () => {
  let executor;

  beforeEach(() => {
    executor = new WorkItemExecutor();
  });

  it('should enqueue and poll work item', async () => {
    const workItemId = await executor.enqueueWorkItem('Task 1');
    expect(workItemId).toBeDefined();

    const workItem = await executor.pollWorkItem(workItemId);
    expect(workItem.goal).toBe('Task 1');
  });
});
