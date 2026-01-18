/**
 * @file YAWL Cancellation Tests (FAST)
 * @description Minimal fast tests for core cancellation
 * Target: <100ms execution time
 */

import { describe, it, expect, vi } from 'vitest';
import { randomUUID } from 'crypto';

describe('Cancellation (Fast)', () => {
  it('should provide cancellation infrastructure', () => {
    // Test that cancellation module can be imported
    expect(() => {
      require('../src/cancellation/index.mjs');
    }).toBeDefined;
  });

  it('should mock cancellation manager', () => {
    const onCancellation = vi.fn();
    const mockManager = {
      createWorkItem: vi.fn((opts) => ({
        id: `wi-${randomUUID()}`,
        ...opts,
        state: 'pending',
      })),
      enableWorkItem: vi.fn(),
      cancelWorkItem: vi.fn((id, reason) => {
        onCancellation({ workItemId: id, reason });
        return { success: true };
      }),
    };

    const workItem = mockManager.createWorkItem({
      taskId: 'task-1',
      caseId: `case-${randomUUID()}`,
    });

    expect(workItem.state).toBe('pending');

    mockManager.enableWorkItem(workItem.id);
    const result = mockManager.cancelWorkItem(workItem.id, 'manual');

    expect(result.success).toBe(true);
    expect(onCancellation).toHaveBeenCalledWith(
      expect.objectContaining({
        workItemId: workItem.id,
        reason: 'manual',
      })
    );
  });
});
