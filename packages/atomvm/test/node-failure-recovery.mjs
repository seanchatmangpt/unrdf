/**
 * @fileoverview Cluster failure recovery tests.
 */
import { describe, it, expect, vi } from 'vitest';
import { runFailureRecoveryTest } from '../src/node-failure-recovery.mjs';

describe('runFailureRecoveryTest', () => {
  it('should successfully kill node and verify recovery', async () => {
    const mockCluster = {
      killNode: vi.fn().mockResolvedValue(true),
      verifyRecovery: vi.fn().mockResolvedValue(true),
    };

    await expect(runFailureRecoveryTest(mockCluster, 1)).resolves.not.toThrow();
    expect(mockCluster.killNode).toHaveBeenCalledWith(1);
    expect(mockCluster.verifyRecovery).toHaveBeenCalled();
  });

  it('should throw error for invalid cluster object', async () => {
    await expect(runFailureRecoveryTest({}, 1)).rejects.toThrow('Invalid cluster object');
  });

  it('should throw error for invalid nodeId', async () => {
    const mockCluster = {
      killNode: vi.fn(),
      verifyRecovery: vi.fn(),
    };
    await expect(runFailureRecoveryTest(mockCluster, 'invalid')).rejects.toThrow('nodeId must be a number');
  });
});
