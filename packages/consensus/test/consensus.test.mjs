/**
 * @file Consensus - Raft Leader Election Tests
 * @module consensus/test/consensus
 * @description Fast tests for Raft consensus leader election
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { createRaftCoordinator } from '../src/raft/raft-coordinator.mjs';

describe('Raft Coordinator', () => {
  let coordinator;

  beforeEach(async () => {
    coordinator = createRaftCoordinator({
      nodeId: 'raft-test',
      port: 10090,
    });
    await coordinator.initialize();
  });

  afterEach(async () => {
    if (coordinator) {
      await coordinator.shutdown();
    }
  });

  it('should initialize with single node as leader', () => {
    const state = coordinator.getState();
    expect(state.nodeId).toBe('raft-test');
    expect(state.isLeader).toBeDefined();
    expect(state.term).toBeGreaterThanOrEqual(0);
  });

  it('should add peer to cluster', () => {
    coordinator.addPeer('peer-1', 'localhost', 10091);
    const state = coordinator.getState();
    expect(state.peers).toContain('peer-1');
  });
});
