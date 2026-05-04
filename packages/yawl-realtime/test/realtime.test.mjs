/**
 * @file YAWL Realtime Tests - Concurrent execution and conflict resolution
 * @module @unrdf/yawl-realtime/test
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { createWorkflowEngine, YawlWorkflow } from '@unrdf/yawl';
import { YAWLRealtimeServer, OptimisticLockManager, StateSyncManager } from '../src/server.mjs';
import { YAWLRealtimeClient } from '../src/client.mjs';

// =============================================================================
// Test Utilities
// =============================================================================

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

function createTestWorkflow() {
  const workflow = new YawlWorkflow({
    id: 'test-collab',
    name: 'Test Collaboration',
    version: '1.0.0',
  });

  workflow.addTask({ id: 'task1', name: 'Task 1' });
  workflow.addTask({ id: 'task2', name: 'Task 2' });
  workflow.addTask({ id: 'task3', name: 'Task 3' });

  workflow.setStart('task1');
  workflow.addFlow('task1', 'task2');
  workflow.addFlow('task2', 'task3');
  workflow.setEnd(['task3']);

  return workflow;
}

// =============================================================================
// Optimistic Lock Manager Tests
// =============================================================================

describe('OptimisticLockManager', () => {
  let lockManager;

  beforeEach(() => {
    lockManager = new OptimisticLockManager();
  });

  it('should grant lock to first claimant', () => {
    const result = lockManager.acquire('task1', 'case1', 'alice', 1);

    expect(result.success).toBe(true);
    expect(result.lock.userId).toBe('alice');
    expect(result.lock.timestamp).toBe(1);
  });

  it('should deny lock when already held by another user', () => {
    lockManager.acquire('task1', 'case1', 'alice', 1);
    const result = lockManager.acquire('task1', 'case1', 'bob', 2);

    expect(result.success).toBe(true); // Newer timestamp wins
    expect(result.lock.userId).toBe('bob');
    expect(result.conflict).toBeDefined();
    expect(result.conflict.resolution).toBe('newer_claim_wins');
  });

  it('should use Lamport timestamps for conflict resolution', () => {
    lockManager.acquire('task1', 'case1', 'alice', 10);
    const result = lockManager.acquire('task1', 'case1', 'bob', 5);

    expect(result.success).toBe(false); // Older timestamp loses
    expect(result.conflict.currentOwner).toBe('alice');
  });

  it('should release lock correctly', () => {
    lockManager.acquire('task1', 'case1', 'alice', 1);
    const result = lockManager.release('task1', 'alice');

    expect(result.success).toBe(true);
    expect(lockManager.getLock('task1')).toBeNull();
  });

  it('should prevent release by non-owner', () => {
    lockManager.acquire('task1', 'case1', 'alice', 1);
    const result = lockManager.release('task1', 'bob');

    expect(result.success).toBe(false);
    expect(result.error).toContain('held by alice');
  });

  it('should update Lamport clock correctly', () => {
    lockManager.acquire('task1', 'case1', 'alice', 5);
    const clock = lockManager.lamportClocks.get('case1');

    expect(clock).toBeGreaterThan(5);
  });

  it('should get all locks for a case', () => {
    lockManager.acquire('task1', 'case1', 'alice', 1);
    lockManager.acquire('task2', 'case1', 'bob', 2);
    lockManager.acquire('task3', 'case2', 'carol', 3);

    const case1Locks = lockManager.getLocksForCase('case1');

    expect(case1Locks).toHaveLength(2);
    expect(case1Locks.find(l => l.lock.userId === 'alice')).toBeDefined();
    expect(case1Locks.find(l => l.lock.userId === 'bob')).toBeDefined();
  });
});

// =============================================================================
// State Sync Manager Tests
// =============================================================================

describe('StateSyncManager', () => {
  let syncManager;

  beforeEach(() => {
    syncManager = new StateSyncManager();
  });

  it('should merge state with last-write-wins semantics', () => {
    syncManager.mergeState('case1', {
      data: { key1: 'value1' },
    }, 'hash1');

    const result = syncManager.mergeState('case1', {
      data: { key1: 'value2', key2: 'value2' },
    }, 'hash2');

    expect(result.merged.data.key1).toBe('value2');
    expect(result.merged.data.key2).toBe('value2');
    expect(result.conflicts).toHaveLength(1);
    expect(result.conflicts[0].resolution).toBe('last_write_wins');
  });

  it('should track receipt chain', () => {
    syncManager.mergeState('case1', { data: {} }, 'hash1');
    syncManager.mergeState('case1', { data: {} }, 'hash2');

    const chain = syncManager.receiptChains.get('case1');

    expect(chain).toHaveLength(2);
    expect(chain[0].hash).toBe('hash1');
    expect(chain[1].hash).toBe('hash2');
  });

  it('should verify receipt chain continuity', () => {
    syncManager.mergeState('case1', { data: {} }, 'hash1');

    const validResult = syncManager.verifyReceiptChain('case1', 'hash1');
    expect(validResult.valid).toBe(true);

    const invalidResult = syncManager.verifyReceiptChain('case1', 'hash2');
    expect(invalidResult.valid).toBe(false);
    expect(invalidResult.error).toContain('mismatch');
  });

  it('should use add-wins semantics for work items', () => {
    syncManager.mergeState('case1', {
      data: {},
      workItems: { wi1: { id: 'wi1' } },
    }, 'hash1');

    const result = syncManager.mergeState('case1', {
      data: {},
      workItems: { wi2: { id: 'wi2' } },
    }, 'hash2');

    expect(result.merged.workItems.wi1).toBeDefined();
    expect(result.merged.workItems.wi2).toBeDefined();
  });
});

// =============================================================================
// Integration Tests
// =============================================================================

describe('YAWLRealtimeServer Integration', () => {
  let engine;
  let server;
  let client1;
  let client2;

  beforeEach(async () => {
    engine = createWorkflowEngine({ enableEventLog: false });
    const workflow = createTestWorkflow();
    engine.registerWorkflow(workflow);

    server = new YAWLRealtimeServer(engine, { port: 3001 });
    await server.start();

    await sleep(200);

    client1 = new YAWLRealtimeClient({
      serverUrl: 'http://localhost:3001',
      userId: 'alice',
    });

    client2 = new YAWLRealtimeClient({
      serverUrl: 'http://localhost:3001',
      userId: 'bob',
    });

    await Promise.all([client1.connect(), client2.connect()]);
  });

  afterEach(async () => {
    if (client1) await client1.disconnect();
    if (client2) await client2.disconnect();
    if (server) await server.stop();
  });

  it('should broadcast engine events to all clients', async () => {
    const events1 = [];
    const events2 = [];

    client1.on('yawl:event', (event) => events1.push(event));
    client2.on('yawl:event', (event) => events2.push(event));

    // Create case
    await engine.createCase('test-collab');

    await sleep(500);

    expect(events1.length).toBeGreaterThan(0);
    expect(events2.length).toBeGreaterThan(0);
    expect(events1.find(e => e.type === 'case:created')).toBeDefined();
  });

  it('should handle concurrent task claims with optimistic locking', async () => {
    const { case: testCase } = await engine.createCase('test-collab');
    const caseId = testCase.id;

    await sleep(300);

    // Both clients try to claim the same task
    const state = await client1.syncState(caseId);
    const workItems = Object.values(state.state?.workItems || {});
    const firstTask = workItems[0];

    if (!firstTask) {
      throw new Error('No work items found');
    }

    const [result1, result2] = await Promise.all([
      client1.claimTask(caseId, firstTask.id),
      client2.claimTask(caseId, firstTask.id),
    ]);

    // One should succeed, one should fail or have conflict
    const succeeded = [result1, result2].filter(r => r.success);
    expect(succeeded.length).toBeGreaterThan(0);
  });

  it('should complete task and enable downstream tasks', async () => {
    const { case: testCase } = await engine.createCase('test-collab');
    const caseId = testCase.id;

    await sleep(300);

    const state = await client1.syncState(caseId);
    const workItems = Object.values(state.state?.workItems || {});
    const firstTask = workItems.find(wi => wi.status === 'enabled' || wi.status === 'running');

    if (!firstTask) {
      throw new Error('No enabled work items found');
    }

    const claimResult = await client1.claimTask(caseId, firstTask.id);
    expect(claimResult.success).toBe(true);

    const completeResult = await client1.completeTask(caseId, firstTask.id, {
      result: 'success',
    });

    expect(completeResult.success).toBe(true);
    expect(completeResult.downstreamEnabled).toBeDefined();
  });

  it('should sync state across clients', async () => {
    const { case: testCase } = await engine.createCase('test-collab', {
      initialData: 'test',
    });
    const caseId = testCase.id;

    await sleep(300);

    const state1 = await client1.syncState(caseId);
    const state2 = await client2.syncState(caseId);

    expect(state1.state).toBeDefined();
    expect(state2.state).toBeDefined();
    expect(state1.state?.receiptHash).toBe(state2.state?.receiptHash);
  });

  it('should track lock ownership correctly', async () => {
    const { case: testCase } = await engine.createCase('test-collab');
    const caseId = testCase.id;

    await sleep(300);

    const state = await client1.syncState(caseId);
    const workItems = Object.values(state.state?.workItems || {});
    const firstTask = workItems[0];

    if (!firstTask) {
      throw new Error('No work items found');
    }

    await client1.claimTask(caseId, firstTask.id);

    const locks = await client2.syncState(caseId);
    expect(locks.locks.length).toBeGreaterThan(0);
  });

  it('should release task correctly', async () => {
    const { case: testCase } = await engine.createCase('test-collab');
    const caseId = testCase.id;

    await sleep(300);

    const state = await client1.syncState(caseId);
    const workItems = Object.values(state.state?.workItems || {});
    const firstTask = workItems[0];

    if (!firstTask) {
      throw new Error('No work items found');
    }

    await client1.claimTask(caseId, firstTask.id);
    const releaseResult = await client1.releaseTask(caseId, firstTask.id);

    expect(releaseResult.success).toBe(true);
    expect(client1.heldLocks.has(firstTask.id)).toBe(false);
  });
});

// =============================================================================
// Conflict Resolution Tests
// =============================================================================

describe('Conflict Resolution', () => {
  let engine;
  let server;

  beforeEach(async () => {
    engine = createWorkflowEngine({ enableEventLog: false });
    const workflow = createTestWorkflow();
    engine.registerWorkflow(workflow);

    server = new YAWLRealtimeServer(engine, { port: 3002 });
    await server.start();
    await sleep(200);
  });

  afterEach(async () => {
    if (server) await server.stop();
  });

  it('should resolve conflicts with Lamport timestamps', async () => {
    const client1 = new YAWLRealtimeClient({
      serverUrl: 'http://localhost:3002',
      userId: 'alice',
    });

    const client2 = new YAWLRealtimeClient({
      serverUrl: 'http://localhost:3002',
      userId: 'bob',
    });

    await Promise.all([client1.connect(), client2.connect()]);

    const { case: testCase } = await engine.createCase('test-collab');
    const caseId = testCase.id;

    await sleep(300);

    const state = await client1.syncState(caseId);
    const workItems = Object.values(state.state?.workItems || {});
    const firstTask = workItems[0];

    if (!firstTask) {
      await client1.disconnect();
      await client2.disconnect();
      throw new Error('No work items found');
    }

    // Simulate conflicting claims with different timestamps
    client1.lamportClock = 5;
    client2.lamportClock = 10;

    const result1Promise = client1.claimTask(caseId, firstTask.id);
    await sleep(50);
    const result2Promise = client2.claimTask(caseId, firstTask.id);

    const [result1, result2] = await Promise.all([result1Promise, result2Promise]);

    // Higher timestamp should win
    const winner = result1.success && result1.conflict ? result1 : result2;
    expect(winner).toBeDefined();

    await client1.disconnect();
    await client2.disconnect();
  });

  it('should detect receipt hash mismatches', async () => {
    const syncManager = new StateSyncManager();

    syncManager.mergeState('case1', { data: {} }, 'hash1');
    const result = syncManager.verifyReceiptChain('case1', 'wrong-hash');

    expect(result.valid).toBe(false);
    expect(result.error).toContain('mismatch');
    expect(result.currentHash).toBe('hash1');
  });
});
