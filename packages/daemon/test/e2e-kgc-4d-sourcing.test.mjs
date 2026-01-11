/**
 * @file KGC-4D Event Sourcing E2E Tests
 * @module @unrdf/daemon/test/e2e-kgc-4d-sourcing
 * @description End-to-end tests for event sourcing with KGC-4D temporal model,
 * universe freeze snapshots, hash chain verification, state reconstruction,
 * temporal queries, and Merkle proof generation/validation.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { DaemonEventStore } from '../src/integrations/kgc-4d-sourcing.mjs';

describe('DaemonEventStore - KGC-4D Event Sourcing', () => {
  let store;

  beforeEach(async () => {
    store = new DaemonEventStore();
    await store.initialize();
  });

  describe('Initialization', () => {
    it('should create event store with no options', async () => {
      // Arrange & Act
      const newStore = new DaemonEventStore();
      await newStore.initialize();

      // Assert
      expect(newStore).toBeDefined();
      expect(newStore.eventLog).toEqual([]);
      expect(newStore.freezeHistory).toEqual([]);
      expect(newStore.previousHash).toBeTruthy();
    });

    it('should accept custom logger', async () => {
      // Arrange
      const mockLogger = { log: () => {}, error: () => {} };

      // Act
      const newStore = new DaemonEventStore({ logger: mockLogger });
      await newStore.initialize();

      // Assert
      expect(newStore.logger).toBe(mockLogger);
    });

    it('should throw on invalid options type', () => {
      // Act & Assert
      expect(() => new DaemonEventStore('invalid')).toThrow('options must be object or undefined');
    });

    it('should initialize with empty event log', async () => {
      // Assert
      expect(store.eventLog.length).toBe(0);
      expect(store.getStats().eventCount).toBe(0);
    });
  });

  describe('Append Event - Event Log Building', () => {
    it('should append single event to log', async () => {
      // Act
      const event = await store.appendEvent('create-task', { taskId: '123' });

      // Assert
      expect(event).toBeDefined();
      expect(event.id).toBeTruthy();
      expect(event.operationId).toBeTruthy();
      expect(event.operationType).toBe('create-task');
      expect(event.status).toBe('enqueued');
      expect(event.payload).toEqual({ taskId: '123' });
    });

    it('should generate unique IDs for each event', async () => {
      // Act
      const event1 = await store.appendEvent('op1', {});
      const event2 = await store.appendEvent('op2', {});

      // Assert
      expect(event1.id).not.toBe(event2.id);
      expect(event1.operationId).not.toBe(event2.operationId);
    });

    it('should maintain hash chain across events', async () => {
      // Act
      const event1 = await store.appendEvent('op1', {});
      const event2 = await store.appendEvent('op2', {});

      // Assert
      expect(event1.previousHash).not.toBe(event1.currentHash);
      expect(event2.previousHash).toBe(event1.currentHash);
      expect(event2.currentHash).not.toBe(event2.previousHash);
    });

    it('should require non-empty operation type', async () => {
      // Act & Assert
      await expect(store.appendEvent('', {})).rejects.toThrow('operationType must be non-empty string');
      await expect(store.appendEvent(null, {})).rejects.toThrow('operationType must be non-empty string');
    });

    it('should accept optional metadata', async () => {
      // Act
      const event = await store.appendEvent('task', {}, { priority: 'high', source: 'user' });

      // Assert
      expect(event.metadata).toEqual({ priority: 'high', source: 'user' });
    });

    it('should use KGC-4D nanosecond timestamps', async () => {
      // Act
      const event = await store.appendEvent('task', {});

      // Assert
      expect(typeof event.timestamp).toBe('bigint');
      expect(event.timestamp > 0n).toBe(true);
    });

    it('should store events in order with monotonic timestamps', async () => {
      // Act
      const event1 = await store.appendEvent('op1', {});
      const event2 = await store.appendEvent('op2', {});
      const event3 = await store.appendEvent('op3', {});

      // Assert
      expect(event1.timestamp <= event2.timestamp).toBe(true);
      expect(event2.timestamp <= event3.timestamp).toBe(true);
      expect(store.eventLog.length).toBe(3);
    });
  });

  describe('Update Event Status - State Transitions', () => {
    it('should transition event from enqueued to started', async () => {
      // Arrange
      const event = await store.appendEvent('task', {});

      // Act
      const updated = await store.updateEventStatus(event.operationId, 'started');

      // Assert
      expect(updated.status).toBe('started');
      expect(updated.operationId).toBe(event.operationId);
    });

    it('should transition event from started to success', async () => {
      // Arrange
      const event = await store.appendEvent('task', {});
      await store.updateEventStatus(event.operationId, 'started');

      // Act
      const success = await store.updateEventStatus(event.operationId, 'success', { result: 'done' });

      // Assert
      expect(success.status).toBe('success');
      expect(success.payload.result).toEqual({ result: 'done' });
    });

    it('should transition event from started to failure', async () => {
      // Arrange
      const event = await store.appendEvent('task', {});
      await store.updateEventStatus(event.operationId, 'started');

      // Act
      const failure = await store.updateEventStatus(event.operationId, 'failure', { error: 'timeout' });

      // Assert
      expect(failure.status).toBe('failure');
      expect(failure.payload.result).toEqual({ error: 'timeout' });
    });

    it('should throw on unknown operation ID', async () => {
      // Act & Assert
      await expect(store.updateEventStatus('unknown-id', 'started')).rejects.toThrow('not found');
    });

    it('should throw on invalid status', async () => {
      // Arrange
      const event = await store.appendEvent('task', {});

      // Act & Assert
      await expect(store.updateEventStatus(event.operationId, 'invalid')).rejects.toThrow();
    });

    it('should maintain hash chain on status update', async () => {
      // Arrange
      const event = await store.appendEvent('task', {});
      const hash1 = store.previousHash;

      // Act
      await store.updateEventStatus(event.operationId, 'started');
      const hash2 = store.previousHash;

      // Assert
      expect(hash1).not.toBe(hash2);
    });
  });

  describe('Universe Freeze - Snapshot Creation', () => {
    it('should create freeze snapshot on empty store', async () => {
      // Act
      const snapshot = await store.freezeUniverse();

      // Assert
      expect(snapshot.freezeId).toBeTruthy();
      expect(snapshot.eventCount).toBe(0);
      expect(snapshot.merkleRoot).toBeTruthy();
      expect(snapshot.previousFreezeId).toBeNull();
    });

    it('should create freeze snapshot with events', async () => {
      // Arrange
      await store.appendEvent('op1', {});
      await store.appendEvent('op2', {});

      // Act
      const snapshot = await store.freezeUniverse();

      // Assert
      expect(snapshot.eventCount).toBe(2);
      expect(snapshot.operations.length).toBe(2);
      expect(snapshot.operations[0].operationType).toBe('op1');
      expect(snapshot.operations[1].operationType).toBe('op2');
    });

    it('should maintain freeze history chain', async () => {
      // Act
      const snap1 = await store.freezeUniverse();
      await store.appendEvent('op1', {});
      const snap2 = await store.freezeUniverse();

      // Assert
      expect(snap2.previousFreezeId).toBe(snap1.freezeId);
      expect(store.freezeHistory.length).toBe(2);
    });

    it('should use nanosecond precision timestamps', async () => {
      // Act
      const snapshot = await store.freezeUniverse();

      // Assert
      expect(typeof snapshot.timestamp).toBe('bigint');
      expect(snapshot.timestamp > 0n).toBe(true);
      expect(snapshot.freezeTimestampISO).toMatch(/^\d{4}-\d{2}-\d{2}T/);
    });

    it('should compute correct Merkle root for events', async () => {
      // Arrange
      await store.appendEvent('op1', {});
      await store.appendEvent('op2', {});
      await store.appendEvent('op3', {});

      // Act
      const snapshot = await store.freezeUniverse();

      // Assert
      expect(snapshot.merkleRoot).toBeTruthy();
      expect(snapshot.merkleRoot.length).toBe(64);
    });

    it('should record operation statuses in snapshot', async () => {
      // Arrange
      const e1 = await store.appendEvent('op1', {});
      await store.appendEvent('op2', {});
      await store.updateEventStatus(e1.operationId, 'success');

      // Act
      const snapshot = await store.freezeUniverse();

      // Assert
      expect(snapshot.operations[0].status).toBe('success');
      expect(snapshot.operations[1].status).toBe('enqueued');
    });
  });

  describe('State Reconstruction - Time-Travel Replay', () => {
    it('should reconstruct state at current time', async () => {
      // Arrange
      const event = await store.appendEvent('task', {});
      const timestamp = event.timestamp;

      // Act
      const state = await store.reconstructState(timestamp);

      // Assert
      expect(state.events.length).toBe(1);
      expect(state.eventCount).toBe(1);
      expect(state.merkleRoot).toBeTruthy();
    });

    it('should reconstruct state at earlier time point', async () => {
      // Arrange
      const e1 = await store.appendEvent('op1', {});
      const t1 = e1.timestamp;
      await store.appendEvent('op2', {});

      // Act
      const state = await store.reconstructState(t1);

      // Assert
      expect(state.eventCount).toBe(1);
      expect(state.events[0].operationId).toBe(e1.operationId);
    });

    it('should return empty state for time before first event', async () => {
      // Arrange
      const event = await store.appendEvent('task', {});
      const beforeTime = event.timestamp - 1000000000n;

      // Act
      const state = await store.reconstructState(beforeTime);

      // Assert
      expect(state.eventCount).toBe(0);
      expect(state.events.length).toBe(0);
    });

    it('should compute correct Merkle root in reconstructed state', async () => {
      // Arrange
      await store.appendEvent('op1', {});
      const e2 = await store.appendEvent('op2', {});
      const snapshot = await store.freezeUniverse();

      // Act
      const state = await store.reconstructState(e2.timestamp);

      // Assert
      expect(state.merkleRoot).toBe(snapshot.merkleRoot);
    });

    it('should throw on invalid timestamp type', async () => {
      // Act & Assert
      await expect(store.reconstructState('invalid')).rejects.toThrow('BigInt');
    });

    it('should return proper ISO timestamp in reconstruction', async () => {
      // Arrange
      const event = await store.appendEvent('task', {});

      // Act
      const state = await store.reconstructState(event.timestamp);

      // Assert
      expect(state.timestampISO).toMatch(/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}/);
    });
  });

  describe('Temporal Queries - Filtering Events', () => {
    it('should query events by timestamp range', async () => {
      // Arrange
      const e1 = await store.appendEvent('op1', {});
      const t1 = e1.timestamp;
      const e2 = await store.appendEvent('op2', {});

      // Act
      const results = await store.queryEvents({
        fromTimestamp: t1,
        toTimestamp: e2.timestamp,
      });

      // Assert
      expect(results.length).toBe(2);
    });

    it('should query events by operation type', async () => {
      // Arrange
      await store.appendEvent('create-task', {});
      await store.appendEvent('delete-task', {});
      await store.appendEvent('create-task', {});

      // Act
      const results = await store.queryEvents({ operationType: 'create-task' });

      // Assert
      expect(results.length).toBe(2);
      expect(results.every((e) => e.operationType === 'create-task')).toBe(true);
    });

    it('should query events by operation ID', async () => {
      // Arrange
      const op1 = await store.appendEvent('op1', {});
      await store.appendEvent('op2', {});

      // Act
      const results = await store.queryEvents({ operationId: op1.operationId });

      // Assert
      expect(results.length).toBe(1);
      expect(results[0].operationId).toBe(op1.operationId);
    });

    it('should query events by status', async () => {
      // Arrange
      const e1 = await store.appendEvent('op1', {});
      await store.appendEvent('op2', {});
      await store.updateEventStatus(e1.operationId, 'success');

      // Act
      const successResults = await store.queryEvents({ status: 'success' });
      const enqueuedResults = await store.queryEvents({ status: 'enqueued' });

      // Assert
      expect(successResults.length).toBe(1);
      expect(enqueuedResults.length).toBe(1);
    });

    it('should combine multiple query filters', async () => {
      // Arrange
      const e1 = await store.appendEvent('create-task', {});
      const t1 = e1.timestamp;
      await store.updateEventStatus(e1.operationId, 'success');
      await store.appendEvent('create-task', {});

      // Act
      const results = await store.queryEvents({
        operationType: 'create-task',
        status: 'success',
        fromTimestamp: t1,
      });

      // Assert
      expect(results.length).toBe(1);
      expect(results[0].operationId).toBe(e1.operationId);
    });

    it('should return results sorted by timestamp', async () => {
      // Arrange
      const e1 = await store.appendEvent('a', {});
      const e2 = await store.appendEvent('b', {});
      const e3 = await store.appendEvent('c', {});

      // Act
      const results = await store.queryEvents({});

      // Assert
      expect(results[0].operationId).toBe(e1.operationId);
      expect(results[1].operationId).toBe(e2.operationId);
      expect(results[2].operationId).toBe(e3.operationId);
    });

    it('should throw on invalid query schema', async () => {
      // Act & Assert
      await expect(store.queryEvents({ fromTimestamp: 'invalid' })).rejects.toThrow();
    });
  });

  describe('Merkle Proof - Audit Trail Verification', () => {
    it('should generate Merkle proof for event in log', async () => {
      // Arrange
      await store.appendEvent('op1', {});
      await store.appendEvent('op2', {});

      // Act
      const proof = await store.generateMerkleProof(0);

      // Assert
      expect(proof.leafIndex).toBe(0);
      expect(proof.leafHash).toBeTruthy();
      expect(proof.merkleRoot).toBeTruthy();
      expect(Array.isArray(proof.proof)).toBe(true);
    });

    it('should generate unique proofs for different events', async () => {
      // Arrange
      await store.appendEvent('op1', {});
      await store.appendEvent('op2', {});

      // Act
      const proof1 = await store.generateMerkleProof(0);
      const proof2 = await store.generateMerkleProof(1);

      // Assert
      expect(proof1.leafHash).not.toBe(proof2.leafHash);
      expect(proof1.merkleRoot).toBe(proof2.merkleRoot);
    });

    it('should throw on invalid event index', async () => {
      // Arrange
      await store.appendEvent('op1', {});

      // Act & Assert
      await expect(store.generateMerkleProof(10)).rejects.toThrow('out of range');
      await expect(store.generateMerkleProof(-1)).rejects.toThrow();
      await expect(store.generateMerkleProof('invalid')).rejects.toThrow();
    });

    it('should verify valid Merkle proof', async () => {
      // Arrange
      await store.appendEvent('op1', {});
      const proof = await store.generateMerkleProof(0);

      // Act
      const isValid = await store.verifyProof(proof);

      // Assert
      expect(isValid).toBe(true);
    });

    it('should detect invalid Merkle proof', async () => {
      // Arrange
      await store.appendEvent('op1', {});
      const proof = await store.generateMerkleProof(0);
      proof.leafHash = 'tampered-hash';

      // Act
      const isValid = await store.verifyProof(proof);

      // Assert
      expect(isValid).toBe(false);
    });

    it('should generate valid proofs for all events in chain', async () => {
      // Arrange
      const n = 5;
      for (let i = 0; i < n; i++) {
        await store.appendEvent(`op${i}`, {});
      }

      // Act & Assert
      for (let i = 0; i < n; i++) {
        const proof = await store.generateMerkleProof(i);
        const isValid = await store.verifyProof(proof);
        expect(isValid).toBe(true);
      }
    });
  });

  describe('Performance - Under 1K Events', () => {
    it('should handle 100 events efficiently', async () => {
      // Act
      for (let i = 0; i < 100; i++) {
        await store.appendEvent(`op${i}`, { index: i });
      }
      const snapshot = await store.freezeUniverse();

      // Assert
      expect(snapshot.eventCount).toBe(100);
      expect(store.eventLog.length).toBe(100);
    });

    it('should perform temporal queries quickly on 100 events', async () => {
      // Arrange
      for (let i = 0; i < 100; i++) {
        await store.appendEvent('task', {});
      }

      // Act
      const results = await store.queryEvents({ operationType: 'task' });

      // Assert
      expect(results.length).toBe(100);
    });

    it('should generate Merkle proofs for 1000 events', async () => {
      // Arrange
      for (let i = 0; i < 1000; i++) {
        await store.appendEvent(`op${i % 10}`, {});
      }

      // Act
      const proof = await store.generateMerkleProof(500);
      const isValid = await store.verifyProof(proof);

      // Assert
      expect(proof).toBeDefined();
      expect(isValid).toBe(true);
    });
  });

  describe('Statistics and Metadata', () => {
    it('should return accurate statistics', async () => {
      // Arrange
      await store.appendEvent('op1', {});
      await store.appendEvent('op2', {});
      await store.freezeUniverse();

      // Act
      const stats = store.getStats();

      // Assert
      expect(stats.eventCount).toBe(2);
      expect(stats.freezeCount).toBe(1);
      expect(stats.currentHash).toBeTruthy();
      expect(typeof stats.oldestEventTimestamp).toBe('bigint');
    });

    it('should return empty statistics for new store', async () => {
      // Act
      const stats = store.getStats();

      // Assert
      expect(stats.eventCount).toBe(0);
      expect(stats.freezeCount).toBe(0);
      expect(stats.oldestEventTimestamp).toBeNull();
    });

    it('should maintain freeze history', async () => {
      // Arrange
      await store.freezeUniverse();
      await store.appendEvent('op1', {});
      await store.freezeUniverse();

      // Act
      const history = store.getFreezeHistory();

      // Assert
      expect(history.length).toBe(2);
      expect(history[0].eventCount).toBe(0);
      expect(history[1].eventCount).toBe(1);
    });
  });

  describe('Hash Chain Integrity - BLAKE3 Verification', () => {
    it('should maintain unbroken hash chain across events', async () => {
      // Arrange
      const e1 = await store.appendEvent('op1', {});
      const e2 = await store.appendEvent('op2', {});
      const e3 = await store.appendEvent('op3', {});

      // Assert
      expect(e1.currentHash).not.toBe(e1.previousHash);
      expect(e2.previousHash).toBe(e1.currentHash);
      expect(e3.previousHash).toBe(e2.currentHash);
    });

    it('should detect hash chain break with different operations', async () => {
      // Arrange
      const e1 = await store.appendEvent('op1', {});
      const hash1 = e1.currentHash;

      // Act
      const e2 = await store.appendEvent('op2', {});

      // Assert
      expect(e2.currentHash).not.toBe(hash1);
    });
  });

  describe('Full Workflow - Integrated Scenario', () => {
    it('should execute complete event sourcing workflow', async () => {
      // Arrange
      const taskEvent = await store.appendEvent('create-task', { name: 'Important' });
      await store.updateEventStatus(taskEvent.operationId, 'started');
      const processEvent = await store.appendEvent('process-task', {});
      await store.updateEventStatus(processEvent.operationId, 'started');

      // Act - Freeze after processing started
      const snapshot1 = await store.freezeUniverse();
      await store.updateEventStatus(taskEvent.operationId, 'success', { duration: '5ms' });
      const snapshot2 = await store.freezeUniverse();

      // Verify state reconstruction at initial task time
      const state = await store.reconstructState(taskEvent.timestamp);
      const proof = await store.generateMerkleProof(0);
      const proofValid = await store.verifyProof(proof);

      // Assert
      expect(snapshot1.eventCount).toBe(2);
      expect(snapshot2.eventCount).toBe(2);
      expect(snapshot2.operations[0].status).toBe('success');
      expect(state.eventCount).toBe(2);
      expect(proofValid).toBe(true);
    });
  });
});
