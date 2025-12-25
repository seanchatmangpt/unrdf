import { YawlResourcePool } from '../../src/index.mjs';
import { createTestWorkflow, createTestEngine, measureTime } from './test-utils.mjs';

/**
 * YAWL Workflow Patterns Test Suite
 *
 * Comprehensive tests for Van der Aalst's 20 Core Workflow Patterns
 * with KGC-4D time-travel and receipt verification
 *
 * @see https://www.workflowpatterns.com/
 * @author UNRDF Team
 *
 * Test Coverage:
 * - WP1-WP7: Core control flow patterns
 * - Control Flow: Cycles, nested conditionals, deferred choice
 * - Resources: Allocation, prioritization, exhaustion, roles
 * - Cancellation: Single, region, timeout, circuit breaker
 * - Time-Travel: Checkpoints, replay, hash verification
 * - Receipts: Hashes, SPARQL, actor/timestamp, chain verification
 * - Integration: Full lifecycle, error paths, resource contention
 */


describe('Resource Tests', () => {
  let engine;
  let resourcePool;

  beforeEach(() => {
    engine = createTestEngine();
    resourcePool = new YawlResourcePool();
  });

  // ===========================================================================
  // Single Participant Allocation
  // ===========================================================================
  describe('Single Participant Allocation', () => {
    test('Allocates single available resource', async () => {
      // Arrange
      resourcePool.addResource({
        id: 'user-1',
        name: 'Alice',
        roles: ['reviewer'],
        available: true,
      });

      // Act
      const allocation = resourcePool.allocate({
        taskId: 'review-task',
        role: 'reviewer',
      });

      // Assert
      expect(allocation.queued).toBe(false);
      expect(allocation.resource.id).toBe('user-1');
      expect(allocation.resource.available).toBe(false);
    }, 5000);
  });

  // ===========================================================================
  // Multiple Eligible Participants
  // ===========================================================================
  describe('Multiple Eligible Participants', () => {
    test('Picks best priority from multiple eligible resources', async () => {
      // Arrange
      resourcePool.addResource({
        id: 'user-1',
        name: 'Alice',
        roles: ['developer'],
        priority: 5,
        available: true,
      });
      resourcePool.addResource({
        id: 'user-2',
        name: 'Bob',
        roles: ['developer'],
        priority: 10,  // Higher priority
        available: true,
      });
      resourcePool.addResource({
        id: 'user-3',
        name: 'Charlie',
        roles: ['developer'],
        priority: 3,
        available: true,
      });

      // Act
      const allocation = resourcePool.allocate({
        taskId: 'dev-task',
        role: 'developer',
      });

      // Assert: Should pick Bob (highest priority)
      expect(allocation.resource.id).toBe('user-2');
      expect(allocation.resource.priority).toBe(10);
    }, 5000);
  });

  // ===========================================================================
  // Resource Pool Exhaustion
  // ===========================================================================
  describe('Resource Pool Exhaustion', () => {
    test('Queues task when no resources available', async () => {
      // Arrange
      resourcePool.addResource({
        id: 'user-1',
        name: 'Alice',
        roles: ['specialist'],
        available: false,  // Already busy
        currentTaskId: 'existing-task',
      });

      // Act
      const allocation = resourcePool.allocate({
        taskId: 'new-task',
        role: 'specialist',
      });

      // Assert: Task should be queued
      expect(allocation.queued).toBe(true);
      expect(allocation.resource).toBeNull();
      expect(resourcePool.getQueueLength()).toBe(1);
    }, 5000);

    test('Dequeues task when resource becomes available', async () => {
      // Arrange
      const resource = resourcePool.addResource({
        id: 'user-1',
        name: 'Alice',
        roles: ['specialist'],
        available: false,
        currentTaskId: 'existing-task',
      });

      // Queue a task
      resourcePool.allocate({ taskId: 'queued-task', role: 'specialist' });
      expect(resourcePool.getQueueLength()).toBe(1);

      // Act: Release the resource
      const nextTask = resourcePool.release('user-1');

      // Assert: Queued task should be allocated
      expect(nextTask).not.toBeNull();
      expect(nextTask.taskId).toBe('queued-task');
      expect(resourcePool.getQueueLength()).toBe(0);
    }, 5000);
  });

  // ===========================================================================
  // Role-Based Allocation
  // ===========================================================================
  describe('Role-Based Allocation', () => {
    test('Only allocates resources with matching role', async () => {
      // Arrange
      resourcePool.addResource({
        id: 'dev-1',
        roles: ['developer'],
        available: true,
      });
      resourcePool.addResource({
        id: 'mgr-1',
        roles: ['manager'],
        available: true,
      });

      // Act
      const devAllocation = resourcePool.allocate({
        taskId: 'dev-task',
        role: 'developer',
      });
      const mgrAllocation = resourcePool.allocate({
        taskId: 'mgr-task',
        role: 'manager',
      });

      // Assert
      expect(devAllocation.resource.id).toBe('dev-1');
      expect(mgrAllocation.resource.id).toBe('mgr-1');
    }, 5000);
  });
});