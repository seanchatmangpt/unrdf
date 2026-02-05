/**
 * @fileoverview Tests for Multi-Swarm Coordination
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  MessageQueue,
  WorkDistributor,
  CoordinationHub,
  MessageType,
  SwarmStatus
} from '../coordination.mjs';

describe('MessageQueue', () => {
  let queue;

  beforeEach(() => {
    queue = new MessageQueue({ maxSize: 10 });
  });

  it('should enqueue and dequeue messages', () => {
    const message = {
      type: MessageType.WORK_REQUEST,
      from: 'queen',
      to: 'worker-1',
      payload: { task: 'test' }
    };

    queue.enqueue(message);
    expect(queue.size).toBe(1);

    const dequeued = queue.dequeue();
    expect(dequeued.type).toBe(MessageType.WORK_REQUEST);
    expect(queue.size).toBe(0);
  });

  it('should handle priority messages', () => {
    const normalMsg = {
      type: MessageType.WORK_REQUEST,
      from: 'queen',
      to: 'worker-1',
      payload: { priority: 0 }
    };

    const highPriorityMsg = {
      type: MessageType.WORK_REQUEST,
      from: 'queen',
      to: 'worker-1',
      payload: { priority: 10 }
    };

    queue.enqueue(normalMsg, 0);
    queue.enqueue(highPriorityMsg, 10);

    // High priority should come first
    const first = queue.dequeue();
    expect(first.payload.priority).toBe(10);

    const second = queue.dequeue();
    expect(second.payload.priority).toBe(0);
  });

  it('should throw error when queue is full', () => {
    for (let i = 0; i < 10; i++) {
      queue.enqueue({
        type: MessageType.WORK_REQUEST,
        from: 'queen',
        to: 'worker-1',
        payload: { i }
      });
    }

    expect(() => {
      queue.enqueue({
        type: MessageType.WORK_REQUEST,
        from: 'queen',
        to: 'worker-1',
        payload: {}
      });
    }).toThrow('Queue full');
  });

  it('should filter messages by swarm', () => {
    queue.enqueue({
      type: MessageType.WORK_REQUEST,
      from: 'queen',
      to: 'swarm-1',
      payload: {}
    });

    queue.enqueue({
      type: MessageType.WORK_REQUEST,
      from: 'queen',
      to: 'swarm-2',
      payload: {}
    });

    const messagesForSwarm1 = queue.getMessagesFor('swarm-1');
    expect(messagesForSwarm1.length).toBe(1);
    expect(messagesForSwarm1[0].to).toBe('swarm-1');
  });

  it('should remove messages for a swarm', () => {
    queue.enqueue({
      type: MessageType.WORK_REQUEST,
      from: 'queen',
      to: 'swarm-1',
      payload: {}
    });

    queue.enqueue({
      type: MessageType.WORK_REQUEST,
      from: 'queen',
      to: 'swarm-2',
      payload: {}
    });

    const removed = queue.removeMessagesFor('swarm-1');
    expect(removed.length).toBe(1);
    expect(queue.size).toBe(1);
  });
});

describe('WorkDistributor', () => {
  let distributor;

  beforeEach(() => {
    distributor = new WorkDistributor({ strategy: 'round-robin' });

    distributor.registerSwarm('swarm-1', {
      domain: 'compression',
      capacity: 10
    });

    distributor.registerSwarm('swarm-2', {
      domain: 'validation',
      capacity: 5
    });

    distributor.registerSwarm('swarm-3', {
      domain: 'compression',
      capacity: 8
    });
  });

  it('should register and unregister swarms', () => {
    expect(distributor.swarms.size).toBe(3);

    distributor.unregisterSwarm('swarm-2');
    expect(distributor.swarms.size).toBe(2);
  });

  it('should select swarm using round-robin', () => {
    const selected1 = distributor.selectSwarm({ type: 'test' });
    const selected2 = distributor.selectSwarm({ type: 'test' });
    const selected3 = distributor.selectSwarm({ type: 'test' });

    // Should cycle through swarms
    expect(selected1).toBe('swarm-1');
    expect(selected2).toBe('swarm-2');
    expect(selected3).toBe('swarm-3');
  });

  it('should filter by domain', () => {
    const selected = distributor.selectSwarm({
      type: 'compress',
      domain: 'validation'
    });

    // Should select from validation swarms only
    expect(selected).toBe('swarm-2');
  });

  it('should use least-loaded strategy', () => {
    distributor.strategy = 'least-loaded';

    // Set different loads
    distributor.updateSwarmStatus('swarm-1', { load: 8 }); // 80% utilization
    distributor.updateSwarmStatus('swarm-2', { load: 1 }); // 20% utilization
    distributor.updateSwarmStatus('swarm-3', { load: 4 }); // 50% utilization

    const selected = distributor.selectSwarm({ type: 'test' });

    // Should select least loaded (swarm-2)
    expect(selected).toBe('swarm-2');
  });

  it('should identify overloaded swarms', () => {
    distributor.updateSwarmStatus('swarm-1', { load: 9 }); // 90% utilization

    const overloaded = distributor.getOverloadedSwarms();
    expect(overloaded.length).toBe(1);
    expect(overloaded[0].id).toBe('swarm-1');
  });

  it('should identify idle swarms', () => {
    distributor.updateSwarmStatus('swarm-1', { status: SwarmStatus.IDLE });
    distributor.updateSwarmStatus('swarm-2', { status: SwarmStatus.WORKING });

    const idle = distributor.getIdleSwarms();
    expect(idle.length).toBeGreaterThanOrEqual(1);
  });

  it('should provide statistics', () => {
    const stats = distributor.getStats();

    expect(stats.totalSwarms).toBe(3);
    expect(stats.totalCapacity).toBe(23); // 10 + 5 + 8
  });
});

describe('CoordinationHub', () => {
  let hub;

  beforeEach(() => {
    hub = new CoordinationHub();
  });

  it('should register swarms', () => {
    hub.registerSwarm('swarm-1', { domain: 'compression', capacity: 10 });

    const stats = hub.getStats();
    expect(stats.totalSwarms).toBe(1);
  });

  it('should send and receive messages', () => {
    hub.registerSwarm('swarm-1', { domain: 'test' });

    hub.sendMessage({
      type: MessageType.WORK_REQUEST,
      from: 'queen',
      to: 'swarm-1',
      payload: { task: 'test' }
    });

    const messages = hub.receiveMessages('swarm-1');
    expect(messages.length).toBe(1);
    expect(messages[0].type).toBe(MessageType.WORK_REQUEST);
  });

  it('should distribute work', () => {
    hub.registerSwarm('swarm-1', { domain: 'compression', capacity: 10 });

    const workId = hub.distributeWork({
      type: 'compress',
      domain: 'compression',
      payload: { data: [] }
    });

    expect(workId).toBeDefined();
    expect(typeof workId).toBe('string');
    expect(hub.activeWork.size).toBe(1);

    // Verify the work was assigned to the correct swarm
    const workItem = hub.activeWork.get(workId);
    expect(workItem.swarmId).toBe('swarm-1');
  });

  it('should handle work completion', () => {
    hub.registerSwarm('swarm-1', { domain: 'test', capacity: 10 });

    const swarmId = hub.distributeWork({
      type: 'test',
      payload: {}
    });

    const workId = Array.from(hub.activeWork.keys())[0];

    hub.submitResult(workId, { success: true });

    expect(hub.activeWork.size).toBe(0);
    expect(hub.results.size).toBe(1);
  });

  it('should handle work failure', () => {
    hub.registerSwarm('swarm-1', { domain: 'test', capacity: 10 });

    const swarmId = hub.distributeWork({
      type: 'test',
      payload: {}
    });

    const workId = Array.from(hub.activeWork.keys())[0];

    hub.reportFailure(workId, new Error('Test failure'));

    expect(hub.activeWork.size).toBe(0);
    expect(hub.results.size).toBe(1);

    const result = hub.results.get(workId);
    expect(result.status).toBe('failed');
  });

  it('should support work stealing', () => {
    hub.registerSwarm('swarm-1', { domain: 'test', capacity: 10 });
    hub.registerSwarm('swarm-2', { domain: 'test', capacity: 10 });

    // Overload swarm-1
    hub.updateSwarmStatus('swarm-1', {
      status: SwarmStatus.OVERLOADED,
      load: 9,
      capacity: 10
    });

    // swarm-2 is idle
    hub.updateSwarmStatus('swarm-2', {
      status: SwarmStatus.IDLE,
      load: 0,
      capacity: 10
    });

    // Add work to swarm-1
    hub.distributeWork({
      type: 'test',
      payload: {}
    });

    // swarm-2 steals work
    const stolen = hub.requestWorkSteal('swarm-2');

    expect(stolen).toBeTruthy();
    expect(stolen.swarmId).toBe('swarm-2');
  });

  it('should emit events', () => {
    let eventFired = false;

    hub.on('swarm:registered', () => {
      eventFired = true;
    });

    hub.registerSwarm('swarm-1', { domain: 'test' });

    expect(eventFired).toBe(true);
  });
});
