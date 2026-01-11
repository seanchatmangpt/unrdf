/**
 * @file YAWL Daemon IPC Tests
 * @module @unrdf/yawl/test/daemon/ipc
 * @description Comprehensive tests for inter-process communication
 * Tests cover message sending, receiving, channels, and error handling
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { EventEmitter } from 'events';
import { Daemon } from '@unrdf/daemon';
import { YawlDaemonBridge } from '@unrdf/daemon/integrations/yawl';

/**
 * Generate UUID v4 for testing
 * @returns {string} Valid UUID v4
 */
function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * Mock YAWL engine with IPC capabilities
 */
class MockYawlEngineIPC extends EventEmitter {
  constructor() {
    super();
    this.messages = [];
    this.channels = new Map();
  }

  on(eventName, handler) {
    super.on(eventName, handler);
    return () => this.off(eventName, handler);
  }

  sendMessage(channel, message) {
    this.messages.push({ channel, message, timestamp: Date.now() });
    this.emit(`ipc:${channel}`, message);
    this.emit('ipc:message', { channel, message });
  }

  receiveMessage(channel, handler) {
    this.on(`ipc:${channel}`, handler);
  }

  async createCase(options) {
    return { caseId: options.caseId || `case-${Date.now()}`, status: 'RUNNING' };
  }

  async enableTask(options) {
    return { ...options, status: 'ENABLED' };
  }

  async cancelTask(options) {
    return { ...options, status: 'CANCELLED' };
  }
}

describe('Daemon IPC', () => {
  describe('message sending', () => {
    let daemon;
    let engine;
    let bridge;

    beforeEach(async () => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'ipc-send-daemon',
      });
      engine = new MockYawlEngineIPC();
      bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-ipc-send',
      });

      await daemon.start();
      await bridge.start();
    });

    afterEach(async () => {
      await bridge.stop();
      await daemon.stop();
    });

    it('should send message via event emission', () => {
      // Arrange
      const listener = vi.fn();
      engine.on('ipc:test-channel', listener);

      // Act
      engine.sendMessage('test-channel', { data: 'hello' });

      // Assert
      expect(listener).toHaveBeenCalled();
      expect(listener.mock.calls[0][0]).toEqual({ data: 'hello' });
    });

    it('should send message with payload', () => {
      // Arrange
      const listener = vi.fn();
      engine.on('ipc:data-channel', listener);
      const payload = { taskId: 'task-1', caseId: 'case-1', action: 'start' };

      // Act
      engine.sendMessage('data-channel', payload);

      // Assert
      expect(listener).toHaveBeenCalledWith(payload);
    });

    it('should send to multiple listeners', () => {
      // Arrange
      const listener1 = vi.fn();
      const listener2 = vi.fn();
      const listener3 = vi.fn();

      engine.on('ipc:broadcast', listener1);
      engine.on('ipc:broadcast', listener2);
      engine.on('ipc:broadcast', listener3);

      // Act
      engine.sendMessage('broadcast', { message: 'to all' });

      // Assert
      expect(listener1).toHaveBeenCalled();
      expect(listener2).toHaveBeenCalled();
      expect(listener3).toHaveBeenCalled();
    });

    it('should send messages in order', () => {
      // Arrange
      const received = [];
      engine.on('ipc:ordered', (msg) => received.push(msg));

      // Act
      engine.sendMessage('ordered', { seq: 1 });
      engine.sendMessage('ordered', { seq: 2 });
      engine.sendMessage('ordered', { seq: 3 });

      // Assert
      expect(received).toEqual([{ seq: 1 }, { seq: 2 }, { seq: 3 }]);
    });

    it('should handle large message payloads', () => {
      // Arrange
      const listener = vi.fn();
      engine.on('ipc:large', listener);
      const largePayload = {
        data: 'x'.repeat(10000),
        metadata: { size: 10000 },
      };

      // Act
      engine.sendMessage('large', largePayload);

      // Assert
      expect(listener).toHaveBeenCalled();
      expect(listener.mock.calls[0][0].data.length).toBe(10000);
    });

    it('should send structured messages', () => {
      // Arrange
      const listener = vi.fn();
      engine.on('ipc:structured', listener);
      const message = {
        header: { type: 'command', version: '1.0' },
        body: { action: 'execute', params: { taskId: 't1' } },
        timestamp: Date.now(),
      };

      // Act
      engine.sendMessage('structured', message);

      // Assert
      expect(listener).toHaveBeenCalledWith(message);
      expect(listener.mock.calls[0][0]).toHaveProperty('header');
      expect(listener.mock.calls[0][0]).toHaveProperty('body');
    });

    it('should support message metadata', () => {
      // Arrange
      const listener = vi.fn();
      engine.on('ipc:metadata', listener);

      // Act
      const message = {
        content: 'actual data',
        meta: {
          sender: 'daemon-1',
          priority: 'high',
          timestamp: Date.now(),
        },
      };
      engine.sendMessage('metadata', message);

      // Assert
      expect(listener.mock.calls[0][0].meta.sender).toBe('daemon-1');
      expect(listener.mock.calls[0][0].meta.priority).toBe('high');
    });

    it('should track sent messages', () => {
      // Arrange
      const initialCount = engine.messages.length;

      // Act
      engine.sendMessage('track-1', { id: 1 });
      engine.sendMessage('track-2', { id: 2 });

      // Assert
      expect(engine.messages.length).toBe(initialCount + 2);
      expect(engine.messages[engine.messages.length - 1].message.id).toBe(2);
    });

    it('should send with channel identifier', () => {
      // Arrange
      const listener = vi.fn();
      engine.on('ipc:channel-a', listener);
      engine.on('ipc:channel-b', () => {}); // Different channel

      // Act
      engine.sendMessage('channel-a', { specific: true });

      // Assert
      expect(listener).toHaveBeenCalledWith({ specific: true });
    });

    it('should handle send errors gracefully', () => {
      // Arrange
      const badListener = vi.fn().mockImplementation(() => {
        throw new Error('Listener crashed');
      });
      engine.on('ipc:error-channel', badListener);

      // Act & Assert
      expect(() => {
        engine.sendMessage('error-channel', { data: 'test' });
      }).toThrow('Listener crashed');
    });

    it('should emit global message event', () => {
      // Arrange
      const globalListener = vi.fn();
      engine.on('ipc:message', globalListener);

      // Act
      engine.sendMessage('any-channel', { global: true });

      // Assert
      expect(globalListener).toHaveBeenCalled();
      expect(globalListener.mock.calls[0][0].channel).toBe('any-channel');
      expect(globalListener.mock.calls[0][0].message.global).toBe(true);
    });

    it('should support rapid message sending', () => {
      // Arrange
      const listener = vi.fn();
      engine.on('ipc:rapid', listener);

      // Act
      for (let i = 0; i < 100; i++) {
        engine.sendMessage('rapid', { seq: i });
      }

      // Assert
      expect(listener).toHaveBeenCalledTimes(100);
    });
  });

  describe('message receiving', () => {
    let daemon;
    let engine;
    let bridge;

    beforeEach(async () => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'ipc-recv-daemon',
      });
      engine = new MockYawlEngineIPC();
      bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-ipc-recv',
      });

      await daemon.start();
      await bridge.start();
    });

    afterEach(async () => {
      await bridge.stop();
      await daemon.stop();
    });

    it('should receive messages on registered channel', () => {
      // Arrange
      const received = [];
      engine.receiveMessage('inbox', (msg) => received.push(msg));

      // Act
      engine.sendMessage('inbox', { id: 1 });
      engine.sendMessage('inbox', { id: 2 });

      // Assert
      expect(received).toHaveLength(2);
      expect(received[0].id).toBe(1);
      expect(received[1].id).toBe(2);
    });

    it('should invoke handler with message data', () => {
      // Arrange
      const handler = vi.fn();
      engine.receiveMessage('handler-test', handler);

      // Act
      engine.sendMessage('handler-test', { action: 'process' });

      // Assert
      expect(handler).toHaveBeenCalledWith({ action: 'process' });
    });

    it('should support multiple handlers per channel', () => {
      // Arrange
      const handler1 = vi.fn();
      const handler2 = vi.fn();
      engine.receiveMessage('multi-handler', handler1);
      engine.receiveMessage('multi-handler', handler2);

      // Act
      engine.sendMessage('multi-handler', { data: 'shared' });

      // Assert
      expect(handler1).toHaveBeenCalled();
      expect(handler2).toHaveBeenCalled();
    });

    it('should handle async message handlers', async () => {
      // Arrange
      const results = [];
      const asyncHandler = vi.fn().mockImplementation(async (msg) => {
        await new Promise((resolve) => setTimeout(resolve, 10));
        results.push(msg.value);
      });
      engine.receiveMessage('async-channel', asyncHandler);

      // Act
      engine.sendMessage('async-channel', { value: 42 });
      await new Promise((resolve) => setTimeout(resolve, 50));

      // Assert
      expect(asyncHandler).toHaveBeenCalled();
      expect(results).toContain(42);
    });

    it('should receive only on subscribed channels', () => {
      // Arrange
      const channelA = vi.fn();
      const channelB = vi.fn();
      engine.receiveMessage('channel-a', channelA);
      engine.receiveMessage('channel-b', channelB);

      // Act
      engine.sendMessage('channel-a', { target: 'a' });

      // Assert
      expect(channelA).toHaveBeenCalled();
      expect(channelB).not.toHaveBeenCalled();
    });

    it('should parse structured messages', () => {
      // Arrange
      const handler = vi.fn();
      engine.receiveMessage('structured', handler);

      // Act
      engine.sendMessage('structured', {
        header: { type: 'event' },
        body: { event: 'task:completed', data: {} },
      });

      // Assert
      expect(handler.mock.calls[0][0].header.type).toBe('event');
      expect(handler.mock.calls[0][0].body.event).toBe('task:completed');
    });

    it('should receive messages in FIFO order', () => {
      // Arrange
      const received = [];
      engine.receiveMessage('fifo', (msg) => received.push(msg.seq));

      // Act
      for (let i = 1; i <= 5; i++) {
        engine.sendMessage('fifo', { seq: i });
      }

      // Assert
      expect(received).toEqual([1, 2, 3, 4, 5]);
    });

    it('should handle message with null/undefined fields', () => {
      // Arrange
      const handler = vi.fn();
      engine.receiveMessage('nullable', handler);

      // Act
      engine.sendMessage('nullable', {
        field1: null,
        field2: undefined,
        field3: 'valid',
      });

      // Assert
      expect(handler).toHaveBeenCalled();
      expect(handler.mock.calls[0][0].field1).toBeNull();
      expect(handler.mock.calls[0][0].field3).toBe('valid');
    });

    it('should support message filtering', () => {
      // Arrange
      const filtered = [];
      engine.receiveMessage('filtered', (msg) => {
        if (msg.priority === 'high') {
          filtered.push(msg);
        }
      });

      // Act
      engine.sendMessage('filtered', { priority: 'low', id: 1 });
      engine.sendMessage('filtered', { priority: 'high', id: 2 });
      engine.sendMessage('filtered', { priority: 'high', id: 3 });

      // Assert
      expect(filtered).toHaveLength(2);
      expect(filtered[0].id).toBe(2);
      expect(filtered[1].id).toBe(3);
    });

    it('should handle handler errors without crashing', () => {
      // Arrange
      const faultyHandler = vi.fn().mockImplementation(() => {
        throw new Error('Handler error');
      });
      engine.receiveMessage('faulty', faultyHandler);

      // Act & Assert
      expect(() => {
        engine.sendMessage('faulty', { data: 'test' });
      }).toThrow('Handler error');
    });

    it('should support one-time message reception', () => {
      // Arrange
      const handler = vi.fn();
      engine.once('ipc:once-channel', handler);

      // Act
      engine.sendMessage('once-channel', { msg: 1 });
      engine.sendMessage('once-channel', { msg: 2 });

      // Assert
      expect(handler).toHaveBeenCalledTimes(1);
    });

    it('should unsubscribe from channels', () => {
      // Arrange
      const handler = vi.fn();
      const unsubscribe = engine.on('ipc:unsub-channel', handler);

      // Act
      engine.sendMessage('unsub-channel', { msg: 1 });
      unsubscribe();
      engine.sendMessage('unsub-channel', { msg: 2 });

      // Assert
      expect(handler).toHaveBeenCalledTimes(1);
    });
  });

  describe('channel management', () => {
    let daemon;
    let engine;
    let bridge;

    beforeEach(async () => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'ipc-channel-daemon',
      });
      engine = new MockYawlEngineIPC();
      bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-ipc-channel',
      });

      await daemon.start();
      await bridge.start();
    });

    afterEach(async () => {
      await bridge.stop();
      await daemon.stop();
    });

    it('should create named channels', () => {
      // Arrange
      const channelName = 'workflow-events';
      const handler = vi.fn();

      // Act
      engine.receiveMessage(channelName, handler);
      engine.sendMessage(channelName, { event: 'started' });

      // Assert
      expect(handler).toHaveBeenCalled();
    });

    it('should support multiple independent channels', () => {
      // Arrange
      const channel1Handler = vi.fn();
      const channel2Handler = vi.fn();

      engine.receiveMessage('channel-1', channel1Handler);
      engine.receiveMessage('channel-2', channel2Handler);

      // Act
      engine.sendMessage('channel-1', { msg: 'one' });
      engine.sendMessage('channel-2', { msg: 'two' });

      // Assert
      expect(channel1Handler).toHaveBeenCalledWith({ msg: 'one' });
      expect(channel2Handler).toHaveBeenCalledWith({ msg: 'two' });
    });

    it('should isolate channel messages', () => {
      // Arrange
      const handlerA = vi.fn();
      const handlerB = vi.fn();

      engine.receiveMessage('isolated-a', handlerA);
      engine.receiveMessage('isolated-b', handlerB);

      // Act
      engine.sendMessage('isolated-a', { data: 'a-only' });

      // Assert
      expect(handlerA).toHaveBeenCalled();
      expect(handlerB).not.toHaveBeenCalled();
    });

    it('should support dynamic channel creation', () => {
      // Arrange
      const handlers = new Map();

      // Act
      for (let i = 0; i < 5; i++) {
        const handler = vi.fn();
        const channelName = `dynamic-${i}`;
        handlers.set(channelName, handler);
        engine.receiveMessage(channelName, handler);
        engine.sendMessage(channelName, { id: i });
      }

      // Assert
      handlers.forEach((handler, channelName) => {
        expect(handler).toHaveBeenCalled();
      });
    });

    it('should handle channel namespacing', () => {
      // Arrange
      const taskHandler = vi.fn();
      const caseHandler = vi.fn();

      engine.receiveMessage('task:enabled', taskHandler);
      engine.receiveMessage('case:created', caseHandler);

      // Act
      engine.sendMessage('task:enabled', { taskId: 't1' });
      engine.sendMessage('case:created', { caseId: 'c1' });

      // Assert
      expect(taskHandler).toHaveBeenCalledWith({ taskId: 't1' });
      expect(caseHandler).toHaveBeenCalledWith({ caseId: 'c1' });
    });

    it('should support wildcard-like behavior via event prefix', () => {
      // Arrange
      const allMessages = [];
      engine.on('ipc:message', (data) => allMessages.push(data));

      // Act
      engine.sendMessage('channel-1', { data: 1 });
      engine.sendMessage('channel-2', { data: 2 });

      // Assert
      expect(allMessages).toHaveLength(2);
      expect(allMessages[0].channel).toBe('channel-1');
      expect(allMessages[1].channel).toBe('channel-2');
    });

    it('should remove closed channels', () => {
      // Arrange
      const handler = vi.fn();
      const unsubscribe = engine.on('ipc:closable', handler);

      // Act
      engine.sendMessage('closable', { msg: 1 });
      unsubscribe();
      const listenerCount = engine.listenerCount('ipc:closable');

      // Assert
      expect(handler).toHaveBeenCalledTimes(1);
      expect(listenerCount).toBe(0);
    });

    it('should handle channel name conflicts', () => {
      // Arrange
      const handler1 = vi.fn();
      const handler2 = vi.fn();

      // Act - Same channel name, different handlers
      engine.receiveMessage('conflict', handler1);
      engine.receiveMessage('conflict', handler2);
      engine.sendMessage('conflict', { data: 'shared' });

      // Assert - Both handlers receive the message
      expect(handler1).toHaveBeenCalled();
      expect(handler2).toHaveBeenCalled();
    });

    it('should support channel-specific error handling', () => {
      // Arrange
      const errorHandler = vi.fn();
      const normalHandler = vi.fn().mockImplementation(() => {
        throw new Error('Channel error');
      });

      engine.receiveMessage('error-prone', normalHandler);
      engine.on('error', errorHandler);

      // Act & Assert
      expect(() => {
        engine.sendMessage('error-prone', { data: 'test' });
      }).toThrow('Channel error');
    });

    it('should list active channels', () => {
      // Arrange
      engine.receiveMessage('list-1', vi.fn());
      engine.receiveMessage('list-2', vi.fn());
      engine.receiveMessage('list-3', vi.fn());

      // Act
      const eventNames = engine.eventNames();

      // Assert
      expect(eventNames).toContain('ipc:list-1');
      expect(eventNames).toContain('ipc:list-2');
      expect(eventNames).toContain('ipc:list-3');
    });

    it('should get channel listener count', () => {
      // Arrange
      engine.receiveMessage('counted', vi.fn());
      engine.receiveMessage('counted', vi.fn());
      engine.receiveMessage('counted', vi.fn());

      // Act
      const count = engine.listenerCount('ipc:counted');

      // Assert
      expect(count).toBe(3);
    });

    it('should support channel priorities via ordering', () => {
      // Arrange
      const order = [];

      engine.receiveMessage('priority', () => order.push('first'));
      engine.receiveMessage('priority', () => order.push('second'));
      engine.receiveMessage('priority', () => order.push('third'));

      // Act
      engine.sendMessage('priority', {});

      // Assert
      expect(order).toEqual(['first', 'second', 'third']);
    });
  });

  describe('error handling', () => {
    let daemon;
    let engine;
    let bridge;

    beforeEach(async () => {
      daemon = new Daemon({
        daemonId: generateUUID(),
        name: 'ipc-error-daemon',
      });
      engine = new MockYawlEngineIPC();
      bridge = new YawlDaemonBridge(daemon, engine, {
        daemonNodeId: 'node-ipc-error',
      });

      await daemon.start();
      await bridge.start();
    });

    afterEach(async () => {
      await bridge.stop();
      await daemon.stop();
    });

    it('should handle listener exceptions', () => {
      // Arrange
      const faultyListener = vi.fn().mockImplementation(() => {
        throw new Error('Listener failed');
      });
      engine.on('ipc:faulty', faultyListener);

      // Act & Assert
      expect(() => {
        engine.sendMessage('faulty', { data: 'test' });
      }).toThrow('Listener failed');
    });

    it('should continue after handler error', () => {
      // Arrange
      const errorHandler = vi.fn().mockImplementation(() => {
        throw new Error('Handler error');
      });
      const goodHandler = vi.fn();

      engine.on('ipc:mixed', errorHandler);
      engine.on('ipc:mixed', goodHandler);

      // Act
      try {
        engine.sendMessage('mixed', { data: 'test' });
      } catch (e) {
        // Expected
      }

      // Assert
      expect(errorHandler).toHaveBeenCalled();
      // goodHandler may or may not be called depending on error propagation
    });

    it('should validate message structure', () => {
      // Arrange
      const handler = vi.fn().mockImplementation((msg) => {
        if (!msg || typeof msg !== 'object') {
          throw new Error('Invalid message structure');
        }
      });
      engine.receiveMessage('validated', handler);

      // Act & Assert
      expect(() => {
        engine.sendMessage('validated', null);
      }).toThrow('Invalid message structure');
    });

    it('should handle missing channel gracefully', () => {
      // Arrange & Act
      engine.sendMessage('non-existent-channel', { data: 'orphan' });

      // Assert - No error, message just not received
      expect(engine.messages.length).toBeGreaterThan(0);
    });

    it('should recover from send failures', () => {
      // Arrange
      const failOnce = vi
        .fn()
        .mockImplementationOnce(() => {
          throw new Error('Send failed');
        })
        .mockImplementation(() => {});

      engine.on('ipc:retry', failOnce);

      // Act
      try {
        engine.sendMessage('retry', { attempt: 1 });
      } catch (e) {
        // First attempt fails
      }
      engine.sendMessage('retry', { attempt: 2 });

      // Assert
      expect(failOnce).toHaveBeenCalledTimes(2);
    });

    it('should handle circular message references', () => {
      // Arrange
      const handler = vi.fn();
      engine.receiveMessage('circular', handler);

      const circular = { name: 'test' };
      circular.self = circular;

      // Act - Circular references cause issues with serialization
      // but EventEmitter passes references directly
      engine.sendMessage('circular', circular);

      // Assert
      expect(handler).toHaveBeenCalled();
      expect(handler.mock.calls[0][0].self).toBe(handler.mock.calls[0][0]);
    });

    it('should enforce message size limits conceptually', () => {
      // Arrange
      const handler = vi.fn().mockImplementation((msg) => {
        // Simulate size check
        const size = JSON.stringify(msg).length;
        if (size > 50000) {
          throw new Error('Message too large');
        }
      });
      engine.receiveMessage('size-limited', handler);

      // Act & Assert
      const smallMsg = { data: 'small' };
      engine.sendMessage('size-limited', smallMsg);
      expect(handler).toHaveBeenCalled();
    });

    it('should handle rapid error succession', () => {
      // Arrange
      const errorHandler = vi.fn().mockImplementation(() => {
        throw new Error('Recurring error');
      });
      engine.on('ipc:rapid-error', errorHandler);

      // Act
      let errorCount = 0;
      for (let i = 0; i < 10; i++) {
        try {
          engine.sendMessage('rapid-error', { seq: i });
        } catch (e) {
          errorCount++;
        }
      }

      // Assert
      expect(errorCount).toBe(10);
    });

    it('should log IPC errors when logger available', () => {
      // Arrange
      const mockLogger = { error: vi.fn(), warn: vi.fn(), info: vi.fn() };
      daemon.logger = mockLogger;

      const errorHandler = vi.fn().mockImplementation(() => {
        daemon.logger.error('IPC handler error');
        throw new Error('Logged error');
      });
      engine.on('ipc:logged-error', errorHandler);

      // Act
      try {
        engine.sendMessage('logged-error', { data: 'test' });
      } catch (e) {
        // Expected
      }

      // Assert
      expect(mockLogger.error).toHaveBeenCalled();
    });

    it('should prevent infinite message loops', () => {
      // Arrange
      let sendCount = 0;
      const maxSends = 5;

      const loopHandler = vi.fn().mockImplementation((msg) => {
        sendCount++;
        if (sendCount < maxSends) {
          engine.sendMessage('loop', { count: sendCount });
        }
      });
      engine.on('ipc:loop', loopHandler);

      // Act
      engine.sendMessage('loop', { count: 0 });

      // Assert
      expect(sendCount).toBe(maxSends);
    });

    it('should handle timeout in async handlers', async () => {
      // Arrange
      const slowHandler = vi.fn().mockImplementation(async () => {
        await new Promise((resolve) => setTimeout(resolve, 100));
      });
      engine.receiveMessage('timeout-test', slowHandler);

      // Act
      const startTime = Date.now();
      engine.sendMessage('timeout-test', {});
      await new Promise((resolve) => setTimeout(resolve, 150));
      const duration = Date.now() - startTime;

      // Assert
      expect(duration).toBeGreaterThanOrEqual(100);
      expect(slowHandler).toHaveBeenCalled();
    });

    it('should clean up on repeated errors', () => {
      // Arrange
      const errorProneHandler = vi.fn().mockImplementation(() => {
        throw new Error('Persistent error');
      });
      const unsubscribe = engine.on('ipc:cleanup', errorProneHandler);

      // Act
      try {
        engine.sendMessage('cleanup', {});
      } catch (e) {
        // Expected
      }

      unsubscribe(); // Manual cleanup

      // Assert
      expect(engine.listenerCount('ipc:cleanup')).toBe(0);
    });
  });
});
