import { describe, test, expect, beforeEach, afterEach, vi } from 'vitest';
import { SSEClient } from '../../src/core/patterns/sse-client.mjs';

// Mock EventSource
class MockEventSource {
  constructor(url) {
    this.url = url;
    this.listeners = {};
    this.closed = false;
  }

  addEventListener(eventType, callback) {
    if (!this.listeners[eventType]) {
      this.listeners[eventType] = [];
    }
    this.listeners[eventType].push(callback);
  }

  emit(eventType, data) {
    if (this.listeners[eventType]) {
      this.listeners[eventType].forEach((cb) => cb(data));
    }
  }

  close() {
    this.closed = true;
  }
}

// Replace global EventSource with mock
global.EventSource = MockEventSource;

describe('SSEClient - Pattern Tests', () => {
  let client;

  beforeEach(() => {
    client = new SSEClient('http://localhost/api/events');
    vi.useFakeTimers();
  });

  afterEach(() => {
    client.disconnect();
    vi.restoreAllMocks();
  });

  test('creates client with default options', () => {
    expect(client.url).toBe('http://localhost/api/events');
    expect(client.isConnected).toBe(false);
    expect(client.reconnectAttempts).toBe(0);
  });

  test('registers and emits event listeners', async () => {
    const handler = vi.fn();
    client.on('test', handler);

    client._emit('test', { data: 'hello' });

    expect(handler).toHaveBeenCalledWith({ data: 'hello' });
  });

  test('connects to SSE stream', async () => {
    const connectedHandler = vi.fn();
    client.on('connected', connectedHandler);

    client.connect();

    expect(client.isConnected).toBe(true);
    expect(connectedHandler).toHaveBeenCalled();
  });

  test('emits connecting event before connection', () => {
    const connectingHandler = vi.fn();
    client.on('connecting', connectingHandler);

    client.connect();

    expect(connectingHandler).toHaveBeenCalled();
  });

  test('disconnects from SSE stream', () => {
    const disconnectedHandler = vi.fn();
    client.on('disconnected', disconnectedHandler);

    client.connect();
    expect(client.isConnected).toBe(true);

    client.disconnect();

    expect(client.isConnected).toBe(false);
    expect(disconnectedHandler).toHaveBeenCalled();
  });

  test('registers and unregisters event listeners', () => {
    const handler1 = vi.fn();
    const handler2 = vi.fn();

    client.on('event', handler1);
    client.on('event', handler2);

    client._emit('event', {});
    expect(handler1).toHaveBeenCalled();
    expect(handler2).toHaveBeenCalled();

    handler1.mockReset();
    handler2.mockReset();

    client.off('event', handler1);
    client._emit('event', {});

    expect(handler1).not.toHaveBeenCalled();
    expect(handler2).toHaveBeenCalled();
  });

  test('handles heartbeat to reset timeout', () => {
    vi.useFakeTimers();
    const heartbeatHandler = vi.fn();
    client.on('heartbeat', heartbeatHandler);

    client.connect();

    // Simulate heartbeat event
    const mockEventSource = client.eventSource;
    mockEventSource.emit('heartbeat', { data: JSON.stringify({ type: 'HEARTBEAT' }) });

    expect(heartbeatHandler).toHaveBeenCalled();

    vi.restoreAllMocks();
  });

  test('detects heartbeat timeout', () => {
    vi.useFakeTimers();
    const timeoutHandler = vi.fn();
    client.on('heartbeat-timeout', timeoutHandler);

    client.connect();

    // Advance time past heartbeat timeout
    vi.advanceTimersByTime(35000);

    expect(timeoutHandler).toHaveBeenCalled();
    expect(client.isConnected).toBe(false);

    vi.restoreAllMocks();
  });

  test('handles message events', () => {
    const messageHandler = vi.fn();
    client.on('message', messageHandler);

    client.connect();

    const mockEventSource = client.eventSource;
    mockEventSource.emit('message', { data: JSON.stringify({ type: 'TEST', value: 42 }) });

    expect(messageHandler).toHaveBeenCalledWith({ type: 'TEST', value: 42 });
  });

  test('handles shard events', () => {
    const shardHandler = vi.fn();
    client.on('shard', shardHandler);

    client.connect();

    const mockEventSource = client.eventSource;
    mockEventSource.emit('shard', {
      data: JSON.stringify({ id: 'shard-1', quads: [] }),
    });

    expect(shardHandler).toHaveBeenCalledWith({ id: 'shard-1', quads: [] });
  });

  test('handles delta events', () => {
    const deltaHandler = vi.fn();
    client.on('delta', deltaHandler);

    client.connect();

    const mockEventSource = client.eventSource;
    mockEventSource.emit('delta', {
      data: JSON.stringify({ delta: { id: 'delta-1', operations: [] } }),
    });

    expect(deltaHandler).toHaveBeenCalledWith({
      delta: { id: 'delta-1', operations: [] },
    });
  });

  test('emits error on parse failure', () => {
    const errorHandler = vi.fn();
    client.on('error', errorHandler);

    client.connect();

    const mockEventSource = client.eventSource;
    mockEventSource.emit('message', { data: 'invalid json' });

    expect(errorHandler).toHaveBeenCalled();
    expect(errorHandler.mock.calls[0][0].message).toContain('Failed to parse');
  });

  test('schedules reconnection on error', () => {
    vi.useFakeTimers();
    const reconnectingHandler = vi.fn();
    client.on('reconnecting', reconnectingHandler);

    client.connect();
    const mockEventSource = client.eventSource;

    // Trigger error
    mockEventSource.emit('error', {});

    expect(client.isConnected).toBe(false);

    // Advance time to trigger reconnect
    vi.advanceTimersByTime(5000);

    expect(reconnectingHandler).toHaveBeenCalled();

    vi.restoreAllMocks();
  });

  test('tracks reconnect attempts', () => {
    const reconnectingHandler = vi.fn();
    client.on('reconnecting', reconnectingHandler);

    client.connect();
    expect(client.reconnectAttempts).toBe(0);

    // Trigger reconnection manually
    client._scheduleReconnect();

    expect(client.reconnectAttempts).toBe(1);
    expect(reconnectingHandler).toHaveBeenCalledWith({
      attempt: 1,
      delay: 5000,
    });
  });

  test('respects max reconnect attempts', () => {
    const maxAttemptsHandler = vi.fn();
    const reconnectClient = new SSEClient('http://localhost/api/events', { maxReconnectAttempts: 2 });
    reconnectClient.on('max-reconnect-attempts', maxAttemptsHandler);

    reconnectClient.connect();

    // Force reconnection attempts up to max
    reconnectClient._scheduleReconnect(); // Attempt 1
    reconnectClient._scheduleReconnect(); // Attempt 2
    reconnectClient._scheduleReconnect(); // Attempt 3 (should trigger max)

    expect(maxAttemptsHandler).toHaveBeenCalledWith(2);
    reconnectClient.disconnect();
  });

  test('returns current status', () => {
    client.connect();

    const status = client.getStatus();

    expect(status.isConnected).toBe(true);
    expect(status.reconnectAttempts).toBe(0);
    expect(status.url).toBe('http://localhost/api/events');
  });

  test('multiple event listeners work independently', () => {
    const handler1 = vi.fn();
    const handler2 = vi.fn();

    client.on('shard', handler1);
    client.on('shard', handler2);

    client.connect();

    const mockEventSource = client.eventSource;
    mockEventSource.emit('shard', { data: JSON.stringify({ id: 'test' }) });

    expect(handler1).toHaveBeenCalled();
    expect(handler2).toHaveBeenCalled();
  });

  test('handles listener callback errors gracefully', () => {
    const errorHandler = vi.fn();
    const badHandler = vi.fn(() => {
      throw new Error('Handler error');
    });

    client.on('message', badHandler);
    client.on('message', errorHandler); // Add another listener after bad one

    client.connect();

    const mockEventSource = client.eventSource;
    mockEventSource.emit('message', { data: JSON.stringify({ type: 'TEST' }) });

    // Bad handler throws but other listeners still execute
    expect(badHandler).toHaveBeenCalled();
    expect(errorHandler).toHaveBeenCalled();
  });
});
