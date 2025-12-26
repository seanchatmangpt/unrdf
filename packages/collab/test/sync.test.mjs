/**
 * @fileoverview Tests for WebSocket sync (mock-based)
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { CollaborativeRDFGraph } from '../src/crdt/rdf-crdt.mjs';
import { WebSocketSync } from '../src/sync/websocket-sync.mjs';

// Mock y-websocket
vi.mock('y-websocket', () => {
  return {
    WebsocketProvider: class MockWebsocketProvider {
      constructor(url, roomName, doc, options) {
        this.url = url;
        this.roomName = roomName;
        this.doc = doc;
        this.options = options;
        this.wsconnected = false;
        this.wsconnecting = false;
        this.synced = false;
        this.awareness = {
          clientID: Math.floor(Math.random() * 1000000),
          states: new Map(),
          setLocalState: (state) => {
            this.awareness.states.set(this.awareness.clientID, state);
          },
          getStates: () => this.awareness.states,
          on: vi.fn(),
        };
        this.eventHandlers = new Map();
      }

      on(event, handler) {
        if (!this.eventHandlers.has(event)) {
          this.eventHandlers.set(event, []);
        }
        this.eventHandlers.get(event).push(handler);
      }

      connect() {
        this.wsconnecting = true;
        setTimeout(() => {
          this.wsconnecting = false;
          this.wsconnected = true;
          this.synced = true;
          this._emit('status', { status: 'connected' });
          this._emit('sync', true);
        }, 10);
      }

      disconnect() {
        this.wsconnected = false;
        this._emit('status', { status: 'disconnected' });
      }

      destroy() {
        this.disconnect();
        this.eventHandlers.clear();
      }

      _emit(event, data) {
        const handlers = this.eventHandlers.get(event);
        if (handlers) {
          handlers.forEach((h) => h(data));
        }
      }
    },
  };
});

describe('WebSocketSync', () => {
  let graph;
  let sync;

  beforeEach(() => {
    graph = new CollaborativeRDFGraph();
  });

  describe('Initialization', () => {
    it('should create sync provider with valid options', () => {
      sync = new WebSocketSync(graph, {
        url: 'ws://localhost:1234',
        roomName: 'test-room',
      });

      expect(sync).toBeDefined();
      expect(sync.options.url).toBe('ws://localhost:1234');
      expect(sync.options.roomName).toBe('test-room');
    });

    it('should reject invalid URL', () => {
      expect(() => {
        new WebSocketSync(graph, {
          url: 'not-a-url',
          roomName: 'test-room',
        });
      }).toThrow();
    });

    it('should reject empty room name', () => {
      expect(() => {
        new WebSocketSync(graph, {
          url: 'ws://localhost:1234',
          roomName: '',
        });
      }).toThrow();
    });
  });

  describe('Connection Management', () => {
    beforeEach(() => {
      sync = new WebSocketSync(graph, {
        url: 'ws://localhost:1234',
        roomName: 'test-room',
        connect: false,
      });
    });

    it('should connect and report status', async () => {
      let connected = false;

      sync.on('status', (event) => {
        if (event.status === 'connected') {
          connected = true;
        }
      });

      sync.connect();

      await new Promise((resolve) => setTimeout(resolve, 50));
      expect(connected).toBe(true);
    });

    it('should track sync state', async () => {
      let synced = false;

      sync.on('synced', (event) => {
        synced = event.isSynced;
      });

      sync.connect();

      await new Promise((resolve) => setTimeout(resolve, 50));
      expect(synced).toBe(true);
    });

    it('should disconnect cleanly', async () => {
      sync.connect();
      await new Promise((resolve) => setTimeout(resolve, 50));

      let disconnected = false;
      sync.on('status', (event) => {
        if (event.status === 'disconnected') {
          disconnected = true;
        }
      });

      sync.disconnect();
      expect(disconnected).toBe(true);
    });
  });

  describe('Awareness (Presence)', () => {
    beforeEach(() => {
      sync = new WebSocketSync(graph, {
        url: 'ws://localhost:1234',
        roomName: 'test-room',
        awareness: {
          user: { name: 'Alice', color: '#ff0000' },
        },
      });
    });

    it('should set local awareness state', () => {
      sync.setAwareness({
        user: { name: 'Bob', color: '#00ff00' },
      });

      const awareness = sync.getClientAwareness(sync.provider.awareness.clientID);
      expect(awareness.user.name).toBe('Bob');
    });

    it('should get all awareness states', () => {
      const states = sync.getAllAwareness();
      expect(states).toBeInstanceOf(Array);
      expect(states[0].user.name).toBe('Alice');
    });
  });

  describe('Event System', () => {
    beforeEach(() => {
      sync = new WebSocketSync(graph, {
        url: 'ws://localhost:1234',
        roomName: 'test-room',
        connect: false,
      });
    });

    it('should subscribe to events', () => {
      const handler = vi.fn();
      const unsubscribe = sync.on('status', handler);

      sync.connect();

      expect(typeof unsubscribe).toBe('function');
    });

    it('should unsubscribe from events', async () => {
      const handler = vi.fn();
      const unsubscribe = sync.on('status', handler);

      unsubscribe();
      sync.connect();

      await new Promise((resolve) => setTimeout(resolve, 50));
      expect(handler).not.toHaveBeenCalled();
    });
  });
});
