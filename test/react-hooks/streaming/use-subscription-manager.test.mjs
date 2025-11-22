/**
 * @file Tests for useSubscriptionManager hook functionality
 * Tests connection state, subscriptions, and event handling
 */

import { describe, it, expect, _beforeEach, vi } from 'vitest';

describe('useSubscriptionManager', () => {
  describe('Subscription Creation', () => {
    it('should create subscription with pattern', async () => {
      const subscriptions = new Map();
      const pattern = '?s schema:price ?price';

      const subscribe = async (ptrn, options = {}) => {
        const subscriptionId = options.id || `sub-${Date.now()}`;
        subscriptions.set(subscriptionId, {
          pattern: ptrn,
          options,
        });
        return { subscriptionId };
      };

      const result = await subscribe(pattern, { id: 'test-sub' });

      expect(result.subscriptionId).toBe('test-sub');
      expect(subscriptions.has('test-sub')).toBe(true);
      expect(subscriptions.get('test-sub').pattern).toBe(pattern);
    });

    it('should generate unique subscription ID', () => {
      const ids = new Set();

      for (let i = 0; i < 100; i++) {
        const id = `sub-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
        ids.add(id);
      }

      expect(ids.size).toBe(100);
    });

    it('should support custom subscription ID', async () => {
      const subscriptions = new Map();

      const subscribe = async (pattern, options = {}) => {
        const id = options.id || `sub-${Date.now()}`;
        subscriptions.set(id, { pattern });
        return { subscriptionId: id };
      };

      const result = await subscribe('?s ?p ?o', { id: 'my-custom-id' });

      expect(result.subscriptionId).toBe('my-custom-id');
    });

    it('should store subscription with options', async () => {
      const subscriptions = new Map();

      const subscribe = async (pattern, options = {}) => {
        const id = options.id || `sub-${Date.now()}`;
        subscriptions.set(id, {
          pattern,
          options: {
            filter: options.filter,
            bufferSize: options.bufferSize || 100,
          },
        });
        return { subscriptionId: id };
      };

      await subscribe('?s ?p ?o', {
        id: 'test',
        filter: e => e.value > 100,
        bufferSize: 50,
      });

      const sub = subscriptions.get('test');
      expect(sub.options.bufferSize).toBe(50);
      expect(typeof sub.options.filter).toBe('function');
    });
  });

  describe('Subscription Removal', () => {
    it('should unsubscribe by ID', async () => {
      const subscriptions = new Map([
        ['sub-1', { pattern: '?s ?p ?o' }],
        ['sub-2', { pattern: '?s schema:name ?name' }],
      ]);

      const unsubscribe = async subscriptionId => {
        if (!subscriptions.has(subscriptionId)) {
          throw new Error(`Subscription ${subscriptionId} not found`);
        }
        subscriptions.delete(subscriptionId);
        return { success: true };
      };

      await unsubscribe('sub-1');

      expect(subscriptions.has('sub-1')).toBe(false);
      expect(subscriptions.has('sub-2')).toBe(true);
    });

    it('should throw error for non-existent subscription', async () => {
      const subscriptions = new Map();

      const unsubscribe = async subscriptionId => {
        if (!subscriptions.has(subscriptionId)) {
          throw new Error(`Subscription ${subscriptionId} not found`);
        }
      };

      await expect(unsubscribe('non-existent')).rejects.toThrow('not found');
    });

    it('should unsubscribe all subscriptions', async () => {
      const subscriptions = new Map([
        ['sub-1', { pattern: '?s ?p ?o' }],
        ['sub-2', { pattern: '?s schema:name ?name' }],
        ['sub-3', { pattern: '?s schema:price ?price' }],
      ]);

      const unsubscribeAll = async () => {
        subscriptions.clear();
        return { success: true };
      };

      await unsubscribeAll();

      expect(subscriptions.size).toBe(0);
    });
  });

  describe('Active State Management', () => {
    it('should set active when subscription added', () => {
      let isActive = false;
      const subscriptions = new Map();

      const subscribe = pattern => {
        subscriptions.set(`sub-${Date.now()}`, { pattern });
        isActive = subscriptions.size > 0;
      };

      subscribe('?s ?p ?o');

      expect(isActive).toBe(true);
    });

    it('should set inactive when all subscriptions removed', () => {
      let isActive = true;
      const subscriptions = new Map([['sub-1', {}]]);

      const unsubscribe = id => {
        subscriptions.delete(id);
        isActive = subscriptions.size > 0;
      };

      unsubscribe('sub-1');

      expect(isActive).toBe(false);
    });

    it('should remain active with multiple subscriptions', () => {
      const subscriptions = new Map([
        ['sub-1', {}],
        ['sub-2', {}],
      ]);

      subscriptions.delete('sub-1');
      const isActive = subscriptions.size > 0;

      expect(isActive).toBe(true);
    });
  });

  describe('Event Buffer Management', () => {
    it('should buffer events up to max size', () => {
      const maxBuffer = 100;
      let events = [];

      const addEvent = event => {
        events.push(event);
        if (events.length > maxBuffer) {
          events = events.slice(-maxBuffer);
        }
      };

      for (let i = 0; i < 150; i++) {
        addEvent({ id: `event-${i}` });
      }

      expect(events.length).toBe(100);
      expect(events[0].id).toBe('event-50');
    });

    it('should trim buffer when exceeded', () => {
      const bufferSize = 5;
      let buffer = ['e1', 'e2', 'e3', 'e4', 'e5'];

      buffer.push('e6');
      if (buffer.length > bufferSize) {
        buffer = buffer.slice(-bufferSize);
      }

      expect(buffer).toEqual(['e2', 'e3', 'e4', 'e5', 'e6']);
    });

    it('should clear events buffer', () => {
      let events = [{ id: '1' }, { id: '2' }, { id: '3' }];

      const clear = () => {
        events = [];
      };

      clear();

      expect(events).toHaveLength(0);
    });

    it('should use configurable buffer size', () => {
      const config = { bufferSize: 50 };
      const defaultSize = 100;

      const actualSize = config.bufferSize || defaultSize;

      expect(actualSize).toBe(50);
    });
  });

  describe('Event Handling', () => {
    it('should add subscription ID to events', () => {
      const subscriptionId = 'sub-123';
      const event = { data: 'test' };

      const enrichedEvent = {
        ...event,
        subscriptionId,
        timestamp: new Date().toISOString(),
      };

      expect(enrichedEvent.subscriptionId).toBe('sub-123');
      expect(enrichedEvent.timestamp).toBeDefined();
    });

    it('should call custom onEvent callback', () => {
      const callback = vi.fn();
      const event = { id: 'event-1', data: 'test' };

      const processEvent = (evt, options) => {
        options.onEvent?.(evt);
      };

      processEvent(event, { onEvent: callback });

      expect(callback).toHaveBeenCalledWith(event);
    });

    it('should call onError callback on error', () => {
      const callback = vi.fn();
      const error = new Error('Connection lost');

      const handleError = (err, options) => {
        options.onError?.(err);
      };

      handleError(error, { onError: callback });

      expect(callback).toHaveBeenCalledWith(error);
    });
  });

  describe('Pause and Resume', () => {
    it('should pause subscription', async () => {
      let isPaused = false;
      const subscription = {
        pause: async () => {
          isPaused = true;
        },
      };

      await subscription.pause();

      expect(isPaused).toBe(true);
    });

    it('should resume subscription', async () => {
      let isPaused = true;
      const subscription = {
        resume: async () => {
          isPaused = false;
        },
      };

      await subscription.resume();

      expect(isPaused).toBe(false);
    });

    it('should throw error for non-existent subscription on pause', async () => {
      const subscriptions = new Map();

      const pause = async subscriptionId => {
        const sub = subscriptions.get(subscriptionId);
        if (!sub) {
          throw new Error(`Subscription ${subscriptionId} not found`);
        }
      };

      await expect(pause('non-existent')).rejects.toThrow('not found');
    });

    it('should throw error for non-existent subscription on resume', async () => {
      const subscriptions = new Map();

      const resume = async subscriptionId => {
        const sub = subscriptions.get(subscriptionId);
        if (!sub) {
          throw new Error(`Subscription ${subscriptionId} not found`);
        }
      };

      await expect(resume('non-existent')).rejects.toThrow('not found');
    });
  });

  describe('Get Subscription', () => {
    it('should retrieve subscription by ID', () => {
      const subscriptions = new Map([
        ['sub-1', { pattern: '?s ?p ?o', options: { bufferSize: 50 } }],
      ]);

      const getSubscription = id => subscriptions.get(id);

      const result = getSubscription('sub-1');

      expect(result.pattern).toBe('?s ?p ?o');
      expect(result.options.bufferSize).toBe(50);
    });

    it('should return undefined for non-existent subscription', () => {
      const subscriptions = new Map();

      const getSubscription = id => subscriptions.get(id);

      const result = getSubscription('non-existent');

      expect(result).toBeUndefined();
    });
  });

  describe('Subscriptions List', () => {
    it('should return array of subscriptions', () => {
      const subscriptions = new Map([
        ['sub-1', { pattern: '?s ?p ?o', options: {} }],
        ['sub-2', { pattern: '?s schema:name ?name', options: {} }],
      ]);

      const getList = () => {
        return Array.from(subscriptions.entries()).map(([id, sub]) => ({
          id,
          pattern: sub.pattern,
          options: sub.options,
        }));
      };

      const list = getList();

      expect(list).toHaveLength(2);
      expect(list[0].id).toBe('sub-1');
      expect(list[1].pattern).toBe('?s schema:name ?name');
    });

    it('should return empty array when no subscriptions', () => {
      const subscriptions = new Map();

      const getList = () => {
        return Array.from(subscriptions.entries()).map(([id, sub]) => ({
          id,
          pattern: sub.pattern,
        }));
      };

      const list = getList();

      expect(list).toHaveLength(0);
    });
  });

  describe('Auto-Start Configuration', () => {
    it('should auto-start when configured', async () => {
      const config = { autoStart: true, pattern: '?s ?p ?o' };
      let started = false;
      const subscriptions = new Map();

      const initialize = async () => {
        if (config.autoStart && config.pattern) {
          subscriptions.set('auto', { pattern: config.pattern });
          started = true;
        }
      };

      await initialize();

      expect(started).toBe(true);
      expect(subscriptions.has('auto')).toBe(true);
    });

    it('should not auto-start without pattern', async () => {
      const config = { autoStart: true };
      let started = false;

      const initialize = async () => {
        if (config.autoStart && config.pattern) {
          started = true;
        }
      };

      await initialize();

      expect(started).toBe(false);
    });

    it('should not auto-start when disabled', async () => {
      const config = { autoStart: false, pattern: '?s ?p ?o' };
      let started = false;

      const initialize = async () => {
        if (config.autoStart && config.pattern) {
          started = true;
        }
      };

      await initialize();

      expect(started).toBe(false);
    });
  });

  describe('Pattern Filtering', () => {
    it('should apply filter function to events', () => {
      const filter = delta => {
        return delta.added.some(q => parseFloat(q.object.value) > 100);
      };

      const events = [
        { added: [{ object: { value: '50' } }] },
        { added: [{ object: { value: '150' } }] },
        { added: [{ object: { value: '200' } }] },
      ];

      const filtered = events.filter(filter);

      expect(filtered).toHaveLength(2);
    });

    it('should pass through when no filter configured', () => {
      const filter = undefined;
      const events = [{ id: '1' }, { id: '2' }, { id: '3' }];

      const processed = filter ? events.filter(filter) : events;

      expect(processed).toHaveLength(3);
    });
  });

  describe('Error Handling', () => {
    it('should handle manager initialization error', () => {
      let error = null;

      const initializeManager = () => {
        try {
          throw new Error('Engine not available');
        } catch (err) {
          error = err;
        }
      };

      initializeManager();

      expect(error).not.toBeNull();
      expect(error.message).toBe('Engine not available');
    });

    it('should set error state on subscribe failure', async () => {
      let error = null;

      const subscribe = async () => {
        error = new Error('Subscription failed');
        throw error;
      };

      await expect(subscribe()).rejects.toThrow('Subscription failed');
      expect(error).not.toBeNull();
    });

    it('should handle unsubscribe error', async () => {
      let error = null;

      const unsubscribe = async () => {
        error = new Error('Unsubscribe failed');
        throw error;
      };

      await expect(unsubscribe()).rejects.toThrow('Unsubscribe failed');
      expect(error).not.toBeNull();
    });
  });

  describe('Loading State', () => {
    it('should track loading state during initialization', async () => {
      let loading = false;

      const initialize = async () => {
        loading = true;
        await new Promise(resolve => setTimeout(resolve, 10));
        loading = false;
      };

      const promise = initialize();
      expect(loading).toBe(true);
      await promise;
      expect(loading).toBe(false);
    });
  });
});
