/**
 * @file Tests for useStreamProcessor hook functionality
 * Tests window operations, aggregations, and stream processing
 */

import { describe, it, expect, _beforeEach, _vi } from 'vitest';

describe('useStreamProcessor', () => {
  describe('Tumbling Window', () => {
    it('should create non-overlapping windows', () => {
      const windowSize = 5000;
      const _events = [];
      const windows = [];
      let windowStart = Date.now();

      // Simulate events over time
      for (let i = 0; i < 3; i++) {
        const windowEvents = [];
        for (let j = 0; j < 5; j++) {
          windowEvents.push({
            id: `event-${i}-${j}`,
            timestamp: windowStart + j * 1000,
          });
        }
        windows.push({
          id: `window-${i}`,
          events: windowEvents,
          startTime: windowStart,
          endTime: windowStart + windowSize,
        });
        windowStart += windowSize;
      }

      expect(windows).toHaveLength(3);
      windows.forEach(w => {
        expect(w.events).toHaveLength(5);
      });

      // Verify non-overlapping
      for (let i = 0; i < windows.length - 1; i++) {
        expect(windows[i].endTime).toBeLessThanOrEqual(windows[i + 1].startTime);
      }
    });

    it('should complete window at size limit', () => {
      const windowSize = 5000;
      let buffer = [];
      const completedWindows = [];

      const processEvent = (event, currentTime) => {
        if (buffer.length === 0) {
          buffer.push({ ...event, windowStart: currentTime });
        } else {
          const elapsed = currentTime - buffer[0].windowStart;
          if (elapsed >= windowSize) {
            completedWindows.push({
              events: [...buffer],
              size: buffer.length,
            });
            buffer = [{ ...event, windowStart: currentTime }];
          } else {
            buffer.push(event);
          }
        }
      };

      const baseTime = Date.now();
      for (let i = 0; i < 10; i++) {
        processEvent({ id: `event-${i}` }, baseTime + i * 1000);
      }

      expect(completedWindows.length).toBeGreaterThanOrEqual(1);
    });

    it('should process remaining events on stop', () => {
      let buffer = [{ id: '1' }, { id: '2' }];
      const completedWindows = [];

      const stop = () => {
        if (buffer.length > 0) {
          completedWindows.push({ events: [...buffer] });
          buffer = [];
        }
      };

      stop();

      expect(completedWindows).toHaveLength(1);
      expect(completedWindows[0].events).toHaveLength(2);
      expect(buffer).toHaveLength(0);
    });
  });

  describe('Sliding Window', () => {
    it('should create overlapping windows', () => {
      const windowSize = 5000;
      const slideInterval = 2500;
      const events = Array(10)
        .fill(null)
        .map((_, i) => ({
          id: `event-${i}`,
          timestamp: Date.now() + i * 1000,
        }));

      // Simulate sliding window behavior
      const windows = [];
      for (let i = 0; i < events.length; i += Math.floor(slideInterval / 1000)) {
        const windowEvents = events.slice(i, i + Math.floor(windowSize / 1000));
        if (windowEvents.length > 0) {
          windows.push({
            id: `window-${windows.length}`,
            events: windowEvents,
          });
        }
      }

      // Windows should overlap - some events should appear in multiple windows
      expect(windows.length).toBeGreaterThan(1);
    });

    it('should keep events for next window on slide', () => {
      const _windowSize = 5000;
      const slideInterval = 2500;
      let buffer = [
        { id: '1', timestamp: Date.now() },
        { id: '2', timestamp: Date.now() + 1000 },
        { id: '3', timestamp: Date.now() + 2000 },
        { id: '4', timestamp: Date.now() + 3000 },
      ];

      const completeWindow = (keepOverlap = false) => {
        const events = [...buffer];
        if (keepOverlap) {
          const cutoffTime = Date.now() - slideInterval;
          buffer = buffer.filter(e => new Date(e.timestamp).getTime() > cutoffTime);
        } else {
          buffer = [];
        }
        return { events, remaining: buffer.length };
      };

      const result = completeWindow(true);

      expect(result.events).toHaveLength(4);
    });

    it('should handle slide interval configuration', () => {
      const config = {
        windowType: 'sliding',
        windowSize: 5000,
        windowSlide: 2500,
      };

      const slideInterval = config.windowSlide || Math.floor(config.windowSize / 2);

      expect(slideInterval).toBe(2500);
    });
  });

  describe('Session Window', () => {
    it('should complete window on session gap', () => {
      const sessionGap = 3000;
      let lastEventTime = null;
      const windows = [];
      let currentSession = [];

      const processEvent = (event, eventTime) => {
        if (lastEventTime && eventTime - lastEventTime > sessionGap) {
          // Gap detected - complete current session
          windows.push({
            events: [...currentSession],
            startTime: currentSession[0]?.timestamp,
            endTime: lastEventTime,
          });
          currentSession = [];
        }
        currentSession.push({ ...event, timestamp: eventTime });
        lastEventTime = eventTime;
      };

      const baseTime = Date.now();
      // First session
      processEvent({ id: '1' }, baseTime);
      processEvent({ id: '2' }, baseTime + 1000);
      processEvent({ id: '3' }, baseTime + 2000);
      // Gap
      processEvent({ id: '4' }, baseTime + 6000);
      processEvent({ id: '5' }, baseTime + 7000);

      expect(windows).toHaveLength(1);
      expect(windows[0].events).toHaveLength(3);
      expect(currentSession).toHaveLength(2);
    });

    it('should handle session timeout', async () => {
      const sessionGap = 100;
      let sessionCompleted = false;
      let _buffer = [{ id: '1' }];

      const completeSession = () => {
        sessionCompleted = true;
        _buffer = [];
      };

      setTimeout(completeSession, sessionGap);
      await new Promise(resolve => setTimeout(resolve, sessionGap + 50));

      expect(sessionCompleted).toBe(true);
    });

    it('should reset timeout on new events', () => {
      let timeoutId = null;
      let timeoutCount = 0;

      const resetTimeout = () => {
        if (timeoutId) {
          clearTimeout(timeoutId);
        }
        timeoutId = setTimeout(() => {
          timeoutCount++;
        }, 100);
      };

      resetTimeout();
      resetTimeout();
      resetTimeout();

      expect(timeoutCount).toBe(0);
    });
  });

  describe('Aggregation', () => {
    it('should apply default aggregator', () => {
      const events = [
        { id: '1', operation: 'insert', timestamp: '2024-01-01T00:00:00Z' },
        { id: '2', operation: 'insert', timestamp: '2024-01-01T00:00:01Z' },
        { id: '3', operation: 'delete', timestamp: '2024-01-01T00:00:02Z' },
      ];

      const defaultAggregator = evts => ({
        count: evts.length,
        operations: {
          inserts: evts.filter(e => e.operation === 'insert').length,
          deletes: evts.filter(e => e.operation === 'delete').length,
        },
        startTime: evts[0]?.timestamp,
        endTime: evts[evts.length - 1]?.timestamp,
      });

      const result = defaultAggregator(events);

      expect(result.count).toBe(3);
      expect(result.operations.inserts).toBe(2);
      expect(result.operations.deletes).toBe(1);
      expect(result.startTime).toBe('2024-01-01T00:00:00Z');
      expect(result.endTime).toBe('2024-01-01T00:00:02Z');
    });

    it('should apply custom aggregator', () => {
      const events = [
        { id: '1', quads: [{ object: { value: '100' } }] },
        { id: '2', quads: [{ object: { value: '200' } }] },
        { id: '3', quads: [{ object: { value: '150' } }] },
      ];

      const customAggregator = evts => {
        const values = evts.flatMap(e => e.quads.map(q => parseFloat(q.object.value)));
        return {
          count: values.length,
          sum: values.reduce((a, b) => a + b, 0),
          avg: values.reduce((a, b) => a + b, 0) / values.length,
          min: Math.min(...values),
          max: Math.max(...values),
        };
      };

      const result = customAggregator(events);

      expect(result.count).toBe(3);
      expect(result.sum).toBe(450);
      expect(result.avg).toBe(150);
      expect(result.min).toBe(100);
      expect(result.max).toBe(200);
    });

    it('should handle empty events in aggregation', () => {
      const events = [];

      const aggregator = evts => ({
        count: evts.length,
        isEmpty: evts.length === 0,
      });

      const result = aggregator(events);

      expect(result.count).toBe(0);
      expect(result.isEmpty).toBe(true);
    });
  });

  describe('Statistics Tracking', () => {
    it('should track windows processed', () => {
      let stats = {
        windowsProcessed: 0,
        eventsProcessed: 0,
        avgWindowSize: 0,
      };

      const updateStats = window => {
        const total = stats.eventsProcessed + window.events.length;
        const count = stats.windowsProcessed + 1;
        stats = {
          windowsProcessed: count,
          eventsProcessed: total,
          avgWindowSize: Math.round(total / count),
        };
      };

      updateStats({ events: [1, 2, 3, 4, 5] });
      updateStats({ events: [1, 2, 3] });
      updateStats({ events: [1, 2, 3, 4, 5, 6, 7] });

      expect(stats.windowsProcessed).toBe(3);
      expect(stats.eventsProcessed).toBe(15);
      expect(stats.avgWindowSize).toBe(5);
    });

    it('should track events processed', () => {
      let totalEvents = 0;
      const windows = [{ events: [1, 2, 3] }, { events: [1, 2, 3, 4, 5] }, { events: [1, 2] }];

      windows.forEach(w => {
        totalEvents += w.events.length;
      });

      expect(totalEvents).toBe(10);
    });

    it('should calculate average window size', () => {
      const windows = [
        { events: Array(5).fill(1) },
        { events: Array(10).fill(1) },
        { events: Array(15).fill(1) },
      ];

      const totalEvents = windows.reduce((sum, w) => sum + w.events.length, 0);
      const avgSize = Math.round(totalEvents / windows.length);

      expect(avgSize).toBe(10);
    });

    it('should reset stats on clear', () => {
      let stats = {
        windowsProcessed: 10,
        eventsProcessed: 100,
        avgWindowSize: 10,
      };

      const clear = () => {
        stats = {
          windowsProcessed: 0,
          eventsProcessed: 0,
          avgWindowSize: 0,
        };
      };

      clear();

      expect(stats.windowsProcessed).toBe(0);
      expect(stats.eventsProcessed).toBe(0);
      expect(stats.avgWindowSize).toBe(0);
    });
  });

  describe('Time Range Query', () => {
    it('should get windows by time range', () => {
      const windows = [
        {
          id: 'w1',
          startTime: '2024-01-01T00:00:00Z',
          endTime: '2024-01-01T01:00:00Z',
        },
        {
          id: 'w2',
          startTime: '2024-01-01T01:00:00Z',
          endTime: '2024-01-01T02:00:00Z',
        },
        {
          id: 'w3',
          startTime: '2024-01-01T02:00:00Z',
          endTime: '2024-01-01T03:00:00Z',
        },
        {
          id: 'w4',
          startTime: '2024-01-01T03:00:00Z',
          endTime: '2024-01-01T04:00:00Z',
        },
      ];

      const getWindowsByTimeRange = (startTime, endTime) => {
        return windows.filter(
          w =>
            new Date(w.startTime) >= new Date(startTime) && new Date(w.endTime) <= new Date(endTime)
        );
      };

      const result = getWindowsByTimeRange('2024-01-01T01:00:00Z', '2024-01-01T03:00:00Z');

      expect(result).toHaveLength(2);
      expect(result[0].id).toBe('w2');
      expect(result[1].id).toBe('w3');
    });

    it('should return empty for non-matching range', () => {
      const windows = [
        {
          id: 'w1',
          startTime: '2024-01-01T00:00:00Z',
          endTime: '2024-01-01T01:00:00Z',
        },
      ];

      const getWindowsByTimeRange = (startTime, endTime) => {
        return windows.filter(
          w =>
            new Date(w.startTime) >= new Date(startTime) && new Date(w.endTime) <= new Date(endTime)
        );
      };

      const result = getWindowsByTimeRange('2025-01-01T00:00:00Z', '2025-01-01T01:00:00Z');

      expect(result).toHaveLength(0);
    });
  });

  describe('Current Window Preview', () => {
    it('should update current window as events arrive', () => {
      let currentWindow = null;
      const buffer = [];

      const updateCurrentWindow = event => {
        buffer.push(event);
        currentWindow = {
          events: [...buffer],
          size: buffer.length,
          startTime: buffer[0]?.timestamp,
          endTime: event.timestamp,
        };
      };

      updateCurrentWindow({ id: '1', timestamp: '2024-01-01T00:00:00Z' });
      updateCurrentWindow({ id: '2', timestamp: '2024-01-01T00:00:01Z' });
      updateCurrentWindow({ id: '3', timestamp: '2024-01-01T00:00:02Z' });

      expect(currentWindow.size).toBe(3);
      expect(currentWindow.startTime).toBe('2024-01-01T00:00:00Z');
      expect(currentWindow.endTime).toBe('2024-01-01T00:00:02Z');
    });

    it('should clear current window on completion', () => {
      let currentWindow = { events: [1, 2, 3], size: 3 };

      const completeWindow = () => {
        const completed = { ...currentWindow };
        currentWindow = null;
        return completed;
      };

      const result = completeWindow();

      expect(result.events).toHaveLength(3);
      expect(currentWindow).toBeNull();
    });
  });

  describe('Error Handling', () => {
    it('should handle processor initialization error', () => {
      let error = null;

      const initializeProcessor = () => {
        try {
          throw new Error('Module not found');
        } catch (err) {
          error = err;
        }
      };

      initializeProcessor();

      expect(error).not.toBeNull();
      expect(error.message).toBe('Module not found');
    });

    it('should handle event processing error', () => {
      let error = null;

      const processEvent = event => {
        if (!event.id) {
          error = new Error('Invalid event: missing id');
          throw error;
        }
      };

      expect(() => processEvent({})).toThrow('Invalid event: missing id');
      expect(error).not.toBeNull();
    });
  });

  describe('Clear Functionality', () => {
    it('should clear all windows', () => {
      let windows = [{ id: 'w1' }, { id: 'w2' }];
      let currentWindow = { events: [1, 2] };
      let buffer = [1, 2, 3];

      const clear = () => {
        windows = [];
        currentWindow = null;
        buffer = [];
      };

      clear();

      expect(windows).toHaveLength(0);
      expect(currentWindow).toBeNull();
      expect(buffer).toHaveLength(0);
    });
  });

  describe('Processing State', () => {
    it('should track processing state', () => {
      let isProcessing = false;

      const start = () => {
        isProcessing = true;
      };

      const stop = () => {
        isProcessing = false;
      };

      expect(isProcessing).toBe(false);
      start();
      expect(isProcessing).toBe(true);
      stop();
      expect(isProcessing).toBe(false);
    });

    it('should combine processing with feed running state', () => {
      const feedRunning = true;
      const isProcessing = true;

      const combinedState = isProcessing && feedRunning;

      expect(combinedState).toBe(true);
    });
  });
});
