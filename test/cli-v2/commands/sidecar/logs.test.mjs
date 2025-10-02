/**
 * @file Unit tests for sidecar logs command
 * @module test/cli-v2/commands/sidecar/logs
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { logsCommand } from '../../../../src/cli-v2/commands/sidecar/logs.mjs';

describe('CLI v2: sidecar logs command', () => {
  let mockClient;

  beforeEach(() => {
    mockClient = {
      connected: false,
      config: {
        getAddress: vi.fn(() => 'localhost:50051')
      },
      connect: vi.fn(async () => {
        mockClient.connected = true;
      }),
      disconnect: vi.fn(async () => {
        mockClient.connected = false;
      }),
      getMetrics: vi.fn(async () => ({
        timestamp: Date.now(),
        metrics: {
          'transactions.applied': {
            int_value: 42,
            unit: 'count'
          },
          'hooks.evaluated': {
            int_value: 128,
            unit: 'count'
          },
          'lockchain.receipts': {
            int_value: 35,
            unit: 'count'
          },
          'performance.p99_latency': {
            double_value: 1.85,
            unit: 'ms'
          },
          'errors.total': {
            int_value: 2,
            unit: 'count'
          }
        }
      }))
    };

    vi.doMock('../../../../src/sidecar/client.mjs', () => ({
      createSidecarClient: vi.fn(() => mockClient)
    }));
  });

  afterEach(() => {
    vi.clearAllMocks();
    vi.restoreAllMocks();
  });

  describe('Basic functionality', () => {
    it('should fetch metrics successfully', async () => {
      const ctx = {
        args: {
          follow: false,
          tail: '100',
          output: 'json'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await logsCommand.run(ctx);

      expect(mockClient.connect).toHaveBeenCalled();
      expect(mockClient.getMetrics).toHaveBeenCalled();
      expect(mockClient.disconnect).toHaveBeenCalled();
      expect(consoleSpy).toHaveBeenCalled();

      consoleSpy.mockRestore();
    });

    it('should handle connection failures', async () => {
      mockClient.connect = vi.fn().mockRejectedValue(new Error('Connection refused'));

      const ctx = {
        args: {
          follow: false,
          tail: '100',
          output: 'json'
        }
      };

      const consoleErrorSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
      const exitSpy = vi.spyOn(process, 'exit').mockImplementation(() => {});

      await logsCommand.run(ctx);

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        expect.stringContaining('Connection refused')
      );
      expect(exitSpy).toHaveBeenCalledWith(1);

      consoleErrorSpy.mockRestore();
      exitSpy.mockRestore();
    });

    it('should filter by specific metrics', async () => {
      const ctx = {
        args: {
          follow: false,
          tail: '100',
          metrics: 'transactions.applied,hooks.evaluated',
          output: 'json'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await logsCommand.run(ctx);

      expect(mockClient.getMetrics).toHaveBeenCalledWith({
        metricNames: ['transactions.applied', 'hooks.evaluated'],
        sinceTimestamp: 0
      });

      consoleSpy.mockRestore();
    });

    it('should use since timestamp when provided', async () => {
      const sinceTimestamp = Math.floor(Date.now() / 1000) - 3600; // 1 hour ago

      const ctx = {
        args: {
          follow: false,
          tail: '100',
          since: sinceTimestamp.toString(),
          output: 'json'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await logsCommand.run(ctx);

      expect(mockClient.getMetrics).toHaveBeenCalledWith({
        metricNames: [],
        sinceTimestamp
      });

      consoleSpy.mockRestore();
    });
  });

  describe('Metric formatting', () => {
    it('should format integer metrics correctly', async () => {
      const ctx = {
        args: {
          follow: false,
          tail: '100',
          output: 'json'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await logsCommand.run(ctx);

      const outputCall = consoleSpy.mock.calls[0][0];
      const parsed = JSON.parse(outputCall);

      const txMetric = parsed.find(m => m.metric === 'transactions.applied');
      expect(txMetric).toBeDefined();
      expect(txMetric.value).toBe('42 count');

      consoleSpy.mockRestore();
    });

    it('should format float metrics correctly', async () => {
      const ctx = {
        args: {
          follow: false,
          tail: '100',
          output: 'json'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await logsCommand.run(ctx);

      const outputCall = consoleSpy.mock.calls[0][0];
      const parsed = JSON.parse(outputCall);

      const latencyMetric = parsed.find(m => m.metric === 'performance.p99_latency');
      expect(latencyMetric).toBeDefined();
      expect(latencyMetric.value).toBe('1.85 ms');

      consoleSpy.mockRestore();
    });

    it('should include timestamp for each metric', async () => {
      const ctx = {
        args: {
          follow: false,
          tail: '100',
          output: 'json'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await logsCommand.run(ctx);

      const outputCall = consoleSpy.mock.calls[0][0];
      const parsed = JSON.parse(outputCall);

      parsed.forEach(metric => {
        expect(metric.timestamp).toBeDefined();
        expect(metric.timestamp).toMatch(/^\d{4}-\d{2}-\d{2}T/); // ISO 8601 format
      });

      consoleSpy.mockRestore();
    });
  });

  describe('Tail functionality', () => {
    it('should limit output to tail count', async () => {
      const ctx = {
        args: {
          follow: false,
          tail: '2',
          output: 'json'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await logsCommand.run(ctx);

      const outputCall = consoleSpy.mock.calls[0][0];
      const parsed = JSON.parse(outputCall);

      // Should only show last 2 metrics
      expect(parsed.length).toBeLessThanOrEqual(2);

      consoleSpy.mockRestore();
    });
  });

  describe('Follow mode', () => {
    it('should set up interval in follow mode', async () => {
      const ctx = {
        args: {
          follow: true,
          interval: '2',
          tail: '100',
          output: 'json'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
      const setIntervalSpy = vi.spyOn(global, 'setInterval');

      // Start follow mode (it will run indefinitely, so we don't await)
      const runPromise = logsCommand.run(ctx);

      // Give it a moment to set up the interval
      await new Promise(resolve => setTimeout(resolve, 100));

      expect(setIntervalSpy).toHaveBeenCalled();
      expect(consoleSpy).toHaveBeenCalledWith(
        expect.stringContaining('Following logs every 2s')
      );

      // Cleanup
      vi.clearAllTimers();
      consoleSpy.mockRestore();
      setIntervalSpy.mockRestore();
    });

    it('should update timestamp between polls', async () => {
      let callCount = 0;
      const timestamps = [Date.now(), Date.now() + 1000];

      mockClient.getMetrics = vi.fn(async () => ({
        timestamp: timestamps[callCount++] || Date.now(),
        metrics: {
          'test.metric': {
            int_value: callCount,
            unit: 'count'
          }
        }
      }));

      const ctx = {
        args: {
          follow: true,
          interval: '1',
          tail: '100',
          output: 'json'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      // Start follow mode
      const runPromise = logsCommand.run(ctx);

      // Wait for a couple of polls
      await new Promise(resolve => setTimeout(resolve, 2500));

      // Verify metrics were called multiple times with updated timestamp
      expect(mockClient.getMetrics).toHaveBeenCalledTimes(2);

      // Cleanup
      vi.clearAllTimers();
      consoleSpy.mockRestore();
    });
  });

  describe('Output formats', () => {
    it('should support JSON output', async () => {
      const ctx = {
        args: {
          follow: false,
          tail: '100',
          output: 'json'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await logsCommand.run(ctx);

      const outputCall = consoleSpy.mock.calls[0][0];
      expect(() => JSON.parse(outputCall)).not.toThrow();

      consoleSpy.mockRestore();
    });

    it('should support table output', async () => {
      const ctx = {
        args: {
          follow: false,
          tail: '100',
          output: 'table'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await logsCommand.run(ctx);

      expect(consoleSpy).toHaveBeenCalled();

      consoleSpy.mockRestore();
    });
  });

  describe('Error handling', () => {
    it('should handle metric fetch failures gracefully', async () => {
      mockClient.getMetrics = vi.fn().mockRejectedValue(new Error('Metrics unavailable'));

      const ctx = {
        args: {
          follow: false,
          tail: '100',
          output: 'json'
        }
      };

      const consoleErrorSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await logsCommand.run(ctx);

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        expect.stringContaining('Metrics unavailable')
      );

      consoleErrorSpy.mockRestore();
      consoleSpy.mockRestore();
    });

    it('should handle empty metrics gracefully', async () => {
      mockClient.getMetrics = vi.fn(async () => ({
        timestamp: Date.now(),
        metrics: {}
      }));

      const ctx = {
        args: {
          follow: false,
          tail: '100',
          output: 'json'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await logsCommand.run(ctx);

      expect(consoleSpy).toHaveBeenCalledWith('No new metrics available');

      consoleSpy.mockRestore();
    });
  });
});
