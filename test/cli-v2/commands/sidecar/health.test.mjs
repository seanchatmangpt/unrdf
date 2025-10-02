/**
 * @file Unit tests for sidecar health command
 * @module test/cli-v2/commands/sidecar/health
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { healthCommand } from '../../../../src/cli-v2/commands/sidecar/health.mjs';

describe('CLI v2: sidecar health command', () => {
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
      healthCheck: vi.fn(async () => ({
        status: 'SERVING',
        uptime_seconds: 3600,
        details: {
          version: '2.0.0',
          store: 'healthy',
          hooks: 'healthy'
        }
      })),
      getClientMetrics: vi.fn(() => ({
        requests: 100,
        successes: 95,
        failures: 5,
        retries: 8,
        circuitBreaker: {
          state: 'CLOSED',
          failures: 2,
          successes: 95,
          halfOpenSuccesses: 0
        },
        connectionPool: {
          active: 3,
          available: 7,
          total: 10,
          pending: 0
        },
        retryStrategy: {
          totalRetries: 8,
          retryRate: 0.08
        },
        health: {
          status: 'HEALTHY',
          consecutiveFailures: 0
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

  describe('Basic health check', () => {
    it('should perform health check successfully', async () => {
      const ctx = {
        args: {
          output: 'json',
          watch: false
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
      const exitSpy = vi.spyOn(process, 'exit').mockImplementation(() => {});

      await healthCommand.run(ctx);

      expect(mockClient.connect).toHaveBeenCalled();
      expect(mockClient.healthCheck).toHaveBeenCalled();
      expect(mockClient.getClientMetrics).toHaveBeenCalled();
      expect(exitSpy).toHaveBeenCalledWith(0); // Healthy exit code

      consoleSpy.mockRestore();
      exitSpy.mockRestore();
    });

    it('should exit with error code when unhealthy', async () => {
      mockClient.healthCheck = vi.fn(async () => ({
        status: 'NOT_SERVING',
        uptime_seconds: 0,
        details: { error: 'Service down' }
      }));

      const ctx = {
        args: {
          output: 'json',
          watch: false
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
      const exitSpy = vi.spyOn(process, 'exit').mockImplementation(() => {});

      await healthCommand.run(ctx);

      expect(exitSpy).toHaveBeenCalledWith(1); // Unhealthy exit code

      consoleSpy.mockRestore();
      exitSpy.mockRestore();
    });

    it('should handle connection failures', async () => {
      mockClient.connect = vi.fn().mockRejectedValue(new Error('Connection refused'));

      const ctx = {
        args: {
          output: 'json',
          watch: false
        }
      };

      const consoleErrorSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
      const exitSpy = vi.spyOn(process, 'exit').mockImplementation(() => {});

      await healthCommand.run(ctx);

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        expect.stringContaining('Connection refused')
      );
      expect(exitSpy).toHaveBeenCalledWith(1);

      consoleErrorSpy.mockRestore();
      consoleSpy.mockRestore();
      exitSpy.mockRestore();
    });
  });

  describe('Health report details', () => {
    it('should include circuit breaker metrics', async () => {
      const ctx = {
        args: {
          output: 'json',
          watch: false
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
      const exitSpy = vi.spyOn(process, 'exit').mockImplementation(() => {});

      await healthCommand.run(ctx);

      const outputCall = consoleSpy.mock.calls[0][0];
      const parsed = JSON.parse(outputCall);

      expect(parsed.circuit_breaker).toBeDefined();
      expect(parsed.circuit_breaker.state).toBe('CLOSED');
      expect(parsed.circuit_breaker.failures).toBe(2);
      expect(parsed.circuit_breaker.successes).toBe(95);

      consoleSpy.mockRestore();
      exitSpy.mockRestore();
    });

    it('should include connection pool metrics', async () => {
      const ctx = {
        args: {
          output: 'json',
          watch: false
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
      const exitSpy = vi.spyOn(process, 'exit').mockImplementation(() => {});

      await healthCommand.run(ctx);

      const outputCall = consoleSpy.mock.calls[0][0];
      const parsed = JSON.parse(outputCall);

      expect(parsed.connection_pool).toBeDefined();
      expect(parsed.connection_pool.active_connections).toBe(3);
      expect(parsed.connection_pool.available_connections).toBe(7);
      expect(parsed.connection_pool.total_connections).toBe(10);

      consoleSpy.mockRestore();
      exitSpy.mockRestore();
    });

    it('should include request metrics with success rate', async () => {
      const ctx = {
        args: {
          output: 'json',
          watch: false
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
      const exitSpy = vi.spyOn(process, 'exit').mockImplementation(() => {});

      await healthCommand.run(ctx);

      const outputCall = consoleSpy.mock.calls[0][0];
      const parsed = JSON.parse(outputCall);

      expect(parsed.request_metrics).toBeDefined();
      expect(parsed.request_metrics.total_requests).toBe(100);
      expect(parsed.request_metrics.successful_requests).toBe(95);
      expect(parsed.request_metrics.failed_requests).toBe(5);
      expect(parsed.request_metrics.success_rate).toBe('95.00%');

      consoleSpy.mockRestore();
      exitSpy.mockRestore();
    });
  });

  describe('Output formats', () => {
    it('should support JSON output', async () => {
      const ctx = {
        args: {
          output: 'json',
          watch: false
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
      const exitSpy = vi.spyOn(process, 'exit').mockImplementation(() => {});

      await healthCommand.run(ctx);

      const outputCall = consoleSpy.mock.calls[0][0];
      expect(() => JSON.parse(outputCall)).not.toThrow();

      consoleSpy.mockRestore();
      exitSpy.mockRestore();
    });

    it('should format health status with emoji indicators', async () => {
      const ctx = {
        args: {
          output: 'json',
          watch: false
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
      const exitSpy = vi.spyOn(process, 'exit').mockImplementation(() => {});

      await healthCommand.run(ctx);

      const outputCall = consoleSpy.mock.calls[0][0];
      const parsed = JSON.parse(outputCall);

      expect(parsed.service.status).toContain('✅'); // Healthy indicator

      consoleSpy.mockRestore();
      exitSpy.mockRestore();
    });
  });

  describe('Watch mode', () => {
    it('should set up interval in watch mode', async () => {
      const ctx = {
        args: {
          output: 'json',
          watch: true,
          interval: '5'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
      const setIntervalSpy = vi.spyOn(global, 'setInterval');

      // Start watch mode (it will run indefinitely, so we don't await)
      const runPromise = healthCommand.run(ctx);

      // Give it a moment to set up the interval
      await new Promise(resolve => setTimeout(resolve, 100));

      expect(setIntervalSpy).toHaveBeenCalled();
      expect(consoleSpy).toHaveBeenCalledWith(
        expect.stringContaining('Monitoring health every 5s')
      );

      // Cleanup
      vi.clearAllTimers();
      consoleSpy.mockRestore();
      setIntervalSpy.mockRestore();
    });
  });
});
