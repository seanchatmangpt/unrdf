/**
 * @file Unit tests for sidecar status command
 * @module test/cli-v2/commands/sidecar/status
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { statusCommand } from '../../../../src/cli-v2/commands/sidecar/status.mjs';

describe('CLI v2: sidecar status command', () => {
  let mockClient;
  let originalCreateSidecarClient;

  beforeEach(async () => {
    // Mock sidecar client
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
          build: 'abc123'
        }
      })),
      getClientMetrics: vi.fn(() => ({
        requests: 42,
        successes: 40,
        failures: 2,
        retries: 3,
        circuitBreaker: {
          state: 'CLOSED',
          failures: 0,
          successes: 40
        },
        connectionPool: {
          active: 2,
          available: 8,
          total: 10
        },
        retryStrategy: {
          totalRetries: 3,
          retryRate: 0.05
        }
      }))
    };

    // Mock module imports
    const { createSidecarClient } = await import('../../../../src/sidecar/client.mjs');
    originalCreateSidecarClient = createSidecarClient;
    vi.doMock('../../../../src/sidecar/client.mjs', () => ({
      createSidecarClient: vi.fn(() => mockClient)
    }));
  });

  afterEach(() => {
    vi.clearAllMocks();
    vi.restoreAllMocks();
  });

  describe('Basic functionality', () => {
    it('should get sidecar status successfully', async () => {
      const ctx = {
        args: {
          output: 'json',
          verbose: false
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await statusCommand.run(ctx);

      expect(mockClient.connect).toHaveBeenCalled();
      expect(mockClient.healthCheck).toHaveBeenCalled();
      expect(mockClient.getClientMetrics).toHaveBeenCalled();
      expect(mockClient.disconnect).toHaveBeenCalled();
      expect(consoleSpy).toHaveBeenCalled();

      consoleSpy.mockRestore();
    });

    it('should handle connection failures gracefully', async () => {
      mockClient.connect = vi.fn().mockRejectedValue(new Error('Connection refused'));

      const ctx = {
        args: {
          output: 'json',
          verbose: false
        }
      };

      const consoleErrorSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
      const exitSpy = vi.spyOn(process, 'exit').mockImplementation(() => {});

      await statusCommand.run(ctx);

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        expect.stringContaining('Connection refused')
      );
      expect(exitSpy).toHaveBeenCalledWith(1);

      consoleErrorSpy.mockRestore();
      consoleSpy.mockRestore();
      exitSpy.mockRestore();
    });

    it('should use custom address when provided', async () => {
      const ctx = {
        args: {
          output: 'json',
          address: 'custom-host:9999',
          verbose: false
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await statusCommand.run(ctx);

      expect(mockClient.connect).toHaveBeenCalledWith('custom-host:9999');

      consoleSpy.mockRestore();
    });

    it('should include detailed metrics in verbose mode', async () => {
      const ctx = {
        args: {
          output: 'json',
          verbose: true
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await statusCommand.run(ctx);

      expect(mockClient.getClientMetrics).toHaveBeenCalled();
      expect(consoleSpy).toHaveBeenCalled();

      // Verify verbose metrics are included
      const outputCall = consoleSpy.mock.calls[0][0];
      const parsed = JSON.parse(outputCall);
      expect(parsed.metrics).toHaveProperty('circuitBreaker');
      expect(parsed.metrics).toHaveProperty('connectionPool');

      consoleSpy.mockRestore();
    });
  });

  describe('Output formats', () => {
    it('should support JSON output', async () => {
      const ctx = {
        args: {
          output: 'json',
          verbose: false
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await statusCommand.run(ctx);

      const outputCall = consoleSpy.mock.calls[0][0];
      expect(() => JSON.parse(outputCall)).not.toThrow();

      consoleSpy.mockRestore();
    });

    it('should support table output', async () => {
      const ctx = {
        args: {
          output: 'table',
          verbose: false
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await statusCommand.run(ctx);

      expect(consoleSpy).toHaveBeenCalled();

      consoleSpy.mockRestore();
    });

    it('should support yaml output', async () => {
      const ctx = {
        args: {
          output: 'yaml',
          verbose: false
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await statusCommand.run(ctx);

      expect(consoleSpy).toHaveBeenCalled();

      consoleSpy.mockRestore();
    });
  });

  describe('Status information', () => {
    it('should report correct connection status', async () => {
      const ctx = {
        args: {
          output: 'json',
          verbose: false
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await statusCommand.run(ctx);

      const outputCall = consoleSpy.mock.calls[0][0];
      const parsed = JSON.parse(outputCall);

      expect(parsed.connected).toBe(true);
      expect(parsed.status).toBe('SERVING');
      expect(parsed.address).toBe('localhost:50051');
      expect(parsed.uptime_seconds).toBe(3600);

      consoleSpy.mockRestore();
    });

    it('should include basic metrics', async () => {
      const ctx = {
        args: {
          output: 'json',
          verbose: false
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await statusCommand.run(ctx);

      const outputCall = consoleSpy.mock.calls[0][0];
      const parsed = JSON.parse(outputCall);

      expect(parsed.metrics).toHaveProperty('requests');
      expect(parsed.metrics).toHaveProperty('successes');
      expect(parsed.metrics).toHaveProperty('failures');

      consoleSpy.mockRestore();
    });
  });
});
