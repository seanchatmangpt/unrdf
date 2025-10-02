/**
 * @file Unit tests for sidecar config command
 * @module test/cli-v2/commands/sidecar/config
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { configCommand } from '../../../../src/cli-v2/commands/sidecar/config.mjs';

describe('CLI v2: sidecar config command', () => {
  let mockConfig;

  beforeEach(() => {
    mockConfig = {
      currentContext: 'local',
      contexts: [
        {
          name: 'local',
          endpoint: {
            address: 'localhost',
            port: 50051,
            tls: { enabled: false }
          },
          timeout: 5000,
          maxRetries: 3,
          namespace: 'default'
        },
        {
          name: 'production',
          endpoint: {
            address: 'prod-sidecar.example.com',
            port: 50051,
            tls: { enabled: true }
          },
          timeout: 10000,
          maxRetries: 5,
          namespace: 'prod'
        }
      ]
    };

    vi.doMock('../../../../src/sidecar/config.mjs', () => ({
      createSidecarConfig: vi.fn(() => ({
        getContext: vi.fn(() => mockConfig.contexts[0]),
        toJSON: vi.fn(() => mockConfig),
        useContext: vi.fn((name) => {
          const context = mockConfig.contexts.find(c => c.name === name);
          if (!context) {
            throw new Error(`Context not found: ${name}`);
          }
          mockConfig.currentContext = name;
        }),
        getAddress: vi.fn(() => 'localhost:50051')
      }))
    }));
  });

  afterEach(() => {
    vi.clearAllMocks();
    vi.restoreAllMocks();
  });

  describe('config get', () => {
    it('should get entire configuration', async () => {
      const ctx = {
        args: {
          output: 'json'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await configCommand.subCommands.get.run(ctx);

      expect(consoleSpy).toHaveBeenCalled();
      const outputCall = consoleSpy.mock.calls[0][0];
      const parsed = JSON.parse(outputCall);

      expect(parsed).toHaveProperty('currentContext');
      expect(parsed).toHaveProperty('contexts');

      consoleSpy.mockRestore();
    });

    it('should get specific configuration key', async () => {
      const ctx = {
        args: {
          key: 'endpoint.address',
          output: 'json'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await configCommand.subCommands.get.run(ctx);

      expect(consoleSpy).toHaveBeenCalled();
      const outputCall = consoleSpy.mock.calls[0][0];
      const parsed = JSON.parse(outputCall);

      expect(parsed['endpoint.address']).toBe('localhost');

      consoleSpy.mockRestore();
    });

    it('should handle non-existent keys', async () => {
      const ctx = {
        args: {
          key: 'nonexistent.key',
          output: 'json'
        }
      };

      const consoleErrorSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
      const exitSpy = vi.spyOn(process, 'exit').mockImplementation(() => {});

      await configCommand.subCommands.get.run(ctx);

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        expect.stringContaining('not found')
      );
      expect(exitSpy).toHaveBeenCalledWith(1);

      consoleErrorSpy.mockRestore();
      exitSpy.mockRestore();
    });
  });

  describe('config set', () => {
    it('should set configuration value', async () => {
      const ctx = {
        args: {
          key: 'endpoint.port',
          value: '9999'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
      const writeFileSyncSpy = vi.spyOn(await import('fs'), 'writeFileSync').mockImplementation(() => {});

      await configCommand.subCommands.set.run(ctx);

      expect(consoleSpy).toHaveBeenCalledWith(
        expect.stringContaining('Set endpoint.port = 9999')
      );
      expect(writeFileSyncSpy).toHaveBeenCalled();

      consoleSpy.mockRestore();
      writeFileSyncSpy.mockRestore();
    });

    it('should parse JSON values', async () => {
      const ctx = {
        args: {
          key: 'endpoint.tls',
          value: '{"enabled":true}'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
      const writeFileSyncSpy = vi.spyOn(await import('fs'), 'writeFileSync').mockImplementation(() => {});

      await configCommand.subCommands.set.run(ctx);

      expect(consoleSpy).toHaveBeenCalled();
      expect(writeFileSyncSpy).toHaveBeenCalled();

      consoleSpy.mockRestore();
      writeFileSyncSpy.mockRestore();
    });
  });

  describe('config list', () => {
    it('should list all contexts', async () => {
      const ctx = {
        args: {
          output: 'json'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await configCommand.subCommands.list.run(ctx);

      expect(consoleSpy).toHaveBeenCalled();
      const outputCall = consoleSpy.mock.calls[0][0];
      const parsed = JSON.parse(outputCall);

      expect(Array.isArray(parsed)).toBe(true);
      expect(parsed.length).toBe(2);
      expect(parsed[0].name).toBe('local');
      expect(parsed[1].name).toBe('production');

      consoleSpy.mockRestore();
    });

    it('should mark current context', async () => {
      const ctx = {
        args: {
          output: 'json'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await configCommand.subCommands.list.run(ctx);

      const outputCall = consoleSpy.mock.calls[0][0];
      const parsed = JSON.parse(outputCall);

      const currentContext = parsed.find(c => c.current === '✓');
      expect(currentContext).toBeDefined();
      expect(currentContext.name).toBe('local');

      consoleSpy.mockRestore();
    });
  });

  describe('config use-context', () => {
    it('should switch to different context', async () => {
      const ctx = {
        args: {
          context: 'production'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});
      const writeFileSyncSpy = vi.spyOn(await import('fs'), 'writeFileSync').mockImplementation(() => {});

      await configCommand.subCommands['use-context'].run(ctx);

      expect(consoleSpy).toHaveBeenCalledWith(
        expect.stringContaining('Switched to context: production')
      );
      expect(writeFileSyncSpy).toHaveBeenCalled();

      consoleSpy.mockRestore();
      writeFileSyncSpy.mockRestore();
    });

    it('should handle non-existent context', async () => {
      const ctx = {
        args: {
          context: 'nonexistent'
        }
      };

      const consoleErrorSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
      const exitSpy = vi.spyOn(process, 'exit').mockImplementation(() => {});

      await configCommand.subCommands['use-context'].run(ctx);

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        expect.stringContaining('Context not found')
      );
      expect(exitSpy).toHaveBeenCalledWith(1);

      consoleErrorSpy.mockRestore();
      exitSpy.mockRestore();
    });
  });

  describe('config current', () => {
    it('should show current context details', async () => {
      const ctx = {
        args: {
          output: 'json'
        }
      };

      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      await configCommand.subCommands.current.run(ctx);

      expect(consoleSpy).toHaveBeenCalled();
      const outputCall = consoleSpy.mock.calls[0][0];
      const parsed = JSON.parse(outputCall);

      expect(parsed.name).toBe('local');
      expect(parsed.address).toBe('localhost:50051');
      expect(parsed.timeout).toBe(5000);

      consoleSpy.mockRestore();
    });
  });
});
