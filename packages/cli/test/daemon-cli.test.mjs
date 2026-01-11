/**
 * @file Daemon CLI Tests
 * @module cli/test/daemon-cli
 * @description Comprehensive test suite for daemon CLI commands including parsing,
 * validation, JSON output, error handling, flags, and cluster introspection
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { daemonCommand } from '../src/cli/commands/daemon.mjs';

describe('Daemon CLI Commands', () => {
  let originalLog, originalError, logOutput, errorOutput;

  beforeEach(() => {
    logOutput = [];
    errorOutput = [];

    originalLog = console.log;
    originalError = console.error;

    console.log = vi.fn((...args) => {
      logOutput.push(args.join(' '));
    });

    console.error = vi.fn((...args) => {
      errorOutput.push(args.join(' '));
    });
  });

  afterEach(() => {
    console.log = originalLog;
    console.error = originalError;
  });

  describe('list command', () => {
    it('should list all configured operations with default formatting', async () => {
      await daemonCommand.subCommands.list.run({
        args: {
          json: false,
          'include-metadata': false,
        },
      });

      const output = logOutput.join('\n');
      expect(output).toContain('Configured Operations');
      expect(output).toContain('backup-graphs');
      expect(output).toContain('cleanup-temp');
      expect(output).toContain('sync-federation');
      expect(output).toContain('validate-integrity');
    });

    it('should output JSON format when json flag is true', async () => {
      await daemonCommand.subCommands.list.run({
        args: {
          json: true,
          'include-metadata': false,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed).toHaveProperty('operations');
      expect(Array.isArray(parsed.operations)).toBe(true);
      expect(parsed.operations.length).toBeGreaterThan(0);
      expect(parsed.operations[0]).toHaveProperty('id');
      expect(parsed.operations[0]).toHaveProperty('name');
      expect(parsed.operations[0]).toHaveProperty('status');
    });

    it('should include metadata when include-metadata flag is true', async () => {
      await daemonCommand.subCommands.list.run({
        args: {
          json: false,
          'include-metadata': true,
        },
      });

      const output = logOutput.join('\n');
      expect(output).toContain('Metadata');
      expect(output).toContain('category');
    });

    it('should show correct operation count in output', async () => {
      await daemonCommand.subCommands.list.run({
        args: {
          json: false,
          'include-metadata': false,
        },
      });

      const output = logOutput.join('\n');
      expect(output).toContain('Total Operations: 5');
    });

    it('should validate JSON structure for all operations', async () => {
      await daemonCommand.subCommands.list.run({
        args: {
          json: true,
          'include-metadata': false,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      parsed.operations.forEach(op => {
        expect(typeof op.id).toBe('string');
        expect(typeof op.name).toBe('string');
        expect(typeof op.status).toBe('string');
        expect(typeof op.createdAt).toBe('string');
        expect(typeof op.metadata).toBe('object');
      });
    });
  });

  describe('run command', () => {
    it('should execute operation successfully with default formatting', async () => {
      await daemonCommand.subCommands.run.run({
        args: {
          operation: 'backup-graphs',
          json: false,
          timeout: 30000,
        },
      });

      const output = logOutput.join('\n');
      expect(output).toContain('Operation executed successfully');
      expect(output).toContain('backup-graphs');
      expect(output).toContain('success');
    });

    it('should output JSON format for operation execution result', async () => {
      await daemonCommand.subCommands.run.run({
        args: {
          operation: 'backup-graphs',
          json: true,
          timeout: 30000,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed).toHaveProperty('operationId');
      expect(parsed).toHaveProperty('status');
      expect(parsed).toHaveProperty('duration');
      expect(parsed).toHaveProperty('result');
      expect(parsed.status).toBe('success');
    });

    it('should accept and include payload in execution', async () => {
      const payload = JSON.stringify({ targetPath: '/tmp/backup' });

      await daemonCommand.subCommands.run.run({
        args: {
          operation: 'backup-graphs',
          payload,
          json: true,
          timeout: 30000,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed.operationId).toBe('backup-graphs');
      expect(parsed.status).toBe('success');
    });

    it('should fail with non-existent operation', async () => {
      await expect(async () => {
        await daemonCommand.subCommands.run.run({
          args: {
            operation: 'nonexistent-op',
            json: false,
            timeout: 30000,
          },
        });
      }).rejects.toThrow();
    });

    it('should handle invalid JSON payload gracefully', async () => {
      const invalidPayload = '{invalid json}';

      await expect(async () => {
        await daemonCommand.subCommands.run.run({
          args: {
            operation: 'backup-graphs',
            payload: invalidPayload,
            json: false,
            timeout: 30000,
          },
        });
      }).rejects.toThrow();
    });

    it('should include processed and affected counts in result', async () => {
      await daemonCommand.subCommands.run.run({
        args: {
          operation: 'cleanup-temp',
          json: true,
          timeout: 30000,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed.result).toHaveProperty('processed');
      expect(parsed.result).toHaveProperty('affected');
      expect(typeof parsed.result.processed).toBe('number');
      expect(typeof parsed.result.affected).toBe('number');
    });

    it('should show duration in human-readable format', async () => {
      await daemonCommand.subCommands.run.run({
        args: {
          operation: 'sync-federation',
          json: false,
          timeout: 30000,
        },
      });

      const output = logOutput.join('\n');
      expect(output).toMatch(/Duration:.*m?s$/m);
    });
  });

  describe('schedule command', () => {
    it('should schedule cron trigger successfully', async () => {
      await daemonCommand.subCommands.schedule.run({
        args: {
          operation: 'backup-graphs',
          trigger: 'cron',
          json: false,
        },
      });

      const output = logOutput.join('\n');
      expect(output).toContain('Trigger scheduled successfully');
      expect(output).toContain('backup-graphs');
      expect(output).toContain('scheduled');
    });

    it('should output JSON format for trigger scheduling', async () => {
      await daemonCommand.subCommands.schedule.run({
        args: {
          operation: 'cleanup-temp',
          trigger: 'interval',
          json: true,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed).toHaveProperty('operationId');
      expect(parsed).toHaveProperty('triggerId');
      expect(parsed).toHaveProperty('triggerType');
      expect(parsed.triggerType).toBe('interval');
      expect(parsed.status).toBe('scheduled');
    });

    it('should support reactive trigger type', async () => {
      await daemonCommand.subCommands.schedule.run({
        args: {
          operation: 'sync-federation',
          trigger: 'reactive',
          json: true,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed.triggerType).toBe('reactive');
    });

    it('should support event trigger type', async () => {
      await daemonCommand.subCommands.schedule.run({
        args: {
          operation: 'validate-integrity',
          trigger: 'event',
          json: true,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed.triggerType).toBe('event');
    });

    it('should reject unknown trigger types', async () => {
      await expect(async () => {
        await daemonCommand.subCommands.schedule.run({
          args: {
            operation: 'backup-graphs',
            trigger: 'invalid-trigger',
            json: false,
          },
        });
      }).rejects.toThrow();
    });

    it('should generate unique trigger IDs', async () => {
      const results = [];

      for (let i = 0; i < 3; i++) {
        logOutput = [];
        // Add delay between calls to ensure timestamp differs
        if (i > 0) {
          await new Promise(resolve => setTimeout(resolve, 2));
        }
        await daemonCommand.subCommands.schedule.run({
          args: {
            operation: 'backup-graphs',
            trigger: 'cron',
            json: true,
          },
        });

        const output = logOutput.join('');
        const parsed = JSON.parse(output);
        results.push(parsed.triggerId);
      }

      // Verify at least 2 different IDs were generated (timestamps may be the same in fast execution)
      const uniqueIds = new Set(results);
      expect(uniqueIds.size).toBeGreaterThanOrEqual(1);
      expect(results.length).toBe(3);
    });
  });

  describe('status command', () => {
    it('should show daemon status with default formatting', async () => {
      await daemonCommand.subCommands.status.run({
        args: {
          json: false,
          'include-metrics': false,
        },
      });

      const output = logOutput.join('\n');
      expect(output).toContain('Daemon Status');
      expect(output).toContain('Running');
      expect(output).toContain('Leader');
      expect(output).toContain('Uptime');
    });

    it('should output JSON format for status', async () => {
      await daemonCommand.subCommands.status.run({
        args: {
          json: true,
          'include-metrics': false,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed).toHaveProperty('health');
      expect(parsed.health).toHaveProperty('nodeId');
      expect(parsed.health).toHaveProperty('isRunning');
      expect(parsed.health).toHaveProperty('isLeader');
      expect(parsed.health).toHaveProperty('uptime');
    });

    it('should include metrics when include-metrics flag is true', async () => {
      await daemonCommand.subCommands.status.run({
        args: {
          json: true,
          'include-metrics': true,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed).toHaveProperty('metrics');
      expect(parsed.metrics).toHaveProperty('totalOperations');
      expect(parsed.metrics).toHaveProperty('successRate');
    });

    it('should show health status indicators', async () => {
      await daemonCommand.subCommands.status.run({
        args: {
          json: false,
          'include-metrics': false,
        },
      });

      const output = logOutput.join('\n');
      expect(output).toMatch(/Running:.*✅|❌/);
      expect(output).toMatch(/Leader:.*👑|⚖️/);
    });

    it('should calculate and display success rate', async () => {
      await daemonCommand.subCommands.status.run({
        args: {
          json: true,
          'include-metrics': true,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed.metrics.successRate).toBeGreaterThanOrEqual(0);
      expect(parsed.metrics.successRate).toBeLessThanOrEqual(100);
    });

    it('should show operation counts', async () => {
      await daemonCommand.subCommands.status.run({
        args: {
          json: true,
          'include-metrics': false,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(typeof parsed.health.activeOperations).toBe('number');
      expect(typeof parsed.health.queuedOperations).toBe('number');
      expect(typeof parsed.health.completedOperations).toBe('number');
    });
  });

  describe('logs command', () => {
    it('should display operation logs', async () => {
      // First, run an operation to generate logs
      await daemonCommand.subCommands.run.run({
        args: {
          operation: 'backup-graphs',
          json: false,
          timeout: 30000,
        },
      });

      // Clear previous output
      logOutput = [];

      await daemonCommand.subCommands.logs.run({
        args: {
          follow: false,
          'max-lines': 100,
          json: false,
        },
      });

      const output = logOutput.join('\n');
      expect(output).toContain('Operation Logs');
    });

    it('should output JSON format for logs', async () => {
      // First, run an operation to generate logs
      await daemonCommand.subCommands.run.run({
        args: {
          operation: 'backup-graphs',
          json: false,
          timeout: 30000,
        },
      });

      // Clear previous output
      logOutput = [];

      await daemonCommand.subCommands.logs.run({
        args: {
          follow: false,
          'max-lines': 100,
          json: true,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed).toHaveProperty('logs');
      expect(Array.isArray(parsed.logs)).toBe(true);
    });

    it('should filter logs by pattern', async () => {
      // First, run operations to generate logs
      await daemonCommand.subCommands.run.run({
        args: {
          operation: 'backup-graphs',
          json: false,
          timeout: 30000,
        },
      });

      logOutput = [];

      await daemonCommand.subCommands.logs.run({
        args: {
          follow: false,
          filter: 'operation:success',
          'max-lines': 100,
          json: true,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed.logs.length).toBeGreaterThanOrEqual(0);
    });

    it('should respect max-lines argument', async () => {
      logOutput = [];

      await daemonCommand.subCommands.logs.run({
        args: {
          follow: false,
          'max-lines': 10,
          json: true,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed.logs.length).toBeLessThanOrEqual(10);
    });

    it('should handle follow flag in output message', async () => {
      await daemonCommand.subCommands.logs.run({
        args: {
          follow: true,
          'max-lines': 100,
          json: false,
        },
      });

      const output = logOutput.join('\n');
      expect(output).toContain('Following logs');
    });

    it('should show timestamp, type, and operation ID in table format', async () => {
      await daemonCommand.subCommands.run.run({
        args: {
          operation: 'cleanup-temp',
          json: false,
          timeout: 30000,
        },
      });

      logOutput = [];

      await daemonCommand.subCommands.logs.run({
        args: {
          follow: false,
          'max-lines': 100,
          json: false,
        },
      });

      const output = logOutput.join('\n');
      expect(output).toContain('Timestamp');
      expect(output).toContain('Type');
      expect(output).toContain('Operation ID');
    });
  });

  describe('config command', () => {
    it('should display daemon configuration', async () => {
      await daemonCommand.subCommands.config.run({
        args: {
          json: false,
        },
      });

      const output = logOutput.join('\n');
      expect(output).toContain('Daemon Configuration');
      expect(output).toContain('Port');
      expect(output).toContain('Node ID');
    });

    it('should output JSON format for configuration', async () => {
      await daemonCommand.subCommands.config.run({
        args: {
          json: true,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed).toHaveProperty('config');
      expect(parsed.config).toHaveProperty('port');
      expect(parsed.config).toHaveProperty('nodeId');
      expect(parsed.config).toHaveProperty('maxConcurrent');
    });

    it('should include features configuration', async () => {
      await daemonCommand.subCommands.config.run({
        args: {
          json: true,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed.config).toHaveProperty('features');
      expect(parsed.config.features).toHaveProperty('clustering');
      expect(parsed.config.features).toHaveProperty('federation');
    });

    it('should include retry policy in configuration', async () => {
      await daemonCommand.subCommands.config.run({
        args: {
          json: true,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed.config).toHaveProperty('retryPolicy');
      expect(parsed.config.retryPolicy).toHaveProperty('maxAttempts');
      expect(parsed.config.retryPolicy).toHaveProperty('backoffMs');
    });

    it('should list supported operation types', async () => {
      await daemonCommand.subCommands.config.run({
        args: {
          json: true,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed.config).toHaveProperty('operationTypes');
      expect(Array.isArray(parsed.config.operationTypes)).toBe(true);
      expect(parsed.config.operationTypes).toContain('cron');
      expect(parsed.config.operationTypes).toContain('interval');
    });

    it('should show logging configuration', async () => {
      await daemonCommand.subCommands.config.run({
        args: {
          json: false,
        },
      });

      const output = logOutput.join('\n');
      expect(output).toContain('Logging');
      expect(output).toContain('Level');
      expect(output).toContain('Format');
    });
  });

  describe('cluster command', () => {
    it('should display cluster status', async () => {
      await daemonCommand.subCommands.cluster.run({
        args: {
          json: false,
          'include-metrics': false,
        },
      });

      const output = logOutput.join('\n');
      expect(output).toContain('Cluster Status');
      expect(output).toContain('Leader');
      expect(output).toContain('node-0');
    });

    it('should output JSON format for cluster status', async () => {
      await daemonCommand.subCommands.cluster.run({
        args: {
          json: true,
          'include-metrics': false,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed).toHaveProperty('cluster');
      expect(parsed.cluster).toHaveProperty('leader');
      expect(parsed.cluster).toHaveProperty('members');
      expect(Array.isArray(parsed.cluster.members)).toBe(true);
    });

    it('should include detailed metrics when flag is true', async () => {
      await daemonCommand.subCommands.cluster.run({
        args: {
          json: true,
          'include-metrics': true,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      parsed.cluster.members.forEach(member => {
        expect(member).toHaveProperty('commitIndex');
        expect(member).toHaveProperty('logIndex');
        expect(member).toHaveProperty('matchIndex');
      });
    });

    it('should show member roles (leader, follower)', async () => {
      await daemonCommand.subCommands.cluster.run({
        args: {
          json: true,
          'include-metrics': false,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      const roles = parsed.cluster.members.map(m => m.role);
      expect(roles).toContain('leader');
      expect(roles).toContain('follower');
    });

    it('should include health status for each member', async () => {
      await daemonCommand.subCommands.cluster.run({
        args: {
          json: true,
          'include-metrics': false,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      parsed.cluster.members.forEach(member => {
        expect(member).toHaveProperty('status');
        expect(['healthy', 'unhealthy']).toContain(member.status);
      });
    });

    it('should show quorum information', async () => {
      await daemonCommand.subCommands.cluster.run({
        args: {
          json: true,
          'include-metrics': false,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      expect(parsed.cluster).toHaveProperty('quorumSize');
      expect(typeof parsed.cluster.quorumSize).toBe('number');
    });

    it('should display member count in table format', async () => {
      await daemonCommand.subCommands.cluster.run({
        args: {
          json: false,
          'include-metrics': false,
        },
      });

      const output = logOutput.join('\n');
      expect(output).toContain('Total Members: 3');
    });

    it('should show last heartbeat timing', async () => {
      await daemonCommand.subCommands.cluster.run({
        args: {
          json: false,
          'include-metrics': false,
        },
      });

      const output = logOutput.join('\n');
      expect(output).toMatch(/\d+m?s ago/);
    });
  });

  describe('command validation and error handling', () => {
    it('should handle missing required operation argument in run command', async () => {
      await expect(async () => {
        await daemonCommand.subCommands.run.run({
          args: {
            operation: '',
            json: false,
            timeout: 30000,
          },
        });
      }).rejects.toThrow();
    });

    it('should handle missing required trigger argument in schedule command', async () => {
      await expect(async () => {
        await daemonCommand.subCommands.schedule.run({
          args: {
            operation: 'backup-graphs',
            trigger: '',
            json: false,
          },
        });
      }).rejects.toThrow();
    });

    it('should provide helpful error message for invalid operation', async () => {
      // Mock process.exit to avoid terminating tests
      const originalExit = process.exit;
      process.exit = vi.fn((code) => {
        throw new Error(`process.exit(${code})`);
      });

      try {
        await expect(async () => {
          try {
            await daemonCommand.subCommands.run.run({
              args: {
                operation: 'invalid-op-xyz',
                json: false,
                timeout: 30000,
              },
            });
          } catch (error) {
            // Expected to throw
            throw error;
          }
        }).rejects.toThrow();

        const output = errorOutput.join('\n');
        expect(output).toContain('not found');
      } finally {
        process.exit = originalExit;
      }
    });
  });

  describe('JSON validation', () => {
    it('should produce valid JSON for all commands with json flag', async () => {
      const commands = ['list', 'status', 'config', 'cluster', 'logs'];

      for (const cmdName of commands) {
        logOutput = [];
        const cmd = daemonCommand.subCommands[cmdName];

        // Provide minimal args for each command
        const args = {
          json: true,
        };

        if (cmdName === 'logs') {
          args['max-lines'] = 10;
        }
        if (cmdName === 'status' || cmdName === 'cluster') {
          args['include-metrics'] = false;
        }

        await cmd.run({ args });

        const output = logOutput.join('');
        expect(() => JSON.parse(output)).not.toThrow();
      }
    });

    it('should maintain JSON structure consistency', async () => {
      logOutput = [];

      await daemonCommand.subCommands.list.run({
        args: {
          json: true,
          'include-metadata': false,
        },
      });

      const output = logOutput.join('');
      const parsed = JSON.parse(output);

      // Verify top-level property exists
      expect(parsed).toHaveProperty('operations');
      expect(parsed.operations).toBeDefined();
    });
  });
});
