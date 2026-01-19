/**
 * @file Daemon CLI Tests
 * @module cli/test/daemon-cli
 * @description Test suite for daemon CLI command structure and exports
 */

import { describe, it, expect } from 'vitest';
import { daemonCommand } from '../src/cli/commands/daemon.mjs';

describe('Daemon CLI Commands', () => {
  describe('command structure', () => {
    it('should export daemonCommand', () => {
      expect(daemonCommand).toBeDefined();
    });

    it('should have proper meta information', () => {
      expect(daemonCommand.meta).toBeDefined();
      expect(daemonCommand.meta.name).toBe('daemon');
      expect(daemonCommand.meta.description).toContain('background operations');
    });

    it('should define all required subcommands', () => {
      expect(daemonCommand.subCommands).toBeDefined();
      expect(daemonCommand.subCommands.list).toBeDefined();
      expect(daemonCommand.subCommands.run).toBeDefined();
      expect(daemonCommand.subCommands.schedule).toBeDefined();
      expect(daemonCommand.subCommands.status).toBeDefined();
      expect(daemonCommand.subCommands.logs).toBeDefined();
      expect(daemonCommand.subCommands.config).toBeDefined();
      expect(daemonCommand.subCommands.cluster).toBeDefined();
    });

    it('should have all subcommands with meta', () => {
      const subcommands = [
        'list',
        'run',
        'schedule',
        'status',
        'logs',
        'config',
        'cluster',
      ];

      for (const cmd of subcommands) {
        expect(daemonCommand.subCommands[cmd]).toBeDefined();
        expect(daemonCommand.subCommands[cmd].meta).toBeDefined();
        expect(daemonCommand.subCommands[cmd].meta.name).toBe(cmd);
      }
    });

    it('list command should be properly defined', () => {
      const listCmd = daemonCommand.subCommands.list;
      expect(listCmd.meta.name).toBe('list');
      expect(listCmd.meta.description).toBeDefined();
    });

    it('run command should be properly defined', () => {
      const runCmd = daemonCommand.subCommands.run;
      expect(runCmd.meta.name).toBe('run');
      expect(runCmd.meta.description).toBeDefined();
    });

    it('schedule command should be properly defined', () => {
      const scheduleCmd = daemonCommand.subCommands.schedule;
      expect(scheduleCmd.meta.name).toBe('schedule');
      expect(scheduleCmd.meta.description).toBeDefined();
    });

    it('status command should be properly defined', () => {
      const statusCmd = daemonCommand.subCommands.status;
      expect(statusCmd.meta.name).toBe('status');
      expect(statusCmd.meta.description).toBeDefined();
    });

    it('logs command should be properly defined', () => {
      const logsCmd = daemonCommand.subCommands.logs;
      expect(logsCmd.meta.name).toBe('logs');
      expect(logsCmd.meta.description).toBeDefined();
    });

    it('config command should be properly defined', () => {
      const configCmd = daemonCommand.subCommands.config;
      expect(configCmd.meta.name).toBe('config');
      expect(configCmd.meta.description).toBeDefined();
    });

    it('cluster command should be properly defined', () => {
      const clusterCmd = daemonCommand.subCommands.cluster;
      expect(clusterCmd.meta.name).toBe('cluster');
      expect(clusterCmd.meta.description).toBeDefined();
    });
  });

  describe('command availability', () => {
    it('should have at least 7 subcommands', () => {
      const subcommandCount = Object.keys(daemonCommand.subCommands).length;
      expect(subcommandCount).toBeGreaterThanOrEqual(7);
    });

    it('all subcommands should have handlers', () => {
      for (const [_name, cmd] of Object.entries(daemonCommand.subCommands)) {
        expect(cmd.handler || cmd.run).toBeDefined();
      }
    });
  });
});
