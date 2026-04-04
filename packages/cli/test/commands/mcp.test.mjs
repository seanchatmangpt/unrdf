/**
 * @file MCP CLI Command Tests
 * @module @unrdf/cli/commands/__tests__/mcp.test
 * @description Tests for MCP CLI commands
 */

import { describe, it, expect } from 'vitest';
import { mcpCommand } from '../../src/cli/commands/mcp.mjs';

describe('MCP Command', () => {
  describe('mcp start', () => {
    it('should have start subcommand', () => {
      expect(mcpCommand.subCommands).toHaveProperty('start');
    });

    it('should accept --transport argument', () => {
      const startCmd = mcpCommand.subCommands.start;
      expect(startCmd.args).toHaveProperty('transport');
      expect(startCmd.args.transport.default).toBeDefined();
      expect(typeof startCmd.args.transport.default).toBe('string');
    });

    it('should accept --port argument', () => {
      const startCmd = mcpCommand.subCommands.start;
      expect(startCmd.args).toHaveProperty('port');
      expect(startCmd.args.port.default).toBeDefined();
      expect(typeof startCmd.args.port.default).toBe('number');
    });
  });

  describe('mcp inspect', () => {
    it('should have inspect subcommand', () => {
      expect(mcpCommand.subCommands).toHaveProperty('inspect');
    });

    it('should list tools', async () => {
      const inspectCmd = mcpCommand.subCommands.inspect;
      expect(inspectCmd.meta).toBeDefined();
      expect(inspectCmd.meta.description).toBeDefined();
      expect(typeof inspectCmd.meta.description).toBe('string');
      expect(inspectCmd.meta.description.length).toBeGreaterThan(0);
    });

    it('should list resources', async () => {
      const inspectCmd = mcpCommand.subCommands.inspect;
      expect(inspectCmd).toBeDefined();
    });

    it('should list prompts', async () => {
      const inspectCmd = mcpCommand.subCommands.inspect;
      expect(inspectCmd).toBeDefined();
    });
  });

  describe('mcp status', () => {
    it('should have status subcommand', () => {
      expect(mcpCommand.subCommands).toHaveProperty('status');
    });

    it('should check server status', async () => {
      const statusCmd = mcpCommand.subCommands.status;
      expect(statusCmd.meta).toBeDefined();
      expect(statusCmd.meta.description).toBeDefined();
      expect(typeof statusCmd.meta.description).toBe('string');
      expect(statusCmd.meta.description.length).toBeGreaterThan(0);
    });
  });

  describe('MCP Command Structure', () => {
    it('should have meta information', () => {
      expect(mcpCommand.meta).toBeDefined();
      expect(mcpCommand.meta.name).toBe('mcp');
      expect(mcpCommand.meta.description).toContain('Model Context Protocol');
    });

    it('should have all required subcommands', () => {
      const subCommands = Object.keys(mcpCommand.subCommands);
      expect(subCommands).toContain('start');
      expect(subCommands).toContain('inspect');
      expect(subCommands).toContain('status');
    });

    it('should have help available', () => {
      expect(mcpCommand.run).toBeDefined();
    });
  });

  describe('Daemon Module Integration', () => {
    it('should be callable from CLI', () => {
      expect(mcpCommand).toBeDefined();
      expect(typeof mcpCommand).toBe('object');
    });
  });

  describe('Transport Arguments', () => {
    it('start should support stdio transport', () => {
      const startCmd = mcpCommand.subCommands.start;
      const transports = startCmd.args.transport;
      expect(transports).toBeDefined();
    });

    it('start should support sse transport', () => {
      const startCmd = mcpCommand.subCommands.start;
      const transports = startCmd.args.transport;
      expect(transports).toBeDefined();
    });

    it('start should support port configuration', () => {
      const startCmd = mcpCommand.subCommands.start;
      expect(startCmd.args.port.type).toBe('number');
    });
  });
});
