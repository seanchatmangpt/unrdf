/**
 * @file MCP Command - Model Context Protocol Server Management
 * @module cli/commands/mcp
 * @description CLI commands for managing MCP (Model Context Protocol) server operations
 *
 * Provides comprehensive MCP server management:
 * - start: Start the MCP server with configurable transport (stdio/SSE) and port
 * - status: Check if MCP server is running and on which transport
 * - inspect: List all exposed tools, resources, and prompts
 * - stop: Stop the running MCP server
 */

import { defineCommand } from 'citty';
import { startCommand } from './mcp/start.mjs';
import { statusCommand } from './mcp/status.mjs';
import { inspectCommand } from './mcp/inspect.mjs';
import { stopCommand } from './mcp/stop.mjs';
import { selfPlayCommand } from './mcp/self-play.mjs';

/**
 * Main MCP command
 */
export const mcpCommand = defineCommand({
  meta: {
    name: 'mcp',
    description: 'Manage MCP (Model Context Protocol) server lifecycle',
  },
  subCommands: {
    start: startCommand,
    status: statusCommand,
    inspect: inspectCommand,
    stop: stopCommand,
    'self-play': selfPlayCommand,
  },
});
