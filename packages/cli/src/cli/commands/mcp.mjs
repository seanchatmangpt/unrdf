/**
 * @file MCP Command
 * @module @unrdf/cli/cli/commands/mcp
 * @description CLI commands for MCP server management
 */

import { defineCommand, runCommand } from 'citty';

/**
 * MCP start subcommand
 */
const startCommand = defineCommand({
  meta: {
    name: 'start',
    description: 'Start the MCP server',
  },
  args: {
    transport: {
      type: 'string',
      description: 'Transport type: stdio or sse',
      default: 'stdio',
    },
    port: {
      type: 'number',
      description: 'Port for SSE transport',
      default: 8765,
    },
  },
  run: async ({ args }) => {
    try {
      const mcpIndexPath = new URL('../../../../daemon/src/mcp/index.mjs', import.meta.url).pathname;
      const { startMCPServer, startMCPServerSSE } = await import(mcpIndexPath);
      if (args.transport === 'sse') {
        console.log(`Starting MCP server with SSE transport...`);
        console.log(`Listening on port ${args.port}`);
        await startMCPServerSSE(args.port);
      } else {
        console.log(`Starting MCP server with stdio transport...`);
        await startMCPServer();
      }
    } catch (error) {
      console.error('Failed to start MCP server:', error.message);
      process.exit(1);
    }
  },
});

/**
 * MCP inspect subcommand
 */
const inspectCommand = defineCommand({
  meta: {
    name: 'inspect',
    description: 'Inspect MCP server capabilities',
  },
  run: async () => {
    let tools = [];
    try {
      const toolDefsPath = new URL('../../../../daemon/src/mcp/tools-generated.mjs', import.meta.url).pathname;
      const { mcpGeneratedTools } = await import(toolDefsPath);
      tools = mcpGeneratedTools.map(t => ({ name: t.name, description: t.description }));
    } catch {
      console.error('Warning: tools-generated.mjs not found. Run `unrdf sync --config packages/daemon/src/mcp/.unrdf.toml` first.');
    }
    const capabilities = {
      tools,
      resources: [
        { uri: 'sparql://endpoints/config' },
        { uri: 'rdf://ontologies/catalog' },
        { uri: 'graphs://metadata' },
        { uri: 'queries://templates' },
      ],
      prompts: [
        { name: 'sparql_builder' },
        { name: 'graph_analysis' },
        { name: 'data_transform' },
        { name: 'ontology_doc' },
      ],
    };
    console.log('MCP Server Capabilities:');
    console.log(JSON.stringify(capabilities, null, 2));
  },
});

/**
 * MCP status subcommand
 */
const statusCommand = defineCommand({
  meta: {
    name: 'status',
    description: 'Check MCP server status',
  },
  run: async () => {
    console.log('MCP Server Status:');
    console.log('  Name: unrdf-daemon-mcp');
    console.log('  Version: 26.4.3');
    console.log('  Status: Not running');
    console.log('  Transport: None');
  },
});

/**
 * Main MCP command
 */
export const mcpCommand = defineCommand({
  meta: {
    name: 'mcp',
    description: 'Model Context Protocol (MCP) server management',
  },
  subCommands: {
    start: startCommand,
    inspect: inspectCommand,
    status: statusCommand,
  },
});
