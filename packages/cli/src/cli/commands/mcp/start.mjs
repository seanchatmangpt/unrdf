/**
 * @file Start MCP Server Command
 * @module cli/commands/mcp/start
 */

import { defineCommand } from 'citty';

export const startCommand = defineCommand({
  meta: {
    name: 'start',
    description: 'Start the MCP server',
  },
  args: {
    transport: {
      type: 'string',
      default: 'stdio',
      description: 'Transport type: stdio or sse',
    },
    port: {
      type: 'number',
      default: 3001,
      description: 'Port for SSE transport (when transport=sse)',
    },
  },
  async run({ args }) {
    try {
      const { startMCPServer } = await import('@unrdf/daemon/mcp');
      const transport = args.transport || 'stdio';
      const port = args.port || 3001;

      if (!['stdio', 'sse'].includes(transport)) {
        console.error(`❌ Invalid transport: ${transport}. Must be 'stdio' or 'sse'`);
        process.exit(1);
      }

      console.log(`Starting MCP server...`);
      const server = await startMCPServer({
        transport,
        port: transport === 'sse' ? port : undefined,
      });

      if (transport === 'sse') {
        console.log(`✅ MCP server started on SSE transport at http://localhost:${port}`);
      } else {
        console.log(`✅ MCP server started on stdio transport`);
      }

      // Keep the process running
      await new Promise(() => {
        // Never resolves - keeps process alive
      });
    } catch (error) {
      console.error(`❌ Error starting MCP server: ${error.message}`);
      process.exit(1);
    }
  },
});
