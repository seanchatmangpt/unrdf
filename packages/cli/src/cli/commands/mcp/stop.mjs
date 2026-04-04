/**
 * @file Stop MCP Server Command
 * @module cli/commands/mcp/stop
 */

import { defineCommand } from 'citty';

export const stopCommand = defineCommand({
  meta: {
    name: 'stop',
    description: 'Stop the running MCP server',
  },
  args: {
    json: {
      type: 'boolean',
      description: 'Output as JSON',
    },
  },
  async run({ args }) {
    try {
      const { stopMCPServer } = await import('@unrdf/daemon/mcp');
      const result = await stopMCPServer();

      if (args.json) {
        console.log(JSON.stringify(result, null, 2));
      } else {
        if (result.stopped) {
          console.log(`✅ MCP server stopped`);
        } else {
          console.log(`ℹ️  MCP server was not running`);
        }
      }
    } catch (error) {
      console.error(`❌ Error stopping MCP server: ${error.message}`);
      process.exit(1);
    }
  },
});
