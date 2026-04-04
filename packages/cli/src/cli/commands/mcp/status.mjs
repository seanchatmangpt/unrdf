/**
 * @file Status MCP Server Command
 * @module cli/commands/mcp/status
 */

import { defineCommand } from 'citty';

export const statusCommand = defineCommand({
  meta: {
    name: 'status',
    description: 'Show if MCP server is running',
  },
  args: {
    json: {
      type: 'boolean',
      description: 'Output as JSON',
    },
  },
  async run({ args }) {
    try {
      const { getMCPServerStatus } = await import('@unrdf/daemon/mcp');
      const status = await getMCPServerStatus();

      if (args.json) {
        console.log(JSON.stringify(status, null, 2));
      } else {
        console.log('\n⚡ MCP Server Status');
        console.log('═'.repeat(50));

        if (status.running) {
          console.log(`Status: ✅ Running`);
          console.log(`Transport: ${status.transport}`);
          if (status.transport === 'sse') {
            console.log(`Port: ${status.port}`);
            console.log(`URL: http://localhost:${status.port}`);
          }
          console.log(`Process ID: ${status.pid}`);
          console.log(`Uptime: ${status.uptime}`);
        } else {
          console.log(`Status: ❌ Not Running`);
        }

        console.log('═'.repeat(50));
      }
    } catch (error) {
      console.error(`❌ Error checking MCP server status: ${error.message}`);
      process.exit(1);
    }
  },
});
