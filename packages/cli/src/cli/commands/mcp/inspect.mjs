/**
 * @file Inspect MCP Server Command
 * @module cli/commands/mcp/inspect
 */

import { defineCommand } from 'citty';

export const inspectCommand = defineCommand({
  meta: {
    name: 'inspect',
    description: 'List all exposed tools, resources, and prompts',
  },
  args: {
    json: {
      type: 'boolean',
      description: 'Output as JSON',
    },
  },
  async run({ args }) {
    try {
      const { inspectMCPServer } = await import('@unrdf/daemon/mcp');
      const capabilities = await inspectMCPServer();

      if (args.json) {
        console.log(JSON.stringify(capabilities, null, 2));
      } else {
        console.log('\n📋 MCP Server Capabilities');
        console.log('═'.repeat(70));

        // Tools
        if (capabilities.tools && capabilities.tools.length > 0) {
          console.log('\n🛠️  Tools');
          console.log('─'.repeat(70));
          console.log(`${'Name'.padEnd(30)} ${'Description'.padEnd(40)}`);
          console.log('─'.repeat(70));
          capabilities.tools.forEach(tool => {
            const desc = (tool.description || '-').substring(0, 39);
            console.log(`${tool.name.padEnd(30)} ${desc.padEnd(40)}`);
          });
        }

        // Resources
        if (capabilities.resources && capabilities.resources.length > 0) {
          console.log('\n📚 Resources');
          console.log('─'.repeat(70));
          console.log(`${'URI'.padEnd(30)} ${'Name'.padEnd(40)}`);
          console.log('─'.repeat(70));
          capabilities.resources.forEach(resource => {
            console.log(`${resource.uri.padEnd(30)} ${(resource.name || '-').padEnd(40)}`);
          });
        }

        // Prompts
        if (capabilities.prompts && capabilities.prompts.length > 0) {
          console.log('\n💬 Prompts');
          console.log('─'.repeat(70));
          console.log(`${'Name'.padEnd(30)} ${'Description'.padEnd(40)}`);
          console.log('─'.repeat(70));
          capabilities.prompts.forEach(prompt => {
            const desc = (prompt.description || '-').substring(0, 39);
            console.log(`${prompt.name.padEnd(30)} ${desc.padEnd(40)}`);
          });
        }

        console.log('═'.repeat(70));
        console.log(`\nTotal: ${(capabilities.tools?.length || 0)} tools, ${(capabilities.resources?.length || 0)} resources, ${(capabilities.prompts?.length || 0)} prompts`);
      }
    } catch (error) {
      console.error(`❌ Error inspecting MCP server: ${error.message}`);
      process.exit(1);
    }
  },
});
