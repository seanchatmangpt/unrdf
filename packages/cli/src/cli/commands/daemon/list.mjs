/**
 * @file List Operations Command
 * @module cli/commands/daemon/list
 */

import { defineCommand } from 'citty';
import { ListArgsSchema } from './schemas.mjs';
import { initializeRegistry, operationRegistry, formatOperation } from './helpers.mjs';

export const listCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'List all configured operations',
  },
  args: {
    json: {
      type: 'boolean',
      description: 'Output as JSON',
    },
    'include-metadata': {
      type: 'boolean',
      description: 'Include metadata in output',
    },
  },
  async run({ args }) {
    try {
      const validated = ListArgsSchema.parse(args);
      initializeRegistry();

      const operations = Array.from(operationRegistry.values()).map(formatOperation);

      if (validated.json) {
        console.log(JSON.stringify({ operations }, null, 2));
      } else {
        console.log('\n📋 Configured Operations');
        console.log('═'.repeat(70));
        console.log(`${'ID'.padEnd(25)} ${'Name'.padEnd(30)} ${'Status'.padEnd(15)}`);
        console.log('─'.repeat(70));

        operations.forEach(op => {
          console.log(
            `${op.id.padEnd(25)} ${(op.name || '-').padEnd(30)} ${op.status.padEnd(15)}`
          );
        });

        console.log('─'.repeat(70));
        console.log(`Total Operations: ${operations.length}`);
        console.log('═'.repeat(70));

        if (validated['include-metadata']) {
          console.log('\n📋 Metadata:');
          operations.forEach(op => {
            if (Object.keys(op.metadata).length > 0) {
              console.log(`  ${op.id}:`, JSON.stringify(op.metadata, null, 4));
            }
          });
        }
      }
    } catch (error) {
      console.error(`❌ Error listing operations: ${error.message}`);
      process.exit(1);
    }
  },
});
