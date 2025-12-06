/**
 * @file Policy List Command
 *
 * Lists all policy packs from ~/.unrdf/policies/ directory
 */

import { defineCommand } from 'citty';
import { readdir, readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { homedir } from 'node:os';
import { formatOutput } from '../../formatters/index.mjs';

export const listCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'List all policy packs'
  },
  args: {
    output: {
      type: 'string',
      description: 'Output format (table, json, csv)',
      default: 'table'
    },
    active: {
      type: 'boolean',
      description: 'Show only active policy packs',
      default: false
    }
  },
  async run(ctx) {
    try {
      const policiesDir = join(homedir(), '.unrdf', 'policies');

      let policies = [];
      try {
        const files = await readdir(policiesDir);
        const jsonFiles = files.filter(f => f.endsWith('.json'));

        for (const file of jsonFiles) {
          try {
            const filePath = join(policiesDir, file);
            const content = await readFile(filePath, 'utf-8');
            const policy = JSON.parse(content);

            policies.push({
              name: policy.name || file.replace('.json', ''),
              hooks: policy.hooks?.length || 0,
              active: policy.enabled !== false
            });
          } catch (error) {
            // Skip invalid policy files
            console.warn(`⚠️  Skipping invalid policy file: ${file}`);
          }
        }
      } catch (error) {
        if (error.code === 'ENOENT') {
          console.log('📋 No policies found.');
          console.log('\n💡 Create a policy file:');
          console.log('   1. Create ~/.unrdf/policies/ directory');
          console.log('   2. Add policy files: <name>.json');
          console.log('\n📚 Or use: unrdf policy apply <file>');
          return;
        }
        throw error;
      }

      // Filter by active if requested
      if (ctx.args.active) {
        policies = policies.filter(p => p.active);
      }

      if (policies.length === 0) {
        console.log('📋 No policies found' + (ctx.args.active ? ' (matching filters)' : '') + '.');
        return;
      }

      const output = formatOutput(policies, ctx.args.output, {
        columns: ['name', 'hooks', 'active'],
        headers: ['NAME', 'HOOKS', 'ACTIVE']
      });

      console.log(output);
    } catch (error) {
      console.error(`❌ Failed to list policies: ${error.message}`);
      process.exit(1);
    }
  }
});
