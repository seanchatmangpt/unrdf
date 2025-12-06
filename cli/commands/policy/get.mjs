/**
 * @file Policy Get Command
 *
 * Alias for policy describe - provides same functionality
 */

import { defineCommand } from 'citty';
import { readdir, readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { homedir } from 'node:os';

export const getCommand = defineCommand({
  meta: {
    name: 'get',
    description: 'Get policy pack details (alias for describe)'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Policy pack name',
      required: true
    }
  },
  async run(ctx) {
    try {
      const { name } = ctx.args;
      const policiesDir = join(homedir(), '.unrdf', 'policies');
      const policyPath = join(policiesDir, `${name}.json`);

      let policy;
      try {
        const content = await readFile(policyPath, 'utf-8');
        policy = JSON.parse(content);
      } catch (error) {
        if (error.code === 'ENOENT') {
          console.error(`\n❌ Policy not found: ${name}`);

          // List available policies
          try {
            const files = await readdir(policiesDir);
            const jsonFiles = files.filter(f => f.endsWith('.json'));

            if (jsonFiles.length > 0) {
              console.error(`\n📋 Available policies:`);
              for (const file of jsonFiles) {
                const policyName = file.replace('.json', '');
                console.error(`   • ${policyName}`);
              }
            }
          } catch {
            // Ignore
          }

          console.error('');
          process.exit(1);
        }
        throw error;
      }

      // Display policy details (same as describe, but more concise)
      console.log(`\n📋 Policy Pack: ${policy.name || name}`);
      console.log(`${'═'.repeat(60)}`);
      console.log(`Version:       ${policy.version || '1.0.0'}`);
      console.log(`Description:   ${policy.description || 'N/A'}`);
      console.log(`Enabled:       ${policy.enabled !== false ? 'Yes' : 'No'}`);
      console.log(`Hooks:         ${policy.hooks?.length || 0}`);
      console.log(`Rules:         ${policy.rules?.length || 0}`);
      console.log('');
    } catch (error) {
      console.error(`❌ Failed to get policy: ${error.message}`);
      process.exit(1);
    }
  }
});
