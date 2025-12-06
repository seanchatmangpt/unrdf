/**
 * @file Policy Describe Command
 *
 * Shows detailed policy information from ~/.unrdf/policies/{name}.json
 */

import { defineCommand } from 'citty';
import { readdir, readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { homedir } from 'node:os';

export const describeCommand = defineCommand({
  meta: {
    name: 'describe',
    description: 'Describe a policy pack in detail'
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
            } else {
              console.error(`\n📋 No policies found in ${policiesDir}`);
            }
          } catch {
            console.error(`\n📋 Policy directory not found: ${policiesDir}`);
          }

          console.error('');
          process.exit(1);
        }
        throw error;
      }

      // Display full policy details
      console.log(`\n📋 Policy Pack: ${policy.name || name}`);
      console.log(`${'═'.repeat(60)}`);
      console.log(`Version:       ${policy.version || '1.0.0'}`);
      console.log(`Description:   ${policy.description || 'N/A'}`);
      console.log(`Status:        ${policy.status === 'active' ? '✅ Active' : '⏸️  Inactive'}`);
      console.log(`Enabled:       ${policy.enabled !== false ? 'Yes' : 'No'}`);

      if (policy.appliedAt) {
        console.log(`Applied:       ${new Date(policy.appliedAt).toLocaleString()}`);
      }

      if (policy.hooks && policy.hooks.length > 0) {
        console.log(`\n🪝 Hooks (${policy.hooks.length}):`);
        policy.hooks.forEach((hook, idx) => {
          console.log(`   ${idx + 1}. ${hook.name}`);
          console.log(`      Type: ${hook.type || 'N/A'}`);
        });
      }

      if (policy.rules && policy.rules.length > 0) {
        console.log(`\n📏 Rules (${policy.rules.length}):`);
        policy.rules.forEach((rule, idx) => {
          console.log(`   ${idx + 1}. ${rule.id || `rule-${idx + 1}`}`);
          console.log(`      Pattern: ${rule.pattern || 'N/A'}`);
          console.log(`      Action:  ${rule.action || 'N/A'}`);
        });
      }

      console.log(`\n📊 Impact Summary:`);
      console.log(`   Hooks configured: ${policy.hooks?.length || 0}`);
      console.log(`   Rules configured: ${policy.rules?.length || 0}`);
      console.log(`   Resources protected: ${(policy.hooks?.length || 0) + (policy.rules?.length || 0)}`);
      console.log('');
    } catch (error) {
      console.error(`❌ Failed to describe policy: ${error.message}`);
      process.exit(1);
    }
  }
});
