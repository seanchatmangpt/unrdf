/**
 * @file Policy Validate Command - Comprehensive validation
 */

import { defineCommand } from 'citty';
import { validatePolicyFile } from '../../utils/policy-schema.mjs';

export const validateCommand = defineCommand({
  meta: {
    name: 'validate',
    description: 'Validate policy pack configuration'
  },
  args: {
    file: {
      type: 'positional',
      description: 'Policy pack file path',
      required: true
    }
  },
  async run(ctx) {
    const { file } = ctx.args;

    try {
      const validation = await validatePolicyFile(file);

      if (!validation.valid) {
        console.error(`\n❌ Policy validation failed`);
        if (validation.issues) {
          console.error('\n📋 Validation Issues:');
          validation.issues.forEach(issue => {
            console.error(`   • ${issue.path}: ${issue.message}`);
          });
        }
        process.exit(1);
      }

      const policy = validation.data;

      // Success - show policy details
      console.log(`\n✅ Policy is valid: ${policy.name}`);
      console.log(`\n📋 Policy Details:`);
      console.log(`   Name:        ${policy.name}`);
      console.log(`   Version:     ${policy.version || '1.0.0'}`);
      console.log(`   Enabled:     ${policy.enabled !== false ? 'Yes' : 'No'}`);
      console.log(`   Hooks:       ${policy.hooks?.length || 0}`);
      console.log(`   Rules:       ${policy.rules?.length || 0}`);

      if (policy.hooks && policy.hooks.length > 0) {
        console.log(`\n🪝 Hooks:`);
        policy.hooks.forEach(hook => {
          console.log(`   • ${hook.name} (${hook.type})`);
        });
      }

      if (policy.rules && policy.rules.length > 0) {
        console.log(`\n📏 Rules:`);
        policy.rules.forEach(rule => {
          console.log(`   • ${rule.id}: ${rule.action}`);
        });
      }

      console.log('');
    } catch (error) {
      console.error(`\n❌ Validation failed: ${error.message}`);
      process.exit(1);
    }
  }
});
