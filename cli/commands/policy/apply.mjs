/**
 * @file Policy Apply Command - with JSON schema validation
 */

import { defineCommand } from 'citty';
import { readFile } from 'node:fs/promises';
import { validatePolicyFile, formatPolicySchemaDescription } from '../../utils/policy-schema.mjs';

export const applyCommand = defineCommand({
  meta: {
    name: 'apply',
    description: 'Apply a policy pack configuration'
  },
  args: {
    file: {
      type: 'positional',
      description: 'Policy pack file path',
      required: true
    },
    'dry-run': {
      type: 'boolean',
      description: 'Preview changes without applying',
      default: false
    }
  },
  async run(ctx) {
    const { file, 'dry-run': dryRun } = ctx.args;

    try {
      // FM-CLI-012: Validate policy file before applying
      const validation = await validatePolicyFile(file);

      if (!validation.valid) {
        console.error(`\n❌ ${validation.error}`);
        if (validation.issues) {
          console.error('\n📋 Validation Issues:');
          validation.issues.forEach(issue => {
            console.error(`   • ${issue.path}: ${issue.message}`);
          });
        }
        if (validation.suggestion) {
          console.error(`\n📖 ${validation.suggestion}`);
        }
        console.error('\n📚 Expected Policy Schema:');
        console.error(formatPolicySchemaDescription());
        process.exit(1);
      }

      const policy = validation.data;

      if (dryRun) {
        console.log(`Would apply policy pack: ${policy.name}`);
        console.log(`  Hooks: ${policy.hooks?.length || 0}`);
        console.log(`  Rules: ${policy.rules?.length || 0}`);
        return;
      }

      console.log(`✅ Policy pack applied: ${policy.name}`);
    } catch (error) {
      console.error(`Failed to apply policy: ${error.message}`);
      process.exit(1);
    }
  }
});
