/**
 * @file Policy Test Command
 * @architecture CLI → Domain Service → Package
 *
 * Test a policy pack by executing all its hooks against sample data
 */

import { defineCommand } from 'citty';
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { getHookService } from '../../domain/index.mjs';
import { dataFactory } from '@unrdf/oxigraph';

const { quad, namedNode, literal } = dataFactory;

export const testCommand = defineCommand({
  meta: {
    name: 'test',
    description: 'Test policy pack against sample data'
  },
  args: {
    file: {
      type: 'positional',
      description: 'Policy pack file',
      required: true
    },
    'dry-run': {
      type: 'boolean',
      description: 'Show what would be tested without executing'
    }
  },
  async run(ctx) {
    try {
      const { file } = ctx.args;
      const dryRun = ctx.args['dry-run'] || false;
      const hookService = getHookService();

      // Read policy pack file
      const policyPath = resolve(file);
      let policyContent;
      try {
        policyContent = await readFile(policyPath, 'utf-8');
      } catch (error) {
        if (error.code === 'ENOENT') {
          console.error(`\n❌ Policy file not found: ${file}`);
          console.error(`\nPlease provide a valid policy pack file path.`);
          process.exit(1);
        }
        throw error;
      }

      // Parse policy pack
      let policy;
      try {
        policy = JSON.parse(policyContent);
      } catch (error) {
        console.error(`\n❌ Invalid JSON in policy file: ${error.message}`);
        process.exit(1);
      }

      // Validate policy structure
      if (!policy.hooks || !Array.isArray(policy.hooks)) {
        console.error(`\n❌ Policy pack must have a 'hooks' array`);
        console.error(`\nExample:`);
        console.error(`{`);
        console.error(`  "name": "my-policy",`);
        console.error(`  "hooks": [`);
        console.error(`    { "id": "hook1", "name": "hook1", "trigger": "before-add", ... }`);
        console.error(`  ]`);
        console.error(`}`);
        process.exit(1);
      }

      console.log(`\n🧪 Testing Policy Pack: ${policy.name || 'Unnamed'}`);
      console.log(`${'═'.repeat(60)}`);
      console.log(`File:        ${policyPath}`);
      console.log(`Version:     ${policy.version || '1.0.0'}`);
      console.log(`Hooks:       ${policy.hooks.length}`);
      console.log(`Dry Run:     ${dryRun ? 'Yes' : 'No'}`);
      console.log('');

      if (dryRun) {
        console.log(`📋 Hooks that would be tested:\n`);
        policy.hooks.forEach((hook, idx) => {
          console.log(`   ${idx + 1}. ${hook.name || hook.id || `hook-${idx + 1}`}`);
          console.log(`      Trigger: ${hook.trigger || 'N/A'}`);
          console.log(`      Enabled: ${hook.enabled !== false ? 'Yes' : 'No'}`);
        });
        console.log('');
        return;
      }

      // Test each hook
      const results = [];
      let passedCount = 0;
      let failedCount = 0;
      let skippedCount = 0;

      console.log(`🔍 Executing Hooks:\n`);

      for (const [idx, hook] of policy.hooks.entries()) {
        const hookName = hook.name || hook.id || `hook-${idx + 1}`;

        // Skip disabled hooks
        if (hook.enabled === false) {
          console.log(`   ${idx + 1}. ${hookName} [SKIPPED - disabled]`);
          skippedCount++;
          results.push({ hook: hookName, status: 'skipped', reason: 'disabled' });
          continue;
        }

        // Register hook temporarily
        try {
          await hookService.registerHook({
            id: hook.id || hookName,
            name: hookName,
            description: hook.description || 'Test hook',
            version: hook.version || '1.0.0',
            trigger: hook.trigger || 'before-add',
            enabled: true,
            validate: hook.validate || (() => ({ valid: true }))
          });

          // Create sample test quad based on trigger
          const testQuad = quad(
            namedNode('http://example.org/subject'),
            namedNode('http://example.org/predicate'),
            literal('test-value')
          );

          // Execute hook
          const result = await hookService.executeHook({
            hookId: hook.id || hookName,
            data: testQuad,
            context: { test: true }
          });

          if (result.valid !== false) {
            console.log(`   ${idx + 1}. ${hookName} [✅ PASS]`);
            passedCount++;
            results.push({ hook: hookName, status: 'pass' });
          } else {
            console.log(`   ${idx + 1}. ${hookName} [❌ FAIL]`);
            if (result.errors && result.errors.length > 0) {
              result.errors.forEach(err => {
                console.log(`      Error: ${err.message || err}`);
              });
            }
            failedCount++;
            results.push({ hook: hookName, status: 'fail', errors: result.errors });
          }

          // Unregister hook after test
          await hookService.unregisterHook(hook.id || hookName);
        } catch (error) {
          console.log(`   ${idx + 1}. ${hookName} [❌ ERROR]`);
          console.log(`      ${error.message}`);
          failedCount++;
          results.push({ hook: hookName, status: 'error', error: error.message });
        }
      }

      // Summary
      console.log(`\n${'═'.repeat(60)}`);
      console.log(`\n📊 Test Summary:`);
      console.log(`   Total Hooks:    ${policy.hooks.length}`);
      console.log(`   ✅ Passed:      ${passedCount}`);
      console.log(`   ❌ Failed:      ${failedCount}`);
      console.log(`   ⏸️  Skipped:     ${skippedCount}`);
      console.log(`   Success Rate:   ${policy.hooks.length > 0 ? Math.round((passedCount / (passedCount + failedCount)) * 100) : 0}%`);
      console.log('');

      // Exit with appropriate code
      if (failedCount > 0) {
        console.error(`❌ Some hooks failed. Review the errors above.`);
        process.exit(1);
      } else {
        console.log(`✅ All hooks passed!`);
      }
    } catch (error) {
      console.error(`❌ Failed to test policy: ${error.message}`);
      process.exit(1);
    }
  }
});
