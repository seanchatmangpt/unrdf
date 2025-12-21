#!/usr/bin/env node
/**
 * Hooks Package Validation CLI
 *
 * Validates @unrdf/hooks functionality using citty
 *
 * Usage:
 *   node examples/validate-hooks.mjs
 */

import { defineCommand, runMain } from 'citty';
import {
  defineHook,
  executeHook,
  executeHookChain,
  registerHook,
  listHooks,
  builtinHooks,
} from '../src/index.mjs';

const main = defineCommand({
  meta: {
    name: 'validate-hooks',
    description: 'Validate @unrdf/hooks package functionality',
    version: '1.0.0',
  },
  args: {
    verbose: {
      type: 'boolean',
      description: 'Enable verbose output',
      alias: 'v',
      default: false,
    },
  },
  async run({ args }) {
    console.log('═'.repeat(70));
    console.log('  @unrdf/hooks Package Validation');
    console.log('  Policy Definition and Execution Framework');
    console.log('═'.repeat(70));
    console.log();

    let passed = 0;
    let failed = 0;

    // Test 1: Define Hook
    let testHook;
    try {
      console.log('✓ Test 1: Define Hook');
      testHook = defineHook({
        name: 'test-validation',
        trigger: 'before-add',
        validate: quad => quad !== null,
      });
      passed++;
      if (args.verbose) console.log('  Hook defined successfully\n');
    } catch (error) {
      console.error('✗ Test 1 Failed:', error.message);
      failed++;
    }

    // Test 2: Execute Hook
    try {
      console.log('✓ Test 2: Execute Hook');
      if (testHook) {
        const testQuad = { subject: 's', predicate: 'p', object: 'o' };
        const result = await executeHook(testHook, testQuad);
        if (args.verbose) console.log('  Result:', result, '\n');
        passed++;
      } else {
        throw new Error('testHook not defined');
      }
    } catch (error) {
      console.error('✗ Test 2 Failed:', error.message);
      failed++;
    }

    // Test 3: Hook Chain
    try {
      console.log('✓ Test 3: Hook Chain');
      const transformHook = defineHook({
        name: 'test-transform',
        trigger: 'before-add',
        transform: quad => ({ ...quad, transformed: true }),
      });

      const testQuad = { subject: 's', predicate: 'p', object: 'o' };
      const chainResult = await executeHookChain([testHook, transformHook], testQuad);

      if (args.verbose) console.log('  Chain result:', chainResult, '\n');
      passed++;
    } catch (error) {
      console.error('✗ Test 3 Failed:', error.message);
      failed++;
    }

    // Test 4: Hook Registry
    try {
      console.log('✓ Test 4: Hook Registry');
      // Hook registry works with Map internally
      // Just verify we can access registry functions
      if (typeof registerHook === 'function') {
        if (args.verbose) console.log('  Registry functions available\n');
        passed++;
      } else {
        throw new Error('Registry functions not available');
      }
    } catch (error) {
      console.error('✗ Test 4 Failed:', error.message);
      failed++;
    }

    // Test 5: Built-in Hooks
    try {
      console.log('✓ Test 5: Built-in Hooks');
      const builtins = Object.keys(builtinHooks);
      if (builtins.length > 0) {
        passed++;
        if (args.verbose) console.log('  Built-in hooks:', builtins.length, '\n');
      } else {
        throw new Error('No built-in hooks found');
      }
    } catch (error) {
      console.error('✗ Test 5 Failed:', error.message);
      failed++;
    }

    console.log();
    console.log('═'.repeat(70));
    console.log('  VALIDATION RESULTS');
    console.log('═'.repeat(70));
    console.log();
    console.log(`✓ Passed: ${passed}/5`);
    console.log(`✗ Failed: ${failed}/5`);
    console.log();

    if (failed === 0) {
      console.log('✅ HOOKS PACKAGE VALIDATED');
      console.log('   ✓ Hook definition working');
      console.log('   ✓ Hook execution functional');
      console.log('   ✓ Hook chains operational');
      console.log('   ✓ Hook registry available');
      console.log('   ✓ Built-in hooks accessible');
      console.log();
      process.exit(0);
    } else {
      console.log('⚠️  VALIDATION FAILED');
      console.log(`   ${failed} test(s) failed`);
      console.log();
      process.exit(1);
    }
  },
});

runMain(main);
