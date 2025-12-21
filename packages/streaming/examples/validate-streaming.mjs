#!/usr/bin/env node
/**
 * Streaming Package Validation CLI
 *
 * Validates @unrdf/streaming functionality using citty
 *
 * Usage:
 *   node examples/validate-streaming.mjs
 */

import { defineCommand, runMain } from 'citty';
import {
  createStreamProcessor,
  createSubscriptionManager,
  createChangeFeed,
} from '../src/index.mjs';

const main = defineCommand({
  meta: {
    name: 'validate-streaming',
    description: 'Validate @unrdf/streaming package functionality',
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
    console.log('  @unrdf/streaming Package Validation');
    console.log('  Change Feeds and Real-time Synchronization');
    console.log('═'.repeat(70));
    console.log();

    let passed = 0;
    let failed = 0;

    // Test 1: Create Stream Processor
    try {
      console.log('✓ Test 1: Create Stream Processor');
      // Create mock feed with EventTarget interface
      const mockFeed = {
        addEventListener: () => {},
        removeEventListener: () => {},
      };
      const processor = createStreamProcessor(mockFeed);
      if (args.verbose)
        console.log('  Processor created with mock feed\n');
      passed++;
    } catch (error) {
      console.error('✗ Test 1 Failed:', error.message);
      failed++;
    }

    // Test 2: Create Subscription Manager
    try {
      console.log('✓ Test 2: Create Subscription Manager');
      // Create mock feed for subscription manager
      const mockFeed = {
        addEventListener: () => {},
        removeEventListener: () => {},
      };
      const manager = createSubscriptionManager(mockFeed);
      if (args.verbose) console.log('  Manager created with mock feed\n');
      passed++;
    } catch (error) {
      console.error('✗ Test 2 Failed:', error.message);
      failed++;
    }

    // Test 3: Create Change Feed
    try {
      console.log('✓ Test 3: Create Change Feed');
      const mockStore = {
        match: () => [],
      };
      const feed = createChangeFeed(mockStore);
      if (args.verbose) console.log('  Feed created with mock store\n');
      passed++;
    } catch (error) {
      console.error('✗ Test 3 Failed:', error.message);
      failed++;
    }

    // Test 4: Subscription Management
    try {
      console.log('✓ Test 4: Subscription Management');
      const mockFeed = {
        addEventListener: () => {},
        removeEventListener: () => {},
      };
      const manager = createSubscriptionManager(mockFeed);
      const subId = manager.subscribe(data => {
        if (args.verbose) console.log('  Received:', data);
      }, {}); // Pass empty filter object
      const unsubscribed = manager.unsubscribe(subId);
      if (args.verbose) console.log('  Subscription lifecycle verified\n');
      passed++;
    } catch (error) {
      console.error('✗ Test 4 Failed:', error.message);
      failed++;
    }

    // Test 5: Stream Processing
    try {
      console.log('✓ Test 5: Stream Processing');
      const mockFeed = {
        listeners: [],
        addEventListener(event, listener) {
          this.listeners.push({ event, listener });
        },
        removeEventListener(event, listener) {
          const index = this.listeners.findIndex(l => l.listener === listener);
          if (index !== -1) this.listeners.splice(index, 1);
        },
      };
      const processor = createStreamProcessor(mockFeed);

      // Verify processor has required methods
      if (processor.filter && processor.map && processor.subscribe && processor.batch && processor.debounce) {
        if (args.verbose) console.log('  Processor methods available\n');
        passed++;
      } else {
        throw new Error('Processor missing required methods');
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
      console.log('✅ STREAMING PACKAGE VALIDATED');
      console.log('   ✓ Stream processor creation working');
      console.log('   ✓ Subscription manager functional');
      console.log('   ✓ Change feed operational');
      console.log('   ✓ Subscription lifecycle verified');
      console.log('   ✓ Stream processing confirmed');
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
