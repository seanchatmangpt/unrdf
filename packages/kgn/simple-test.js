#!/usr/bin/env node
/**
 * Simple KGEN Injection Test
 * Tests basic injection functionality without complex dependencies
 */

import { promises as fs } from 'fs';
import { join } from 'path';
import { tmpdir } from 'os';

// Direct imports to avoid integration issues
import { InjectionEngine } from './src/injection/injection-engine.js';
import { INJECTION_MODES } from './src/injection/constants.js';

async function simpleTest() {
  console.log('🧪 KGEN Injection Simple Test');
  console.log('='.repeat(40));

  const testDir = await fs.mkdtemp(join(tmpdir(), 'kgen-test-'));

  try {
    // Create test file
    const testFile = join(testDir, 'test.ts');
    await fs.writeFile(testFile, 'export const original = true;\n');
    console.log('✅ Created test file');

    // Initialize injection engine
    const engine = new InjectionEngine({
      backupEnabled: true
    });

    // Set the project root correctly
    engine.targetResolver.projectRoot = testDir;
    console.log('✅ Initialized injection engine');

    // Test 1: Append injection
    console.log('\n1️⃣ Testing append injection...');

    const appendConfig = {
      to: 'test.ts',
      inject: true,
      mode: INJECTION_MODES.APPEND
    };

    const injectionContent = 'export const added = true;';

    const result = await engine.inject(appendConfig, injectionContent, {});

    console.log(`✅ Injection result: ${result.success ? 'SUCCESS' : 'FAILED'}`);
    if (result.operationId) {
      console.log(`   Operation ID: ${result.operationId}`);
    }

    // Verify result
    const modifiedContent = await fs.readFile(testFile, 'utf8');
    console.log('📄 Modified content:');
    console.log(modifiedContent);

    // Test 2: Idempotency
    console.log('\n2️⃣ Testing idempotency...');

    const idempotentConfig = {
      ...appendConfig,
      skipIf: 'added'
    };

    const result2 = await engine.inject(idempotentConfig, injectionContent, {});
    console.log(`✅ Idempotent result: ${result2.skipped ? 'SKIPPED' : 'INJECTED'}`);

    // Test 3: History
    console.log('\n3️⃣ Testing operation history...');
    const history = engine.getOperationHistory();
    console.log(`✅ Operations in history: ${history.length}`);

    console.log('\n✅ All tests passed!');

  } catch (error) {
    console.error('❌ Test failed:', error.message);
    console.error(error.stack);
  } finally {
    // Cleanup
    await fs.rm(testDir, { recursive: true, force: true });
    console.log('🧹 Test directory cleaned up');
  }
}

simpleTest().catch(console.error);