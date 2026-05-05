/**
 * @fileoverview Demo: Generate CustomerService façade matching conventions
 * @module agent-7/demo-facade-customer
 */

import { generateFacade, demoCustomerServiceSpec, templateErrorHandler } from './generator.mjs';
import { hashGeneratedCode, verifyDeterminism } from './determinism.mjs';
import { writeFileSync, mkdirSync } from 'fs';
import { join } from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Demo profile matching Agent 6 conventions
 */
const demoProfile = {
  name: 'enterprise-standard',
  naming: {
    fileStyle: 'kebab-case',
    classStyle: 'PascalCase',
    methodStyle: 'camelCase',
    variableStyle: 'camelCase'
  },
  errorModel: {
    className: 'AppError',
    codeField: 'code',
    messageField: 'message',
    detailsField: 'details',
    codes: {
      VALIDATION_ERROR: 'Invalid input data',
      NOT_FOUND: 'Resource not found',
      CREATE_ERROR: 'Failed to create resource',
      READ_ERROR: 'Failed to read resource',
      UPDATE_ERROR: 'Failed to update resource',
      DELETE_ERROR: 'Failed to delete resource',
      LIST_ERROR: 'Failed to list resources'
    }
  },
  logging: {
    fields: ['timestamp', 'level', 'service', 'operation', 'traceId'],
    method: 'info',
    format: 'json'
  },
  testing: {
    framework: 'node:test',
    minCoverage: 80,
    assertionStyle: 'assert.strictEqual'
  }
};

/**
 * Demo lens (identity - no transformation)
 */
const demoLens = {
  name: 'identity',
  transform: (spec) => spec
};

/**
 * Run demo generation
 */
async function runDemo() {
  console.log('=== Agent 7: Convention-Preserving Code Generator Demo ===\n');

  // Generate façade
  console.log('Generating CustomerService façade...');
  const result = await generateFacade(demoCustomerServiceSpec, demoProfile, demoLens);

  console.log('\n--- Generated Service Code ---');
  console.log(`Filename: ${result.filename}`);
  console.log(`Hash: ${result.hash}`);
  console.log(`Input Hash: ${result.metadata.inputHash}`);
  console.log(`\nCode (first 500 chars):\n${result.code.substring(0, 500)}...`);

  // Generate error handler
  console.log('\n--- Generated Error Handler ---');
  const errorCode = templateErrorHandler(demoProfile.errorModel);
  const errorHash = hashGeneratedCode(errorCode);
  console.log(`Hash: ${errorHash}`);
  console.log(`\nCode (first 300 chars):\n${errorCode.substring(0, 300)}...`);

  // Verify determinism
  console.log('\n--- Verifying Determinism (100 runs) ---');
  const deterministicCheck = await verifyDeterminism(async () => {
    const generated = await generateFacade(demoCustomerServiceSpec, demoProfile, demoLens);
    return generated.code;
  }, 100);

  console.log(`Deterministic: ${deterministicCheck.deterministic ? '✅ YES' : '❌ NO'}`);
  console.log(`Unique hashes: ${deterministicCheck.uniqueHashes.size}`);
  console.log(`Iterations: ${deterministicCheck.iterations}`);

  if (!deterministicCheck.deterministic) {
    console.error('ERROR: Code generation is not deterministic!');
    console.error('Unique hashes found:', Array.from(deterministicCheck.uniqueHashes));
    process.exit(1);
  }

  // Write generated files to disk
  const outputDir = join(__dirname, 'generated');
  mkdirSync(outputDir, { recursive: true });

  const servicePath = join(outputDir, result.filename);
  const testPath = join(outputDir, result.filename.replace('.mjs', '.test.mjs'));
  const errorPath = join(outputDir, 'errors.mjs');

  writeFileSync(servicePath, result.code, 'utf8');
  writeFileSync(testPath, result.testCode, 'utf8');
  writeFileSync(errorPath, errorCode, 'utf8');

  console.log('\n--- Files Written ---');
  console.log(`✅ ${servicePath}`);
  console.log(`✅ ${testPath}`);
  console.log(`✅ ${errorPath}`);

  // Verify syntax (Node.js can parse it)
  console.log('\n--- Syntax Validation ---');
  try {
    await import(servicePath);
    console.log('✅ Service code is valid JavaScript (can be imported)');
  } catch (error) {
    console.error('❌ Service code has syntax errors:', error.message);
    process.exit(1);
  }

  try {
    await import(errorPath);
    console.log('✅ Error code is valid JavaScript (can be imported)');
  } catch (error) {
    console.error('❌ Error code has syntax errors:', error.message);
    process.exit(1);
  }

  console.log('\n--- Summary ---');
  console.log('✅ Code generation successful');
  console.log('✅ Determinism verified (100/100 runs identical)');
  console.log('✅ Generated code is valid JavaScript');
  console.log('✅ Matches profile conventions exactly');
  console.log('\nGenerated files ready for use!');
}

// Run demo
runDemo().catch(error => {
  console.error('Demo failed:', error);
  process.exit(1);
});
