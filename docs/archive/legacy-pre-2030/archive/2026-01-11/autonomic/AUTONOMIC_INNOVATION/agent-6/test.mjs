/**
 * @file Conventions Profile Tests
 * @description Comprehensive test suite for profile compilation and validation
 */

import { strict as assert } from 'node:assert';
import { test } from 'node:test';
import { writeFileSync, mkdirSync, rmSync } from 'node:fs';
import { join } from 'node:path';
import { compileProfile, validateAgainstProfile, diagnosticReport } from './compiler.mjs';
import { demoProfile, minimalProfile, strictProfile } from './demo-profile.mjs';
import { ConventionsProfileSchema } from './profile-schema.mjs';

const TEST_DIR = '/tmp/conventions-profile-test';

/**
 * Setup test directory
 */
function setupTestDir() {
  try {
    rmSync(TEST_DIR, { recursive: true, force: true });
  } catch {}
  mkdirSync(TEST_DIR, { recursive: true });
}

/**
 * Create test file
 * @param {string} name - File name
 * @param {string} content - File content
 * @returns {string} Full path
 */
function createTestFile(name, content) {
  const path = join(TEST_DIR, name);
  writeFileSync(path, content, 'utf-8');
  return path;
}

// ============================================================================
// Schema Validation Tests
// ============================================================================

test('Profile schema validation - valid profile passes', () => {
  const result = ConventionsProfileSchema.safeParse(demoProfile);
  assert.ok(result.success, 'Demo profile should be valid');
});

test('Profile schema validation - missing required field fails', () => {
  const invalidProfile = { ...demoProfile };
  delete invalidProfile.name;

  const result = ConventionsProfileSchema.safeParse(invalidProfile);
  assert.ok(!result.success, 'Profile without name should fail');
});

test('Profile schema validation - invalid enum value fails', () => {
  const invalidProfile = {
    ...demoProfile,
    fileLayout: {
      ...demoProfile.fileLayout,
      naming: {
        ...demoProfile.fileLayout.naming,
        serviceClass: 'InvalidCase'
      }
    }
  };

  const result = ConventionsProfileSchema.safeParse(invalidProfile);
  assert.ok(!result.success, 'Invalid naming convention should fail');
});

test('Profile schema validation - all demo profiles valid', () => {
  const profiles = [demoProfile, minimalProfile, strictProfile];

  for (const profile of profiles) {
    const result = ConventionsProfileSchema.safeParse(profile);
    assert.ok(result.success, `Profile ${profile.name} should be valid`);
  }
});

// ============================================================================
// Profile Compilation Tests
// ============================================================================

test('Profile compilation - valid profile compiles successfully', () => {
  const compiled = compileProfile(demoProfile);

  assert.ok(compiled.compiled, 'Profile should be compiled');
  assert.strictEqual(compiled.violations.length, 0, 'Should have no violations');
  assert.strictEqual(compiled.schema.name, demoProfile.name);
  assert.ok(compiled.timestamp, 'Should have timestamp');
});

test('Profile compilation - invalid profile throws', () => {
  const invalidProfile = { name: 'Test' }; // Missing required fields

  assert.throws(
    () => compileProfile(invalidProfile),
    /Invalid profile/,
    'Should throw for invalid profile'
  );
});

test('Profile compilation - deterministic output', () => {
  const results = [];

  for (let i = 0; i < 100; i++) {
    const compiled = compileProfile(demoProfile);
    // Remove timestamp for comparison
    const { timestamp, ...rest } = compiled;
    results.push(JSON.stringify(rest));
  }

  // All results should be identical (except timestamp)
  const unique = new Set(results);
  assert.strictEqual(unique.size, 1, 'Compilation should be deterministic');
});

// ============================================================================
// Code Validation Tests
// ============================================================================

test('Code validation - PascalCase class passes', () => {
  setupTestDir();

  const file = createTestFile('service.mjs', `
    class CustomerService {
      getCustomer() {}
    }
  `);

  const compiled = compileProfile(demoProfile);
  const result = validateAgainstProfile(compiled, [file]);

  assert.ok(result.valid, 'Should pass validation');
  assert.strictEqual(result.violations.length, 0);
  assert.strictEqual(result.filesChecked, 1);
});

test('Code validation - wrong class naming detected', () => {
  setupTestDir();

  const file = createTestFile('service.mjs', `
    class customer_service {
      getCustomer() {}
    }
  `);

  const compiled = compileProfile(demoProfile);
  const result = validateAgainstProfile(compiled, [file]);

  assert.ok(!result.valid, 'Should fail validation');
  assert.strictEqual(result.violations.length, 1);
  assert.strictEqual(result.violations[0].rule, 'naming.serviceClass');
  assert.ok(result.violations[0].suggestion.includes('CustomerService'));
});

test('Code validation - wrong method naming detected', () => {
  setupTestDir();

  const file = createTestFile('service.mjs', `
    class CustomerService {
      GetCustomer() {}
      get_order() {}
    }
  `);

  const compiled = compileProfile(demoProfile);
  const result = validateAgainstProfile(compiled, [file]);

  assert.ok(!result.valid, 'Should fail validation');
  assert.strictEqual(result.violations.length, 2, 'Should detect both violations');

  const methodViolations = result.violations.filter(v => v.rule === 'naming.method');
  assert.strictEqual(methodViolations.length, 2);
});

test('Code validation - wrong constant naming detected', () => {
  setupTestDir();

  const file = createTestFile('constants.mjs', `
    const maxRetries = 3;
    const API_KEY = 'secret';
  `);

  const compiled = compileProfile(demoProfile);
  const result = validateAgainstProfile(compiled, [file]);

  assert.ok(!result.valid, 'Should fail validation');

  const constantViolations = result.violations.filter(v => v.rule === 'naming.constant');
  assert.strictEqual(constantViolations.length, 1, 'Should detect camelCase constant');
  assert.ok(constantViolations[0].message.includes('maxRetries'));
});

test('Code validation - multiple files', () => {
  setupTestDir();

  const file1 = createTestFile('service1.mjs', `
    class UserService {
      createUser() {}
    }
  `);

  const file2 = createTestFile('service2.mjs', `
    class order_service {
      get_order() {}
    }
  `);

  const compiled = compileProfile(demoProfile);
  const result = validateAgainstProfile(compiled, [file1, file2]);

  assert.ok(!result.valid, 'Should fail validation');
  assert.strictEqual(result.filesChecked, 2);

  // file2 should have 2 violations (class + method)
  const file2Violations = result.violations.filter(v => v.file === file2);
  assert.ok(file2Violations.length >= 2, 'Should detect multiple violations in file2');
});

test('Code validation - syntax error handled gracefully', () => {
  setupTestDir();

  const file = createTestFile('broken.mjs', `
    class CustomerService {
      getCustomer() {
        // Missing closing brace
  `);

  const compiled = compileProfile(demoProfile);
  const result = validateAgainstProfile(compiled, [file]);

  assert.ok(!result.valid, 'Should fail validation');
  assert.strictEqual(result.violations.length, 1);
  assert.strictEqual(result.violations[0].rule, 'syntax');
});

test('Code validation - empty file list', () => {
  const compiled = compileProfile(demoProfile);
  const result = validateAgainstProfile(compiled, []);

  assert.ok(result.valid, 'Empty list should be valid');
  assert.strictEqual(result.violations.length, 0);
  assert.strictEqual(result.filesChecked, 0);
});

test('Code validation - 100% compliant code', () => {
  setupTestDir();

  const file = createTestFile('perfect.mjs', `
    const MAX_RETRIES = 3;
    const API_TIMEOUT = 5000;

    class CustomerService {
      createCustomer() {}
      getCustomer() {}
      updateCustomer() {}
      deleteCustomer() {}
    }

    class OrderService {
      processOrder() {}
    }
  `);

  const compiled = compileProfile(demoProfile);
  const result = validateAgainstProfile(compiled, [file]);

  assert.ok(result.valid, 'Perfect code should pass');
  assert.strictEqual(result.violations.length, 0);
});

// ============================================================================
// Diagnostic Report Tests
// ============================================================================

test('Diagnostic report - no violations', () => {
  const report = diagnosticReport([]);

  assert.ok(report.includes('No violations found'));
  assert.ok(report.includes('✅'));
});

test('Diagnostic report - single violation', () => {
  const violations = [
    {
      file: '/test/file.mjs',
      rule: 'naming.serviceClass',
      message: 'Class name wrong',
      suggestion: 'Rename to CustomerService',
      line: 1,
      column: 0
    }
  ];

  const report = diagnosticReport(violations);

  assert.ok(report.includes('1 violation'));
  assert.ok(report.includes('naming.serviceClass'));
  assert.ok(report.includes('/test/file.mjs'));
  assert.ok(report.includes('Rename to CustomerService'));
});

test('Diagnostic report - multiple violations grouped by rule', () => {
  const violations = [
    {
      file: '/test/file1.mjs',
      rule: 'naming.serviceClass',
      message: 'Class name wrong',
      suggestion: 'Fix it'
    },
    {
      file: '/test/file2.mjs',
      rule: 'naming.serviceClass',
      message: 'Another class name wrong',
      suggestion: 'Fix it too'
    },
    {
      file: '/test/file3.mjs',
      rule: 'naming.method',
      message: 'Method name wrong',
      suggestion: 'Fix method'
    }
  ];

  const report = diagnosticReport(violations);

  assert.ok(report.includes('3 violations'));
  assert.ok(report.includes('naming.serviceClass (2 violations)'));
  assert.ok(report.includes('naming.method (1 violation)'));
  assert.ok(report.includes('3 files'));
});

test('Diagnostic report - violation without suggestion', () => {
  const violations = [
    {
      file: '/test/file.mjs',
      rule: 'syntax',
      message: 'Parse error'
    }
  ];

  const report = diagnosticReport(violations);

  assert.ok(report.includes('Parse error'));
  assert.ok(!report.includes('Fix:'), 'Should not show Fix line if no suggestion');
});

// ============================================================================
// Integration Tests
// ============================================================================

test('Integration - complete workflow', () => {
  setupTestDir();

  // 1. Create test files
  const goodFile = createTestFile('good-service.mjs', `
    const MAX_ITEMS = 100;

    class ProductService {
      createProduct() {}
      getProduct() {}
    }
  `);

  const badFile = createTestFile('bad-service.mjs', `
    const maxItems = 100;

    class product_service {
      GetProduct() {}
    }
  `);

  // 2. Compile profile
  const compiled = compileProfile(demoProfile);
  assert.ok(compiled.compiled);

  // 3. Validate files
  const result = validateAgainstProfile(compiled, [goodFile, badFile]);
  assert.ok(!result.valid);
  assert.ok(result.violations.length > 0);

  // 4. Generate report
  const report = diagnosticReport(result.violations);
  assert.ok(report.includes('violation'));
  assert.ok(report.includes('naming'));
});

test('Integration - different profiles same code', () => {
  setupTestDir();

  const file = createTestFile('service.mjs', `
    const MAX_RETRIES = 3;

    class userService {
      createUser() {}
    }
  `);

  // Minimal profile (camelCase classes)
  const minimalCompiled = compileProfile(minimalProfile);
  const minimalResult = validateAgainstProfile(minimalCompiled, [file]);
  assert.ok(minimalResult.valid, 'Should pass minimal profile');

  // Demo profile (PascalCase classes)
  const demoCompiled = compileProfile(demoProfile);
  const demoResult = validateAgainstProfile(demoCompiled, [file]);
  assert.ok(!demoResult.valid, 'Should fail demo profile');
});

// ============================================================================
// Performance Tests
// ============================================================================

test('Performance - compilation speed', () => {
  const start = Date.now();

  for (let i = 0; i < 1000; i++) {
    compileProfile(demoProfile);
  }

  const duration = Date.now() - start;
  console.log(`  1000 compilations in ${duration}ms (${(duration/1000).toFixed(2)}ms each)`);

  assert.ok(duration < 5000, 'Should compile 1000 profiles in under 5 seconds');
});

test('Performance - validation speed', () => {
  setupTestDir();

  const file = createTestFile('perf-test.mjs', `
    class TestService {
      method1() {}
      method2() {}
      method3() {}
    }
  `);

  const compiled = compileProfile(demoProfile);
  const start = Date.now();

  for (let i = 0; i < 100; i++) {
    validateAgainstProfile(compiled, [file]);
  }

  const duration = Date.now() - start;
  console.log(`  100 validations in ${duration}ms (${(duration/100).toFixed(2)}ms each)`);

  assert.ok(duration < 5000, 'Should validate 100 times in under 5 seconds');
});

console.log('\n✅ All tests defined and ready to execute');
