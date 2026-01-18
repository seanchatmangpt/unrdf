/**
 * @file Conventions Profile Demonstration
 * @description Interactive demonstration of the profile system
 */

import { writeFileSync, mkdirSync, rmSync } from 'node:fs';
import { join } from 'node:path';
import { compileProfile, validateAgainstProfile, diagnosticReport } from './compiler.mjs';
import { demoProfile, minimalProfile, strictProfile } from './demo-profile.mjs';

const DEMO_DIR = '/tmp/conventions-profile-demo';

/**
 * Setup demo directory
 */
function setupDemo() {
  try {
    rmSync(DEMO_DIR, { recursive: true, force: true });
  } catch {}
  mkdirSync(DEMO_DIR, { recursive: true });
  console.log(`📁 Created demo directory: ${DEMO_DIR}\n`);
}

/**
 * Create demo file
 */
function createDemoFile(name, content) {
  const path = join(DEMO_DIR, name);
  writeFileSync(path, content, 'utf-8');
  return path;
}

/**
 * Demo 1: Perfect compliance
 */
function demo1PerfectCompliance() {
  console.log('═══════════════════════════════════════════════════════════');
  console.log('Demo 1: Perfect Compliance');
  console.log('═══════════════════════════════════════════════════════════\n');

  const file = createDemoFile('perfect-service.mjs', `
/**
 * Perfect service following all conventions
 */

const MAX_RETRIES = 3;
const API_TIMEOUT = 5000;
const DEFAULT_PAGE_SIZE = 20;

class CustomerService {
  constructor() {
    this.customers = new Map();
  }

  createCustomer(data) {
    const id = crypto.randomUUID();
    this.customers.set(id, { id, ...data });
    return id;
  }

  getCustomer(id) {
    return this.customers.get(id);
  }

  updateCustomer(id, data) {
    const customer = this.customers.get(id);
    if (!customer) return null;

    const updated = { ...customer, ...data };
    this.customers.set(id, updated);
    return updated;
  }

  deleteCustomer(id) {
    return this.customers.delete(id);
  }
}

export { CustomerService, MAX_RETRIES, API_TIMEOUT };
`);

  const compiled = compileProfile(demoProfile);
  const result = validateAgainstProfile(compiled, [file]);

  console.log('Profile:', demoProfile.name);
  console.log('File:', file);
  console.log('\nResult:', result.valid ? '✅ PASS' : '❌ FAIL');
  console.log('Violations:', result.violations.length);

  if (result.violations.length > 0) {
    console.log('\n' + diagnosticReport(result.violations));
  }

  console.log('\n');
}

/**
 * Demo 2: Multiple violations
 */
function demo2MultipleViolations() {
  console.log('═══════════════════════════════════════════════════════════');
  console.log('Demo 2: Multiple Convention Violations');
  console.log('═══════════════════════════════════════════════════════════\n');

  const file = createDemoFile('bad-service.mjs', `
/**
 * Service with multiple convention violations
 */

const maxRetries = 3;              // ❌ Should be UPPER_SNAKE_CASE
const api_timeout = 5000;           // ❌ Should be UPPER_SNAKE_CASE

class customer_service {            // ❌ Should be PascalCase
  constructor() {
    this.customers = new Map();
  }

  CreateCustomer(data) {            // ❌ Should be camelCase
    const id = crypto.randomUUID();
    this.customers.set(id, { id, ...data });
    return id;
  }

  get_customer(id) {                // ❌ Should be camelCase
    return this.customers.get(id);
  }

  UPDATE_CUSTOMER(id, data) {       // ❌ Should be camelCase
    const customer = this.customers.get(id);
    if (!customer) return null;

    const updated = { ...customer, ...data };
    this.customers.set(id, updated);
    return updated;
  }
}

export { customer_service };
`);

  const compiled = compileProfile(demoProfile);
  const result = validateAgainstProfile(compiled, [file]);

  console.log('Profile:', demoProfile.name);
  console.log('File:', file);
  console.log('\nResult:', result.valid ? '✅ PASS' : '❌ FAIL');
  console.log('Violations:', result.violations.length);

  console.log('\n' + diagnosticReport(result.violations));
  console.log('\n');
}

/**
 * Demo 3: Different profiles, same code
 */
function demo3DifferentProfiles() {
  console.log('═══════════════════════════════════════════════════════════');
  console.log('Demo 3: Same Code, Different Profiles');
  console.log('═══════════════════════════════════════════════════════════\n');

  const file = createDemoFile('flexible-service.mjs', `
const MAX_ITEMS = 100;

class userService {
  createUser() {}
  getUser() {}
}
`);

  console.log('Code under test:');
  console.log('  - Class: userService (camelCase)');
  console.log('  - Methods: createUser, getUser (camelCase)');
  console.log('  - Constant: MAX_ITEMS (UPPER_SNAKE_CASE)\n');

  // Test with minimal profile (camelCase classes allowed)
  const minimalCompiled = compileProfile(minimalProfile);
  const minimalResult = validateAgainstProfile(minimalCompiled, [file]);

  console.log('─────────────────────────────────────────────────────────');
  console.log('Profile: Minimal Conventions (camelCase classes OK)');
  console.log('Result:', minimalResult.valid ? '✅ PASS' : '❌ FAIL');
  console.log('Violations:', minimalResult.violations.length);

  // Test with demo profile (PascalCase classes required)
  const demoCompiled = compileProfile(demoProfile);
  const demoResult = validateAgainstProfile(demoCompiled, [file]);

  console.log('\n─────────────────────────────────────────────────────────');
  console.log('Profile: Enterprise Standards (PascalCase classes required)');
  console.log('Result:', demoResult.valid ? '✅ PASS' : '❌ FAIL');
  console.log('Violations:', demoResult.violations.length);

  if (demoResult.violations.length > 0) {
    console.log(diagnosticReport(demoResult.violations));
  }

  console.log('\n');
}

/**
 * Demo 4: Profile compilation
 */
function demo4ProfileCompilation() {
  console.log('═══════════════════════════════════════════════════════════');
  console.log('Demo 4: Profile Compilation & Metadata');
  console.log('═══════════════════════════════════════════════════════════\n');

  const profiles = [
    { profile: demoProfile, name: 'Enterprise Standards' },
    { profile: minimalProfile, name: 'Minimal Conventions' },
    { profile: strictProfile, name: 'Strict Production' }
  ];

  for (const { profile, name } of profiles) {
    const compiled = compileProfile(profile);

    console.log(`📋 ${name}`);
    console.log('   Description:', profile.description);
    console.log('   Class naming:', profile.fileLayout.naming.serviceClass);
    console.log('   Method naming:', profile.fileLayout.naming.method);
    console.log('   Constant naming:', profile.fileLayout.naming.constant);
    console.log('   Test framework:', profile.testing.framework);
    console.log('   Min coverage:', profile.testing.minCoverage + '%');
    console.log('   Validation:', profile.dataContracts.validation);
    console.log('   Compiled:', compiled.compiled ? '✅' : '❌');
    console.log('   Timestamp:', compiled.timestamp);
    console.log('');
  }
}

/**
 * Demo 5: Performance metrics
 */
function demo5Performance() {
  console.log('═══════════════════════════════════════════════════════════');
  console.log('Demo 5: Performance Characteristics');
  console.log('═══════════════════════════════════════════════════════════\n');

  // Compilation performance
  const compileStart = Date.now();
  for (let i = 0; i < 1000; i++) {
    compileProfile(demoProfile);
  }
  const compileTime = Date.now() - compileStart;

  console.log('Compilation Performance:');
  console.log(`  1000 compilations: ${compileTime}ms`);
  console.log(`  Average: ${(compileTime / 1000).toFixed(3)}ms per profile`);
  console.log(`  Rate: ${Math.round(1000 / (compileTime / 1000))} profiles/second\n`);

  // Validation performance
  const file = createDemoFile('perf-test.mjs', `
    class TestService {
      method1() {}
      method2() {}
      method3() {}
      method4() {}
      method5() {}
    }
  `);

  const compiled = compileProfile(demoProfile);
  const validStart = Date.now();
  for (let i = 0; i < 100; i++) {
    validateAgainstProfile(compiled, [file]);
  }
  const validTime = Date.now() - validStart;

  console.log('Validation Performance:');
  console.log(`  100 validations: ${validTime}ms`);
  console.log(`  Average: ${(validTime / 100).toFixed(3)}ms per file`);
  console.log(`  Rate: ${Math.round(100 / (validTime / 1000))} files/second\n`);

  // Determinism check
  const results = [];
  for (let i = 0; i < 100; i++) {
    const c = compileProfile(demoProfile);
    const { timestamp, ...rest } = c;
    results.push(JSON.stringify(rest));
  }
  const unique = new Set(results);

  console.log('Determinism:');
  console.log(`  100 compilations: ${unique.size} unique outputs`);
  console.log(`  Deterministic: ${unique.size === 1 ? '✅ YES' : '❌ NO'}\n`);
}

/**
 * Run all demos
 */
function runAllDemos() {
  setupDemo();

  demo1PerfectCompliance();
  demo2MultipleViolations();
  demo3DifferentProfiles();
  demo4ProfileCompilation();
  demo5Performance();

  console.log('═══════════════════════════════════════════════════════════');
  console.log('Demo Complete');
  console.log('═══════════════════════════════════════════════════════════\n');

  console.log('Key Takeaways:');
  console.log('  ✅ Profiles are machine-checkable specifications');
  console.log('  ✅ Violations are clearly reported with suggestions');
  console.log('  ✅ Multiple profiles can coexist for different contexts');
  console.log('  ✅ Compilation is fast and deterministic');
  console.log('  ✅ Validation provides actionable diagnostics\n');
}

// Run demos
runAllDemos();
