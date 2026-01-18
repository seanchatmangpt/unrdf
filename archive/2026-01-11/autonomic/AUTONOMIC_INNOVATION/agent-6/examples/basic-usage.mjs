/**
 * Basic Usage Example: Conventions Profile Compiler
 * Demonstrates defining, compiling, and validating against a conventions profile
 */

import {
  defineProfile,
  compileProfile,
  validateFileLayout,
  validateNaming,
  validateErrors,
  validateLogging,
  diagnosticReport,
} from '../src/index.mjs';

// Step 1: Define company conventions profile
const companyProfile = defineProfile({
  id: 'company-api-v1',
  fileLayout: {
    src: 'src/**/*.mjs',
    test: 'test/**/*.test.mjs',
    examples: 'examples/**/*.mjs',
  },
  naming: {
    filePrefix: 'api-',
    functionPrefix: 'api',
    exportPattern: '/^api[A-Z]\\w+$/',
    reservedWords: ['delete', 'new', 'class'],
  },
  errors: {
    namespace: 'Api',
    fields: ['code', 'message', 'context', 'timestamp'],
    codes: {
      NOT_FOUND: 'API_ERR_404',
      UNAUTHORIZED: 'API_ERR_401',
      INVALID_INPUT: 'API_ERR_400',
      SERVER_ERROR: 'API_ERR_500',
    },
  },
  logging: {
    fields: ['timestamp', 'level', 'message', 'context', 'requestId'],
    levels: ['debug', 'info', 'warn', 'error'],
    format: 'json',
    requireContext: true,
  },
});

// Step 2: Compile profile into executable validators
const compiled = compileProfile(companyProfile);

console.log('✅ Profile compiled:', companyProfile.id);
console.log('');

// Step 3: Validate file layout
const files = [
  'src/api-users.mjs',      // ✅ valid
  'src/api-posts.mjs',      // ✅ valid
  'test/api.test.mjs',      // ✅ valid
  'lib/helper.mjs',         // ❌ wrong directory
  'src/users.mjs',          // ❌ missing file prefix
];

const layoutResult = validateFileLayout(files, compiled);
console.log('📂 File Layout Validation:');
console.log(`  Files checked: ${layoutResult.fileCount}`);
console.log(`  Violations: ${layoutResult.violationCount}`);
if (!layoutResult.ok) {
  console.log(diagnosticReport(layoutResult.violations, companyProfile.id));
}

// Step 4: Validate naming conventions
const exports = [
  { name: 'apiCreateUser', file: 'src/api-users.mjs', line: 10 },  // ✅ valid
  { name: 'apiGetUser', file: 'src/api-users.mjs', line: 25 },     // ✅ valid
  { name: 'createUser', file: 'src/api-users.mjs', line: 40 },     // ❌ missing prefix
  { name: 'delete', file: 'src/api-users.mjs', line: 55 },         // ❌ reserved word
];

const namingResult = validateNaming(exports, compiled);
console.log('\n🏷️  Naming Convention Validation:');
console.log(`  Exports checked: ${namingResult.fileCount}`);
console.log(`  Violations: ${namingResult.violationCount}`);
if (!namingResult.ok) {
  console.log(diagnosticReport(namingResult.violations, companyProfile.id));
}

// Step 5: Validate error model
const errorClasses = [
  {
    name: 'ApiNotFoundError',
    fields: ['code', 'message', 'context', 'timestamp'],
    codes: ['API_ERR_404'],
    file: 'src/api-errors.mjs',
    line: 10,
  },
  {
    name: 'ValidationError',  // ❌ missing namespace
    fields: ['message'],      // ❌ missing required fields
    codes: ['UNKNOWN'],       // ❌ invalid code
    file: 'src/api-errors.mjs',
    line: 30,
  },
];

const errorsResult = validateErrors(errorClasses, compiled);
console.log('\n⚠️  Error Model Validation:');
console.log(`  Error classes checked: ${errorsResult.fileCount}`);
console.log(`  Violations: ${errorsResult.violationCount}`);
if (!errorsResult.ok) {
  console.log(diagnosticReport(errorsResult.violations, companyProfile.id));
}

// Step 6: Validate logging compliance
const logStatements = [
  {
    fields: ['timestamp', 'level', 'message', 'context', 'requestId'],
    level: 'info',
    format: 'json',
    file: 'src/api-users.mjs',
    line: 15,
  },
  {
    fields: ['timestamp', 'level', 'message'],  // ❌ missing context & requestId
    level: 'trace',                             // ❌ invalid level
    format: 'text',                             // ❌ wrong format
    file: 'src/api-users.mjs',
    line: 42,
  },
];

const loggingResult = validateLogging(logStatements, compiled);
console.log('\n📝 Logging Validation:');
console.log(`  Log statements checked: ${loggingResult.fileCount}`);
console.log(`  Violations: ${loggingResult.violationCount}`);
if (!loggingResult.ok) {
  console.log(diagnosticReport(loggingResult.violations, companyProfile.id));
}

// Step 7: Aggregate all violations
const allViolations = [
  ...layoutResult.violations,
  ...namingResult.violations,
  ...errorsResult.violations,
  ...loggingResult.violations,
];

console.log('\n' + '='.repeat(60));
console.log('📊 Summary Report');
console.log('='.repeat(60));
console.log(`Total violations: ${allViolations.length}`);
console.log(`  File layout: ${layoutResult.violationCount}`);
console.log(`  Naming: ${namingResult.violationCount}`);
console.log(`  Errors: ${errorsResult.violationCount}`);
console.log(`  Logging: ${loggingResult.violationCount}`);

if (allViolations.length > 0) {
  console.log('\n📋 Full Diagnostic Report:');
  console.log(diagnosticReport(allViolations, companyProfile.id));
}
