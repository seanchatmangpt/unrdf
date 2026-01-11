import { strict as assert } from 'node:assert';
import { test } from 'node:test';
import {
  defineProfile,
  compileProfile,
  validateFileLayout,
  validateNaming,
  validateErrors,
  validateLogging,
  diagnosticReport,
} from '../src/index.mjs';

test('defineProfile - valid company API profile', () => {
  const profile = defineProfile({
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
    },
    errors: {
      namespace: 'Api',
      fields: ['code', 'message', 'context', 'timestamp'],
      codes: {
        NOT_FOUND: 'API_ERR_404',
        UNAUTHORIZED: 'API_ERR_401',
        INVALID_INPUT: 'API_ERR_400',
      },
    },
    logging: {
      fields: ['timestamp', 'level', 'message', 'context', 'requestId'],
      levels: ['debug', 'info', 'warn', 'error'],
      format: 'json',
      requireContext: true,
    },
  });

  assert.equal(profile.id, 'company-api-v1');
  assert.equal(profile.naming.functionPrefix, 'api');
  assert.equal(profile.errors.namespace, 'Api');
  assert.equal(profile.logging.format, 'json');
});

test('validateFileLayout - detects misplaced files', () => {
  const profile = defineProfile({
    id: 'test-profile',
    fileLayout: {
      src: 'src/**/*.mjs',
      test: 'test/**/*.test.mjs',
    },
    naming: { exportPattern: '/.*/' },
    errors: { fields: ['message'], codes: {} },
    logging: { fields: ['message'], levels: ['info'] },
  });

  const compiled = compileProfile(profile);

  const files = [
    'src/api.mjs',           // ✅ valid
    'test/api.test.mjs',     // ✅ valid
    'lib/helper.mjs',        // ❌ misplaced
    'utils/format.mjs',      // ❌ misplaced
  ];

  const result = validateFileLayout(files, compiled);

  assert.equal(result.ok, false);
  assert.equal(result.violationCount, 2);
  assert.match(result.violations[0].message, /does not match any layout pattern/);
  assert.ok(result.violations.some(v => v.file === 'lib/helper.mjs'));
  assert.ok(result.violations.some(v => v.file === 'utils/format.mjs'));
});

test('validateNaming - detects pattern violations', () => {
  const profile = defineProfile({
    id: 'test-profile',
    fileLayout: { src: 'src/**/*.mjs', test: 'test/**/*.test.mjs' },
    naming: {
      functionPrefix: 'api',
      exportPattern: '/^api[A-Z]\\w+$/',
      reservedWords: ['delete', 'new'],
    },
    errors: { fields: ['message'], codes: {} },
    logging: { fields: ['message'], levels: ['info'] },
  });

  const compiled = compileProfile(profile);

  const exports = [
    { name: 'apiCreateUser', file: 'src/api.mjs', line: 10 },    // ✅ valid
    { name: 'apiGetUser', file: 'src/api.mjs', line: 20 },       // ✅ valid
    { name: 'createUser', file: 'src/api.mjs', line: 30 },       // ❌ missing prefix
    { name: 'delete', file: 'src/api.mjs', line: 40 },           // ❌ reserved word
  ];

  const result = validateNaming(exports, compiled);

  assert.equal(result.ok, false);
  // createUser: pattern + prefix violations
  // delete: pattern + prefix + reserved word violations
  assert.ok(result.violationCount >= 4, `Expected at least 4 violations, got ${result.violationCount}`);

  const prefixViolation = result.violations.find(v => v.actual === 'createUser' && v.message.includes('pattern'));
  assert.ok(prefixViolation);

  const reservedViolation = result.violations.find(v => v.actual === 'delete' && v.message.includes('reserved word'));
  assert.ok(reservedViolation);
});

test('diagnosticReport - generates stable, readable output', () => {
  const violations = [
    {
      type: 'naming',
      message: "Function 'foo' does not match pattern",
      file: 'src/api.mjs',
      line: 42,
      column: 10,
      suggestion: "Use 'apiFoo' instead",
      expected: '/^api[A-Z]\\w+$/',
      actual: 'foo',
    },
    {
      type: 'fileLayout',
      message: 'File does not match any layout pattern',
      file: 'lib/helper.mjs',
      suggestion: 'Move to src/**/*.mjs',
      expected: 'Matches src/**/*.mjs',
      actual: 'lib/helper.mjs',
    },
    {
      type: 'logging',
      message: "Log statement missing required fields: context",
      file: 'src/api.mjs',
      line: 50,
      suggestion: 'Add context field',
      expected: 'timestamp, level, message, context',
      actual: 'timestamp, level, message',
    },
  ];

  const report = diagnosticReport(violations, 'company-api-v1');

  // Verify deterministic ordering (file → line → type)
  const lines = report.split('\n');
  assert.ok(lines[0].includes('Convention violations found'));
  assert.ok(lines[1].includes('Total violations: 3'));

  // lib/helper.mjs comes before src/api.mjs (alphabetical)
  const libIndex = report.indexOf('lib/helper.mjs');
  const srcIndex = report.indexOf('src/api.mjs');
  assert.ok(libIndex < srcIndex);

  // Within src/api.mjs, line 42 comes before line 50
  const line42Index = report.indexOf(':42');
  const line50Index = report.indexOf(':50');
  assert.ok(line42Index < line50Index);

  // Verify suggestions present
  assert.ok(report.includes("💡 Suggestion: Use 'apiFoo' instead"));
  assert.ok(report.includes('💡 Suggestion: Add context field'));

  // Run twice to verify stability
  const report2 = diagnosticReport(violations, 'company-api-v1');
  assert.equal(report, report2);
});

test('validateErrors - validates error class structure', () => {
  const profile = defineProfile({
    id: 'test-profile',
    fileLayout: { src: 'src/**/*.mjs', test: 'test/**/*.test.mjs' },
    naming: { exportPattern: '/.*/' },
    errors: {
      namespace: 'Api',
      fields: ['code', 'message', 'context'],
      codes: {
        NOT_FOUND: 'API_ERR_404',
        INVALID: 'API_ERR_400',
      },
    },
    logging: { fields: ['message'], levels: ['info'] },
  });

  const compiled = compileProfile(profile);

  const errorClasses = [
    {
      name: 'ApiNotFoundError',
      fields: ['code', 'message', 'context'],
      codes: ['API_ERR_404'],
      file: 'src/errors.mjs',
      line: 10,
    },
    {
      name: 'ValidationError',  // ❌ missing namespace
      fields: ['message'],      // ❌ missing fields
      codes: ['UNKNOWN_CODE'],  // ❌ invalid code
      file: 'src/errors.mjs',
      line: 20,
    },
  ];

  const result = validateErrors(errorClasses, compiled);

  assert.equal(result.ok, false);
  assert.ok(result.violationCount >= 3); // namespace, fields, codes

  const namespaceViolation = result.violations.find(v => v.message.includes('namespace'));
  assert.ok(namespaceViolation);
  assert.equal(namespaceViolation.line, 20);
});

test('validateLogging - validates log statement compliance', () => {
  const profile = defineProfile({
    id: 'test-profile',
    fileLayout: { src: 'src/**/*.mjs', test: 'test/**/*.test.mjs' },
    naming: { exportPattern: '/.*/' },
    errors: { fields: ['message'], codes: {} },
    logging: {
      fields: ['timestamp', 'level', 'message', 'context'],
      levels: ['debug', 'info', 'warn', 'error'],
      format: 'json',
      requireContext: true,
    },
  });

  const compiled = compileProfile(profile);

  const logStatements = [
    {
      fields: ['timestamp', 'level', 'message', 'context'],
      level: 'info',
      format: 'json',
      file: 'src/api.mjs',
      line: 15,
    },
    {
      fields: ['timestamp', 'level', 'message'], // ❌ missing context
      level: 'trace',                            // ❌ invalid level
      format: 'text',                            // ❌ wrong format
      file: 'src/api.mjs',
      line: 25,
    },
  ];

  const result = validateLogging(logStatements, compiled);

  assert.equal(result.ok, false);
  assert.ok(result.violationCount >= 3); // context, level, format

  const contextViolation = result.violations.find(v => v.message.includes('context'));
  assert.ok(contextViolation);
});

test('defineProfile - rejects invalid profiles', () => {
  assert.throws(() => {
    defineProfile({
      id: '',  // ❌ empty ID
      fileLayout: { src: 'src/**/*.mjs', test: 'test/**/*.test.mjs' },
      naming: { exportPattern: '/.*/' },
      errors: { fields: ['message'], codes: {} },
      logging: { fields: ['message'], levels: ['info'] },
    });
  }, /Profile ID required/);

  assert.throws(() => {
    defineProfile({
      id: 'test',
      fileLayout: { src: 'invalid', test: 'test/**/*.test.mjs' }, // ❌ invalid glob
      naming: { exportPattern: '/.*/' },
      errors: { fields: ['message'], codes: {} },
      logging: { fields: ['message'], levels: ['info'] },
    });
  }, /Must be a glob pattern/);
});

test('defineProfile - detects duplicate error codes', () => {
  assert.throws(() => {
    defineProfile({
      id: 'test-profile',
      fileLayout: { src: 'src/**/*.mjs', test: 'test/**/*.test.mjs' },
      naming: { exportPattern: '/.*/' },
      errors: {
        fields: ['message'],
        codes: {
          NOT_FOUND: 'ERR_404',
          MISSING: 'ERR_404',  // ❌ duplicate code
        },
      },
      logging: { fields: ['message'], levels: ['info'] },
    });
  }, /Duplicate error codes detected/);
});
