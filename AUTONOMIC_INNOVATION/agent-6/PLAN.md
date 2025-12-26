# Agent 6: Conventions Profile Compiler

**Role**: Enforce file layout, naming conventions, error models, and logging fields through compiled profiles

## Overview

The Conventions Profile Compiler transforms declarative convention definitions into executable validators that enforce:
- **File Layout**: Directory structure and file organization
- **Naming Patterns**: File prefixes, function names, export patterns
- **Error Models**: Error class structure, fields, and code namespaces
- **Logging Standards**: Required fields, levels, and formats

**Core Principle**: Conventions as code - define once, validate everywhere, fail fast with actionable diagnostics.

## Files to Create

### Core Modules
- `./src/profile.mjs` - Profile schema definition (Zod schemas)
- `./src/compiler.mjs` - Compile profiles into validators
- `./src/diagnose.mjs` - Diagnostic output for violations
- `./src/index.mjs` - Public API exports

### Tests
- `./test/profile.test.mjs` - Profile compilation and validation tests

## Profile Schema Design

### Complete Profile Type (Zod Schema)

```javascript
/**
 * @typedef {Object} ConventionProfile
 * @property {string} id - Unique profile identifier (e.g., "company-api-v1")
 * @property {FileLayoutRules} fileLayout - Directory structure rules
 * @property {NamingRules} naming - Naming convention rules
 * @property {ErrorModelRules} errors - Error class and code rules
 * @property {LoggingRules} logging - Logging field and level rules
 */

/**
 * @typedef {Object} FileLayoutRules
 * @property {string} src - Glob pattern for source files (e.g., "src/**\/*.mjs")
 * @property {string} test - Glob pattern for test files (e.g., "test/**\/*.test.mjs")
 * @property {string} [examples] - Glob pattern for example files
 * @property {string} [docs] - Glob pattern for documentation
 */

/**
 * @typedef {Object} NamingRules
 * @property {string} [filePrefix] - Required file name prefix (e.g., "api-")
 * @property {string} [functionPrefix] - Required function name prefix (e.g., "api")
 * @property {string} exportPattern - Regex pattern for export names
 * @property {string[]} [reservedWords] - Words that cannot be used in names
 */

/**
 * @typedef {Object} ErrorModelRules
 * @property {string} [namespace] - Error namespace/prefix (e.g., "ApiError")
 * @property {string[]} fields - Required error fields (e.g., ["code", "message", "context"])
 * @property {Object.<string, string>} codes - Error code mappings
 * @property {string} [baseClass] - Required base class name
 */

/**
 * @typedef {Object} LoggingRules
 * @property {string[]} fields - Required log fields (e.g., ["timestamp", "level", "message"])
 * @property {string[]} levels - Valid log levels (e.g., ["debug", "info", "warn", "error"])
 * @property {"json"|"text"} [format] - Required log format
 * @property {boolean} [requireContext] - Context field mandatory
 */
```

### Zod Schema Implementation

```javascript
import { z } from 'zod';

export const FileLayoutSchema = z.object({
  src: z.string().regex(/\*\*\/\*/, 'Must be a glob pattern'),
  test: z.string().regex(/\*\*\/\*/, 'Must be a glob pattern'),
  examples: z.string().optional(),
  docs: z.string().optional(),
});

export const NamingSchema = z.object({
  filePrefix: z.string().optional(),
  functionPrefix: z.string().optional(),
  exportPattern: z.string().regex(/^\/.*\/$/, 'Must be a regex pattern'),
  reservedWords: z.array(z.string()).optional(),
});

export const ErrorModelSchema = z.object({
  namespace: z.string().optional(),
  fields: z.array(z.string()).min(1, 'At least one field required'),
  codes: z.record(z.string(), z.string()),
  baseClass: z.string().optional(),
});

export const LoggingSchema = z.object({
  fields: z.array(z.string()).min(1, 'At least one field required'),
  levels: z.array(z.string()).min(1, 'At least one level required'),
  format: z.enum(['json', 'text']).optional(),
  requireContext: z.boolean().optional(),
});

export const ConventionProfileSchema = z.object({
  id: z.string().min(1, 'Profile ID required'),
  fileLayout: FileLayoutSchema,
  naming: NamingSchema,
  errors: ErrorModelSchema,
  logging: LoggingSchema,
});
```

## Compiler Architecture

### Compilation Strategy

1. **Profile Validation**: Validate input profile against Zod schema
2. **Rule Compilation**: Transform declarative rules into executable validators
3. **Violation Collection**: Accumulate all violations (don't fail fast)
4. **Deterministic Ordering**: Sort violations by file → line → column → type

### CompiledProfile Structure

```javascript
/**
 * @typedef {Object} CompiledProfile
 * @property {string} id - Profile ID
 * @property {FileLayoutValidator} validateFileLayout
 * @property {NamingValidator} validateNaming
 * @property {ErrorModelValidator} validateErrors
 * @property {LoggingValidator} validateLogging
 * @property {Function} diagnosticReport - Generate full diagnostic
 */

/**
 * @typedef {Object} Violation
 * @property {'fileLayout'|'naming'|'errors'|'logging'} type
 * @property {string} message - Human-readable violation message
 * @property {string} [file] - File path where violation occurred
 * @property {number} [line] - Line number (if applicable)
 * @property {number} [column] - Column number (if applicable)
 * @property {string} [suggestion] - Suggested fix
 * @property {string} [actual] - Actual value found
 * @property {string} [expected] - Expected value
 */

/**
 * @typedef {Object} ValidationResult
 * @property {boolean} ok - True if no violations
 * @property {Violation[]} violations - Sorted violation list
 * @property {number} fileCount - Files checked
 * @property {number} violationCount - Total violations
 */
```

## Key Functions

### 1. defineProfile(config)

```javascript
/**
 * Define and validate a conventions profile
 * @param {Object} config - Profile configuration
 * @returns {ConventionProfile} Validated profile
 * @throws {Error} If profile invalid
 */
export function defineProfile(config) {
  // 1. Parse and validate with Zod
  const profile = ConventionProfileSchema.parse(config);

  // 2. Normalize regex patterns (add delimiters if missing)
  if (profile.naming.exportPattern && !profile.naming.exportPattern.startsWith('/')) {
    profile.naming.exportPattern = `/${profile.naming.exportPattern}/`;
  }

  // 3. Validate error codes are unique
  const errorCodes = Object.values(profile.errors.codes);
  const uniqueCodes = new Set(errorCodes);
  if (errorCodes.length !== uniqueCodes.size) {
    throw new Error('Duplicate error codes detected');
  }

  // 4. Return validated profile
  return profile;
}
```

### 2. compileProfile(profile)

```javascript
/**
 * Compile profile into executable validators
 * @param {ConventionProfile} profile - Validated profile
 * @returns {CompiledProfile} Compiled validators
 */
export function compileProfile(profile) {
  // Compile regex patterns once
  const exportRegex = new RegExp(
    profile.naming.exportPattern.slice(1, -1) // Remove / delimiters
  );

  return {
    id: profile.id,

    validateFileLayout: (files) => validateFileLayoutImpl(files, profile.fileLayout),
    validateNaming: (exports) => validateNamingImpl(exports, profile.naming, exportRegex),
    validateErrors: (errorClasses) => validateErrorsImpl(errorClasses, profile.errors),
    validateLogging: (logStatements) => validateLoggingImpl(logStatements, profile.logging),

    diagnosticReport: (violations) => diagnosticReportImpl(violations, profile.id),
  };
}
```

### 3. validateFileLayout(files, compiled)

```javascript
/**
 * Validate file layout against profile
 * @param {string[]} files - List of file paths
 * @param {CompiledProfile} compiled - Compiled profile
 * @returns {ValidationResult}
 */
export function validateFileLayout(files, compiled) {
  return compiled.validateFileLayout(files);
}

// Internal implementation
function validateFileLayoutImpl(files, rules) {
  const violations = [];
  const srcPattern = globToRegex(rules.src);
  const testPattern = globToRegex(rules.test);

  for (const file of files) {
    const isSrc = srcPattern.test(file);
    const isTest = testPattern.test(file);

    if (file.endsWith('.mjs') && !isSrc && !isTest) {
      violations.push({
        type: 'fileLayout',
        message: `File does not match any layout pattern`,
        file,
        suggestion: `Move to ${rules.src} or ${rules.test}`,
        expected: `Matches ${rules.src} or ${rules.test}`,
        actual: file,
      });
    }
  }

  return createValidationResult(violations, files.length);
}
```

### 4. validateNaming(exports, compiled)

```javascript
/**
 * Validate naming conventions
 * @param {Array<{name: string, file: string, line: number}>} exports - Export declarations
 * @param {CompiledProfile} compiled - Compiled profile
 * @returns {ValidationResult}
 */
export function validateNaming(exports, compiled) {
  return compiled.validateNaming(exports);
}

// Internal implementation
function validateNamingImpl(exports, rules, exportRegex) {
  const violations = [];

  for (const exp of exports) {
    const { name, file, line } = exp;

    // Check export pattern
    if (!exportRegex.test(name)) {
      violations.push({
        type: 'naming',
        message: `Export '${name}' does not match pattern`,
        file,
        line,
        suggestion: `Use pattern: ${rules.exportPattern}`,
        expected: rules.exportPattern,
        actual: name,
      });
    }

    // Check file prefix
    if (rules.filePrefix) {
      const fileName = file.split('/').pop();
      if (!fileName.startsWith(rules.filePrefix)) {
        violations.push({
          type: 'naming',
          message: `File '${fileName}' missing required prefix`,
          file,
          suggestion: `Rename to ${rules.filePrefix}${fileName}`,
          expected: `${rules.filePrefix}*`,
          actual: fileName,
        });
      }
    }

    // Check function prefix
    if (rules.functionPrefix && !name.startsWith(rules.functionPrefix)) {
      violations.push({
        type: 'naming',
        message: `Function '${name}' missing required prefix`,
        file,
        line,
        suggestion: `Rename to ${rules.functionPrefix}${name.charAt(0).toUpperCase() + name.slice(1)}`,
        expected: `${rules.functionPrefix}*`,
        actual: name,
      });
    }

    // Check reserved words
    if (rules.reservedWords?.includes(name)) {
      violations.push({
        type: 'naming',
        message: `Export '${name}' uses reserved word`,
        file,
        line,
        suggestion: `Choose a different name (reserved: ${rules.reservedWords.join(', ')})`,
        expected: 'Non-reserved name',
        actual: name,
      });
    }
  }

  return createValidationResult(violations, exports.length);
}
```

### 5. validateErrors(errorClasses, compiled)

```javascript
/**
 * Validate error model compliance
 * @param {Array<{name: string, fields: string[], codes: string[], file: string, line: number}>} errorClasses
 * @param {CompiledProfile} compiled - Compiled profile
 * @returns {ValidationResult}
 */
export function validateErrors(errorClasses, compiled) {
  return compiled.validateErrors(errorClasses);
}

// Internal implementation
function validateErrorsImpl(errorClasses, rules) {
  const violations = [];

  for (const errorClass of errorClasses) {
    const { name, fields, codes, file, line } = errorClass;

    // Check namespace
    if (rules.namespace && !name.startsWith(rules.namespace)) {
      violations.push({
        type: 'errors',
        message: `Error class '${name}' missing namespace prefix`,
        file,
        line,
        suggestion: `Rename to ${rules.namespace}${name}`,
        expected: `${rules.namespace}*`,
        actual: name,
      });
    }

    // Check required fields
    const missingFields = rules.fields.filter(f => !fields.includes(f));
    if (missingFields.length > 0) {
      violations.push({
        type: 'errors',
        message: `Error class '${name}' missing required fields: ${missingFields.join(', ')}`,
        file,
        line,
        suggestion: `Add fields: ${missingFields.join(', ')}`,
        expected: rules.fields.join(', '),
        actual: fields.join(', '),
      });
    }

    // Check error codes
    const invalidCodes = codes.filter(c => !Object.values(rules.codes).includes(c));
    if (invalidCodes.length > 0) {
      violations.push({
        type: 'errors',
        message: `Error class '${name}' uses invalid codes: ${invalidCodes.join(', ')}`,
        file,
        line,
        suggestion: `Use codes from: ${Object.keys(rules.codes).join(', ')}`,
        expected: Object.values(rules.codes).join(', '),
        actual: invalidCodes.join(', '),
      });
    }
  }

  return createValidationResult(violations, errorClasses.length);
}
```

### 6. validateLogging(logStatements, compiled)

```javascript
/**
 * Validate logging compliance
 * @param {Array<{fields: string[], level: string, format: string, file: string, line: number}>} logStatements
 * @param {CompiledProfile} compiled - Compiled profile
 * @returns {ValidationResult}
 */
export function validateLogging(logStatements, compiled) {
  return compiled.validateLogging(logStatements);
}

// Internal implementation
function validateLoggingImpl(logStatements, rules) {
  const violations = [];

  for (const stmt of logStatements) {
    const { fields, level, format, file, line } = stmt;

    // Check required fields
    const missingFields = rules.fields.filter(f => !fields.includes(f));
    if (missingFields.length > 0) {
      violations.push({
        type: 'logging',
        message: `Log statement missing required fields: ${missingFields.join(', ')}`,
        file,
        line,
        suggestion: `Add fields: ${missingFields.join(', ')}`,
        expected: rules.fields.join(', '),
        actual: fields.join(', '),
      });
    }

    // Check log level
    if (!rules.levels.includes(level)) {
      violations.push({
        type: 'logging',
        message: `Invalid log level '${level}'`,
        file,
        line,
        suggestion: `Use one of: ${rules.levels.join(', ')}`,
        expected: rules.levels.join(', '),
        actual: level,
      });
    }

    // Check format
    if (rules.format && format !== rules.format) {
      violations.push({
        type: 'logging',
        message: `Log format '${format}' does not match required format`,
        file,
        line,
        suggestion: `Use format: ${rules.format}`,
        expected: rules.format,
        actual: format,
      });
    }

    // Check context requirement
    if (rules.requireContext && !fields.includes('context')) {
      violations.push({
        type: 'logging',
        message: `Log statement missing required 'context' field`,
        file,
        line,
        suggestion: `Add context field to log statement`,
        expected: 'context field present',
        actual: 'context field missing',
      });
    }
  }

  return createValidationResult(violations, logStatements.length);
}
```

### 7. diagnosticReport(violations, profileId)

```javascript
/**
 * Generate human-readable diagnostic report
 * @param {Violation[]} violations - Sorted violations
 * @param {string} [profileId] - Profile identifier
 * @returns {string} Formatted diagnostic report
 */
export function diagnosticReport(violations, profileId) {
  if (violations.length === 0) {
    return `✅ All conventions validated (profile: ${profileId || 'unknown'})\n`;
  }

  // Sort violations deterministically:
  // 1. file (alphabetical)
  // 2. line (numerical)
  // 3. column (numerical)
  // 4. type (alphabetical)
  const sorted = [...violations].sort((a, b) => {
    if (a.file !== b.file) return (a.file || '').localeCompare(b.file || '');
    if (a.line !== b.line) return (a.line || 0) - (b.line || 0);
    if (a.column !== b.column) return (a.column || 0) - (b.column || 0);
    return a.type.localeCompare(b.type);
  });

  const lines = [
    `❌ Convention violations found (profile: ${profileId || 'unknown'})`,
    `Total violations: ${violations.length}`,
    '',
  ];

  // Group by file
  const byFile = new Map();
  for (const v of sorted) {
    const key = v.file || '(no file)';
    if (!byFile.has(key)) byFile.set(key, []);
    byFile.get(key).push(v);
  }

  for (const [file, fileViolations] of byFile) {
    lines.push(`📄 ${file}`);

    for (const v of fileViolations) {
      const location = v.line ? `:${v.line}${v.column ? `:${v.column}` : ''}` : '';
      lines.push(`  [${v.type}]${location} ${v.message}`);

      if (v.expected) {
        lines.push(`    Expected: ${v.expected}`);
      }
      if (v.actual) {
        lines.push(`    Actual: ${v.actual}`);
      }
      if (v.suggestion) {
        lines.push(`    💡 Suggestion: ${v.suggestion}`);
      }
      lines.push('');
    }
  }

  return lines.join('\n');
}
```

## Utility Functions

### globToRegex(pattern)

```javascript
/**
 * Convert glob pattern to regex
 * @param {string} pattern - Glob pattern (e.g., "src/**\/*.mjs")
 * @returns {RegExp}
 */
function globToRegex(pattern) {
  // Simple glob to regex conversion
  // ** → .* (match any path segments)
  // * → [^/]* (match within segment)
  // Escape special regex chars
  let regex = pattern
    .replace(/\./g, '\\.')
    .replace(/\*\*/g, '.*')
    .replace(/\*/g, '[^/]*');

  return new RegExp(`^${regex}$`);
}
```

### createValidationResult(violations, itemCount)

```javascript
/**
 * Create standardized validation result
 * @param {Violation[]} violations
 * @param {number} itemCount
 * @returns {ValidationResult}
 */
function createValidationResult(violations, itemCount) {
  return {
    ok: violations.length === 0,
    violations,
    fileCount: itemCount,
    violationCount: violations.length,
  };
}
```

## Test Scenarios

### Test 1: Company API Profile

```javascript
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
});
```

### Test 2: File Layout Violation Detection

```javascript
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
  assert.equal(result.violations[0].file, 'lib/helper.mjs');
});
```

### Test 3: Naming Pattern Violations

```javascript
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
  assert.equal(result.violationCount, 2);

  const prefixViolation = result.violations.find(v => v.actual === 'createUser');
  assert.ok(prefixViolation);
  assert.match(prefixViolation.message, /missing required prefix/);

  const reservedViolation = result.violations.find(v => v.actual === 'delete');
  assert.ok(reservedViolation);
  assert.match(reservedViolation.message, /reserved word/);
});
```

### Test 4: Comprehensive Diagnostic Output

```javascript
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
```

### Test 5: Error Model Validation

```javascript
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
```

### Test 6: Logging Validation

```javascript
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
```

## Public API Exports

```javascript
// ./src/index.mjs
export {
  defineProfile,
  compileProfile,
  validateFileLayout,
  validateNaming,
  validateErrors,
  validateLogging,
  diagnosticReport,
} from './compiler.mjs';

export {
  ConventionProfileSchema,
  FileLayoutSchema,
  NamingSchema,
  ErrorModelSchema,
  LoggingSchema,
} from './profile.mjs';
```

## Dependencies

- **zod**: Profile schema validation
- **No external file I/O**: Tests provide file/export lists as arrays
- **No OTEL in business logic**: Pure validation functions

## Implementation Checklist

### Phase 1: Schema Definition
- [ ] Create `./src/profile.mjs` with Zod schemas
- [ ] Define all TypeDef JSDoc comments
- [ ] Export all schemas

### Phase 2: Compiler Implementation
- [ ] Implement `defineProfile()` with Zod validation
- [ ] Implement `compileProfile()` with regex compilation
- [ ] Implement `validateFileLayoutImpl()`
- [ ] Implement `validateNamingImpl()`
- [ ] Implement `validateErrorsImpl()`
- [ ] Implement `validateLoggingImpl()`
- [ ] Implement utility functions (globToRegex, createValidationResult)

### Phase 3: Diagnostics
- [ ] Implement `diagnosticReport()` with deterministic sorting
- [ ] Ensure stable output across runs
- [ ] Add human-readable formatting with emojis
- [ ] Include suggestions for all violation types

### Phase 4: Testing
- [ ] Test 1: Company API profile definition
- [ ] Test 2: File layout violation detection
- [ ] Test 3: Naming pattern violations
- [ ] Test 4: Comprehensive diagnostic output (stability test)
- [ ] Test 5: Error model validation
- [ ] Test 6: Logging validation
- [ ] Run all tests with `timeout 5s npm test`

### Phase 5: Integration
- [ ] Create `./src/index.mjs` with exports
- [ ] Verify all exports work
- [ ] Document example usage
- [ ] Update agent-1 integration

## Success Criteria (Adversarial PM Questions)

### Claims vs Reality
- [ ] ❓ Did I RUN all tests? (Show `timeout 5s npm test` output)
- [ ] ❓ Did diagnostic output produce IDENTICAL results on two runs?
- [ ] ❓ Can I PROVE violations are sorted deterministically?
- [ ] ❓ Does Zod validation reject ALL invalid inputs?

### Evidence Quality
- [ ] Test output showing 100% pass rate
- [ ] Two diagnostic runs with identical hashes
- [ ] Violation sorting verified (file → line → column → type)
- [ ] All JSDoc types validated (no TypeScript, pure JSDoc)

### Process Quality
- [ ] All validation functions are pure (no side effects, no OTEL)
- [ ] No external file I/O (tests provide arrays)
- [ ] Deterministic output (sorted violations, stable report)
- [ ] Pattern reuse from existing Zod schemas

### Red Flags (Must be ZERO)
- ❌ "Tests should pass" → Didn't run them
- ❌ "Output looks deterministic" → Didn't verify with hash
- ❌ "Validates correctly" → Didn't test edge cases
- ❌ Non-deterministic ordering → Breaks reproducibility

## Example Usage

```javascript
import { defineProfile, compileProfile, validateNaming, diagnosticReport } from '@unrdf/autonomic';

// Define company conventions
const profile = defineProfile({
  id: 'company-api-v1',
  fileLayout: {
    src: 'src/**/*.mjs',
    test: 'test/**/*.test.mjs',
  },
  naming: {
    functionPrefix: 'api',
    exportPattern: '/^api[A-Z]\\w+$/',
  },
  errors: {
    namespace: 'Api',
    fields: ['code', 'message', 'context'],
    codes: {
      NOT_FOUND: 'API_ERR_404',
      UNAUTHORIZED: 'API_ERR_401',
    },
  },
  logging: {
    fields: ['timestamp', 'level', 'message', 'context'],
    levels: ['debug', 'info', 'warn', 'error'],
    format: 'json',
  },
});

// Compile into validators
const compiled = compileProfile(profile);

// Validate exports (from AST parsing)
const exports = [
  { name: 'apiCreateUser', file: 'src/api.mjs', line: 10 },
  { name: 'createUser', file: 'src/api.mjs', line: 20 }, // ❌ violation
];

const result = validateNaming(exports, compiled);

if (!result.ok) {
  console.log(diagnosticReport(result.violations, profile.id));
}
```

## Determinism Guarantees

1. **Profile Validation**: Zod ensures consistent parsing
2. **Violation Sorting**: Deterministic multi-key sort (file → line → column → type)
3. **Report Generation**: Stable string formatting (no Date.now(), no random)
4. **Regex Compilation**: Same input → same compiled regex
5. **Hash Verification**: Run diagnostic twice, compare output hashes

## Integration with Agent 1

Agent 1 will export:
```javascript
export { compileProfile, defineProfile, diagnosticReport } from '../agent-6/src/index.mjs';
```

And use in demo:
```javascript
// Step 1: Define conventions
const profile = defineProfile({ /* company rules */ });

// Step 2: Compile validators
const compiled = compileProfile(profile);

// Step 3: Validate codebase (from static analysis)
const violations = [
  ...validateFileLayout(files, compiled).violations,
  ...validateNaming(exports, compiled).violations,
  ...validateErrors(errorClasses, compiled).violations,
  ...validateLogging(logStatements, compiled).violations,
];

// Step 4: Generate report
if (violations.length > 0) {
  console.log(diagnosticReport(violations, profile.id));
}
```

---

**Final Verification**: Before declaring complete, run:
```bash
timeout 5s npm test                           # All tests pass?
node -e "import('./src/index.mjs')"          # Imports work?
grep -r "from 'n3'" src/                     # Zero N3 imports?
node test/stability.mjs                       # Two runs = identical output?
```

**This plan is COMPLETE when**: All checkboxes verified with evidence, tests at 100%, and diagnostic output is deterministically stable across runs.
