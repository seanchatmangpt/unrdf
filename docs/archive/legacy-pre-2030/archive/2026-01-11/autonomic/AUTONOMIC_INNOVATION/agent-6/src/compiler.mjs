import { ConventionProfileSchema } from './profile.mjs';
import { diagnosticReport as diagReport } from './diagnose.mjs';

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

/**
 * @typedef {Object} FileLayoutValidator
 * @property {function(string[]): ValidationResult} validateFileLayout
 */

/**
 * @typedef {Object} NamingValidator
 * @property {function(Array<{name: string, file: string, line: number}>): ValidationResult} validateNaming
 */

/**
 * @typedef {Object} ErrorModelValidator
 * @property {function(Array<{name: string, fields: string[], codes: string[], file: string, line: number}>): ValidationResult} validateErrors
 */

/**
 * @typedef {Object} LoggingValidator
 * @property {function(Array<{fields: string[], level: string, format: string, file: string, line: number}>): ValidationResult} validateLogging
 */

/**
 * @typedef {Object} CompiledProfile
 * @property {string} id - Profile ID
 * @property {function(string[]): ValidationResult} validateFileLayout
 * @property {function(Array<{name: string, file: string, line: number}>): ValidationResult} validateNaming
 * @property {function(Array<{name: string, fields: string[], codes: string[], file: string, line: number}>): ValidationResult} validateErrors
 * @property {function(Array<{fields: string[], level: string, format: string, file: string, line: number}>): ValidationResult} validateLogging
 * @property {function(Violation[], string=): string} diagnosticReport - Generate full diagnostic
 */

/**
 * Define and validate a conventions profile
 * @param {Object} config - Profile configuration
 * @returns {import('./profile.mjs').ConventionProfile} Validated profile
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

/**
 * Convert glob pattern to regex
 * @param {string} pattern - Glob pattern (e.g., "src/**\/*.mjs")
 * @returns {RegExp}
 */
function globToRegex(pattern) {
  // Simple glob to regex conversion
  // **/ → (.*/)? (match zero or more path segments)
  // ** → .* (match any characters)
  // * → [^/]* (match within segment)
  // Escape special regex chars
  let regex = pattern
    .replace(/\./g, '\\.') // Escape dots
    .replace(/\/\*\*\//g, '(/.*)?/') // Match **/ as optional path segments
    .replace(/\*\*/g, '.*') // Match remaining ** as any characters
    .replace(/\*/g, '[^/]*'); // Match * as non-slash characters

  return new RegExp(`^${regex}$`);
}

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

/**
 * Validate file layout against profile
 * @param {string[]} files - List of file paths
 * @param {import('./profile.mjs').FileLayoutRules} rules - File layout rules
 * @returns {ValidationResult}
 */
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

/**
 * Validate naming conventions
 * @param {Array<{name: string, file: string, line: number}>} exports - Export declarations
 * @param {import('./profile.mjs').NamingRules} rules - Naming rules
 * @param {RegExp} exportRegex - Compiled export pattern regex
 * @returns {ValidationResult}
 */
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

/**
 * Validate error model compliance
 * @param {Array<{name: string, fields: string[], codes: string[], file: string, line: number}>} errorClasses
 * @param {import('./profile.mjs').ErrorModelRules} rules - Error model rules
 * @returns {ValidationResult}
 */
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

/**
 * Validate logging compliance
 * @param {Array<{fields: string[], level: string, format: string, file: string, line: number}>} logStatements
 * @param {import('./profile.mjs').LoggingRules} rules - Logging rules
 * @returns {ValidationResult}
 */
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

/**
 * Compile profile into executable validators
 * @param {import('./profile.mjs').ConventionProfile} profile - Validated profile
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

    diagnosticReport: (violations) => diagReport(violations, profile.id),
  };
}

/**
 * Validate file layout against compiled profile
 * @param {string[]} files - List of file paths
 * @param {CompiledProfile} compiled - Compiled profile
 * @returns {ValidationResult}
 */
export function validateFileLayout(files, compiled) {
  return compiled.validateFileLayout(files);
}

/**
 * Validate naming conventions against compiled profile
 * @param {Array<{name: string, file: string, line: number}>} exports - Export declarations
 * @param {CompiledProfile} compiled - Compiled profile
 * @returns {ValidationResult}
 */
export function validateNaming(exports, compiled) {
  return compiled.validateNaming(exports);
}

/**
 * Validate error model against compiled profile
 * @param {Array<{name: string, fields: string[], codes: string[], file: string, line: number}>} errorClasses
 * @param {CompiledProfile} compiled - Compiled profile
 * @returns {ValidationResult}
 */
export function validateErrors(errorClasses, compiled) {
  return compiled.validateErrors(errorClasses);
}

/**
 * Validate logging against compiled profile
 * @param {Array<{fields: string[], level: string, format: string, file: string, line: number}>} logStatements
 * @param {CompiledProfile} compiled - Compiled profile
 * @returns {ValidationResult}
 */
export function validateLogging(logStatements, compiled) {
  return compiled.validateLogging(logStatements);
}
