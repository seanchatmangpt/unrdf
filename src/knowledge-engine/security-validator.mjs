/**
 * @file Security Validator for Knowledge Hooks
 * @module security-validator
 * 
 * @description
 * Security validation functions for preventing malicious patterns,
 * injection attacks, and unauthorized access in knowledge hooks.
 */

import { z } from 'zod';

/**
 * Schema for security validation result
 */
const SecurityValidationResultSchema = z.object({
  valid: z.boolean(),
  violations: z.array(z.string()).default([]),
  blocked: z.boolean().default(false),
  blockReason: z.string().optional(),
  securityViolation: z.string().optional()
});

/**
 * Security Validator for Knowledge Hooks
 */
export class SecurityValidator {
  constructor(options = {}) {
    this.strictMode = options.strictMode ?? true;
    this.enablePathTraversalCheck = options.enablePathTraversalCheck ?? true;
    this.enableInjectionCheck = options.enableInjectionCheck ?? true;
    this.enableResourceLimitCheck = options.enableResourceLimitCheck ?? true;
  }

  /**
   * Validate a file URI for malicious patterns
   * @param {string} uri - The file URI to validate
   * @returns {Object} Validation result
   */
  validateFileUri(uri) {
    const violations = [];

    if (!uri || typeof uri !== 'string') {
      return {
        valid: false,
        violations: ['Invalid URI: must be a non-empty string'],
        blocked: true,
        blockReason: 'Invalid URI format'
      };
    }

    // Check for path traversal patterns
    if (this.enablePathTraversalCheck) {
      const pathTraversalPatterns = [
        /\.\.\//g,           // ../ 
        /\.\.\\/g,           // ..\
        /\.\.%2f/gi,         // ..%2f (URL encoded)
        /\.\.%5c/gi,         // ..%5c (URL encoded)
        /\.\.%252f/gi,       // Double URL encoded
        /\.\.%255c/gi,       // Double URL encoded
        /\.\.%c0%af/gi,      // Unicode bypass
        /\.\.%c1%9c/gi,      // Unicode bypass
      ];

      for (const pattern of pathTraversalPatterns) {
        if (pattern.test(uri)) {
          violations.push('Path traversal detected');
          break;
        }
      }

      // Check for absolute paths
      if (uri.includes('://') && !uri.startsWith('file://')) {
        violations.push('Absolute path detected');
      }

      // Check for system paths
      const systemPaths = ['/etc/', '/usr/', '/bin/', '/sbin/', '/var/', 'C:\\', 'D:\\'];
      for (const sysPath of systemPaths) {
        if (uri.includes(sysPath)) {
          violations.push('System path access detected');
          break;
        }
      }
    }

    // Check for injection patterns
    if (this.enableInjectionCheck) {
      const injectionPatterns = [
        /<script[^>]*>/gi,           // Script tags
        /javascript:/gi,             // JavaScript protocol
        /data:text\/html/gi,         // Data URLs
        /vbscript:/gi,               // VBScript protocol
        /on\w+\s*=/gi,              // Event handlers
        /eval\s*\(/gi,              // Eval function
        /Function\s*\(/gi,          // Function constructor
        /setTimeout\s*\(/gi,        // setTimeout
        /setInterval\s*\(/gi,       // setInterval
      ];

      for (const pattern of injectionPatterns) {
        if (pattern.test(uri)) {
          violations.push('Code injection pattern detected');
          break;
        }
      }
    }

    const blocked = violations.length > 0;
    const result = {
      valid: !blocked,
      violations,
      blocked,
      blockReason: blocked ? violations.join(', ') : undefined,
      securityViolation: blocked ? violations[0] : undefined
    };

    return SecurityValidationResultSchema.parse(result);
  }

  /**
   * Validate a SPARQL query for dangerous operations
   * @param {string} sparql - The SPARQL query to validate
   * @returns {Object} Validation result
   */
  validateSparqlQuery(sparql) {
    const violations = [];

    if (!sparql || typeof sparql !== 'string') {
      return {
        valid: false,
        violations: ['Invalid SPARQL: must be a non-empty string'],
        blocked: true,
        blockReason: 'Invalid SPARQL format'
      };
    }

    const upperSparql = sparql.toUpperCase();

    // Check for dangerous SPARQL operations
    const dangerousOperations = [
      'INSERT', 'DELETE', 'DROP', 'CREATE', 'LOAD', 'CLEAR',
      'COPY', 'MOVE', 'ADD', 'MODIFY', 'ALTER'
    ];

    for (const operation of dangerousOperations) {
      if (upperSparql.includes(operation)) {
        violations.push(`Dangerous SPARQL operation: ${operation}`);
      }
    }

    // Check for injection patterns
    if (this.enableInjectionCheck) {
      const injectionPatterns = [
        /UNION\s+SELECT/gi,          // UNION injection
        /';.*--/gi,                  // SQL comment injection
        /\/\*.*\*\//gi,              // Block comment injection
        /0x[0-9a-f]+/gi,             // Hex encoding
        /CHAR\s*\(/gi,               // CHAR function
        /ASCII\s*\(/gi,              // ASCII function
        /SUBSTRING\s*\(/gi,          // SUBSTRING function
        /CONCAT\s*\(/gi,             // CONCAT function
      ];

      for (const pattern of injectionPatterns) {
        if (pattern.test(sparql)) {
          violations.push('SPARQL injection pattern detected');
          break;
        }
      }
    }

    // Check for resource exhaustion patterns
    if (this.enableResourceLimitCheck) {
      const resourcePatterns = [
        /SELECT\s+\*\s+WHERE\s*\{[^}]*\?[a-zA-Z_][a-zA-Z0-9_]*\s+\?[a-zA-Z_][a-zA-Z0-9_]*\s+\?[a-zA-Z_][a-zA-Z0-9_]*[^}]*\}/gi, // Cartesian product
        /OPTIONAL\s*\{[^}]*OPTIONAL\s*\{[^}]*OPTIONAL\s*\{/gi, // Deep optional nesting
      ];

      for (const pattern of resourcePatterns) {
        if (pattern.test(sparql)) {
          violations.push('Resource exhaustion pattern detected');
          break;
        }
      }
    }

    const blocked = violations.length > 0;
    const result = {
      valid: !blocked,
      violations,
      blocked,
      blockReason: blocked ? violations.join(', ') : undefined,
      securityViolation: blocked ? violations[0] : undefined
    };

    return SecurityValidationResultSchema.parse(result);
  }

  /**
   * Validate a hook effect for security violations
   * @param {string} effectCode - The effect code to validate
   * @returns {Object} Validation result
   */
  validateEffectCode(effectCode) {
    const violations = [];

    if (!effectCode || typeof effectCode !== 'string') {
      return {
        valid: false,
        violations: ['Invalid effect code: must be a non-empty string'],
        blocked: true,
        blockReason: 'Invalid effect code format'
      };
    }

    // Check for dangerous JavaScript patterns
    const dangerousPatterns = [
      /require\s*\(/gi,             // require() calls
      /import\s+.*\s+from/gi,       // ES6 imports
      /process\./gi,                // process object access
      /global\./gi,                 // global object access
      /window\./gi,                 // window object access
      /document\./gi,               // document object access
      /eval\s*\(/gi,                // eval function
      /Function\s*\(/gi,            // Function constructor
      /setTimeout\s*\(/gi,          // setTimeout
      /setInterval\s*\(/gi,         // setInterval
      /setImmediate\s*\(/gi,        // setImmediate
      /fs\./gi,                     // filesystem access
      /child_process/gi,            // child process
      /os\./gi,                     // OS access
      /crypto\./gi,                 // crypto access
      /http\./gi,                   // HTTP access
      /https\./gi,                  // HTTPS access
      /net\./gi,                    // network access
      /dns\./gi,                    // DNS access
      /tls\./gi,                    // TLS access
      /cluster\./gi,                // cluster access
      /worker_threads/gi,           // worker threads
    ];

    for (const pattern of dangerousPatterns) {
      if (pattern.test(effectCode)) {
        violations.push(`Dangerous JavaScript pattern: ${pattern.source}`);
      }
    }

    // Check for infinite loops
    const loopPatterns = [
      /while\s*\(\s*true\s*\)/gi,   // while(true)
      /for\s*\(\s*;\s*;\s*\)/gi,    // for(;;)
      /for\s*\(\s*.*\s*;\s*.*\s*;\s*\)/gi, // for loops without increment
    ];

    for (const pattern of loopPatterns) {
      if (pattern.test(effectCode)) {
        violations.push('Potential infinite loop detected');
      }
    }

    const blocked = violations.length > 0;
    const result = {
      valid: !blocked,
      violations,
      blocked,
      blockReason: blocked ? violations.join(', ') : undefined,
      securityViolation: blocked ? violations[0] : undefined
    };

    return SecurityValidationResultSchema.parse(result);
  }

  /**
   * Validate a knowledge hook for security violations
   * @param {Object} hook - The knowledge hook to validate
   * @returns {Object} Validation result
   */
  validateKnowledgeHook(hook) {
    const violations = [];

    if (!hook || typeof hook !== 'object') {
      return {
        valid: false,
        violations: ['Invalid hook: must be an object'],
        blocked: true,
        blockReason: 'Invalid hook format'
      };
    }

    // Validate hook metadata
    if (!hook.meta || !hook.meta.name) {
      violations.push('Hook missing required metadata');
    }

    // Validate condition
    if (hook.when) {
      if (hook.when.kind === 'sparql-ask' || hook.when.kind === 'sparql-select') {
        if (hook.when.ref && hook.when.ref.uri) {
          const uriValidation = this.validateFileUri(hook.when.ref.uri);
          if (!uriValidation.valid) {
            violations.push(...uriValidation.violations);
          }
        }
      }
    }

    // Validate effect code
    if (hook.run && typeof hook.run === 'function') {
      const effectCode = hook.run.toString();
      const effectValidation = this.validateEffectCode(effectCode);
      if (!effectValidation.valid) {
        violations.push(...effectValidation.violations);
      }
    }

    const blocked = violations.length > 0;
    const result = {
      valid: !blocked,
      violations,
      blocked,
      blockReason: blocked ? violations.join(', ') : undefined,
      securityViolation: blocked ? violations[0] : undefined
    };

    return SecurityValidationResultSchema.parse(result);
  }
}

/**
 * Create a security validator instance
 * @param {Object} [options] - Validator options
 * @returns {SecurityValidator} Validator instance
 */
export function createSecurityValidator(options = {}) {
  return new SecurityValidator(options);
}

/**
 * Default security validator instance
 */
export const defaultSecurityValidator = new SecurityValidator();
