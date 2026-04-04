/**
 * @file Effect Executor - Sandboxed file operations with path traversal defense
 * @module effect-executor
 *
 * @description
 * Provides safe file read/write/mkdir operations that defend against:
 * - Symlink traversal (resolves real paths before access)
 * - Path traversal via ../ sequences
 * - Absolute path escapes outside sandbox
 * - Encoded traversal patterns (%2e%2e, %252f, etc.)
 * - Null byte injection
 * - Oversized file reads (memory DoS)
 */

import { promises as fs } from 'fs';
import { resolve, normalize, isAbsolute } from 'path';

/** @type {string} Root directory for all sandboxed file operations */
export const SANDBOX_ROOT = '/tmp/hooks-sandbox';

/** @type {number} Maximum file size in bytes (10 MB) */
export const MAX_FILE_SIZE = 10 * 1024 * 1024;

/** @type {number} Maximum path length */
const MAX_PATH_LENGTH = 4096;

/**
 * Cache for the real (symlink-resolved) sandbox root path.
 * On macOS, /tmp is a symlink to /private/tmp, so we must compare
 * against the resolved path when checking fs.realpath() results.
 * @type {string|null}
 */
let _resolvedSandboxRoot = null;

/**
 * Get the real sandbox root path, resolving symlinks (e.g., /tmp → /private/tmp on macOS).
 * @returns {Promise<string>}
 */
async function getResolvedSandboxRoot() {
  if (_resolvedSandboxRoot === null) {
    await fs.mkdir(SANDBOX_ROOT, { recursive: true });
    _resolvedSandboxRoot = await fs.realpath(SANDBOX_ROOT);
  }
  return _resolvedSandboxRoot;
}

/**
 * Reset the resolved sandbox root cache (for testing).
 */
export function _resetSandboxRootCache() {
  _resolvedSandboxRoot = null;
}

/**
 * Dangerous patterns that indicate path traversal attempts.
 * Checked against the raw input before any normalization.
 */
const TRAVERSAL_PATTERNS = [
  /\.\.\//, // ../
  /\.\.\\/, // ..\
  /\.\.%2f/i, // ..%2f (URL encoded /)
  /\.\.%5c/i, // ..%5c (URL encoded \)
  /\.\.%252f/i, // ..%252f (double URL encoded)
  /\.\.%255c/i, // ..%255c (double URL encoded)
  /\.\.%c0%af/i, // overlong UTF-8 /
  /\.\.%c1%9c/i, // overlong UTF-8 \
  /%2e%2e/i, // encoded ..
  /%252e%252e/i, // double-encoded ..
];

/**
 * Validate and resolve a path to ensure it stays within SANDBOX_ROOT.
 *
 * Steps:
 *   1. Reject null bytes, encoded traversal, and overly long paths
 *   2. Reject absolute paths (must be relative to sandbox)
 *   3. Normalize and resolve against SANDBOX_ROOT
 *   4. Resolve symlinks via fs.realpath (if target exists)
 *   5. Confirm the final real path is still under SANDBOX_ROOT
 *
 * @param {string} untrustedPath - The user-supplied path to validate
 * @returns {Promise<string>} The resolved, safe absolute path
 * @throws {Error} On any policy violation
 */
export async function validatePath(untrustedPath) {
  // --- input type check ---
  if (typeof untrustedPath !== 'string') {
    throw new Error('Path must be a string');
  }

  // --- length check ---
  if (untrustedPath.length === 0) {
    throw new Error('Path must not be empty');
  }
  if (untrustedPath.length > MAX_PATH_LENGTH) {
    throw new Error('Path exceeds maximum length');
  }

  // --- null byte injection ---
  if (untrustedPath.includes('\x00')) {
    throw new Error('Path traversal detected: null byte');
  }

  // --- encoded traversal patterns (check raw input) ---
  for (const pattern of TRAVERSAL_PATTERNS) {
    if (pattern.test(untrustedPath)) {
      throw new Error('Path traversal detected');
    }
  }

  // --- reject absolute paths (must be relative to sandbox) ---
  if (isAbsolute(untrustedPath)) {
    throw new Error('Absolute paths are not allowed');
  }

  // --- normalize and resolve ---
  const normalized = normalize(untrustedPath);

  // After normalization, check again for traversal (handles edge cases)
  if (normalized.startsWith('..') || normalized.includes('/..') || normalized.includes('\\..')) {
    throw new Error('Path traversal detected');
  }

  const resolved = resolve(SANDBOX_ROOT, normalized);

  // --- boundary check (pre-symlink) ---
  if (!resolved.startsWith(SANDBOX_ROOT + '/') && resolved !== SANDBOX_ROOT) {
    throw new Error('Path traversal detected: escapes sandbox');
  }

  // --- symlink resolution ---
  // Resolve the real sandbox root (handles /tmp → /private/tmp on macOS)
  const realRoot = await getResolvedSandboxRoot();

  // If the file/directory exists, resolve symlinks and re-check
  try {
    const realPath = await fs.realpath(resolved);
    if (!realPath.startsWith(realRoot + '/') && realPath !== realRoot) {
      throw new Error('Path traversal detected: symlink escapes sandbox');
    }
    return realPath;
  } catch (err) {
    if (err.code === 'ENOENT') {
      // File doesn't exist yet (e.g., for writes) — check parent exists and is safe
      const parentResolved = resolve(resolved, '..');
      try {
        const parentReal = await fs.realpath(parentResolved);
        if (!parentReal.startsWith(realRoot + '/') && parentReal !== realRoot) {
          throw new Error('Path traversal detected: parent symlink escapes sandbox');
        }
      } catch (parentErr) {
        if (parentErr.code === 'ENOENT') {
          // Parent doesn't exist either — that's OK for mkdir -p style usage
          // The resolved path already passed the prefix check above
          return resolved;
        }
        throw parentErr;
      }
      return resolved;
    }
    // Re-throw symlink escape errors
    if (err.message.includes('Path traversal detected')) {
      throw err;
    }
    throw err;
  }
}

/**
 * Safely read a file from within the sandbox.
 *
 * @param {string} filePath - Relative path within SANDBOX_ROOT
 * @param {Object} [options] - Read options
 * @param {string} [options.encoding='utf-8'] - File encoding
 * @param {number} [options.maxSize] - Maximum file size in bytes (default: MAX_FILE_SIZE)
 * @returns {Promise<string|Buffer>} File contents
 * @throws {Error} On path violation or oversized file
 */
export async function safeFileRead(filePath, options = {}) {
  const safePath = await validatePath(filePath);
  const maxSize = options.maxSize ?? MAX_FILE_SIZE;
  const encoding = options.encoding ?? 'utf-8';

  // Check file size before reading to prevent memory DoS
  const stat = await fs.stat(safePath);
  if (stat.size > maxSize) {
    throw new Error(`File size ${stat.size} exceeds limit of ${maxSize} bytes`);
  }

  return fs.readFile(safePath, { encoding });
}

/**
 * Safely write a file within the sandbox.
 *
 * @param {string} filePath - Relative path within SANDBOX_ROOT
 * @param {string|Buffer} data - Data to write
 * @param {Object} [options] - Write options
 * @param {string} [options.encoding='utf-8'] - File encoding
 * @param {number} [options.maxSize] - Maximum data size in bytes (default: MAX_FILE_SIZE)
 * @returns {Promise<void>}
 * @throws {Error} On path violation or oversized data
 */
export async function safeFileWrite(filePath, data, options = {}) {
  const maxSize = options.maxSize ?? MAX_FILE_SIZE;
  const encoding = options.encoding ?? 'utf-8';

  // Check data size before writing
  const dataSize = Buffer.byteLength(data);
  if (dataSize > maxSize) {
    throw new Error(`Data size ${dataSize} exceeds limit of ${maxSize} bytes`);
  }

  const safePath = await validatePath(filePath);
  await fs.writeFile(safePath, data, { encoding });
}

/**
 * Safely create a directory within the sandbox.
 *
 * @param {string} dirPath - Relative path within SANDBOX_ROOT
 * @param {Object} [options] - mkdir options
 * @param {boolean} [options.recursive=true] - Create parent directories
 * @returns {Promise<string|undefined>} The first directory path created, or undefined
 * @throws {Error} On path violation
 */
export async function safeMkdir(dirPath, options = {}) {
  const safePath = await validatePath(dirPath);
  return fs.mkdir(safePath, { recursive: options.recursive ?? true });
}

/**
 * Safely stat a file within the sandbox.
 *
 * @param {string} filePath - Relative path within SANDBOX_ROOT
 * @returns {Promise<import('fs').Stats>} File stats
 * @throws {Error} On path violation
 */
export async function safeStat(filePath) {
  const safePath = await validatePath(filePath);
  return fs.stat(safePath);
}

/**
 * Safely list directory contents within the sandbox.
 *
 * @param {string} dirPath - Relative path within SANDBOX_ROOT
 * @returns {Promise<string[]>} Directory entries
 * @throws {Error} On path violation
 */
export async function safeReaddir(dirPath) {
  const safePath = await validatePath(dirPath);
  return fs.readdir(safePath);
}

/**
 * Safely delete a file within the sandbox.
 *
 * @param {string} filePath - Relative path within SANDBOX_ROOT
 * @returns {Promise<void>}
 * @throws {Error} On path violation
 */
export async function safeUnlink(filePath) {
  const safePath = await validatePath(filePath);
  return fs.unlink(safePath);
}

/**
 * Ensure the sandbox root directory exists.
 *
 * @returns {Promise<void>}
 */
export async function ensureSandboxRoot() {
  await fs.mkdir(SANDBOX_ROOT, { recursive: true });
}

// ============================================================================
// Code Injection Defense Layer
// ============================================================================

/**
 * Dangerous code patterns that indicate injection attempts.
 * Each entry has a regex pattern and a human-readable description.
 */
const DANGEROUS_CODE_PATTERNS = [
  // Function constructor variants
  { pattern: /\bFunction\s*\(/, desc: 'Function constructor call' },
  { pattern: /\bnew\s+Function\b/, desc: 'new Function constructor' },
  { pattern: /\bFunction\s*\[/, desc: 'Function bracket access' },
  { pattern: /\bFunction\.prototype\b/, desc: 'Function.prototype access' },
  { pattern: /\bFunction\.constructor\b/, desc: 'Function.constructor access' },
  { pattern: /=\s*Function\b/, desc: 'Function constructor assignment' },

  // eval variants
  { pattern: /\beval\s*\(/, desc: 'eval() call' },
  { pattern: /\beval\s*\[/, desc: 'eval bracket access' },
  { pattern: /\(['"]eval['"]\)/, desc: 'indirect eval reference' },

  // Constructor escape
  { pattern: /\.constructor\s*\(/, desc: 'constructor invocation' },
  { pattern: /\.constructor\s*\[/, desc: 'constructor bracket access' },
  { pattern: /\['constructor'\]/, desc: 'constructor string access' },
  { pattern: /\["constructor"\]/, desc: 'constructor string access (double quotes)' },
  { pattern: /\[`constructor`\]/, desc: 'constructor template literal access' },

  // Prototype pollution
  { pattern: /__proto__/, desc: '__proto__ access' },
  { pattern: /\.prototype\s*\[/, desc: 'prototype bracket access' },
  { pattern: /\.prototype\.constructor/, desc: 'prototype.constructor chain' },
  { pattern: /Object\s*\.\s*setPrototypeOf/, desc: 'Object.setPrototypeOf' },
  { pattern: /Object\s*\.\s*getPrototypeOf/, desc: 'Object.getPrototypeOf' },
  { pattern: /Object\s*\.\s*defineProperty/, desc: 'Object.defineProperty' },
  { pattern: /Object\s*\.\s*defineProperties/, desc: 'Object.defineProperties' },
  { pattern: /Reflect\s*\.\s*setPrototypeOf/, desc: 'Reflect.setPrototypeOf' },
  { pattern: /Reflect\s*\.\s*defineProperty/, desc: 'Reflect.defineProperty' },
  { pattern: /Reflect\s*\.\s*construct/, desc: 'Reflect.construct' },

  // require/import injection
  { pattern: /\brequire\s*\(/, desc: 'require() call' },
  { pattern: /\bimport\s*\(/, desc: 'dynamic import()' },
  { pattern: /\bimport\.meta\b/, desc: 'import.meta access' },

  // Process/global access
  { pattern: /\bprocess\s*\./, desc: 'process object access' },
  { pattern: /\bprocess\s*\[/, desc: 'process bracket access' },
  { pattern: /\bglobalThis\b/, desc: 'globalThis access' },

  // Dangerous object access
  { pattern: /\bnew\s+Proxy\b/, desc: 'new Proxy constructor' },
  { pattern: /\bWeakRef\s*\(/, desc: 'WeakRef constructor' },
  { pattern: /\bFinalizationRegistry\b/, desc: 'FinalizationRegistry access' },

  // WebAssembly
  { pattern: /\bWebAssembly\b/, desc: 'WebAssembly access' },

  // SharedArrayBuffer (Spectre-class attacks)
  { pattern: /\bSharedArrayBuffer\b/, desc: 'SharedArrayBuffer access' },
  { pattern: /\bAtomics\b/, desc: 'Atomics access' },

  // Indirect eval via various global accessors
  { pattern: /\(0,\s*eval\)/, desc: 'indirect eval via comma operator' },
  { pattern: /\bthis\s*\.\s*constructor/, desc: 'this.constructor access' },
  { pattern: /\bthis\s*\[\s*['"`]constructor/, desc: 'this["constructor"] access' },

  // String-based code execution
  { pattern: /\bsetTimeout\s*\(\s*['"`]/, desc: 'setTimeout with string code' },
  { pattern: /\bsetInterval\s*\(\s*['"`]/, desc: 'setInterval with string code' },
  { pattern: /\bsetTimeout\b/, desc: 'setTimeout access' },
  { pattern: /\bsetInterval\b/, desc: 'setInterval access' },
  { pattern: /\bsetImmediate\b/, desc: 'setImmediate access' },

  // Symbol manipulation for sandbox escape
  { pattern: /Symbol\s*\.\s*unscopables/, desc: 'Symbol.unscopables manipulation' },
  { pattern: /Symbol\s*\.\s*hasInstance/, desc: 'Symbol.hasInstance manipulation' },
  { pattern: /Symbol\s*\.\s*toPrimitive/, desc: 'Symbol.toPrimitive manipulation' },
];

/** @type {number} Maximum allowed code length */
const MAX_CODE_LENGTH = 100000;

/**
 * Validate code string against dangerous injection patterns
 * @param {string} codeString - Code to validate
 * @returns {{ valid: boolean, violations: string[] }} Validation result
 */
export function validateCodeInjection(codeString) {
  if (typeof codeString !== 'string') {
    return { valid: false, violations: ['Code must be a string'] };
  }

  if (codeString.length > MAX_CODE_LENGTH) {
    return { valid: false, violations: [`Code exceeds maximum length of ${MAX_CODE_LENGTH}`] };
  }

  const violations = [];

  for (const { pattern, desc } of DANGEROUS_CODE_PATTERNS) {
    if (pattern.test(codeString)) {
      violations.push(`Blocked: ${desc}`);
    }
  }

  return {
    valid: violations.length === 0,
    violations,
  };
}

/**
 * Validate a function by converting to string and checking patterns
 * @param {Function} fn - Function to validate
 * @returns {{ valid: boolean, violations: string[] }} Validation result
 */
export function validateFunction(fn) {
  if (typeof fn !== 'function') {
    return { valid: false, violations: ['Input must be a function'] };
  }

  return validateCodeInjection(fn.toString());
}

/**
 * Create a hardened execution context with all dangerous
 * constructors and functions neutralized
 * @returns {Object} Hardened context (frozen)
 */
export function createHardenedContext() {
  const context = Object.create(null);

  // Safe globals
  context.Math = Math;
  context.JSON = JSON;
  context.Array = Array;
  context.String = String;
  context.Object = Object;
  context.Number = Number;
  context.Boolean = Boolean;
  context.Date = Date;
  context.RegExp = RegExp;
  context.Map = Map;
  context.Set = Set;
  context.parseInt = parseInt;
  context.parseFloat = parseFloat;
  context.isNaN = isNaN;
  context.isFinite = isFinite;
  context.encodeURI = encodeURI;
  context.decodeURI = decodeURI;
  context.encodeURIComponent = encodeURIComponent;
  context.decodeURIComponent = decodeURIComponent;

  // Disable dangerous constructors
  context.Function = undefined;
  context.eval = undefined;
  context.constructor = undefined;

  // Disable process/global/require
  context.process = undefined;
  context.global = undefined;
  context.globalThis = undefined;
  context.require = undefined;

  // Disable timers (DoS prevention)
  context.setTimeout = undefined;
  context.setInterval = undefined;
  context.setImmediate = undefined;
  context.clearTimeout = undefined;
  context.clearInterval = undefined;
  context.clearImmediate = undefined;

  // Disable dangerous APIs
  context.Proxy = undefined;
  context.Reflect = undefined;
  context.WebAssembly = undefined;
  context.SharedArrayBuffer = undefined;
  context.Atomics = undefined;
  context.WeakRef = undefined;
  context.FinalizationRegistry = undefined;
  context.Buffer = undefined;

  // Safe console (write-only logging)
  context.console = Object.freeze({
    log: (...args) => console.log('[Sandbox]', ...args),
    warn: (...args) => console.warn('[Sandbox]', ...args),
    error: (...args) => console.error('[Sandbox]', ...args),
    info: (...args) => console.info('[Sandbox]', ...args),
  });

  return Object.freeze(context);
}

/**
 * Create a SafeFunction proxy that rejects all construction
 * and execution attempts. Used as a drop-in replacement for
 * the Function constructor in sandboxed contexts.
 * @returns {Proxy} SafeFunction proxy
 */
export function createSafeFunctionProxy() {
  const handler = {
    construct() {
      throw new Error(
        'SecurityError: Function constructor is disabled in sandbox'
      );
    },
    apply() {
      throw new Error(
        'SecurityError: Function execution is disabled in sandbox'
      );
    },
    get(target, prop) {
      if (prop === 'constructor' || prop === 'prototype' || prop === '__proto__') {
        return undefined;
      }
      if (prop === Symbol.hasInstance) {
        return () => false;
      }
      return undefined;
    },
    set() {
      throw new Error(
        'SecurityError: Cannot modify SafeFunction proxy'
      );
    },
    defineProperty() {
      throw new Error(
        'SecurityError: Cannot define properties on SafeFunction proxy'
      );
    },
    deleteProperty() {
      throw new Error(
        'SecurityError: Cannot delete properties on SafeFunction proxy'
      );
    },
    getPrototypeOf() {
      return null;
    },
    setPrototypeOf() {
      throw new Error(
        'SecurityError: Cannot set prototype on SafeFunction proxy'
      );
    },
  };

  // Use arrow function (no .prototype property) with null prototype
  // to satisfy Proxy invariants for non-configurable properties
  const target = () => {
    throw new Error('SecurityError: SafeFunction cannot be called');
  };
  Object.setPrototypeOf(target, null);
  Object.freeze(target);

  return new Proxy(target, handler);
}

/**
 * Log an injection attempt with full context
 * @param {string} code - The code that was blocked (truncated)
 * @param {string[]} violations - List of violations detected
 * @param {object[]} log - Array to append the attempt to
 */
export function logInjectionAttempt(code, violations, log = []) {
  const truncated = typeof code === 'string' ? code.substring(0, 500) : '';
  let hash = 0;
  for (let i = 0; i < truncated.length; i++) {
    hash = ((hash << 5) - hash + truncated.charCodeAt(i)) | 0;
  }

  const attempt = {
    code: truncated,
    violations,
    timestamp: new Date(),
    codeHash: hash.toString(16),
  };

  log.push(attempt);

  console.warn(
    `[EffectExecutor] INJECTION ATTEMPT BLOCKED:\n` +
    `  Violations: ${violations.join(', ')}\n` +
    `  Code hash: ${attempt.codeHash}\n` +
    `  Timestamp: ${attempt.timestamp.toISOString()}`
  );

  return attempt;
}

/**
 * Execute a function with full code injection validation and
 * hardened context. Combines validation + context creation + execution.
 * @param {Function} effect - Effect function to execute
 * @param {Object} [context={}] - Execution context (event, store, delta)
 * @param {Object} [options={}] - Options
 * @param {number} [options.timeout=30000] - Execution timeout in ms
 * @param {boolean} [options.logAttempts=true] - Whether to log blocked attempts
 * @returns {Promise<Object>} Execution result
 */
export async function executeHardened(effect, context = {}, options = {}) {
  const timeout = options.timeout ?? 30000;
  const startTime = Date.now();
  const injectionLog = options._injectionLog ?? [];

  // Step 1: Validate the function code
  const validation = validateFunction(effect);

  if (!validation.valid) {
    if (options.logAttempts !== false) {
      logInjectionAttempt(effect.toString(), validation.violations, injectionLog);
    }

    return {
      success: false,
      error: `SecurityError: Code injection detected - ${validation.violations.join('; ')}`,
      violations: validation.violations,
      duration: Date.now() - startTime,
      blocked: true,
    };
  }

  // Step 2: Create hardened context
  const hardenedContext = createHardenedContext();

  // Step 3: Execute with timeout
  try {
    const timeoutPromise = new Promise((_, reject) => {
      const timer = setTimeout(() => {
        reject(new Error(`Execution timeout after ${timeout}ms`));
      }, timeout);
      if (timer.unref) timer.unref();
    });

    const executionPromise = Promise.resolve().then(() => {
      return effect.call(hardenedContext, context);
    });

    const result = await Promise.race([executionPromise, timeoutPromise]);

    return {
      success: true,
      result,
      duration: Date.now() - startTime,
      blocked: false,
    };
  } catch (error) {
    return {
      success: false,
      error: error.message,
      duration: Date.now() - startTime,
      blocked: false,
    };
  }
}
