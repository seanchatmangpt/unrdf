/**
 * KGEN Validation Engine
 *
 * Provides comprehensive validation for injection targets and content
 * including syntax, semantics, encoding, and security checks.
 */

import { promises as fs } from 'fs';
import { basename, extname } from 'path';
import { createHash } from 'crypto';

import {
  ERROR_CODES,
  VALIDATION_RULES,
  BINARY_PATTERNS,
  CONTENT_PATTERNS,
  ENCODINGS,
  CHECKSUM_ALGORITHMS
} from './constants.js';

export class ValidationEngine {
  constructor(config = {}) {
    this.config = config;
    this.validationCache = new Map();
  }

  /**
   * Validate injection target
   */
  async validateTarget(target) {
    const cacheKey = this._getCacheKey(target);

    if (this.validationCache.has(cacheKey)) {
      return this.validationCache.get(cacheKey);
    }

    const result = {
      valid: true,
      errors: [],
      warnings: [],
      checks: {
        existence: null,
        permissions: null,
        encoding: null,
        size: null,
        binary: null,
        syntax: null
      }
    };

    try {
      // Check file existence
      result.checks.existence = await this._validateExistence(target);

      // If file doesn't exist and creation is not allowed, stop here
      if (!result.checks.existence.exists && !target.createIfMissing) {
        if (target.mode !== 'create') {
          result.valid = false;
          result.errors.push(`Target file does not exist: ${target.resolvedPath}`);
          this.validationCache.set(cacheKey, result);
          return result;
        }
      }

      // Skip further checks if file doesn't exist
      if (!result.checks.existence.exists) {
        result.valid = true;
        this.validationCache.set(cacheKey, result);
        return result;
      }

      // Validate permissions
      result.checks.permissions = await this._validatePermissions(target);
      if (!result.checks.permissions.writable) {
        result.valid = false;
        result.errors.push(`Target file is not writable: ${target.resolvedPath}`);
      }

      // Validate file is not binary
      result.checks.binary = await this._validateNotBinary(target);
      if (result.checks.binary.isBinary) {
        result.valid = false;
        result.errors.push(`Target file is binary - text injection not supported: ${target.resolvedPath}`);
      }

      // Validate file size
      result.checks.size = await this._validateSize(target);
      if (!result.checks.size.withinLimits) {
        result.valid = false;
        result.errors.push(`Target file exceeds size limit: ${result.checks.size.actualSize} > ${result.checks.size.maxSize}`);
      }

      // Validate encoding
      if (this.config.validateEncoding) {
        result.checks.encoding = await this._validateEncoding(target);
        if (!result.checks.encoding.valid) {
          result.warnings.push(`Encoding issue detected: ${result.checks.encoding.issue}`);
        }
      }

      // Validate syntax if applicable
      if (target.validateSyntax) {
        result.checks.syntax = await this._validateSyntax(target);
        if (!result.checks.syntax.valid) {
          result.errors.push(`Syntax validation failed: ${result.checks.syntax.error}`);
          result.valid = false;
        }
      }

    } catch (error) {
      result.valid = false;
      result.errors.push(`Validation error: ${error.message}`);
    }

    this.validationCache.set(cacheKey, result);
    return result;
  }

  /**
   * Validate injection content
   */
  async validateContent(content, target) {
    const result = {
      valid: true,
      errors: [],
      warnings: []
    };

    // Validate content is not empty if required
    if (target.requireNonEmpty && (!content || content.trim() === '')) {
      result.valid = false;
      result.errors.push('Content cannot be empty');
    }

    // Validate content encoding
    if (this.config.validateEncoding) {
      const encodingValid = this._validateContentEncoding(content);
      if (!encodingValid.valid) {
        result.valid = false;
        result.errors.push(`Content encoding invalid: ${encodingValid.error}`);
      }
    }

    // Validate content doesn't contain binary data
    if (this._containsBinaryData(content)) {
      result.valid = false;
      result.errors.push('Content contains binary data');
    }

    // Validate syntax if target file type supports it
    if (target.validateSyntax) {
      const syntaxResult = await this._validateContentSyntax(content, target);
      if (!syntaxResult.valid) {
        result.valid = false;
        result.errors.push(`Content syntax invalid: ${syntaxResult.error}`);
      }
    }

    // Validate content size
    const contentSize = Buffer.byteLength(content, 'utf8');
    if (contentSize > this.config.maxContentSize) {
      result.valid = false;
      result.errors.push(`Content too large: ${contentSize} > ${this.config.maxContentSize} bytes`);
    }

    // Validate line endings consistency
    if (this.config.consistentLineEndings) {
      const lineEndingResult = this._validateLineEndings(content);
      if (!lineEndingResult.consistent) {
        result.warnings.push(`Inconsistent line endings detected: ${lineEndingResult.types.join(', ')}`);
      }
    }

    // Custom content validation
    if (target.customValidation && typeof target.customValidation === 'function') {
      try {
        const customResult = await target.customValidation(content, target);
        if (!customResult.valid) {
          result.valid = false;
          result.errors.push(customResult.error || 'Custom validation failed');
        }
      } catch (error) {
        result.valid = false;
        result.errors.push(`Custom validation error: ${error.message}`);
      }
    }

    if (!result.valid) {
      throw new ValidationError('Content validation failed', result.errors);
    }

    return result;
  }

  /**
   * Validate merged content (after injection)
   */
  async validateMergedContent(mergedContent, target) {
    // First run standard content validation
    await this.validateContent(mergedContent, target);

    // Additional validations specific to merged content
    const result = {
      valid: true,
      errors: [],
      warnings: []
    };

    // Check for duplicate imports/exports
    if (this._looksLikeCode(target.resolvedPath)) {
      const duplicates = this._findDuplicateStatements(mergedContent);
      if (duplicates.length > 0) {
        result.warnings.push(`Duplicate statements detected: ${duplicates.join(', ')}`);
      }
    }

    // Validate structural integrity (balanced brackets, etc.)
    const structureResult = this._validateStructure(mergedContent, target);
    if (!structureResult.valid) {
      result.valid = false;
      result.errors.push(`Structural validation failed: ${structureResult.error}`);
    }

    // Check for naming conflicts
    if (target.checkConflicts) {
      const conflicts = await this._checkNamingConflicts(mergedContent, target);
      if (conflicts.length > 0) {
        result.warnings.push(`Potential naming conflicts: ${conflicts.join(', ')}`);
      }
    }

    if (!result.valid) {
      throw new ValidationError('Merged content validation failed', result.errors);
    }

    return result;
  }

  /**
   * Private validation methods
   */

  async _validateExistence(target) {
    try {
      const stats = await fs.stat(target.resolvedPath);
      return {
        exists: true,
        isFile: stats.isFile(),
        isDirectory: stats.isDirectory(),
        size: stats.size,
        modified: stats.mtime
      };
    } catch (error) {
      if (error.code === 'ENOENT') {
        return { exists: false };
      }
      throw error;
    }
  }

  async _validatePermissions(target) {
    try {
      const stats = await fs.stat(target.resolvedPath);
      const mode = stats.mode;

      return {
        readable: !!(mode & 0o444),
        writable: !!(mode & 0o222),
        executable: !!(mode & 0o111),
        mode: mode.toString(8)
      };
    } catch (error) {
      throw new Error(`Permission check failed: ${error.message}`);
    }
  }

  async _validateNotBinary(target) {
    try {
      // Read first 1KB to check for binary content
      const buffer = await fs.readFile(target.resolvedPath, { encoding: null, flag: 'r' });
      const sample = buffer.slice(0, 1024);

      const isBinary = BINARY_PATTERNS.some(pattern => pattern.test(sample.toString('utf8', 0, Math.min(512, sample.length))));

      return {
        isBinary,
        confidence: isBinary ? 'high' : 'low'
      };
    } catch (error) {
      throw new Error(`Binary check failed: ${error.message}`);
    }
  }

  async _validateSize(target) {
    try {
      const stats = await fs.stat(target.resolvedPath);
      const maxSize = this.config.maxFileSize;

      return {
        withinLimits: stats.size <= maxSize,
        actualSize: stats.size,
        maxSize
      };
    } catch (error) {
      throw new Error(`Size check failed: ${error.message}`);
    }
  }

  async _validateEncoding(target) {
    try {
      const content = await fs.readFile(target.resolvedPath, 'utf8');

      // Check for encoding issues
      const hasReplacementChar = content.includes('\uFFFD');
      const validUtf8 = this._isValidUtf8(content);

      return {
        valid: !hasReplacementChar && validUtf8,
        encoding: 'utf8',
        issue: hasReplacementChar ? 'Contains replacement characters' :
               !validUtf8 ? 'Invalid UTF-8 sequences' : null
      };
    } catch (error) {
      return {
        valid: false,
        issue: `Encoding validation failed: ${error.message}`
      };
    }
  }

  async _validateSyntax(target) {
    const ext = extname(target.resolvedPath).toLowerCase();

    try {
      const content = await fs.readFile(target.resolvedPath, 'utf8');

      switch (ext) {
        case '.js':
        case '.mjs':
          return this._validateJavaScript(content);
        case '.ts':
          return this._validateTypeScript(content);
        case '.json':
          return this._validateJSON(content);
        default:
          return { valid: true, message: 'No syntax validation available' };
      }
    } catch (error) {
      return {
        valid: false,
        error: error.message
      };
    }
  }

  _validateJavaScript(content) {
    try {
      // Basic syntax check - validate without executing
      const wrappedContent = `(function() { ${content} })`;
      new Function(wrappedContent);
      return { valid: true };
    } catch (error) {
      return {
        valid: false,
        error: error.message
      };
    }
  }

  _validateTypeScript(content) {
    // TypeScript validation would require the TS compiler
    // For now, just check basic syntax patterns
    const hasBasicSyntaxErrors = /\b(interface|type|class)\s*\{/.test(content) &&
                                !content.includes('}');

    return {
      valid: !hasBasicSyntaxErrors,
      error: hasBasicSyntaxErrors ? 'Unbalanced braces detected' : null
    };
  }

  _validateJSON(content) {
    try {
      JSON.parse(content);
      return { valid: true };
    } catch (error) {
      return {
        valid: false,
        error: error.message
      };
    }
  }

  async _validateContentSyntax(content, target) {
    const ext = extname(target.resolvedPath).toLowerCase();

    switch (ext) {
      case '.js':
      case '.mjs':
        return this._validateJavaScript(content);
      case '.ts':
        return this._validateTypeScript(content);
      case '.json':
        return this._validateJSON(content);
      default:
        return { valid: true };
    }
  }

  _validateContentEncoding(content) {
    try {
      // Check if content can be encoded as UTF-8
      Buffer.from(content, 'utf8');
      return { valid: true };
    } catch (error) {
      return {
        valid: false,
        error: error.message
      };
    }
  }

  _containsBinaryData(content) {
    return BINARY_PATTERNS.some(pattern => pattern.test(content));
  }

  _validateLineEndings(content) {
    const types = [];

    if (content.includes('\r\n')) types.push('CRLF');
    if (content.includes('\n') && !content.includes('\r\n')) types.push('LF');
    if (content.includes('\r') && !content.includes('\r\n')) types.push('CR');

    return {
      consistent: types.length <= 1,
      types
    };
  }

  _isValidUtf8(str) {
    try {
      return str === Buffer.from(str, 'utf8').toString('utf8');
    } catch {
      return false;
    }
  }

  _looksLikeCode(filePath) {
    const ext = extname(filePath).toLowerCase();
    return ['.js', '.ts', '.jsx', '.tsx', '.mjs'].includes(ext);
  }

  _findDuplicateStatements(content) {
    const duplicates = [];
    const statements = new Set();

    // Check for duplicate imports
    const imports = content.match(CONTENT_PATTERNS.IMPORT_STATEMENT) || [];
    for (const imp of imports) {
      if (statements.has(imp)) {
        duplicates.push(imp.trim());
      }
      statements.add(imp);
    }

    return duplicates;
  }

  _validateStructure(content, target) {
    // Basic structural validation
    const brackets = { '(': 0, '[': 0, '{': 0 };
    const closers = { ')': '(', ']': '[', '}': '{' };

    for (const char of content) {
      if (brackets.hasOwnProperty(char)) {
        brackets[char]++;
      } else if (closers.hasOwnProperty(char)) {
        const opener = closers[char];
        if (brackets[opener] > 0) {
          brackets[opener]--;
        } else {
          return {
            valid: false,
            error: `Unmatched closing ${char}`
          };
        }
      }
    }

    const unmatched = Object.entries(brackets).filter(([, count]) => count > 0);
    if (unmatched.length > 0) {
      return {
        valid: false,
        error: `Unmatched opening brackets: ${unmatched.map(([b]) => b).join(', ')}`
      };
    }

    return { valid: true };
  }

  async _checkNamingConflicts(content, target) {
    // Simple naming conflict detection
    const conflicts = [];

    // Extract function names
    const functions = content.match(/function\s+(\w+)/g) || [];
    const functionNames = functions.map(f => f.replace('function ', ''));

    // Check for duplicates
    const seen = new Set();
    for (const name of functionNames) {
      if (seen.has(name)) {
        conflicts.push(`function ${name}`);
      }
      seen.add(name);
    }

    return conflicts;
  }

  _getCacheKey(target) {
    const hash = createHash(CHECKSUM_ALGORITHMS.SHA256);
    hash.update(target.resolvedPath);
    hash.update(target.mode);
    hash.update(JSON.stringify(target.skipIf || {}));
    return hash.digest('hex').substring(0, 16);
  }

  /**
   * Clear validation cache
   */
  clearCache() {
    this.validationCache.clear();
  }
}

/**
 * Custom validation error
 */
export class ValidationError extends Error {
  constructor(message, errors = []) {
    super(message);
    this.name = 'ValidationError';
    this.errors = errors;
    this.code = ERROR_CODES.VALIDATION_FAILED;
  }
}