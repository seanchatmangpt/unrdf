/**
 * KGEN Idempotency Manager
 *
 * Manages idempotent injection operations using skipIf conditions
 * to prevent duplicate content and ensure deterministic behavior.
 */

import { promises as fs } from 'fs';
import { createHash } from 'crypto';

import { SKIP_IF_LOGIC, CHECKSUM_ALGORITHMS } from './constants.js';

export class IdempotencyManager {
  constructor(config = {}) {
    this.config = config;
    this.contentCache = new Map();
    this.hashCache = new Map();
  }

  /**
   * Check if injection should be skipped based on skipIf conditions
   */
  async shouldSkipInjection(target, content, variables) {
    const skipConditions = target.skipIf;

    if (!skipConditions) {
      return { skip: false, reason: null };
    }

    // Handle different skipIf condition types
    if (typeof skipConditions === 'string') {
      return await this._evaluateSingleCondition(skipConditions, target, content, variables);
    }

    if (Array.isArray(skipConditions)) {
      return await this._evaluateMultipleConditions(skipConditions, target, content, variables);
    }

    if (typeof skipConditions === 'object') {
      return await this._evaluateObjectCondition(skipConditions, target, content, variables);
    }

    return { skip: false, reason: 'Invalid skipIf condition' };
  }

  /**
   * Generate content hash for duplicate detection
   */
  async generateContentHash(content, target) {
    const cacheKey = `${target.resolvedPath}:${content}`;

    if (this.hashCache.has(cacheKey)) {
      return this.hashCache.get(cacheKey);
    }

    const hash = createHash(CHECKSUM_ALGORITHMS.SHA256);
    hash.update(content);
    hash.update(target.resolvedPath);
    hash.update(target.mode);

    const result = hash.digest('hex');
    this.hashCache.set(cacheKey, result);

    return result;
  }

  /**
   * Check if content already exists in target file
   */
  async contentExists(target, content, options = {}) {
    try {
      const fileContent = await this._getFileContent(target.resolvedPath);
      const { exact = false, ignoreWhitespace = false, regex = false } = options;

      if (regex) {
        const pattern = new RegExp(content, options.regexFlags || 'gm');
        return pattern.test(fileContent);
      }

      let searchContent = content;
      let searchFileContent = fileContent;

      if (ignoreWhitespace) {
        searchContent = content.replace(/\s+/g, ' ').trim();
        searchFileContent = fileContent.replace(/\s+/g, ' ').trim();
      }

      if (exact) {
        return searchFileContent === searchContent;
      }

      return searchFileContent.includes(searchContent);

    } catch (error) {
      if (error.code === 'ENOENT') {
        return false; // File doesn't exist, content can't exist
      }
      throw error;
    }
  }

  /**
   * Private Methods
   */

  async _evaluateSingleCondition(condition, target, content, variables) {
    // Built-in conditions
    if (condition === 'file_exists') {
      const exists = await this._fileExists(target.resolvedPath);
      return {
        skip: exists,
        reason: exists ? 'Target file already exists' : null
      };
    }

    if (condition.startsWith('file_size >')) {
      const sizeLimit = parseInt(condition.split('>')[1].trim());
      const actualSize = await this._getFileSize(target.resolvedPath);
      const skip = actualSize > sizeLimit;
      return {
        skip,
        reason: skip ? `File size exceeds limit (${actualSize} > ${sizeLimit} bytes)` : null
      };
    }

    if (condition.startsWith('line_count >')) {
      const lineLimit = parseInt(condition.split('>')[1].trim());
      const actualLines = await this._getLineCount(target.resolvedPath);
      const skip = actualLines > lineLimit;
      return {
        skip,
        reason: skip ? `File has too many lines (${actualLines} > ${lineLimit})` : null
      };
    }

    // Pattern matching conditions
    if (condition.startsWith('/') && condition.endsWith('/')) {
      // Regex pattern
      const pattern = condition.slice(1, -1);
      const exists = await this.contentExists(target, pattern, { regex: true });
      return {
        skip: exists,
        reason: exists ? `Pattern already exists: ${condition}` : null
      };
    }

    // Variable interpolation
    const interpolatedCondition = this._interpolateVariables(condition, variables);

    // Check if interpolated content exists in file
    const exists = await this.contentExists(target, interpolatedCondition);
    return {
      skip: exists,
      reason: exists ? `Content already exists: ${interpolatedCondition}` : null
    };
  }

  async _evaluateMultipleConditions(conditions, target, content, variables) {
    const logic = target.skipIfLogic || SKIP_IF_LOGIC.OR;
    const results = [];

    for (const condition of conditions) {
      const result = await this._evaluateSingleCondition(condition, target, content, variables);
      results.push(result);
    }

    if (logic === SKIP_IF_LOGIC.AND) {
      const allTrue = results.every(r => r.skip);
      return {
        skip: allTrue,
        reason: allTrue ? `All conditions matched (AND logic): ${results.map(r => r.reason).join(', ')}` : null
      };
    } else {
      // OR logic (default)
      const anyTrue = results.some(r => r.skip);
      const matchingReasons = results.filter(r => r.skip).map(r => r.reason);
      return {
        skip: anyTrue,
        reason: anyTrue ? `Condition matched (OR logic): ${matchingReasons[0]}` : null
      };
    }
  }

  async _evaluateObjectCondition(conditionObj, target, content, variables) {
    // Complex object-based conditions
    const { pattern, exists, custom, hash } = conditionObj;

    if (pattern) {
      return await this._evaluateSingleCondition(pattern, target, content, variables);
    }

    if (exists !== undefined) {
      const fileExists = await this._fileExists(target.resolvedPath);
      return {
        skip: fileExists === exists,
        reason: fileExists === exists ? `File existence matches condition: ${exists}` : null
      };
    }

    if (hash) {
      const contentHash = await this.generateContentHash(content, target);
      const existingHash = await this._getExistingContentHash(target);
      const skip = contentHash === existingHash;
      return {
        skip,
        reason: skip ? 'Content hash already exists' : null
      };
    }

    if (custom && typeof custom === 'function') {
      const result = await custom(target, content, variables);
      return {
        skip: result.skip,
        reason: result.reason || 'Custom condition matched'
      };
    }

    return { skip: false, reason: 'Invalid object condition' };
  }

  _interpolateVariables(template, variables) {
    return template.replace(/\{\{(\w+)\}\}/g, (match, variable) => {
      return variables[variable] || match;
    });
  }

  async _getFileContent(filePath) {
    const cacheKey = filePath;

    if (this.contentCache.has(cacheKey)) {
      return this.contentCache.get(cacheKey);
    }

    try {
      const content = await fs.readFile(filePath, 'utf8');
      this.contentCache.set(cacheKey, content);
      return content;
    } catch (error) {
      if (error.code === 'ENOENT') {
        this.contentCache.set(cacheKey, '');
        return '';
      }
      throw error;
    }
  }

  async _fileExists(filePath) {
    try {
      await fs.access(filePath);
      return true;
    } catch (error) {
      return false;
    }
  }

  async _getFileSize(filePath) {
    try {
      const stats = await fs.stat(filePath);
      return stats.size;
    } catch (error) {
      if (error.code === 'ENOENT') {
        return 0;
      }
      throw error;
    }
  }

  async _getLineCount(filePath) {
    try {
      const content = await this._getFileContent(filePath);
      return content.split('\n').length;
    } catch (error) {
      return 0;
    }
  }

  async _getExistingContentHash(target) {
    try {
      const content = await this._getFileContent(target.resolvedPath);
      const hash = createHash(CHECKSUM_ALGORITHMS.SHA256);
      hash.update(content);
      return hash.digest('hex');
    } catch (error) {
      return null;
    }
  }

  /**
   * Clear caches (useful for testing or long-running processes)
   */
  clearCache() {
    this.contentCache.clear();
    this.hashCache.clear();
  }
}