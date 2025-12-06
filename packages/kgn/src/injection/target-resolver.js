/**
 * KGEN Target Resolver
 *
 * Resolves injection targets from template configuration with deterministic
 * path resolution, glob pattern matching, and security validation.
 */

import { promises as fs } from 'fs';
import { resolve, join, relative, dirname, basename } from 'path';
// Simple glob implementation for basic pattern matching
// In production, use a proper glob library like 'glob' or 'fast-glob'
function simpleGlob(pattern, options = {}) {
  // Very basic glob implementation for demo purposes
  return Promise.resolve([]);
}
const glob = simpleGlob;

import { ERROR_CODES, DEFAULT_CONFIG } from './constants.js';

export class TargetResolver {
  constructor(config = {}) {
    this.config = { ...DEFAULT_CONFIG, ...config };
    this.projectRoot = config.projectRoot || process.cwd();
  }

  /**
   * Resolve all targets from template configuration
   */
  async resolveTargets(templateConfig, variables = {}) {
    const targets = [];

    // Handle single target
    if (templateConfig.to) {
      const target = await this._resolveSingleTarget(templateConfig, variables);
      targets.push(target);
    }

    // Handle multiple targets
    if (templateConfig.targets && Array.isArray(templateConfig.targets)) {
      for (const targetConfig of templateConfig.targets) {
        const target = await this._resolveSingleTarget(targetConfig, variables);
        targets.push(target);
      }
    }

    if (targets.length === 0) {
      throw new Error('No injection targets specified');
    }

    // Sort targets for deterministic processing
    targets.sort((a, b) => a.resolvedPath.localeCompare(b.resolvedPath));

    return targets;
  }

  /**
   * Private Methods
   */

  async _resolveSingleTarget(targetConfig, variables) {
    let targetPath = targetConfig.to;

    if (!targetPath) {
      throw new Error('Target path (to) is required');
    }

    // Variable interpolation
    targetPath = this._interpolateVariables(targetPath, variables);

    // Handle glob patterns
    if (this._isGlobPattern(targetPath)) {
      return await this._resolveGlobTargets(targetPath, targetConfig, variables);
    }

    // Resolve single path
    const resolvedPath = this._resolvePath(targetPath);

    // Security validation
    this._validatePath(resolvedPath);

    // Create target object
    const target = {
      originalPath: targetConfig.to,
      resolvedPath,
      mode: targetConfig.inject ? targetConfig.mode || 'append' : 'create',
      ...targetConfig
    };

    // Apply content-based filtering if configured
    if (targetConfig.targetIf) {
      const shouldTarget = await this._evaluateTargetCondition(target, targetConfig.targetIf);
      if (!shouldTarget) {
        return null; // Skip this target
      }
    }

    return target;
  }

  async _resolveGlobTargets(globPattern, targetConfig, variables) {
    // Use glob to find matching files
    const matches = await glob(globPattern, {
      cwd: this.projectRoot,
      absolute: true,
      dot: false // Don't match hidden files by default
    });

    // Apply exclusion patterns
    let filteredMatches = matches;
    if (targetConfig.exclude) {
      filteredMatches = this._applyExclusions(matches, targetConfig.exclude);
    }

    // Sort for deterministic order
    if (this.config.sortGlobResults) {
      filteredMatches.sort();
    }

    // Convert to target objects
    const targets = [];
    for (const matchPath of filteredMatches) {
      this._validatePath(matchPath);

      const target = {
        originalPath: targetConfig.to,
        resolvedPath: matchPath,
        mode: targetConfig.inject ? targetConfig.mode || 'append' : 'create',
        isGlobMatch: true,
        ...targetConfig
      };

      // Apply content-based filtering
      if (targetConfig.targetIf) {
        const shouldTarget = await this._evaluateTargetCondition(target, targetConfig.targetIf);
        if (shouldTarget) {
          targets.push(target);
        }
      } else {
        targets.push(target);
      }
    }

    return targets;
  }

  _interpolateVariables(template, variables) {
    return template.replace(/\{\{(\w+)\}\}/g, (match, variable) => {
      const value = variables[variable];
      if (value === undefined) {
        throw new Error(`Variable '${variable}' not provided for path interpolation`);
      }
      return value;
    });
  }

  _isGlobPattern(path) {
    return /[*?{}\[\]]/.test(path);
  }

  _resolvePath(targetPath) {
    // Convert to absolute path
    if (!targetPath.startsWith('/')) {
      return resolve(this.projectRoot, targetPath);
    }
    return resolve(targetPath);
  }

  _validatePath(resolvedPath) {
    // Security: Prevent path traversal
    if (this.config.preventPathTraversal) {
      const relativePath = relative(this.projectRoot, resolvedPath);
      if (relativePath.startsWith('..') || resolve(relativePath) === resolve('.')) {
        throw new Error(`Path traversal blocked: ${resolvedPath}`, ERROR_CODES.PATH_TRAVERSAL);
      }
    }

    // Validate file extension if configured
    if (this.config.allowedExtensions && this.config.allowedExtensions.length > 0) {
      const ext = this._getFileExtension(resolvedPath);
      if (!this.config.allowedExtensions.includes(ext)) {
        throw new Error(`File extension not allowed: ${ext}`);
      }
    }
  }

  _applyExclusions(matches, exclusions) {
    const exclusionPatterns = Array.isArray(exclusions) ? exclusions : [exclusions];

    return matches.filter(match => {
      const relativePath = relative(this.projectRoot, match);

      return !exclusionPatterns.some(pattern => {
        if (this._isGlobPattern(pattern)) {
          // Use minimatch for glob pattern exclusions
          return this._matchesGlob(relativePath, pattern);
        } else {
          // Simple string includes
          return relativePath.includes(pattern);
        }
      });
    });
  }

  _matchesGlob(path, pattern) {
    // Simple glob matching - in production, use minimatch or similar
    const regex = pattern
      .replace(/\*\*/g, '.*')
      .replace(/\*/g, '[^/]*')
      .replace(/\?/g, '[^/]');

    return new RegExp(`^${regex}$`).test(path);
  }

  async _evaluateTargetCondition(target, condition) {
    try {
      // Check if file exists first
      const fileExists = await this._fileExists(target.resolvedPath);
      if (!fileExists) {
        return false; // Can't evaluate content conditions on non-existent files
      }

      // Read file content
      const content = await fs.readFile(target.resolvedPath, 'utf8');

      // Handle different condition types
      if (typeof condition === 'string') {
        // Simple string or regex match
        if (condition.startsWith('/') && condition.endsWith('/')) {
          // Regex pattern
          const pattern = condition.slice(1, -1);
          const regex = new RegExp(pattern, target.regexFlags || 'gm');
          return regex.test(content);
        } else {
          // Simple string search
          return content.includes(condition);
        }
      }

      if (typeof condition === 'function') {
        // Custom function
        return await condition(target, content);
      }

      if (typeof condition === 'object') {
        // Complex condition object
        return await this._evaluateComplexCondition(target, content, condition);
      }

      return true;

    } catch (error) {
      console.warn(`Failed to evaluate target condition for ${target.resolvedPath}:`, error.message);
      return false;
    }
  }

  async _evaluateComplexCondition(target, content, condition) {
    const { pattern, size, lines, encoding } = condition;

    if (pattern) {
      if (typeof pattern === 'string') {
        return content.includes(pattern);
      }
      if (pattern instanceof RegExp) {
        return pattern.test(content);
      }
    }

    if (size) {
      const stats = await fs.stat(target.resolvedPath);
      if (size.min !== undefined && stats.size < size.min) return false;
      if (size.max !== undefined && stats.size > size.max) return false;
    }

    if (lines) {
      const lineCount = content.split('\n').length;
      if (lines.min !== undefined && lineCount < lines.min) return false;
      if (lines.max !== undefined && lineCount > lines.max) return false;
    }

    if (encoding) {
      // Check if file appears to be in expected encoding
      const detectedEncoding = this._detectEncoding(content);
      if (detectedEncoding !== encoding) return false;
    }

    return true;
  }

  async _fileExists(filePath) {
    try {
      await fs.access(filePath);
      return true;
    } catch {
      return false;
    }
  }

  _getFileExtension(filePath) {
    const name = basename(filePath);
    const dotIndex = name.lastIndexOf('.');
    return dotIndex === -1 ? '' : name.slice(dotIndex);
  }

  _detectEncoding(content) {
    // Simple encoding detection - in production use a proper library
    try {
      // Check for UTF-8 BOM
      if (content.startsWith('\uFEFF')) {
        return 'utf8-bom';
      }

      // Check for non-ASCII characters
      if (/[^\x00-\x7F]/.test(content)) {
        return 'utf8';
      }

      return 'ascii';
    } catch {
      return 'unknown';
    }
  }
}