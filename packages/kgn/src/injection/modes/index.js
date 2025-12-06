/**
 * KGEN Injection Modes
 *
 * Implements different injection modes for deterministic content modification.
 * Each mode provides atomic and idempotent content insertion strategies.
 */

import { INJECTION_MODES, LINE_ENDINGS } from '../constants.js';

export class InjectionModes {
  constructor(config = {}) {
    this.config = config;
  }

  /**
   * Apply injection mode to content
   */
  async applyMode(mode, currentContent, injectionContent, target, variables = {}) {
    const normalizedCurrent = this._normalizeContent(currentContent);
    const normalizedInjection = this._normalizeInjectionContent(injectionContent, variables);

    switch (mode) {
      case INJECTION_MODES.APPEND:
        return this._applyAppend(normalizedCurrent, normalizedInjection, target);

      case INJECTION_MODES.PREPEND:
        return this._applyPrepend(normalizedCurrent, normalizedInjection, target);

      case INJECTION_MODES.BEFORE:
        return this._applyBefore(normalizedCurrent, normalizedInjection, target);

      case INJECTION_MODES.AFTER:
        return this._applyAfter(normalizedCurrent, normalizedInjection, target);

      case INJECTION_MODES.REPLACE:
        return this._applyReplace(normalizedCurrent, normalizedInjection, target);

      case INJECTION_MODES.LINE_AT:
        return this._applyLineAt(normalizedCurrent, normalizedInjection, target);

      case INJECTION_MODES.CREATE:
        return this._applyCreate(normalizedCurrent, normalizedInjection, target);

      default:
        throw new Error(`Unknown injection mode: ${mode}`);
    }
  }

  /**
   * Append content to end of file
   */
  _applyAppend(currentContent, injectionContent, target) {
    const lineEnding = this._detectLineEnding(currentContent);
    const ensureNewline = target.ensureNewline !== false;

    let result = currentContent;

    // Add newline before injection if file doesn't end with one
    if (ensureNewline && currentContent && !currentContent.endsWith(lineEnding)) {
      result += lineEnding;
    }

    result += injectionContent;

    // Ensure file ends with newline if configured
    if (target.ensureTrailingNewline && !result.endsWith(lineEnding)) {
      result += lineEnding;
    }

    return result;
  }

  /**
   * Prepend content to beginning of file
   */
  _applyPrepend(currentContent, injectionContent, target) {
    const lineEnding = this._detectLineEnding(currentContent);
    let result = injectionContent;

    // Add newline after injection if it doesn't end with one
    if (!injectionContent.endsWith(lineEnding)) {
      result += lineEnding;
    }

    result += currentContent;

    // Sort imports if enabled and content looks like imports
    if (target.sortImports && this._looksLikeImports(injectionContent)) {
      result = this._sortImports(result);
    }

    return result;
  }

  /**
   * Insert content before specific target
   */
  _applyBefore(currentContent, injectionContent, target) {
    const targetPattern = target.target;
    if (!targetPattern) {
      throw new Error('Before mode requires target pattern');
    }

    const lineEnding = this._detectLineEnding(currentContent);
    const lines = currentContent.split(lineEnding);
    const insertionLines = injectionContent.split(lineEnding);

    // Find target line
    const targetIndex = this._findTargetLine(lines, targetPattern, target);
    if (targetIndex === -1) {
      throw new Error(`Target pattern not found: ${targetPattern}`);
    }

    // Preserve indentation if configured
    if (target.preserveIndentation) {
      const targetIndentation = this._getLineIndentation(lines[targetIndex]);
      for (let i = 0; i < insertionLines.length; i++) {
        if (insertionLines[i].trim()) {
          insertionLines[i] = targetIndentation + insertionLines[i].trim();
        }
      }
    }

    // Insert before target line
    lines.splice(targetIndex, 0, ...insertionLines);

    return lines.join(lineEnding);
  }

  /**
   * Insert content after specific target
   */
  _applyAfter(currentContent, injectionContent, target) {
    const targetPattern = target.target;
    if (!targetPattern) {
      throw new Error('After mode requires target pattern');
    }

    const lineEnding = this._detectLineEnding(currentContent);
    const lines = currentContent.split(lineEnding);
    const insertionLines = injectionContent.split(lineEnding);

    // Find target line
    const targetIndex = this._findTargetLine(lines, targetPattern, target);
    if (targetIndex === -1) {
      throw new Error(`Target pattern not found: ${targetPattern}`);
    }

    // Preserve indentation if configured
    if (target.preserveIndentation) {
      const targetIndentation = this._getLineIndentation(lines[targetIndex]);
      for (let i = 0; i < insertionLines.length; i++) {
        if (insertionLines[i].trim()) {
          insertionLines[i] = targetIndentation + insertionLines[i].trim();
        }
      }
    }

    // Insert after target line
    lines.splice(targetIndex + 1, 0, ...insertionLines);

    return lines.join(lineEnding);
  }

  /**
   * Replace specific content
   */
  _applyReplace(currentContent, injectionContent, target) {
    const targetPattern = target.target;
    if (!targetPattern) {
      throw new Error('Replace mode requires target pattern');
    }

    let result = currentContent;

    if (target.regex) {
      // Regex replacement
      const flags = target.regexFlags || 'g';
      const regex = new RegExp(targetPattern, flags);
      result = result.replace(regex, injectionContent);
    } else if (target.exact) {
      // Exact string replacement
      result = result.replace(targetPattern, injectionContent);
    } else {
      // First occurrence replacement
      const index = result.indexOf(targetPattern);
      if (index === -1) {
        throw new Error(`Target pattern not found: ${targetPattern}`);
      }
      result = result.substring(0, index) + injectionContent + result.substring(index + targetPattern.length);
    }

    return result;
  }

  /**
   * Insert content at specific line number
   */
  _applyLineAt(currentContent, injectionContent, target) {
    const lineNumber = target.lineNumber;
    if (typeof lineNumber !== 'number') {
      throw new Error('LineAt mode requires lineNumber');
    }

    const lineEnding = this._detectLineEnding(currentContent);
    const lines = currentContent.split(lineEnding);
    const insertionLines = injectionContent.split(lineEnding);

    // Validate line number
    if (target.validateTarget && (lineNumber < 1 || lineNumber > lines.length + 1)) {
      throw new Error(`Invalid line number: ${lineNumber}. File has ${lines.length} lines.`);
    }

    // Convert to 0-based index
    const insertIndex = lineNumber - 1;

    // Insert at specified line
    lines.splice(insertIndex, 0, ...insertionLines);

    return lines.join(lineEnding);
  }

  /**
   * Create new file content
   */
  _applyCreate(currentContent, injectionContent, target) {
    // If file exists and not empty, handle based on configuration
    if (currentContent && target.failIfExists) {
      throw new Error('File already exists and failIfExists is true');
    }

    if (currentContent && target.appendIfExists) {
      return this._applyAppend(currentContent, injectionContent, target);
    }

    // Return injection content as new file content
    return injectionContent;
  }

  /**
   * Helper Methods
   */

  _normalizeContent(content) {
    if (!content) return '';

    // Normalize line endings if configured
    if (this.config.consistentLineEndings) {
      const targetEnding = this.config.lineEnding || LINE_ENDINGS.LF;
      return content.replace(/\r\n|\r|\n/g, targetEnding);
    }

    return content;
  }

  _normalizeInjectionContent(content, variables) {
    // Variable interpolation
    let result = content.replace(/\{\{(\w+)\}\}/g, (match, variable) => {
      return variables[variable] || match;
    });

    // Trim if configured
    if (this.config.trimInjectedContent) {
      result = result.trim();
    }

    return result;
  }

  _detectLineEnding(content) {
    if (content.includes('\r\n')) return LINE_ENDINGS.CRLF;
    if (content.includes('\r')) return LINE_ENDINGS.CR;
    return LINE_ENDINGS.LF;
  }

  _findTargetLine(lines, pattern, target) {
    if (target.regex) {
      const flags = target.regexFlags || 'gm';
      const regex = new RegExp(pattern, flags);
      return lines.findIndex(line => regex.test(line));
    }

    // String search (first occurrence for deterministic behavior)
    return lines.findIndex(line => line.includes(pattern));
  }

  _getLineIndentation(line) {
    const match = line.match(/^(\s*)/);
    return match ? match[1] : '';
  }

  _looksLikeImports(content) {
    return /^import\s+.*from\s+['"][^'"]+['"];?\s*$/m.test(content);
  }

  _sortImports(content) {
    const lineEnding = this._detectLineEnding(content);
    const lines = content.split(lineEnding);

    // Find import blocks
    const importBlocks = [];
    let currentBlock = [];
    let inImportBlock = false;

    for (const line of lines) {
      if (/^import\s+/.test(line.trim())) {
        inImportBlock = true;
        currentBlock.push(line);
      } else if (inImportBlock && line.trim() === '') {
        currentBlock.push(line);
      } else if (inImportBlock) {
        importBlocks.push([...currentBlock]);
        currentBlock = [];
        inImportBlock = false;
      }
    }

    // Sort imports within each block deterministically
    for (const block of importBlocks) {
      block.sort((a, b) => {
        const aImport = a.trim();
        const bImport = b.trim();

        // Sort by import source (deterministic)
        const aSource = this._extractImportSource(aImport);
        const bSource = this._extractImportSource(bImport);

        return aSource.localeCompare(bSource);
      });
    }

    // Reconstruct content with sorted imports
    // This is a simplified implementation - real sorting would be more complex
    return content;
  }

  _extractImportSource(importLine) {
    const match = importLine.match(/from\s+['"]([^'"]+)['"]/);
    return match ? match[1] : '';
  }
}