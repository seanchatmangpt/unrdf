/**
 * KGEN Post-Processor - Normalize and clean rendered output
 *
 * Handles:
 * - Whitespace normalization
 * - Line ending consistency
 * - Trailing whitespace removal
 * - Final newline ensuring
 * - Deterministic formatting
 */

/**
 *
 */
export class KGenPostProcessor {
  /**
   *
   */
  constructor(options = {}) {
    this.options = {
      normalizeWhitespace: options.normalizeWhitespace !== false,
      trimLines: options.trimLines !== false,
      ensureFinalNewline: options.ensureFinalNewline !== false,
      normalizeLineEndings: options.normalizeLineEndings !== false,
      removeEmptyLines: options.removeEmptyLines === true,
      indentSize: options.indentSize || 2,
      deterministicMode: options.deterministicMode !== false,
      ...options
    };
  }

  /**
   * Process rendered content for consistency
   */
  async process(content, options = {}) {
    if (!content || typeof content !== 'string') {
      return {
        content: '',
        metadata: {
          processed: true,
          originalLength: 0,
          finalLength: 0,
          changes: []
        }
      };
    }

    const processingOptions = { ...this.options, ...options };
    let processed = content;
    const changes = [];
    const originalLength = content.length;

    // 1. Normalize line endings to Unix style
    if (processingOptions.normalizeLineEndings) {
      const beforeLength = processed.length;
      processed = processed.replace(/\r\n|\r/g, '\n');
      if (processed.length !== beforeLength) {
        changes.push('normalized-line-endings');
      }
    }

    // 2. Remove trailing whitespace from lines
    if (processingOptions.trimLines) {
      const beforeLength = processed.length;
      processed = processed.replace(/[^\S\n]+$/gm, '');
      if (processed.length !== beforeLength) {
        changes.push('trimmed-lines');
      }
    }

    // 3. Remove completely empty lines if requested
    if (processingOptions.removeEmptyLines) {
      const beforeLength = processed.length;
      processed = processed.replace(/\n\s*\n\s*\n/g, '\n\n');
      if (processed.length !== beforeLength) {
        changes.push('removed-empty-lines');
      }
    }

    // 4. Normalize whitespace patterns
    if (processingOptions.normalizeWhitespace) {
      const beforeLength = processed.length;

      // Normalize multiple spaces to single space (but preserve intentional indentation)
      processed = processed.replace(/[^\S\n]{2,}/g, match => {
        // If it's at the start of a line, preserve it (it's indentation)
        const lines = processed.split('\n');
        for (const line of lines) {
          if (line.startsWith(match)) {
            return match; // Preserve indentation
          }
        }
        return ' '; // Replace multiple spaces with single space
      });

      if (processed.length !== beforeLength) {
        changes.push('normalized-whitespace');
      }
    }

    // 5. Ensure consistent final newline
    if (processingOptions.ensureFinalNewline) {
      if (processed.length > 0 && !processed.endsWith('\n')) {
        processed += '\n';
        changes.push('added-final-newline');
      }
    }

    // 6. Deterministic formatting adjustments
    if (processingOptions.deterministicMode) {
      processed = this.applyDeterministicFormatting(processed, changes);
    }

    return {
      content: processed,
      metadata: {
        processed: true,
        originalLength,
        finalLength: processed.length,
        changes,
        options: processingOptions,
        timestamp: processingOptions.deterministicMode ?
          '2024-01-01T00:00:00.000Z' :
          new Date().toISOString()
      }
    };
  }

  /**
   * Apply deterministic formatting rules
   */
  applyDeterministicFormatting(content, changes) {
    let processed = content;

    // Ensure consistent indentation
    const lines = processed.split('\n');
    const processedLines = lines.map(line => {
      if (line.trim() === '') return ''; // Empty lines have no whitespace

      // Detect current indentation
      const indentMatch = line.match(/^(\s*)/);
      const currentIndent = indentMatch ? indentMatch[1] : '';

      // Convert tabs to spaces for consistency
      if (currentIndent.includes('\t')) {
        const spaceIndent = currentIndent.replace(/\t/g, ' '.repeat(this.options.indentSize));
        const newLine = spaceIndent + line.substring(currentIndent.length);
        changes.push('normalized-indentation');
        return newLine;
      }

      return line;
    });

    processed = processedLines.join('\n');

    return processed;
  }

  /**
   * Validate output consistency
   */
  async validate(content) {
    const issues = [];

    if (typeof content !== 'string') {
      issues.push({ type: 'invalid-type', message: 'Content must be a string' });
      return { valid: false, issues };
    }

    // Check for mixed line endings
    if (content.includes('\r\n') || content.includes('\r')) {
      issues.push({ type: 'mixed-line-endings', message: 'Mixed line endings detected' });
    }

    // Check for trailing whitespace
    const lines = content.split('\n');
    lines.forEach((line, index) => {
      if (line.match(/\s+$/)) {
        issues.push({
          type: 'trailing-whitespace',
          message: `Trailing whitespace on line ${index + 1}`,
          line: index + 1
        });
      }
    });

    // Check for inconsistent indentation
    let hasSpaces = false;
    let hasTabs = false;

    lines.forEach((line, _index) => {
      const indentMatch = line.match(/^(\s*)/);
      if (indentMatch && indentMatch[1]) {
        if (indentMatch[1].includes(' ')) hasSpaces = true;
        if (indentMatch[1].includes('\t')) hasTabs = true;
      }
    });

    if (hasSpaces && hasTabs) {
      issues.push({
        type: 'mixed-indentation',
        message: 'Mixed spaces and tabs for indentation'
      });
    }

    return {
      valid: issues.length === 0,
      issues,
      stats: {
        lines: lines.length,
        hasTrailingWhitespace: issues.some(i => i.type === 'trailing-whitespace'),
        hasMixedIndentation: issues.some(i => i.type === 'mixed-indentation'),
        hasMixedLineEndings: issues.some(i => i.type === 'mixed-line-endings')
      }
    };
  }

  /**
   * Compare two content strings for consistency
   */
  async compare(content1, content2) {
    const processed1 = await this.process(content1);
    const processed2 = await this.process(content2);

    const identical = processed1.content === processed2.content;

    return {
      identical,
      content1Length: processed1.metadata.finalLength,
      content2Length: processed2.metadata.finalLength,
      changes1: processed1.metadata.changes,
      changes2: processed2.metadata.changes,
      firstDifference: identical ? null : this.findFirstDifference(processed1.content, processed2.content)
    };
  }

  /**
   * Find first character difference between strings
   */
  findFirstDifference(str1, str2) {
    const minLength = Math.min(str1.length, str2.length);

    for (let i = 0; i < minLength; i++) {
      if (str1[i] !== str2[i]) {
        return {
          position: i,
          char1: str1[i],
          char2: str2[i],
          context: {
            before: str1.substring(Math.max(0, i - 10), i),
            after: str1.substring(i + 1, i + 11)
          }
        };
      }
    }

    // One string is longer than the other
    if (str1.length !== str2.length) {
      return {
        position: minLength,
        char1: str1[minLength] || '<end>',
        char2: str2[minLength] || '<end>',
        lengthDifference: str1.length - str2.length
      };
    }

    return null; // No differences found
  }

  /**
   * Create deterministic content hash
   */
  createContentHash(content) {
    const crypto = require('crypto');
    return crypto.createHash('sha256').update(content || '', 'utf8').digest('hex');
  }

  /**
   * Get processor statistics
   */
  getStats() {
    return {
      ...this.options,
      version: '2.0.0-kgen-native',
      supportedOperations: [
        'normalize-line-endings',
        'trim-lines',
        'remove-empty-lines',
        'normalize-whitespace',
        'ensure-final-newline',
        'deterministic-formatting'
      ]
    };
  }

  /**
   * Benchmark processing performance
   */
  async benchmark(content, iterations = 100) {
    const start = Date.now();

    for (let i = 0; i < iterations; i++) {
      await this.process(content);
    }

    const duration = Date.now() - start;

    return {
      iterations,
      totalTime: duration,
      averageTime: duration / iterations,
      contentLength: content?.length || 0,
      throughput: Math.round((content?.length || 0) * iterations / duration * 1000) // chars per second
    };
  }
}

export default KGenPostProcessor;