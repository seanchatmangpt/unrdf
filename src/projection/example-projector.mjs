/**
 * @fileoverview Example Projector - Extract examples from test files for documentation
 *
 * **Purpose**: Deterministic projection from test files to executable example documentation.
 *
 * **Projection Formula**:
 *   M_examples = Pi_example(test_files)
 *   Same test files -> Same Markdown (deterministic, reproducible)
 *
 * **Features**:
 * - Extracts test cases as runnable examples
 * - Parses describe/it/test blocks
 * - Groups examples by category
 * - Includes expected outputs
 *
 * @module projection/example-projector
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';

/**
 * Schema for extracted example
 */
const ExampleSchema = z.object({
  name: z.string(),
  description: z.string(),
  code: z.string(),
  category: z.string().optional(),
  tags: z.array(z.string()).default([]),
  expectedOutput: z.string().optional(),
  runnable: z.boolean().default(true),
  order: z.number().default(0),
});

/**
 * Schema for example category
 */
const CategorySchema = z.object({
  name: z.string(),
  description: z.string().optional(),
  examples: z.array(ExampleSchema),
});

/**
 * Example Projector - Transforms test files to example documentation
 *
 * @class ExampleProjector
 *
 * @example
 * const projector = new ExampleProjector();
 * const result = await projector.project(testContent, 'universe.test.mjs');
 */
export class ExampleProjector {
  /**
   * Create a new Example projector
   *
   * @param {Object} [options] - Projector options
   * @param {boolean} [options.includeExpectations=true] - Include expect() calls as output
   * @param {boolean} [options.stripTestWrapper=true] - Remove describe/it wrappers
   * @param {string} [options.headingPrefix='##'] - Markdown heading prefix
   */
  constructor(options = {}) {
    this.includeExpectations = options.includeExpectations !== false;
    this.stripTestWrapper = options.stripTestWrapper !== false;
    this.headingPrefix = options.headingPrefix || '##';
  }

  /**
   * Parse test file and extract examples
   *
   * @param {string} testContent - Test file content
   * @returns {Array<Object>} Extracted examples
   */
  parseTestFile(testContent) {
    const examples = [];

    // Match describe blocks
    const describePattern = /describe\s*\(\s*['"`]([^'"`]+)['"`]\s*,\s*(?:async\s*)?\(\s*\)\s*=>\s*\{/g;
    const testPattern = /(?:it|test)\s*\(\s*['"`]([^'"`]+)['"`]\s*,\s*(?:async\s*)?\(\s*\)\s*=>\s*\{([^}]*(?:\{[^}]*\}[^}]*)*)\}/gs;

    // Extract describe names for categories
    const categories = [];
    let match;
    while ((match = describePattern.exec(testContent)) !== null) {
      categories.push({
        name: match[1],
        position: match.index,
      });
    }

    // Extract test cases
    let order = 0;
    while ((match = testPattern.exec(testContent)) !== null) {
      const testName = match[1];
      const testBody = match[2];
      const position = match.index;

      // Find category
      let category = 'General';
      for (let i = categories.length - 1; i >= 0; i--) {
        if (categories[i].position < position) {
          category = categories[i].name;
          break;
        }
      }

      // Clean up code
      const cleanedCode = this._cleanTestCode(testBody);
      if (cleanedCode.trim()) {
        examples.push({
          name: testName,
          description: this._generateDescription(testName),
          code: cleanedCode,
          category,
          tags: this._extractTags(testName, testBody),
          expectedOutput: this._extractExpectedOutput(testBody),
          runnable: this._isRunnable(testBody),
          order: order++,
        });
      }
    }

    // Sort for determinism
    examples.sort((a, b) => {
      const catCmp = a.category.localeCompare(b.category);
      if (catCmp !== 0) return catCmp;
      return a.order - b.order;
    });

    return examples;
  }

  /**
   * Clean test code for documentation
   *
   * @param {string} code - Raw test code
   * @returns {string} Cleaned code
   * @private
   */
  _cleanTestCode(code) {
    if (!this.stripTestWrapper) {
      return code.trim();
    }

    let lines = code.split('\n');

    // Remove common indentation
    const minIndent = lines
      .filter(l => l.trim())
      .reduce((min, l) => {
        const indent = l.match(/^(\s*)/)[1].length;
        return Math.min(min, indent);
      }, Infinity);

    if (minIndent < Infinity) {
      lines = lines.map(l => l.slice(minIndent));
    }

    // Filter out expect() and assertion lines if not including expectations
    if (!this.includeExpectations) {
      lines = lines.filter(l => !l.trim().startsWith('expect('));
    }

    return lines.join('\n').trim();
  }

  /**
   * Generate description from test name
   *
   * @param {string} testName - Test name
   * @returns {string} Description
   * @private
   */
  _generateDescription(testName) {
    // Convert "should create partition" to "Create partition"
    let desc = testName
      .replace(/^should\s+/i, '')
      .replace(/^it\s+/i, '')
      .replace(/^can\s+/i, '');

    // Capitalize first letter
    desc = desc.charAt(0).toUpperCase() + desc.slice(1);

    return desc;
  }

  /**
   * Extract tags from test
   *
   * @param {string} testName - Test name
   * @param {string} testBody - Test body
   * @returns {Array<string>} Tags
   * @private
   */
  _extractTags(testName, testBody) {
    const tags = [];

    // Check for common patterns
    if (testBody.includes('async') || testBody.includes('await')) {
      tags.push('async');
    }
    if (testBody.includes('createStore') || testBody.includes('Store')) {
      tags.push('rdf');
    }
    if (testBody.includes('blake3') || testBody.includes('sha256') || testBody.includes('hash')) {
      tags.push('crypto');
    }
    if (testBody.includes('Receipt')) {
      tags.push('receipts');
    }
    if (testName.toLowerCase().includes('error') || testBody.includes('throw')) {
      tags.push('error-handling');
    }

    return tags.sort();
  }

  /**
   * Extract expected output from assertions
   *
   * @param {string} testBody - Test body
   * @returns {string|undefined} Expected output
   * @private
   */
  _extractExpectedOutput(testBody) {
    if (!this.includeExpectations) {
      return undefined;
    }

    const outputs = [];

    // Match expect().toBe() or expect().toEqual()
    const expectPattern = /expect\(([^)]+)\)\.(toBe|toEqual|toStrictEqual)\(([^)]+)\)/g;
    let match;
    while ((match = expectPattern.exec(testBody)) !== null) {
      outputs.push(`${match[1]} => ${match[3]}`);
    }

    // Match expect().toBeTruthy/toBeFalsy
    const boolPattern = /expect\(([^)]+)\)\.(toBeTruthy|toBeFalsy|toBeDefined|toBeUndefined)\(\)/g;
    while ((match = boolPattern.exec(testBody)) !== null) {
      const expected = match[2] === 'toBeTruthy' ? 'true' :
                      match[2] === 'toBeFalsy' ? 'false' :
                      match[2] === 'toBeDefined' ? 'defined' : 'undefined';
      outputs.push(`${match[1]} => ${expected}`);
    }

    return outputs.length > 0 ? outputs.join('\n') : undefined;
  }

  /**
   * Check if code is runnable standalone
   *
   * @param {string} testBody - Test body
   * @returns {boolean} True if runnable
   * @private
   */
  _isRunnable(testBody) {
    // Check for mock/spy patterns that make it non-runnable
    const nonRunnablePatterns = [
      'vi.mock',
      'jest.mock',
      'sinon.',
      'beforeEach',
      'afterEach',
    ];

    return !nonRunnablePatterns.some(p => testBody.includes(p));
  }

  /**
   * Group examples by category
   *
   * @param {Array<Object>} examples - Extracted examples
   * @returns {Array<Object>} Grouped categories
   */
  groupByCategory(examples) {
    const categoryMap = new Map();

    for (const example of examples) {
      const cat = example.category || 'General';
      if (!categoryMap.has(cat)) {
        categoryMap.set(cat, {
          name: cat,
          description: `Examples for ${cat}`,
          examples: [],
        });
      }
      categoryMap.get(cat).examples.push(example);
    }

    // Sort categories for determinism
    return Array.from(categoryMap.values()).sort((a, b) =>
      a.name.localeCompare(b.name)
    );
  }

  /**
   * Generate Markdown from examples
   *
   * @param {Array<Object>} categories - Grouped categories
   * @param {string} fileName - Source file name
   * @returns {string} Markdown documentation
   */
  toMarkdown(categories, fileName) {
    const lines = [];

    // Header
    lines.push(`# Examples: ${fileName}`);
    lines.push('');
    lines.push(`Executable examples extracted from \`${fileName}\``);
    lines.push('');
    lines.push('---');
    lines.push('');

    // Table of contents
    lines.push(`${this.headingPrefix} Table of Contents`);
    lines.push('');
    for (const cat of categories) {
      const anchor = cat.name.toLowerCase().replace(/\s+/g, '-');
      lines.push(`- [${cat.name}](#${anchor}) (${cat.examples.length} examples)`);
    }
    lines.push('');
    lines.push('---');
    lines.push('');

    // Document each category
    for (const category of categories) {
      lines.push(`${this.headingPrefix} ${category.name}`);
      lines.push('');
      if (category.description) {
        lines.push(category.description);
        lines.push('');
      }

      for (const example of category.examples) {
        lines.push(`### ${example.name}`);
        lines.push('');
        lines.push(example.description);
        lines.push('');

        // Tags
        if (example.tags.length > 0) {
          lines.push(`*Tags: ${example.tags.map(t => `\`${t}\``).join(', ')}*`);
          lines.push('');
        }

        // Runnable badge
        if (!example.runnable) {
          lines.push('> **Note:** This example requires test framework setup');
          lines.push('');
        }

        // Code
        lines.push('```javascript');
        lines.push(example.code);
        lines.push('```');
        lines.push('');

        // Expected output
        if (example.expectedOutput) {
          lines.push('**Expected Output:**');
          lines.push('');
          lines.push('```');
          lines.push(example.expectedOutput);
          lines.push('```');
          lines.push('');
        }

        lines.push('---');
        lines.push('');
      }
    }

    return lines.join('\n');
  }

  /**
   * Project test file to example documentation
   *
   * @param {string} testContent - Test file content
   * @param {string} fileName - Test file name
   * @returns {Promise<{markdown: string, hash: string, examples: Array}>} Projection result
   *
   * @example
   * const result = await projector.project(
   *   fs.readFileSync('universe.test.mjs', 'utf-8'),
   *   'universe.test.mjs'
   * );
   */
  async project(testContent, fileName) {
    const examples = this.parseTestFile(testContent);
    const categories = this.groupByCategory(examples);
    const markdown = this.toMarkdown(categories, fileName);
    const hash = await blake3(markdown);

    return {
      markdown,
      hash,
      examples,
      categories,
      fileName,
      exampleCount: examples.length,
      categoryCount: categories.length,
    };
  }

  /**
   * Project multiple test files
   *
   * @param {Array<{content: string, name: string}>} testFiles - Test files
   * @returns {Promise<{files: Array, combinedHash: string}>} Projection results
   */
  async projectMultiple(testFiles) {
    const files = [];
    const hashes = [];

    for (const file of testFiles) {
      const result = await this.project(file.content, file.name);
      files.push(result);
      hashes.push(result.hash);
    }

    // Sort hashes for determinism
    hashes.sort();
    const combinedHash = await blake3(hashes.join(':'));

    return {
      files,
      combinedHash,
      totalExamples: files.reduce((sum, f) => sum + f.exampleCount, 0),
    };
  }

  /**
   * Verify projection determinism
   *
   * @param {string} testContent - Test content
   * @param {string} fileName - File name
   * @param {string} expectedHash - Expected hash
   * @returns {Promise<boolean>} True if hash matches
   */
  async verifyDeterminism(testContent, fileName, expectedHash) {
    const result = await this.project(testContent, fileName);
    return result.hash === expectedHash;
  }
}

export default ExampleProjector;
