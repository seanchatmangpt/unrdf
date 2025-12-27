/**
 * @fileoverview JSDoc Projector - Extract JSDoc comments and project to Markdown API docs
 *
 * **Purpose**: Deterministic projection from JSDoc + exported types to Markdown documentation.
 *
 * **Projection Formula**:
 *   M_api = Pi_jsdoc(source_files)
 *   Same source files -> Same Markdown (deterministic, reproducible)
 *
 * **Features**:
 * - Extracts @param, @returns, @example, @throws annotations
 * - Generates function/class signatures
 * - Sorts entries alphabetically for determinism
 * - Computes content hash for verification
 *
 * @module projection/jsdoc-projector
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';

/**
 * Schema for JSDoc entry
 */
const JsDocEntrySchema = z.object({
  name: z.string(),
  type: z.enum(['function', 'class', 'constant', 'method', 'property']),
  description: z.string(),
  params: z.array(z.object({
    name: z.string(),
    type: z.string(),
    description: z.string(),
    optional: z.boolean().default(false),
    default: z.string().optional(),
  })).default([]),
  returns: z.object({
    type: z.string(),
    description: z.string(),
  }).optional(),
  examples: z.array(z.string()).default([]),
  throws: z.array(z.object({
    type: z.string(),
    description: z.string(),
  })).default([]),
  since: z.string().optional(),
  deprecated: z.string().optional(),
  see: z.array(z.string()).default([]),
  memberof: z.string().optional(),
  async: z.boolean().default(false),
  static: z.boolean().default(false),
  private: z.boolean().default(false),
});

/**
 * Schema for module documentation
 */
const ModuleDocSchema = z.object({
  name: z.string(),
  description: z.string(),
  entries: z.array(JsDocEntrySchema),
  exports: z.array(z.string()).default([]),
  dependencies: z.array(z.string()).default([]),
});

/**
 * JSDoc Projector - Transforms source code JSDoc to Markdown API documentation
 *
 * @class JsDocProjector
 *
 * @example
 * const projector = new JsDocProjector();
 * const result = await projector.project(sourceContent, 'module-name');
 * console.log(result.markdown);
 * console.log(result.hash);
 */
export class JsDocProjector {
  /**
   * Create a new JSDoc projector
   *
   * @param {Object} [options] - Projector options
   * @param {boolean} [options.includePrivate=false] - Include private members
   * @param {boolean} [options.includeExamples=true] - Include examples in output
   * @param {string} [options.headingPrefix='##'] - Markdown heading prefix
   */
  constructor(options = {}) {
    this.includePrivate = options.includePrivate || false;
    this.includeExamples = options.includeExamples !== false;
    this.headingPrefix = options.headingPrefix || '##';
  }

  /**
   * Parse JSDoc comments from source code
   *
   * @param {string} sourceContent - JavaScript/MJS source content
   * @returns {Array<Object>} Parsed JSDoc entries
   */
  parseJsDoc(sourceContent) {
    const entries = [];

    // Match JSDoc blocks followed by function/class declarations
    const jsdocPattern = /\/\*\*\s*([\s\S]*?)\s*\*\/\s*(?:export\s+)?(?:async\s+)?(?:(function|class|const|let|var)\s+)?(\w+)/g;

    let match;
    while ((match = jsdocPattern.exec(sourceContent)) !== null) {
      const docBlock = match[1];
      const declType = match[2] || 'function';
      const name = match[3];

      const entry = this._parseDocBlock(docBlock, name, declType);
      if (entry && (!entry.private || this.includePrivate)) {
        entries.push(entry);
      }
    }

    // Sort for determinism
    entries.sort((a, b) => a.name.localeCompare(b.name));

    return entries;
  }

  /**
   * Parse a single JSDoc block
   *
   * @param {string} docBlock - Raw JSDoc content (without delimiters)
   * @param {string} name - Declaration name
   * @param {string} declType - Declaration type (function, class, const)
   * @returns {Object|null} Parsed entry
   * @private
   */
  _parseDocBlock(docBlock, name, declType) {
    const lines = docBlock.split('\n').map(line =>
      line.replace(/^\s*\*\s?/, '').trim()
    );

    const entry = {
      name,
      type: declType === 'class' ? 'class' :
            declType === 'const' ? 'constant' : 'function',
      description: '',
      params: [],
      examples: [],
      throws: [],
      see: [],
      async: false,
      static: false,
      private: false,
    };

    let currentTag = null;
    let currentContent = [];

    for (const line of lines) {
      if (line.startsWith('@')) {
        // Process previous tag
        if (currentTag) {
          this._processTag(entry, currentTag, currentContent.join('\n').trim());
        }

        // Parse new tag
        const tagMatch = line.match(/^@(\w+)\s*(.*)/);
        if (tagMatch) {
          currentTag = tagMatch[1];
          currentContent = [tagMatch[2]];
        }
      } else if (currentTag) {
        currentContent.push(line);
      } else {
        // Description line
        entry.description += (entry.description ? ' ' : '') + line;
      }
    }

    // Process last tag
    if (currentTag) {
      this._processTag(entry, currentTag, currentContent.join('\n').trim());
    }

    return entry;
  }

  /**
   * Process a JSDoc tag
   *
   * @param {Object} entry - Entry to update
   * @param {string} tag - Tag name
   * @param {string} content - Tag content
   * @private
   */
  _processTag(entry, tag, content) {
    switch (tag) {
      case 'param': {
        const paramMatch = content.match(/^\{([^}]+)\}\s*(?:\[)?(\w+)(?:\])?(?:\s*-?\s*(.*))?/);
        if (paramMatch) {
          entry.params.push({
            type: paramMatch[1],
            name: paramMatch[2],
            description: paramMatch[3] || '',
            optional: content.includes('['),
            default: undefined,
          });
        }
        break;
      }
      case 'returns':
      case 'return': {
        const returnMatch = content.match(/^\{([^}]+)\}\s*(.*)/);
        if (returnMatch) {
          entry.returns = {
            type: returnMatch[1],
            description: returnMatch[2],
          };
        }
        break;
      }
      case 'example':
        entry.examples.push(content);
        break;
      case 'throws':
      case 'exception': {
        const throwMatch = content.match(/^\{([^}]+)\}\s*(.*)/);
        if (throwMatch) {
          entry.throws.push({
            type: throwMatch[1],
            description: throwMatch[2],
          });
        }
        break;
      }
      case 'since':
        entry.since = content;
        break;
      case 'deprecated':
        entry.deprecated = content || 'This API is deprecated';
        break;
      case 'see':
        entry.see.push(content);
        break;
      case 'memberof':
        entry.memberof = content;
        break;
      case 'async':
        entry.async = true;
        break;
      case 'static':
        entry.static = true;
        break;
      case 'private':
        entry.private = true;
        break;
      case 'class':
        entry.type = 'class';
        break;
      case 'function':
      case 'method':
        entry.type = tag === 'method' ? 'method' : 'function';
        break;
      case 'fileoverview':
      case 'file':
      case 'module':
        // Module-level documentation - update description
        entry.description = content || entry.description;
        break;
    }
  }

  /**
   * Project JSDoc entries to Markdown
   *
   * @param {Array<Object>} entries - Parsed JSDoc entries
   * @param {string} moduleName - Module name for heading
   * @returns {string} Markdown documentation
   */
  toMarkdown(entries, moduleName) {
    const lines = [];

    // Module header
    lines.push(`# ${moduleName}`);
    lines.push('');
    lines.push(`API Reference for \`${moduleName}\``);
    lines.push('');
    lines.push('---');
    lines.push('');

    // Table of contents
    lines.push(`${this.headingPrefix} Table of Contents`);
    lines.push('');

    const classes = entries.filter(e => e.type === 'class');
    const functions = entries.filter(e => e.type === 'function');
    const constants = entries.filter(e => e.type === 'constant');

    if (classes.length > 0) {
      lines.push('### Classes');
      for (const cls of classes) {
        lines.push(`- [${cls.name}](#${cls.name.toLowerCase()})`);
      }
      lines.push('');
    }

    if (functions.length > 0) {
      lines.push('### Functions');
      for (const fn of functions) {
        lines.push(`- [${fn.name}](#${fn.name.toLowerCase()})`);
      }
      lines.push('');
    }

    if (constants.length > 0) {
      lines.push('### Constants');
      for (const c of constants) {
        lines.push(`- [${c.name}](#${c.name.toLowerCase()})`);
      }
      lines.push('');
    }

    lines.push('---');
    lines.push('');

    // Document each entry
    for (const entry of entries) {
      lines.push(...this._entryToMarkdown(entry));
      lines.push('');
    }

    return lines.join('\n');
  }

  /**
   * Convert single entry to Markdown
   *
   * @param {Object} entry - JSDoc entry
   * @returns {Array<string>} Markdown lines
   * @private
   */
  _entryToMarkdown(entry) {
    const lines = [];

    // Heading with anchor
    const typeLabel = entry.type.charAt(0).toUpperCase() + entry.type.slice(1);
    lines.push(`${this.headingPrefix} ${entry.name}`);
    lines.push('');

    // Type badge
    lines.push(`\`${typeLabel}\``);
    if (entry.async) lines.push(' `async`');
    if (entry.static) lines.push(' `static`');
    if (entry.deprecated) lines.push(' `deprecated`');
    lines.push('');

    // Description
    if (entry.description) {
      lines.push(entry.description);
      lines.push('');
    }

    // Deprecated notice
    if (entry.deprecated) {
      lines.push(`> **Deprecated**: ${entry.deprecated}`);
      lines.push('');
    }

    // Parameters
    if (entry.params.length > 0) {
      lines.push('**Parameters:**');
      lines.push('');
      lines.push('| Name | Type | Description |');
      lines.push('|------|------|-------------|');
      for (const param of entry.params) {
        const optionalMark = param.optional ? ' (optional)' : '';
        lines.push(`| \`${param.name}\`${optionalMark} | \`${param.type}\` | ${param.description} |`);
      }
      lines.push('');
    }

    // Returns
    if (entry.returns) {
      lines.push(`**Returns:** \`${entry.returns.type}\` - ${entry.returns.description}`);
      lines.push('');
    }

    // Throws
    if (entry.throws.length > 0) {
      lines.push('**Throws:**');
      lines.push('');
      for (const t of entry.throws) {
        lines.push(`- \`${t.type}\`: ${t.description}`);
      }
      lines.push('');
    }

    // Examples
    if (this.includeExamples && entry.examples.length > 0) {
      lines.push('**Example:**');
      lines.push('');
      for (const example of entry.examples) {
        if (example.includes('```')) {
          lines.push(example);
        } else {
          lines.push('```javascript');
          lines.push(example);
          lines.push('```');
        }
        lines.push('');
      }
    }

    // See also
    if (entry.see.length > 0) {
      lines.push('**See also:**');
      for (const see of entry.see) {
        lines.push(`- ${see}`);
      }
      lines.push('');
    }

    // Since
    if (entry.since) {
      lines.push(`*Since: ${entry.since}*`);
      lines.push('');
    }

    lines.push('---');

    return lines;
  }

  /**
   * Project source content to Markdown documentation
   *
   * @param {string} sourceContent - JavaScript source content
   * @param {string} moduleName - Module name
   * @returns {Promise<{markdown: string, hash: string, entries: Array}>} Projection result
   *
   * @example
   * const result = await projector.project(fs.readFileSync('module.mjs', 'utf-8'), 'my-module');
   */
  async project(sourceContent, moduleName) {
    const entries = this.parseJsDoc(sourceContent);
    const markdown = this.toMarkdown(entries, moduleName);
    const hash = await blake3(markdown);

    return {
      markdown,
      hash,
      entries,
      moduleName,
      entryCount: entries.length,
    };
  }

  /**
   * Project multiple source files
   *
   * @param {Array<{content: string, name: string}>} sources - Source files
   * @returns {Promise<{modules: Array, combinedHash: string}>} Projection results
   */
  async projectMultiple(sources) {
    const modules = [];
    const hashes = [];

    for (const source of sources) {
      const result = await this.project(source.content, source.name);
      modules.push(result);
      hashes.push(result.hash);
    }

    // Sort hashes for determinism
    hashes.sort();
    const combinedHash = await blake3(hashes.join(':'));

    return {
      modules,
      combinedHash,
      totalEntries: modules.reduce((sum, m) => sum + m.entryCount, 0),
    };
  }

  /**
   * Verify projection determinism
   *
   * @param {string} sourceContent - Source content
   * @param {string} moduleName - Module name
   * @param {string} expectedHash - Expected hash
   * @returns {Promise<boolean>} True if hash matches
   */
  async verifyDeterminism(sourceContent, moduleName, expectedHash) {
    const result = await this.project(sourceContent, moduleName);
    return result.hash === expectedHash;
  }
}

export default JsDocProjector;
