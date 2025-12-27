/**
 * UI Components Capabilities
 *
 * Provides terminal-based UI patterns for progress indicators,
 * interactive prompts, and result visualization. Works in both
 * CLI and IDE terminal contexts.
 *
 * Universe:
 *   Σ_ui ≔ {progress, prompts, visualizations}
 *   Progress := (current, total, label)
 *   Prompt := (question, options)
 *   Viz := (data, format)
 *
 * @module @unrdf/kgc-claude/capabilities/ui-components
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now } from '@unrdf/kgc-4d';

/**
 * Progress indicator schema
 */
export const ProgressSchema = z.object({
  id: z.string(),
  label: z.string(),
  current: z.number().min(0),
  total: z.number().min(0),
  unit: z.string().default('items'),
  startTime: z.bigint(),
  endTime: z.bigint().optional(),
  status: z.enum(['active', 'completed', 'failed']).default('active'),
});

/**
 * @typedef {z.infer<typeof ProgressSchema>} Progress
 */

/**
 * Interactive prompt schema
 */
export const PromptSchema = z.object({
  id: z.string(),
  question: z.string(),
  type: z.enum(['text', 'confirm', 'select', 'multiselect']),
  options: z.array(z.object({
    label: z.string(),
    value: z.any(),
    description: z.string().optional(),
  })).optional(),
  default: z.any().optional(),
  response: z.any().optional(),
  timestamp: z.bigint(),
});

/**
 * @typedef {z.infer<typeof PromptSchema>} Prompt
 */

/**
 * Visualization schema
 */
export const VisualizationSchema = z.object({
  id: z.string(),
  type: z.enum(['table', 'tree', 'graph', 'chart', 'json']),
  data: z.any(),
  format: z.object({
    columns: z.array(z.string()).optional(),
    colorize: z.boolean().default(false),
    compact: z.boolean().default(false),
  }).default({}),
  timestamp: z.bigint(),
});

/**
 * @typedef {z.infer<typeof VisualizationSchema>} Visualization
 */

/**
 * UI Components manager
 */
export class UIComponents {
  constructor() {
    /** @type {Map<string, Progress>} */
    this.progressIndicators = new Map();
    /** @type {Prompt[]} */
    this.prompts = [];
    /** @type {Visualization[]} */
    this.visualizations = [];
  }

  /**
   * Generate UUID
   */
  generateId() {
    if (typeof crypto !== 'undefined' && crypto.randomUUID) {
      return crypto.randomUUID();
    }
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
      const r = (Math.random() * 16) | 0;
      const v = c === 'x' ? r : (r & 0x3) | 0x8;
      return v.toString(16);
    });
  }

  /**
   * Create progress indicator
   * @param {string} label
   * @param {number} total
   * @param {Object} [options]
   * @param {string} [options.unit]
   * @returns {Progress}
   */
  createProgress(label, total, options = {}) {
    const id = this.generateId();
    const startTime = now();

    const progress = ProgressSchema.parse({
      id,
      label,
      current: 0,
      total,
      unit: options.unit || 'items',
      startTime,
      status: 'active',
    });

    this.progressIndicators.set(id, progress);
    return progress;
  }

  /**
   * Update progress
   * @param {string} id
   * @param {number} current
   * @returns {Progress}
   */
  updateProgress(id, current) {
    const progress = this.progressIndicators.get(id);
    if (!progress) {
      throw new Error(`Progress indicator not found: ${id}`);
    }

    progress.current = Math.min(current, progress.total);

    if (progress.current >= progress.total) {
      progress.status = 'completed';
      progress.endTime = now();
    }

    return progress;
  }

  /**
   * Increment progress
   * @param {string} id
   * @param {number} [delta=1]
   * @returns {Progress}
   */
  incrementProgress(id, delta = 1) {
    const progress = this.progressIndicators.get(id);
    if (!progress) {
      throw new Error(`Progress indicator not found: ${id}`);
    }

    return this.updateProgress(id, progress.current + delta);
  }

  /**
   * Fail progress indicator
   * @param {string} id
   * @returns {Progress}
   */
  failProgress(id) {
    const progress = this.progressIndicators.get(id);
    if (!progress) {
      throw new Error(`Progress indicator not found: ${id}`);
    }

    progress.status = 'failed';
    progress.endTime = now();

    return progress;
  }

  /**
   * Get progress percentage
   * @param {string} id
   * @returns {number} 0-100
   */
  getProgressPercentage(id) {
    const progress = this.progressIndicators.get(id);
    if (!progress) return 0;

    if (progress.total === 0) return 100;
    return Math.round((progress.current / progress.total) * 100);
  }

  /**
   * Format progress as string
   * @param {string} id
   * @returns {string}
   */
  formatProgress(id) {
    const progress = this.progressIndicators.get(id);
    if (!progress) return '';

    const percentage = this.getProgressPercentage(id);
    const bar = this.renderProgressBar(percentage, 30);
    const status = progress.status === 'active' ? '⏳' :
                   progress.status === 'completed' ? '✅' : '❌';

    return `${status} ${progress.label}: ${bar} ${percentage}% (${progress.current}/${progress.total} ${progress.unit})`;
  }

  /**
   * Render progress bar
   * @param {number} percentage - 0-100
   * @param {number} width - Bar width in characters
   * @returns {string}
   */
  renderProgressBar(percentage, width = 30) {
    const filled = Math.round((percentage / 100) * width);
    const empty = width - filled;
    return `[${'█'.repeat(filled)}${' '.repeat(empty)}]`;
  }

  /**
   * Create interactive prompt
   * @param {string} question
   * @param {string} type
   * @param {Object} [options]
   * @returns {Prompt}
   */
  createPrompt(question, type, options = {}) {
    const id = this.generateId();
    const timestamp = now();

    const prompt = PromptSchema.parse({
      id,
      question,
      type,
      options: options.options,
      default: options.default,
      timestamp,
    });

    this.prompts.push(prompt);
    return prompt;
  }

  /**
   * Record prompt response
   * @param {string} id
   * @param {any} response
   * @returns {Prompt}
   */
  recordResponse(id, response) {
    const prompt = this.prompts.find(p => p.id === id);
    if (!prompt) {
      throw new Error(`Prompt not found: ${id}`);
    }

    prompt.response = response;
    return prompt;
  }

  /**
   * Format prompt as string
   * @param {Prompt} prompt
   * @returns {string}
   */
  formatPrompt(prompt) {
    let output = `❓ ${prompt.question}\n`;

    if (prompt.type === 'confirm') {
      output += `   [y/n]${prompt.default ? ` (default: ${prompt.default})` : ''}`;
    } else if (prompt.type === 'select' && prompt.options) {
      prompt.options.forEach((opt, i) => {
        output += `   ${i + 1}. ${opt.label}`;
        if (opt.description) output += ` - ${opt.description}`;
        output += '\n';
      });
    } else if (prompt.type === 'multiselect' && prompt.options) {
      prompt.options.forEach((opt, i) => {
        output += `   [ ] ${i + 1}. ${opt.label}`;
        if (opt.description) output += ` - ${opt.description}`;
        output += '\n';
      });
    }

    return output;
  }

  /**
   * Create visualization
   * @param {string} type
   * @param {any} data
   * @param {Object} [format]
   * @returns {Visualization}
   */
  createVisualization(type, data, format = {}) {
    const id = this.generateId();
    const timestamp = now();

    const viz = VisualizationSchema.parse({
      id,
      type,
      data,
      format,
      timestamp,
    });

    this.visualizations.push(viz);
    return viz;
  }

  /**
   * Render table visualization
   * @param {Visualization} viz
   * @returns {string}
   */
  renderTable(viz) {
    if (viz.type !== 'table') {
      throw new Error('Visualization is not a table');
    }

    const data = Array.isArray(viz.data) ? viz.data : [viz.data];
    if (data.length === 0) return '(empty table)';

    const columns = viz.format.columns || Object.keys(data[0]);
    const rows = data.map(row => columns.map(col => String(row[col] ?? '')));

    // Calculate column widths
    const widths = columns.map((col, i) => {
      const values = [col, ...rows.map(row => row[i])];
      return Math.max(...values.map(v => v.length));
    });

    // Header
    const header = columns.map((col, i) => col.padEnd(widths[i])).join(' | ');
    const separator = widths.map(w => '-'.repeat(w)).join('-+-');

    // Rows
    const rowStrings = rows.map(row =>
      row.map((cell, i) => cell.padEnd(widths[i])).join(' | ')
    );

    return [header, separator, ...rowStrings].join('\n');
  }

  /**
   * Render tree visualization
   * @param {Visualization} viz
   * @returns {string}
   */
  renderTree(viz) {
    if (viz.type !== 'tree') {
      throw new Error('Visualization is not a tree');
    }

    const renderNode = (node, prefix = '', isLast = true) => {
      const connector = isLast ? '└── ' : '├── ';
      const label = typeof node === 'object' ? node.label || String(node) : String(node);
      let output = prefix + connector + label + '\n';

      if (typeof node === 'object' && node.children) {
        const children = Array.isArray(node.children) ? node.children : [node.children];
        const childPrefix = prefix + (isLast ? '    ' : '│   ');

        children.forEach((child, i) => {
          output += renderNode(child, childPrefix, i === children.length - 1);
        });
      }

      return output;
    };

    return renderNode(viz.data);
  }

  /**
   * Render JSON visualization
   * @param {Visualization} viz
   * @returns {string}
   */
  renderJSON(viz) {
    if (viz.type !== 'json') {
      throw new Error('Visualization is not JSON');
    }

    const indent = viz.format.compact ? 0 : 2;
    return JSON.stringify(viz.data, null, indent);
  }

  /**
   * Render visualization as string
   * @param {string} id
   * @returns {string}
   */
  renderVisualization(id) {
    const viz = this.visualizations.find(v => v.id === id);
    if (!viz) {
      throw new Error(`Visualization not found: ${id}`);
    }

    switch (viz.type) {
      case 'table':
        return this.renderTable(viz);
      case 'tree':
        return this.renderTree(viz);
      case 'json':
        return this.renderJSON(viz);
      default:
        return JSON.stringify(viz.data, null, 2);
    }
  }

  /**
   * Create receipt for UI operations
   * @returns {Promise<Object>}
   */
  async createReceipt() {
    const data = {
      progress: this.progressIndicators.size,
      prompts: this.prompts.length,
      visualizations: this.visualizations.length,
      t_ns: now().toString(),
    };

    const hash = await blake3(JSON.stringify(data));

    return {
      ...data,
      hash,
    };
  }

  /**
   * Clear all state
   */
  clear() {
    this.progressIndicators.clear();
    this.prompts = [];
    this.visualizations = [];
  }
}

/**
 * Create UI components manager
 * @returns {UIComponents}
 */
export function createUIComponents() {
  return new UIComponents();
}

/**
 * Helper: Create spinner animation frames
 * @returns {string[]}
 */
export function getSpinnerFrames() {
  return ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];
}

/**
 * Helper: Create box drawing characters
 * @returns {Object}
 */
export function getBoxChars() {
  return {
    topLeft: '┌',
    topRight: '┐',
    bottomLeft: '└',
    bottomRight: '┘',
    horizontal: '─',
    vertical: '│',
    cross: '┼',
    teeLeft: '├',
    teeRight: '┤',
    teeTop: '┬',
    teeBottom: '┴',
  };
}

export default UIComponents;
