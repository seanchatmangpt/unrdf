/**
 * IDE Integration Capabilities
 *
 * Provides abstractions for editor integration patterns that work
 * across CLI and IDE surfaces. Enables code actions, diagnostics,
 * and inline suggestions with surface-agnostic interfaces.
 *
 * Universe:
 *   Σ_editor ≔ {positions, ranges, edits, diagnostics}
 *   Position := (file, line, column)
 *   Range := (start: Position, end: Position)
 *   Edit := (range: Range, newText: string)
 *
 * @module @unrdf/kgc-claude/capabilities/ide-integration
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now } from '@unrdf/kgc-4d';

/**
 * Position schema - zero-indexed line and column
 */
export const PositionSchema = z.object({
  file: z.string(),
  line: z.number().int().min(0),
  column: z.number().int().min(0),
});

/**
 * @typedef {z.infer<typeof PositionSchema>} Position
 */

/**
 * Range schema - start and end positions
 */
export const RangeSchema = z.object({
  start: PositionSchema,
  end: PositionSchema,
});

/**
 * @typedef {z.infer<typeof RangeSchema>} Range
 */

/**
 * Edit schema - text replacement at range
 */
export const EditSchema = z.object({
  range: RangeSchema,
  newText: z.string(),
});

/**
 * @typedef {z.infer<typeof EditSchema>} Edit
 */

/**
 * Code action schema - suggested refactoring or fix
 */
export const CodeActionSchema = z.object({
  id: z.string(),
  title: z.string(),
  kind: z.enum(['quickfix', 'refactor', 'source', 'info']),
  edits: z.array(EditSchema),
  description: z.string().optional(),
  isPreferred: z.boolean().default(false),
});

/**
 * @typedef {z.infer<typeof CodeActionSchema>} CodeAction
 */

/**
 * Diagnostic schema - error, warning, or info message
 */
export const DiagnosticSchema = z.object({
  id: z.string(),
  range: RangeSchema,
  severity: z.enum(['error', 'warning', 'info', 'hint']),
  message: z.string(),
  code: z.string().optional(),
  source: z.string().default('kgc-claude'),
  relatedInformation: z.array(z.object({
    location: RangeSchema,
    message: z.string(),
  })).default([]),
});

/**
 * @typedef {z.infer<typeof DiagnosticSchema>} Diagnostic
 */

/**
 * Inline suggestion schema - autocomplete or snippet
 */
export const InlineSuggestionSchema = z.object({
  id: z.string(),
  position: PositionSchema,
  text: z.string(),
  label: z.string(),
  detail: z.string().optional(),
  documentation: z.string().optional(),
  kind: z.enum(['text', 'method', 'function', 'variable', 'snippet']).default('text'),
  sortText: z.string().optional(),
});

/**
 * @typedef {z.infer<typeof InlineSuggestionSchema>} InlineSuggestion
 */

/**
 * IDE Integration manager
 */
export class IDEIntegration {
  constructor() {
    /** @type {Map<string, CodeAction[]>} */
    this.codeActions = new Map();
    /** @type {Map<string, Diagnostic[]>} */
    this.diagnostics = new Map();
    /** @type {Map<string, InlineSuggestion[]>} */
    this.suggestions = new Map();
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
   * Register code action
   * @param {string} file
   * @param {Omit<CodeAction, 'id'>} action
   * @returns {CodeAction}
   */
  registerCodeAction(file, action) {
    const id = this.generateId();
    const fullAction = CodeActionSchema.parse({ ...action, id });

    if (!this.codeActions.has(file)) {
      this.codeActions.set(file, []);
    }
    this.codeActions.get(file).push(fullAction);

    return fullAction;
  }

  /**
   * Get code actions for file and range
   * @param {string} file
   * @param {Range} [range]
   * @returns {CodeAction[]}
   */
  getCodeActions(file, range) {
    const actions = this.codeActions.get(file) || [];

    if (!range) {
      return actions;
    }

    // Filter actions that intersect with range
    return actions.filter(action =>
      action.edits.some(edit =>
        this.rangesIntersect(edit.range, range)
      )
    );
  }

  /**
   * Check if two ranges intersect
   * @param {Range} r1
   * @param {Range} r2
   * @returns {boolean}
   */
  rangesIntersect(r1, r2) {
    if (r1.start.file !== r2.start.file) return false;

    // r1 starts before r2 ends AND r1 ends after r2 starts
    return (
      this.positionCompare(r1.start, r2.end) <= 0 &&
      this.positionCompare(r1.end, r2.start) >= 0
    );
  }

  /**
   * Compare positions
   * @param {Position} p1
   * @param {Position} p2
   * @returns {number} -1 if p1 < p2, 0 if equal, 1 if p1 > p2
   */
  positionCompare(p1, p2) {
    if (p1.line !== p2.line) {
      return p1.line < p2.line ? -1 : 1;
    }
    if (p1.column !== p2.column) {
      return p1.column < p2.column ? -1 : 1;
    }
    return 0;
  }

  /**
   * Add diagnostic
   * @param {string} file
   * @param {Omit<Diagnostic, 'id'>} diagnostic
   * @returns {Diagnostic}
   */
  addDiagnostic(file, diagnostic) {
    const id = this.generateId();
    const fullDiagnostic = DiagnosticSchema.parse({ ...diagnostic, id });

    if (!this.diagnostics.has(file)) {
      this.diagnostics.set(file, []);
    }
    this.diagnostics.get(file).push(fullDiagnostic);

    return fullDiagnostic;
  }

  /**
   * Get diagnostics for file
   * @param {string} file
   * @param {string} [severity] - Filter by severity
   * @returns {Diagnostic[]}
   */
  getDiagnostics(file, severity) {
    const diagnostics = this.diagnostics.get(file) || [];

    if (!severity) {
      return diagnostics;
    }

    return diagnostics.filter(d => d.severity === severity);
  }

  /**
   * Clear diagnostics for file
   * @param {string} file
   */
  clearDiagnostics(file) {
    this.diagnostics.delete(file);
  }

  /**
   * Add inline suggestion
   * @param {string} file
   * @param {Omit<InlineSuggestion, 'id'>} suggestion
   * @returns {InlineSuggestion}
   */
  addSuggestion(file, suggestion) {
    const id = this.generateId();
    const fullSuggestion = InlineSuggestionSchema.parse({ ...suggestion, id });

    if (!this.suggestions.has(file)) {
      this.suggestions.set(file, []);
    }
    this.suggestions.get(file).push(fullSuggestion);

    return fullSuggestion;
  }

  /**
   * Get suggestions for position
   * @param {Position} position
   * @returns {InlineSuggestion[]}
   */
  getSuggestions(position) {
    const suggestions = this.suggestions.get(position.file) || [];

    // Return suggestions at or near the position
    return suggestions.filter(s =>
      s.position.file === position.file &&
      s.position.line === position.line &&
      s.position.column <= position.column
    ).sort((a, b) => {
      // Sort by sortText if available, otherwise by label
      const aSort = a.sortText || a.label;
      const bSort = b.sortText || b.label;
      return aSort.localeCompare(bSort);
    });
  }

  /**
   * Apply edits to text
   * @param {string} text
   * @param {Edit[]} edits
   * @returns {string}
   */
  applyEdits(text, edits) {
    const lines = text.split('\n');

    // Sort edits by position (reverse order to apply from end to start)
    const sortedEdits = [...edits].sort((a, b) => {
      const cmp = this.positionCompare(b.range.start, a.range.start);
      return cmp !== 0 ? cmp : this.positionCompare(b.range.end, a.range.end);
    });

    for (const edit of sortedEdits) {
      const { start, end } = edit.range;

      // Extract before, during, and after sections
      const before = lines.slice(0, start.line);
      const startLine = lines[start.line] || '';
      const endLine = lines[end.line] || '';

      const beforeColumn = startLine.substring(0, start.column);
      const afterColumn = endLine.substring(end.column);

      // Apply edit
      const newLines = edit.newText.split('\n');

      if (newLines.length === 1) {
        // Single line replacement
        const middle = [beforeColumn + newLines[0] + afterColumn];
        const after = lines.slice(end.line + 1);
        lines.splice(0, lines.length, ...before, ...middle, ...after);
      } else {
        // Multi-line replacement
        const firstNewLine = beforeColumn + newLines[0];
        const lastNewLine = newLines[newLines.length - 1] + afterColumn;
        const middle = [firstNewLine, ...newLines.slice(1, -1), lastNewLine];
        const after = lines.slice(end.line + 1);
        lines.splice(0, lines.length, ...before, ...middle, ...after);
      }
    }

    return lines.join('\n');
  }

  /**
   * Create receipt for integration operations
   * @returns {Promise<Object>}
   */
  async createReceipt() {
    const data = {
      codeActions: this.codeActions.size,
      diagnostics: this.diagnostics.size,
      suggestions: this.suggestions.size,
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
    this.codeActions.clear();
    this.diagnostics.clear();
    this.suggestions.clear();
  }
}

/**
 * Create IDE integration manager
 * @returns {IDEIntegration}
 */
export function createIDEIntegration() {
  return new IDEIntegration();
}

/**
 * Format position as string
 * @param {Position} position
 * @returns {string}
 */
export function formatPosition(position) {
  return `${position.file}:${position.line + 1}:${position.column + 1}`;
}

/**
 * Format range as string
 * @param {Range} range
 * @returns {string}
 */
export function formatRange(range) {
  if (range.start.file === range.end.file &&
      range.start.line === range.end.line) {
    return `${range.start.file}:${range.start.line + 1}:${range.start.column + 1}-${range.end.column + 1}`;
  }
  return `${formatPosition(range.start)}-${formatPosition(range.end)}`;
}

export default IDEIntegration;
