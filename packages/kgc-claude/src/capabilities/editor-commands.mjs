/**
 * Editor Commands Capabilities
 *
 * Provides command registration, execution, and keyboard shortcut
 * management patterns. Adapts between CLI slash commands and
 * IDE command palette integrations.
 *
 * Universe:
 *   Σ_cmd ≔ {commands, shortcuts, handlers}
 *   Command := (id, title, handler)
 *   Shortcut := (key, modifiers, commandId)
 *
 * @module @unrdf/kgc-claude/capabilities/editor-commands
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now } from '@unrdf/kgc-4d';

/**
 * Command schema
 */
export const CommandSchema = z.object({
  id: z.string(),
  title: z.string(),
  category: z.string().default('General'),
  description: z.string().optional(),
  handler: z.function(), // Zod function validation
  when: z.string().optional(), // Condition expression
  enablement: z.string().optional(), // Enablement expression
});

/**
 * @typedef {z.infer<typeof CommandSchema>} Command
 */

/**
 * Keyboard shortcut schema
 */
export const ShortcutSchema = z.object({
  id: z.string(),
  key: z.string(), // e.g., "ctrl+shift+p"
  commandId: z.string(),
  when: z.string().optional(),
});

/**
 * @typedef {z.infer<typeof ShortcutSchema>} Shortcut
 */

/**
 * Command execution result schema
 */
export const ExecutionResultSchema = z.object({
  commandId: z.string(),
  success: z.boolean(),
  result: z.any().optional(),
  error: z.string().optional(),
  duration: z.number(), // milliseconds
  timestamp: z.bigint(),
});

/**
 * @typedef {z.infer<typeof ExecutionResultSchema>} ExecutionResult
 */

/**
 * Command context schema
 */
export const CommandContextSchema = z.object({
  activeFile: z.string().optional(),
  selectedText: z.string().optional(),
  cursorPosition: z.object({
    line: z.number(),
    column: z.number(),
  }).optional(),
  env: z.record(z.string()).default({}),
});

/**
 * @typedef {z.infer<typeof CommandContextSchema>} CommandContext
 */

/**
 * Editor Commands manager
 */
export class EditorCommands {
  constructor() {
    /** @type {Map<string, Command>} */
    this.commands = new Map();
    /** @type {Map<string, Shortcut>} */
    this.shortcuts = new Map();
    /** @type {ExecutionResult[]} */
    this.executionHistory = [];
    /** @type {CommandContext} */
    this.currentContext = {};
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
   * Register command
   * @param {Omit<Command, 'handler'> & {handler: Function}} command
   * @returns {Command}
   */
  registerCommand(command) {
    const validated = CommandSchema.parse(command);
    this.commands.set(validated.id, validated);
    return validated;
  }

  /**
   * Unregister command
   * @param {string} commandId
   * @returns {boolean}
   */
  unregisterCommand(commandId) {
    // Remove associated shortcuts
    for (const [key, shortcut] of this.shortcuts) {
      if (shortcut.commandId === commandId) {
        this.shortcuts.delete(key);
      }
    }

    return this.commands.delete(commandId);
  }

  /**
   * Get command by ID
   * @param {string} commandId
   * @returns {Command | undefined}
   */
  getCommand(commandId) {
    return this.commands.get(commandId);
  }

  /**
   * Get all commands
   * @param {string} [category] - Filter by category
   * @returns {Command[]}
   */
  getAllCommands(category) {
    const commands = Array.from(this.commands.values());

    if (!category) {
      return commands;
    }

    return commands.filter(cmd => cmd.category === category);
  }

  /**
   * Register keyboard shortcut
   * @param {string} key - e.g., "ctrl+shift+p"
   * @param {string} commandId
   * @param {Object} [options]
   * @returns {Shortcut}
   */
  registerShortcut(key, commandId, options = {}) {
    if (!this.commands.has(commandId)) {
      throw new Error(`Command not found: ${commandId}`);
    }

    const id = this.generateId();
    const shortcut = ShortcutSchema.parse({
      id,
      key: this.normalizeKey(key),
      commandId,
      when: options.when,
    });

    this.shortcuts.set(shortcut.key, shortcut);
    return shortcut;
  }

  /**
   * Normalize key combination
   * @param {string} key
   * @returns {string}
   */
  normalizeKey(key) {
    const parts = key.toLowerCase().split('+').map(p => p.trim());
    const modifiers = parts.filter(p => ['ctrl', 'shift', 'alt', 'meta', 'cmd'].includes(p));
    const mainKey = parts.find(p => !modifiers.includes(p));

    // Sort modifiers for consistency
    modifiers.sort();

    return [...modifiers, mainKey].join('+');
  }

  /**
   * Get shortcut for key combination
   * @param {string} key
   * @returns {Shortcut | undefined}
   */
  getShortcut(key) {
    const normalized = this.normalizeKey(key);
    return this.shortcuts.get(normalized);
  }

  /**
   * Get shortcuts for command
   * @param {string} commandId
   * @returns {Shortcut[]}
   */
  getShortcutsForCommand(commandId) {
    return Array.from(this.shortcuts.values())
      .filter(s => s.commandId === commandId);
  }

  /**
   * Update command context
   * @param {Partial<CommandContext>} context
   */
  updateContext(context) {
    this.currentContext = {
      ...this.currentContext,
      ...context,
    };
  }

  /**
   * Evaluate condition expression
   * @param {string} expression
   * @param {CommandContext} context
   * @returns {boolean}
   */
  evaluateCondition(expression, context) {
    if (!expression) return true;

    try {
      // Simple expression evaluator (can be extended)
      // Supports: "activeFile", "selectedText", etc.
      const vars = {
        activeFile: context.activeFile,
        selectedText: context.selectedText,
        cursorPosition: context.cursorPosition,
        ...context.env,
      };

      // eslint-disable-next-line no-new-func
      const fn = new Function(...Object.keys(vars), `return ${expression}`);
      return Boolean(fn(...Object.values(vars)));
    } catch (error) {
      console.warn(`Failed to evaluate condition: ${expression}`, error);
      return false;
    }
  }

  /**
   * Execute command
   * @param {string} commandId
   * @param {any} [args]
   * @param {CommandContext} [context]
   * @returns {Promise<ExecutionResult>}
   */
  async executeCommand(commandId, args, context) {
    const command = this.commands.get(commandId);
    if (!command) {
      throw new Error(`Command not found: ${commandId}`);
    }

    const executionContext = context || this.currentContext;

    // Check when condition
    if (command.when && !this.evaluateCondition(command.when, executionContext)) {
      throw new Error(`Command ${commandId} cannot be executed in current context`);
    }

    const startTime = Date.now();
    const timestamp = now();
    let success = true;
    let result;
    let error;

    try {
      result = await command.handler(args);
    } catch (err) {
      success = false;
      error = err.message;
    }

    const duration = Date.now() - startTime;

    const executionResult = ExecutionResultSchema.parse({
      commandId,
      success,
      result,
      error,
      duration,
      timestamp,
    });

    this.executionHistory.push(executionResult);
    return executionResult;
  }

  /**
   * Execute shortcut
   * @param {string} key
   * @param {any} [args]
   * @returns {Promise<ExecutionResult>}
   */
  async executeShortcut(key, args) {
    const shortcut = this.getShortcut(key);
    if (!shortcut) {
      throw new Error(`Shortcut not found: ${key}`);
    }

    // Check shortcut condition
    if (shortcut.when && !this.evaluateCondition(shortcut.when, this.currentContext)) {
      throw new Error(`Shortcut ${key} cannot be executed in current context`);
    }

    return this.executeCommand(shortcut.commandId, args);
  }

  /**
   * Get execution history
   * @param {string} [commandId] - Filter by command ID
   * @returns {ExecutionResult[]}
   */
  getExecutionHistory(commandId) {
    if (!commandId) {
      return [...this.executionHistory];
    }

    return this.executionHistory.filter(e => e.commandId === commandId);
  }

  /**
   * Get command palette items
   * @returns {Array<{id: string, title: string, category: string, shortcut?: string}>}
   */
  getCommandPaletteItems() {
    return Array.from(this.commands.values()).map(cmd => {
      const shortcuts = this.getShortcutsForCommand(cmd.id);
      const shortcut = shortcuts.length > 0 ? shortcuts[0].key : undefined;

      return {
        id: cmd.id,
        title: cmd.title,
        category: cmd.category,
        description: cmd.description,
        shortcut,
      };
    });
  }

  /**
   * Search commands
   * @param {string} query
   * @returns {Command[]}
   */
  searchCommands(query) {
    const lowerQuery = query.toLowerCase();

    return Array.from(this.commands.values()).filter(cmd =>
      cmd.id.toLowerCase().includes(lowerQuery) ||
      cmd.title.toLowerCase().includes(lowerQuery) ||
      cmd.category.toLowerCase().includes(lowerQuery) ||
      (cmd.description && cmd.description.toLowerCase().includes(lowerQuery))
    );
  }

  /**
   * Create receipt for command operations
   * @returns {Promise<Object>}
   */
  async createReceipt() {
    const data = {
      commands: this.commands.size,
      shortcuts: this.shortcuts.size,
      executions: this.executionHistory.length,
      successRate: this.calculateSuccessRate(),
      t_ns: now().toString(),
    };

    const hash = await blake3(JSON.stringify(data));

    return {
      ...data,
      hash,
    };
  }

  /**
   * Calculate command execution success rate
   * @returns {number} 0-100
   */
  calculateSuccessRate() {
    if (this.executionHistory.length === 0) return 100;

    const successful = this.executionHistory.filter(e => e.success).length;
    return Math.round((successful / this.executionHistory.length) * 100);
  }

  /**
   * Clear all state
   */
  clear() {
    this.commands.clear();
    this.shortcuts.clear();
    this.executionHistory = [];
    this.currentContext = {};
  }
}

/**
 * Create editor commands manager
 * @returns {EditorCommands}
 */
export function createEditorCommands() {
  return new EditorCommands();
}

/**
 * Create command from slash command syntax
 * @param {string} slashCommand - e.g., "/build"
 * @param {Function} handler
 * @param {Object} [options]
 * @returns {Command}
 */
export function fromSlashCommand(slashCommand, handler, options = {}) {
  const id = slashCommand.replace(/^\//, 'cmd.');

  return {
    id,
    title: options.title || slashCommand,
    category: options.category || 'Custom',
    description: options.description,
    handler,
    when: options.when,
    enablement: options.enablement,
  };
}

/**
 * Format shortcut for display
 * @param {string} key
 * @returns {string}
 */
export function formatShortcut(key) {
  return key
    .split('+')
    .map(part => {
      switch (part) {
        case 'ctrl': return '⌃';
        case 'shift': return '⇧';
        case 'alt': return '⌥';
        case 'meta':
        case 'cmd': return '⌘';
        default: return part.toUpperCase();
      }
    })
    .join('');
}

export default EditorCommands;
