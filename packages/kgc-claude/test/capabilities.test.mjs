/**
 * Tests for IDE Capabilities modules
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  IDEIntegration,
  createIDEIntegration,
  formatPosition,
  formatRange,
  PositionSchema,
  RangeSchema,
  EditSchema,
  CodeActionSchema,
  DiagnosticSchema,
  InlineSuggestionSchema,
} from '../src/capabilities/ide-integration.mjs';

import {
  UIComponents,
  createUIComponents,
  getSpinnerFrames,
  getBoxChars,
  ProgressSchema,
  PromptSchema,
  VisualizationSchema,
} from '../src/capabilities/ui-components.mjs';

import {
  EditorCommands,
  createEditorCommands,
  fromSlashCommand,
  formatShortcut,
  CommandSchema,
  ShortcutSchema,
  ExecutionResultSchema,
  CommandContextSchema,
} from '../src/capabilities/editor-commands.mjs';

// ============================================================================
// IDE Integration Tests
// ============================================================================

describe('IDEIntegration', () => {
  /** @type {IDEIntegration} */
  let ide;

  beforeEach(() => {
    ide = createIDEIntegration();
  });

  describe('Schemas', () => {
    it('should validate Position schema', () => {
      const position = { file: 'test.js', line: 10, column: 5 };
      expect(() => PositionSchema.parse(position)).not.toThrow();
    });

    it('should validate Range schema', () => {
      const range = {
        start: { file: 'test.js', line: 10, column: 5 },
        end: { file: 'test.js', line: 10, column: 15 },
      };
      expect(() => RangeSchema.parse(range)).not.toThrow();
    });

    it('should validate Edit schema', () => {
      const edit = {
        range: {
          start: { file: 'test.js', line: 10, column: 5 },
          end: { file: 'test.js', line: 10, column: 15 },
        },
        newText: 'newValue',
      };
      expect(() => EditSchema.parse(edit)).not.toThrow();
    });
  });

  describe('Code Actions', () => {
    it('should register code action', () => {
      const action = ide.registerCodeAction('test.js', {
        title: 'Extract variable',
        kind: 'refactor',
        edits: [{
          range: {
            start: { file: 'test.js', line: 10, column: 5 },
            end: { file: 'test.js', line: 10, column: 15 },
          },
          newText: 'myVar',
        }],
      });

      expect(action.id).toBeDefined();
      expect(action.title).toBe('Extract variable');
      expect(action.kind).toBe('refactor');
    });

    it('should get code actions for file', () => {
      ide.registerCodeAction('test.js', {
        title: 'Fix import',
        kind: 'quickfix',
        edits: [],
      });

      const actions = ide.getCodeActions('test.js');
      expect(actions).toHaveLength(1);
      expect(actions[0].title).toBe('Fix import');
    });

    it('should filter code actions by range', () => {
      ide.registerCodeAction('test.js', {
        title: 'Action 1',
        kind: 'quickfix',
        edits: [{
          range: {
            start: { file: 'test.js', line: 5, column: 0 },
            end: { file: 'test.js', line: 5, column: 10 },
          },
          newText: 'new',
        }],
      });

      ide.registerCodeAction('test.js', {
        title: 'Action 2',
        kind: 'quickfix',
        edits: [{
          range: {
            start: { file: 'test.js', line: 15, column: 0 },
            end: { file: 'test.js', line: 15, column: 10 },
          },
          newText: 'new',
        }],
      });

      const actions = ide.getCodeActions('test.js', {
        start: { file: 'test.js', line: 4, column: 0 },
        end: { file: 'test.js', line: 6, column: 0 },
      });

      expect(actions).toHaveLength(1);
      expect(actions[0].title).toBe('Action 1');
    });
  });

  describe('Diagnostics', () => {
    it('should add diagnostic', () => {
      const diagnostic = ide.addDiagnostic('test.js', {
        range: {
          start: { file: 'test.js', line: 10, column: 5 },
          end: { file: 'test.js', line: 10, column: 15 },
        },
        severity: 'error',
        message: 'Undefined variable',
      });

      expect(diagnostic.id).toBeDefined();
      expect(diagnostic.severity).toBe('error');
      expect(diagnostic.message).toBe('Undefined variable');
    });

    it('should get diagnostics for file', () => {
      ide.addDiagnostic('test.js', {
        range: {
          start: { file: 'test.js', line: 10, column: 5 },
          end: { file: 'test.js', line: 10, column: 15 },
        },
        severity: 'error',
        message: 'Error 1',
      });

      ide.addDiagnostic('test.js', {
        range: {
          start: { file: 'test.js', line: 20, column: 0 },
          end: { file: 'test.js', line: 20, column: 10 },
        },
        severity: 'warning',
        message: 'Warning 1',
      });

      const all = ide.getDiagnostics('test.js');
      expect(all).toHaveLength(2);

      const errors = ide.getDiagnostics('test.js', 'error');
      expect(errors).toHaveLength(1);
    });

    it('should clear diagnostics', () => {
      ide.addDiagnostic('test.js', {
        range: {
          start: { file: 'test.js', line: 10, column: 5 },
          end: { file: 'test.js', line: 10, column: 15 },
        },
        severity: 'error',
        message: 'Error',
      });

      expect(ide.getDiagnostics('test.js')).toHaveLength(1);

      ide.clearDiagnostics('test.js');
      expect(ide.getDiagnostics('test.js')).toHaveLength(0);
    });
  });

  describe('Inline Suggestions', () => {
    it('should add suggestion', () => {
      const suggestion = ide.addSuggestion('test.js', {
        position: { file: 'test.js', line: 10, column: 5 },
        text: 'myVariable',
        label: 'myVariable',
        kind: 'variable',
      });

      expect(suggestion.id).toBeDefined();
      expect(suggestion.text).toBe('myVariable');
    });

    it('should get suggestions for position', () => {
      ide.addSuggestion('test.js', {
        position: { file: 'test.js', line: 10, column: 5 },
        text: 'varA',
        label: 'varA',
      });

      ide.addSuggestion('test.js', {
        position: { file: 'test.js', line: 10, column: 5 },
        text: 'varB',
        label: 'varB',
      });

      const suggestions = ide.getSuggestions({ file: 'test.js', line: 10, column: 10 });
      expect(suggestions).toHaveLength(2);
    });
  });

  describe('Edit Application', () => {
    it('should apply single line edit', () => {
      const text = 'hello world';
      const edits = [{
        range: {
          start: { file: 'test.js', line: 0, column: 6 },
          end: { file: 'test.js', line: 0, column: 11 },
        },
        newText: 'universe',
      }];

      const result = ide.applyEdits(text, edits);
      expect(result).toBe('hello universe');
    });

    it('should apply multi-line edit', () => {
      const text = 'line 1\nline 2\nline 3';
      // Replace from column 4 of line 0 (" 1") to column 4 of line 1 (" 2")
      // beforeColumn = "line", afterColumn = " 2"
      // Result: "line" + "modified\nline" + " 2" = "linemodified\nline 2"
      const edits = [{
        range: {
          start: { file: 'test.js', line: 0, column: 4 },
          end: { file: 'test.js', line: 1, column: 4 },
        },
        newText: ' modified\nline',
      }];

      const result = ide.applyEdits(text, edits);
      expect(result).toBe('line modified\nline 2\nline 3');
    });
  });

  describe('Formatting', () => {
    it('should format position', () => {
      const pos = { file: 'test.js', line: 10, column: 5 };
      const formatted = formatPosition(pos);
      expect(formatted).toBe('test.js:11:6');
    });

    it('should format range', () => {
      const range = {
        start: { file: 'test.js', line: 10, column: 5 },
        end: { file: 'test.js', line: 10, column: 15 },
      };
      const formatted = formatRange(range);
      expect(formatted).toBe('test.js:11:6-16');
    });
  });
});

// ============================================================================
// UI Components Tests
// ============================================================================

describe('UIComponents', () => {
  /** @type {UIComponents} */
  let ui;

  beforeEach(() => {
    ui = createUIComponents();
  });

  describe('Progress Indicators', () => {
    it('should create progress indicator', () => {
      const progress = ui.createProgress('Loading files', 100);

      expect(progress.id).toBeDefined();
      expect(progress.label).toBe('Loading files');
      expect(progress.total).toBe(100);
      expect(progress.current).toBe(0);
      expect(progress.status).toBe('active');
    });

    it('should update progress', () => {
      const progress = ui.createProgress('Processing', 100);
      ui.updateProgress(progress.id, 50);

      expect(progress.current).toBe(50);
      expect(progress.status).toBe('active');
    });

    it('should auto-complete progress', () => {
      const progress = ui.createProgress('Tasks', 10);
      ui.updateProgress(progress.id, 10);

      expect(progress.status).toBe('completed');
      expect(progress.endTime).toBeDefined();
    });

    it('should increment progress', () => {
      const progress = ui.createProgress('Items', 10);
      ui.incrementProgress(progress.id);
      ui.incrementProgress(progress.id, 2);

      expect(progress.current).toBe(3);
    });

    it('should fail progress', () => {
      const progress = ui.createProgress('Tasks', 10);
      ui.failProgress(progress.id);

      expect(progress.status).toBe('failed');
      expect(progress.endTime).toBeDefined();
    });

    it('should calculate progress percentage', () => {
      const progress = ui.createProgress('Items', 100);
      ui.updateProgress(progress.id, 25);

      const percentage = ui.getProgressPercentage(progress.id);
      expect(percentage).toBe(25);
    });

    it('should format progress', () => {
      const progress = ui.createProgress('Loading', 100, { unit: 'files' });
      ui.updateProgress(progress.id, 50);

      const formatted = ui.formatProgress(progress.id);
      expect(formatted).toContain('Loading');
      expect(formatted).toContain('50%');
      expect(formatted).toContain('50/100 files');
    });
  });

  describe('Interactive Prompts', () => {
    it('should create text prompt', () => {
      const prompt = ui.createPrompt('Enter name', 'text');

      expect(prompt.id).toBeDefined();
      expect(prompt.question).toBe('Enter name');
      expect(prompt.type).toBe('text');
    });

    it('should create confirm prompt', () => {
      const prompt = ui.createPrompt('Continue?', 'confirm', { default: true });

      expect(prompt.type).toBe('confirm');
      expect(prompt.default).toBe(true);
    });

    it('should create select prompt', () => {
      const prompt = ui.createPrompt('Choose option', 'select', {
        options: [
          { label: 'Option 1', value: 1 },
          { label: 'Option 2', value: 2 },
        ],
      });

      expect(prompt.type).toBe('select');
      expect(prompt.options).toHaveLength(2);
    });

    it('should record response', () => {
      const prompt = ui.createPrompt('Name?', 'text');
      ui.recordResponse(prompt.id, 'Alice');

      expect(prompt.response).toBe('Alice');
    });

    it('should format prompt', () => {
      const prompt = ui.createPrompt('Continue?', 'confirm');
      const formatted = ui.formatPrompt(prompt);

      expect(formatted).toContain('Continue?');
      expect(formatted).toContain('[y/n]');
    });
  });

  describe('Visualizations', () => {
    it('should create table visualization', () => {
      const viz = ui.createVisualization('table', [
        { name: 'Alice', age: 30 },
        { name: 'Bob', age: 25 },
      ]);

      expect(viz.id).toBeDefined();
      expect(viz.type).toBe('table');
    });

    it('should render table', () => {
      const viz = ui.createVisualization('table', [
        { name: 'Alice', age: 30 },
        { name: 'Bob', age: 25 },
      ]);

      const rendered = ui.renderVisualization(viz.id);
      expect(rendered).toContain('Alice');
      expect(rendered).toContain('30');
      expect(rendered).toContain('Bob');
    });

    it('should create tree visualization', () => {
      const tree = {
        label: 'root',
        children: [
          { label: 'child1' },
          { label: 'child2', children: [{ label: 'grandchild' }] },
        ],
      };

      const viz = ui.createVisualization('tree', tree);
      const rendered = ui.renderVisualization(viz.id);

      expect(rendered).toContain('root');
      expect(rendered).toContain('child1');
      expect(rendered).toContain('grandchild');
    });

    it('should create JSON visualization', () => {
      const viz = ui.createVisualization('json', { foo: 'bar', baz: 123 });
      const rendered = ui.renderVisualization(viz.id);

      expect(rendered).toContain('"foo"');
      expect(rendered).toContain('"bar"');
    });
  });

  describe('Helpers', () => {
    it('should provide spinner frames', () => {
      const frames = getSpinnerFrames();
      expect(frames.length).toBeGreaterThan(0);
    });

    it('should provide box chars', () => {
      const chars = getBoxChars();
      expect(chars.topLeft).toBe('┌');
      expect(chars.horizontal).toBe('─');
    });
  });
});

// ============================================================================
// Editor Commands Tests
// ============================================================================

describe('EditorCommands', () => {
  /** @type {EditorCommands} */
  let commands;

  beforeEach(() => {
    commands = createEditorCommands();
  });

  describe('Command Registration', () => {
    it('should register command', () => {
      const cmd = commands.registerCommand({
        id: 'test.command',
        title: 'Test Command',
        category: 'Test',
        handler: async () => 'result',
      });

      expect(cmd.id).toBe('test.command');
      expect(cmd.title).toBe('Test Command');
    });

    it('should get command by ID', () => {
      commands.registerCommand({
        id: 'test.command',
        title: 'Test Command',
        handler: async () => {},
      });

      const cmd = commands.getCommand('test.command');
      expect(cmd).toBeDefined();
      expect(cmd.id).toBe('test.command');
    });

    it('should get all commands', () => {
      commands.registerCommand({
        id: 'cmd1',
        title: 'Command 1',
        handler: async () => {},
      });

      commands.registerCommand({
        id: 'cmd2',
        title: 'Command 2',
        handler: async () => {},
      });

      const all = commands.getAllCommands();
      expect(all).toHaveLength(2);
    });

    it('should filter commands by category', () => {
      commands.registerCommand({
        id: 'cmd1',
        title: 'Command 1',
        category: 'Edit',
        handler: async () => {},
      });

      commands.registerCommand({
        id: 'cmd2',
        title: 'Command 2',
        category: 'View',
        handler: async () => {},
      });

      const editCommands = commands.getAllCommands('Edit');
      expect(editCommands).toHaveLength(1);
      expect(editCommands[0].category).toBe('Edit');
    });

    it('should unregister command', () => {
      commands.registerCommand({
        id: 'test.command',
        title: 'Test',
        handler: async () => {},
      });

      const removed = commands.unregisterCommand('test.command');
      expect(removed).toBe(true);
      expect(commands.getCommand('test.command')).toBeUndefined();
    });
  });

  describe('Keyboard Shortcuts', () => {
    beforeEach(() => {
      commands.registerCommand({
        id: 'test.save',
        title: 'Save',
        handler: async () => 'saved',
      });
    });

    it('should register shortcut', () => {
      const shortcut = commands.registerShortcut('ctrl+s', 'test.save');

      expect(shortcut.id).toBeDefined();
      expect(shortcut.key).toBe('ctrl+s');
      expect(shortcut.commandId).toBe('test.save');
    });

    it('should normalize key combinations', () => {
      const shortcut = commands.registerShortcut('SHIFT+CTRL+P', 'test.save');
      expect(shortcut.key).toBe('ctrl+shift+p');
    });

    it('should get shortcut', () => {
      commands.registerShortcut('ctrl+s', 'test.save');

      const shortcut = commands.getShortcut('ctrl+s');
      expect(shortcut).toBeDefined();
      expect(shortcut.commandId).toBe('test.save');
    });

    it('should get shortcuts for command', () => {
      commands.registerShortcut('ctrl+s', 'test.save');
      commands.registerShortcut('cmd+s', 'test.save');

      const shortcuts = commands.getShortcutsForCommand('test.save');
      expect(shortcuts).toHaveLength(2);
    });
  });

  describe('Command Execution', () => {
    it('should execute command', async () => {
      commands.registerCommand({
        id: 'test.add',
        title: 'Add',
        handler: async (args) => args.a + args.b,
      });

      const result = await commands.executeCommand('test.add', { a: 2, b: 3 });

      expect(result.success).toBe(true);
      expect(result.result).toBe(5);
      expect(result.commandId).toBe('test.add');
    });

    it('should handle command errors', async () => {
      commands.registerCommand({
        id: 'test.fail',
        title: 'Fail',
        handler: async () => {
          throw new Error('Command failed');
        },
      });

      const result = await commands.executeCommand('test.fail');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Command failed');
    });

    it('should execute shortcut', async () => {
      commands.registerCommand({
        id: 'test.action',
        title: 'Action',
        handler: async () => 'done',
      });

      commands.registerShortcut('ctrl+k', 'test.action');

      const result = await commands.executeShortcut('ctrl+k');
      expect(result.success).toBe(true);
      expect(result.result).toBe('done');
    });

    it('should record execution history', async () => {
      commands.registerCommand({
        id: 'test.cmd',
        title: 'Test',
        handler: async () => 'ok',
      });

      await commands.executeCommand('test.cmd');
      await commands.executeCommand('test.cmd');

      const history = commands.getExecutionHistory('test.cmd');
      expect(history).toHaveLength(2);
    });
  });

  describe('Command Context', () => {
    it('should update context', () => {
      commands.updateContext({ activeFile: 'test.js' });
      expect(commands.currentContext.activeFile).toBe('test.js');
    });

    it('should evaluate condition', () => {
      commands.updateContext({ activeFile: 'test.js' });

      const result = commands.evaluateCondition('activeFile === "test.js"', commands.currentContext);
      expect(result).toBe(true);
    });

    it('should respect when condition', async () => {
      commands.registerCommand({
        id: 'test.conditional',
        title: 'Conditional',
        handler: async () => 'ok',
        when: 'activeFile === "allowed.js"',
      });

      commands.updateContext({ activeFile: 'denied.js' });

      await expect(commands.executeCommand('test.conditional')).rejects.toThrow();
    });
  });

  describe('Command Palette', () => {
    it('should get command palette items', () => {
      commands.registerCommand({
        id: 'test.cmd',
        title: 'Test Command',
        category: 'Test',
        handler: async () => {},
      });

      commands.registerShortcut('ctrl+t', 'test.cmd');

      const items = commands.getCommandPaletteItems();
      expect(items).toHaveLength(1);
      expect(items[0].title).toBe('Test Command');
      expect(items[0].shortcut).toBe('ctrl+t');
    });

    it('should search commands', () => {
      commands.registerCommand({
        id: 'file.save',
        title: 'Save File',
        handler: async () => {},
      });

      commands.registerCommand({
        id: 'file.open',
        title: 'Open File',
        handler: async () => {},
      });

      const results = commands.searchCommands('save');
      expect(results).toHaveLength(1);
      expect(results[0].title).toBe('Save File');
    });
  });

  describe('Helpers', () => {
    it('should create command from slash syntax', () => {
      const cmd = fromSlashCommand('/build', async () => 'built', {
        title: 'Build Project',
        category: 'Build',
      });

      expect(cmd.id).toBe('cmd.build');
      expect(cmd.title).toBe('Build Project');
    });

    it('should format shortcut', () => {
      const formatted = formatShortcut('ctrl+shift+p');
      expect(formatted).toContain('⌃');
      expect(formatted).toContain('⇧');
      expect(formatted).toContain('P');
    });
  });

  describe('Metrics', () => {
    it('should calculate success rate', async () => {
      commands.registerCommand({
        id: 'test.ok',
        title: 'OK',
        handler: async () => 'ok',
      });

      commands.registerCommand({
        id: 'test.fail',
        title: 'Fail',
        handler: async () => {
          throw new Error('fail');
        },
      });

      await commands.executeCommand('test.ok');
      await commands.executeCommand('test.fail');

      const rate = commands.calculateSuccessRate();
      expect(rate).toBe(50);
    });
  });
});
