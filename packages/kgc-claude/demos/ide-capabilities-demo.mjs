/**
 * IDE Capabilities Demonstration
 *
 * Shows how the three IDE capability modules work together to provide
 * editor integration, UI components, and command management.
 */

import {
  createIDEIntegration,
  formatPosition,
  formatRange,
} from '../src/capabilities/ide-integration.mjs';

import {
  createUIComponents,
  getSpinnerFrames,
  getBoxChars,
} from '../src/capabilities/ui-components.mjs';

import {
  createEditorCommands,
  fromSlashCommand,
  formatShortcut,
} from '../src/capabilities/editor-commands.mjs';

console.log('='.repeat(80));
console.log('IDE Capabilities Demonstration');
console.log('='.repeat(80));

// ============================================================================
// Demo 1: IDE Integration - Code Actions and Diagnostics
// ============================================================================

console.log('\n📝 Demo 1: IDE Integration');
console.log('-'.repeat(80));

const ide = createIDEIntegration();

// Add diagnostics
ide.addDiagnostic('example.js', {
  range: {
    start: { file: 'example.js', line: 10, column: 5 },
    end: { file: 'example.js', line: 10, column: 15 },
  },
  severity: 'error',
  message: 'Undefined variable "myVar"',
  code: 'E001',
});

ide.addDiagnostic('example.js', {
  range: {
    start: { file: 'example.js', line: 15, column: 0 },
    end: { file: 'example.js', line: 15, column: 10 },
  },
  severity: 'warning',
  message: 'Unused import statement',
  code: 'W002',
});

console.log('Diagnostics:');
const diagnostics = ide.getDiagnostics('example.js');
diagnostics.forEach(d => {
  const pos = formatRange(d.range);
  const icon = d.severity === 'error' ? '❌' : '⚠️';
  console.log(`  ${icon} ${pos}: ${d.message} [${d.code}]`);
});

// Register code actions
ide.registerCodeAction('example.js', {
  title: 'Declare variable "myVar"',
  kind: 'quickfix',
  edits: [{
    range: {
      start: { file: 'example.js', line: 9, column: 0 },
      end: { file: 'example.js', line: 9, column: 0 },
    },
    newText: 'let myVar;\n',
  }],
  isPreferred: true,
});

console.log('\nCode Actions:');
const actions = ide.getCodeActions('example.js');
actions.forEach(a => {
  const icon = a.isPreferred ? '⭐' : '💡';
  console.log(`  ${icon} ${a.title} [${a.kind}]`);
});

// Add inline suggestions
ide.addSuggestion('example.js', {
  position: { file: 'example.js', line: 20, column: 10 },
  text: 'myVariable',
  label: 'myVariable',
  detail: 'local variable',
  kind: 'variable',
});

console.log('\nInline Suggestions:');
const suggestions = ide.getSuggestions({ file: 'example.js', line: 20, column: 12 });
suggestions.forEach(s => {
  console.log(`  💡 ${s.label} - ${s.detail || s.kind}`);
});

// Apply an edit
const originalText = 'function hello() {\n  console.log("world");\n}';
const edits = [{
  range: {
    start: { file: 'example.js', line: 1, column: 15 },
    end: { file: 'example.js', line: 1, column: 22 },
  },
  newText: 'Hello, Claude!',
}];

const modifiedText = ide.applyEdits(originalText, edits);
console.log('\nText Edit Demo:');
console.log('Original:\n', originalText);
console.log('\nModified:\n', modifiedText);

// ============================================================================
// Demo 2: UI Components - Progress, Prompts, Visualizations
// ============================================================================

console.log('\n\n🎨 Demo 2: UI Components');
console.log('-'.repeat(80));

const ui = createUIComponents();

// Progress indicator
const progress = ui.createProgress('Processing files', 100, { unit: 'files' });
ui.updateProgress(progress.id, 25);
console.log(ui.formatProgress(progress.id));

ui.updateProgress(progress.id, 50);
console.log(ui.formatProgress(progress.id));

ui.updateProgress(progress.id, 100);
console.log(ui.formatProgress(progress.id));

// Interactive prompts
console.log('\nInteractive Prompts:');
const confirmPrompt = ui.createPrompt('Continue with operation?', 'confirm', {
  default: true,
});
console.log(ui.formatPrompt(confirmPrompt));

const selectPrompt = ui.createPrompt('Choose build target', 'select', {
  options: [
    { label: 'Development', value: 'dev', description: 'Fast build with source maps' },
    { label: 'Production', value: 'prod', description: 'Optimized build' },
    { label: 'Test', value: 'test', description: 'Build for testing' },
  ],
});
console.log(ui.formatPrompt(selectPrompt));

// Table visualization
console.log('\nTable Visualization:');
const tableViz = ui.createVisualization('table', [
  { file: 'index.js', lines: 150, coverage: '95%' },
  { file: 'utils.js', lines: 75, coverage: '100%' },
  { file: 'api.js', lines: 200, coverage: '87%' },
]);
console.log(ui.renderVisualization(tableViz.id));

// Tree visualization
console.log('\nTree Visualization:');
const treeViz = ui.createVisualization('tree', {
  label: 'src/',
  children: [
    { label: 'components/', children: [
      { label: 'Button.js' },
      { label: 'Input.js' },
    ]},
    { label: 'utils/', children: [
      { label: 'helpers.js' },
    ]},
    { label: 'index.js' },
  ],
});
console.log(ui.renderVisualization(treeViz.id));

// Spinner and box chars
console.log('\nUI Elements:');
const spinnerFrames = getSpinnerFrames();
console.log('Spinner frames:', spinnerFrames.slice(0, 5).join(' '));

const box = getBoxChars();
console.log(`Box drawing:\n${box.topLeft}${box.horizontal.repeat(10)}${box.topRight}`);
console.log(`${box.vertical}${' '.repeat(10)}${box.vertical}`);
console.log(`${box.bottomLeft}${box.horizontal.repeat(10)}${box.bottomRight}`);

// ============================================================================
// Demo 3: Editor Commands - Command Registration and Execution
// ============================================================================

console.log('\n\n⌨️  Demo 3: Editor Commands');
console.log('-'.repeat(80));

const commands = createEditorCommands();

// Register commands
commands.registerCommand({
  id: 'build.project',
  title: 'Build Project',
  category: 'Build',
  handler: async (args) => {
    console.log('  Building project with args:', args);
    return { success: true, files: 42 };
  },
});

commands.registerCommand({
  id: 'test.run',
  title: 'Run Tests',
  category: 'Test',
  handler: async () => {
    console.log('  Running tests...');
    return { passed: 54, failed: 0 };
  },
});

// Create from slash command
const deployCmd = fromSlashCommand('/deploy', async (target) => {
  console.log(`  Deploying to ${target}...`);
  return { deployed: true, target };
}, {
  title: 'Deploy Application',
  category: 'Deploy',
});

commands.registerCommand(deployCmd);

// Register shortcuts
commands.registerShortcut('ctrl+b', 'build.project');
commands.registerShortcut('ctrl+shift+t', 'test.run');
commands.registerShortcut('ctrl+d', 'cmd.deploy');

console.log('Registered Commands:');
const allCommands = commands.getCommandPaletteItems();
allCommands.forEach(cmd => {
  const shortcut = cmd.shortcut ? ` (${formatShortcut(cmd.shortcut)})` : '';
  console.log(`  [${cmd.category}] ${cmd.title}${shortcut}`);
});

// Execute commands
console.log('\nExecuting Commands:');
const buildResult = await commands.executeCommand('build.project', { target: 'production' });
console.log(`  Build result:`, buildResult.result);

const testResult = await commands.executeShortcut('ctrl+shift+t');
console.log(`  Test result:`, testResult.result);

const deployResult = await commands.executeCommand('cmd.deploy', 'staging');
console.log(`  Deploy result:`, deployResult.result);

// Execution stats
console.log('\nExecution Statistics:');
console.log(`  Total executions: ${commands.executionHistory.length}`);
console.log(`  Success rate: ${commands.calculateSuccessRate()}%`);

// Search commands
console.log('\nSearch Results for "test":');
const searchResults = commands.searchCommands('test');
searchResults.forEach(cmd => {
  console.log(`  → ${cmd.title} [${cmd.category}]`);
});

// ============================================================================
// Summary and Receipts
// ============================================================================

console.log('\n\n📊 Summary');
console.log('-'.repeat(80));

const ideReceipt = await ide.createReceipt();
const uiReceipt = await ui.createReceipt();
const cmdReceipt = await commands.createReceipt();

console.log('IDE Integration:');
console.log(`  Code Actions: ${ideReceipt.codeActions}`);
console.log(`  Diagnostics: ${ideReceipt.diagnostics}`);
console.log(`  Suggestions: ${ideReceipt.suggestions}`);
console.log(`  Hash: ${ideReceipt.hash.substring(0, 16)}...`);

console.log('\nUI Components:');
console.log(`  Progress Indicators: ${uiReceipt.progress}`);
console.log(`  Prompts: ${uiReceipt.prompts}`);
console.log(`  Visualizations: ${uiReceipt.visualizations}`);
console.log(`  Hash: ${uiReceipt.hash.substring(0, 16)}...`);

console.log('\nEditor Commands:');
console.log(`  Commands: ${cmdReceipt.commands}`);
console.log(`  Shortcuts: ${cmdReceipt.shortcuts}`);
console.log(`  Executions: ${cmdReceipt.executions}`);
console.log(`  Success Rate: ${cmdReceipt.successRate}%`);
console.log(`  Hash: ${cmdReceipt.hash.substring(0, 16)}...`);

console.log('\n' + '='.repeat(80));
console.log('✅ Demo completed successfully!');
console.log('='.repeat(80));
