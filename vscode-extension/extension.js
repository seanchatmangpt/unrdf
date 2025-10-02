/**
 * UNRDF VS Code Extension
 * Provides syntax highlighting, IntelliSense, and command integration
 */

const vscode = require('vscode');
const { exec } = require('child_process');
const { promisify } = require('util');

const execAsync = promisify(exec);

/**
 * Extension activation
 */
function activate(context) {
  console.log('UNRDF extension is now active');

  // Register commands
  const commands = [
    {
      id: 'unrdf.validateHook',
      handler: validateHook
    },
    {
      id: 'unrdf.evaluateHook',
      handler: evaluateHook
    },
    {
      id: 'unrdf.applyPolicy',
      handler: applyPolicy
    },
    {
      id: 'unrdf.querySPARQL',
      handler: querySPARQL
    }
  ];

  commands.forEach(({ id, handler }) => {
    const disposable = vscode.commands.registerCommand(id, handler);
    context.subscriptions.push(disposable);
  });

  // Register document formatting provider
  context.subscriptions.push(
    vscode.languages.registerDocumentFormattingEditProvider('hook', {
      provideDocumentFormattingEdits(document) {
        return formatDocument(document);
      }
    })
  );

  // Register validation on save
  context.subscriptions.push(
    vscode.workspace.onDidSaveTextDocument((document) => {
      const config = vscode.workspace.getConfiguration('unrdf');
      if (config.get('validation.onSave') && document.languageId === 'hook') {
        validateHook();
      }
    })
  );

  // Register hover provider for hook functions
  context.subscriptions.push(
    vscode.languages.registerHoverProvider('hook', {
      provideHover(document, position) {
        const range = document.getWordRangeAtPosition(position);
        const word = document.getText(range);

        const documentation = getHookDocumentation(word);
        if (documentation) {
          return new vscode.Hover(documentation);
        }
      }
    })
  );
}

/**
 * Validate current hook file
 */
async function validateHook() {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('No active editor');
    return;
  }

  const document = editor.document;
  const filePath = document.uri.fsPath;

  try {
    vscode.window.showInformationMessage('Validating hook...');

    const config = vscode.workspace.getConfiguration('unrdf');
    const cliPath = config.get('cli.path', 'unrdf');

    const { stdout, stderr } = await execAsync(`${cliPath} hook validate ${filePath}`);

    if (stderr) {
      vscode.window.showErrorMessage(`Validation failed: ${stderr}`);
    } else {
      vscode.window.showInformationMessage('Hook validation passed!');
      outputChannel.appendLine(stdout);
    }
  } catch (error) {
    vscode.window.showErrorMessage(`Validation error: ${error.message}`);
  }
}

/**
 * Evaluate current hook file
 */
async function evaluateHook() {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('No active editor');
    return;
  }

  const document = editor.document;
  const filePath = document.uri.fsPath;

  try {
    vscode.window.showInformationMessage('Evaluating hook...');

    const config = vscode.workspace.getConfiguration('unrdf');
    const cliPath = config.get('cli.path', 'unrdf');

    const { stdout } = await execAsync(`${cliPath} hook eval ${filePath}`);

    // Show results in output panel
    const outputChannel = getOutputChannel();
    outputChannel.show();
    outputChannel.appendLine('=== Hook Evaluation Results ===');
    outputChannel.appendLine(stdout);
  } catch (error) {
    vscode.window.showErrorMessage(`Evaluation error: ${error.message}`);
  }
}

/**
 * Apply current policy pack
 */
async function applyPolicy() {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('No active editor');
    return;
  }

  const document = editor.document;
  const filePath = document.uri.fsPath;

  try {
    vscode.window.showInformationMessage('Applying policy pack...');

    const config = vscode.workspace.getConfiguration('unrdf');
    const cliPath = config.get('cli.path', 'unrdf');

    const { stdout } = await execAsync(`${cliPath} policy apply ${filePath}`);

    vscode.window.showInformationMessage('Policy pack applied successfully!');

    const outputChannel = getOutputChannel();
    outputChannel.appendLine('=== Policy Application Results ===');
    outputChannel.appendLine(stdout);
  } catch (error) {
    vscode.window.showErrorMessage(`Policy application error: ${error.message}`);
  }
}

/**
 * Run SPARQL query
 */
async function querySPARQL() {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('No active editor');
    return;
  }

  const selection = editor.selection;
  const query = editor.document.getText(selection.isEmpty ? undefined : selection);

  if (!query.trim()) {
    vscode.window.showErrorMessage('No query selected');
    return;
  }

  try {
    const config = vscode.workspace.getConfiguration('unrdf');
    const cliPath = config.get('cli.path', 'unrdf');
    const endpoint = config.get('sparql.endpoint');

    const { stdout } = await execAsync(`${cliPath} store query --endpoint="${endpoint}" "${query}"`);

    const outputChannel = getOutputChannel();
    outputChannel.show();
    outputChannel.appendLine('=== SPARQL Query Results ===');
    outputChannel.appendLine(stdout);
  } catch (error) {
    vscode.window.showErrorMessage(`Query error: ${error.message}`);
  }
}

/**
 * Format hook document
 */
function formatDocument(document) {
  // Basic formatting - can be enhanced
  const edits = [];
  // Add formatting logic here
  return edits;
}

/**
 * Get documentation for hook functions
 */
function getHookDocumentation(word) {
  const docs = {
    'defineHook': '**defineHook(options)**\n\nDefine a new knowledge hook with triggers and execution logic.\n\n```javascript\ndefineHook({\n  name: string,\n  description: string,\n  triggers: {...},\n  execute: async (context) => {...}\n})\n```',
    'chainHooks': '**chainHooks(hooks, options)**\n\nChain multiple hooks together for sequential or parallel execution.\n\n```javascript\nchainHooks([hook1, hook2], {\n  stopOnError: boolean,\n  parallel: boolean\n})\n```',
    'definePolicy': '**definePolicyPack(options)**\n\nDefine a policy pack with validation and remediation rules.\n\n```javascript\ndefinePolicyPack({\n  name: string,\n  rules: [{\n    validate: async (context) => {...},\n    remediate: async (context) => {...}\n  }]\n})\n```'
  };

  return docs[word] ? new vscode.MarkdownString(docs[word]) : null;
}

/**
 * Get or create output channel
 */
let outputChannel;
function getOutputChannel() {
  if (!outputChannel) {
    outputChannel = vscode.window.createOutputChannel('UNRDF');
  }
  return outputChannel;
}

/**
 * Extension deactivation
 */
function deactivate() {
  if (outputChannel) {
    outputChannel.dispose();
  }
}

module.exports = {
  activate,
  deactivate
};
