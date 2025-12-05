/**
 * @file Stub command handler - FM-CLI-011
 * @module cli/utils/stub-handler
 *
 * Provides clear error messages for unimplemented commands
 * Prevents "unknown command" confusion
 */

/**
 * Error for stub/unimplemented commands
 * Provides next steps and timeline
 */
export class StubCommandError extends Error {
  constructor(command, alternativeCommand = null) {
    const message = formatStubError(command, alternativeCommand);
    super(message);
    this.name = 'StubCommandError';
    this.command = command;
    this.alternativeCommand = alternativeCommand;
  }
}

/**
 * Format error message for stub command
 */
export function formatStubError(command, alternativeCommand = null) {
  const lines = [];

  lines.push('');
  lines.push(`⚠️  Command not yet implemented: ${command}`);
  lines.push('');
  lines.push('This command is planned but not yet available.');
  lines.push('');

  if (alternativeCommand) {
    lines.push('📌 Alternative:');
    lines.push(`   You can use: ${alternativeCommand}`);
    lines.push('');
  }

  lines.push('📋 Roadmap:');
  lines.push('   Phase 1: ✅ Input validation & confirmation');
  lines.push('   Phase 2: ✅ Network resilience');
  lines.push('   Phase 3: ✅ Session safety');
  lines.push('   Phase 4: ⏳ Full feature implementation (Q1 2026)');
  lines.push('');

  lines.push('💬 Feedback:');
  lines.push('   Report missing features at: https://github.com/unrdf/unrdf/issues');
  lines.push('');

  return lines.join('\n');
}

/**
 * Check if command is stub and throw helpful error
 */
export function ensureImplemented(command, alternativeCommand = null) {
  throw new StubCommandError(command, alternativeCommand);
}

/**
 * Return stub response (for queries)
 */
export function stubResponse(command, hint = '') {
  return {
    error: `Command not implemented: ${command}`,
    status: 'not_implemented',
    hint: hint || `Use '${command}' when available in Phase 4`,
    roadmap: 'https://github.com/unrdf/unrdf/blob/main/ROADMAP.md'
  };
}

export default {
  StubCommandError,
  formatStubError,
  ensureImplemented,
  stubResponse
};
