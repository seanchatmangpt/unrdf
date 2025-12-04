/**
 * @fileoverview Shell Completions Generator Index
 *
 * @description
 * Exports all shell completion generators and provides
 * a unified API for generating completions for any shell.
 *
 * @module cli/completions
 * @version 1.0.0
 * @license MIT
 */

import { generateBashCompletions, getBashInstallInstructions } from './bash.mjs';
import { generateZshCompletions, getZshInstallInstructions } from './zsh.mjs';
import { generateFishCompletions, getFishInstallInstructions } from './fish.mjs';
import { generatePowerShellCompletions, getPowerShellInstallInstructions } from './powershell.mjs';

/**
 * Supported shells
 * @type {Array<string>}
 */
export const SUPPORTED_SHELLS = ['bash', 'zsh', 'fish', 'powershell'];

/**
 * Shell generators map
 * @type {Object<string, Function>}
 */
const generators = {
  bash: generateBashCompletions,
  zsh: generateZshCompletions,
  fish: generateFishCompletions,
  powershell: generatePowerShellCompletions,
};

/**
 * Installation instructions map
 * @type {Object<string, Function>}
 */
const installInstructions = {
  bash: getBashInstallInstructions,
  zsh: getZshInstallInstructions,
  fish: getFishInstallInstructions,
  powershell: getPowerShellInstallInstructions,
};

/**
 * Default output filenames
 * @type {Object<string, string>}
 */
export const DEFAULT_FILENAMES = {
  bash: 'playground-completion.bash',
  zsh: '_playground',
  fish: 'playground.fish',
  powershell: 'playground-completion.ps1',
};

/**
 * Default installation paths
 * @type {Object<string, string>}
 */
export const DEFAULT_PATHS = {
  bash: '/usr/local/etc/bash_completion.d/',
  zsh: '/usr/local/share/zsh/site-functions/',
  fish: '/usr/local/share/fish/vendor_completions.d/',
  powershell: '$PROFILE directory',
};

/**
 * Generate completions for a specific shell
 * @param {string} shell - Target shell (bash, zsh, fish, powershell)
 * @param {Object} [options] - Generation options
 * @returns {string} Completion script
 * @throws {Error} If shell is not supported
 */
export function generateCompletions(shell, options = {}) {
  const normalizedShell = shell.toLowerCase();

  if (!SUPPORTED_SHELLS.includes(normalizedShell)) {
    throw new Error(
      `Unsupported shell: ${shell}. Supported shells: ${SUPPORTED_SHELLS.join(', ')}`
    );
  }

  const generator = generators[normalizedShell];
  return generator(options);
}

/**
 * Get installation instructions for a shell
 * @param {string} shell - Target shell
 * @param {string} [outputPath] - Output path for the completion file
 * @returns {string} Installation instructions
 */
export function getInstallInstructions(shell, outputPath) {
  const normalizedShell = shell.toLowerCase();

  if (!SUPPORTED_SHELLS.includes(normalizedShell)) {
    throw new Error(`Unsupported shell: ${shell}`);
  }

  const getInstructions = installInstructions[normalizedShell];
  const defaultPath = outputPath || DEFAULT_FILENAMES[normalizedShell];
  return getInstructions(defaultPath);
}

/**
 * Generate completions for all supported shells
 * @param {Object} [options] - Generation options
 * @returns {Object<string, string>} Map of shell to completion script
 */
export function generateAllCompletions(options = {}) {
  const result = {};

  for (const shell of SUPPORTED_SHELLS) {
    result[shell] = generateCompletions(shell, options);
  }

  return result;
}

/**
 * Detect current shell
 * @returns {string|null} Detected shell name or null
 */
export function detectShell() {
  const shell = process.env.SHELL || '';
  const psModulePath = process.env.PSModulePath;

  if (psModulePath) {
    return 'powershell';
  }

  if (shell.includes('bash')) {
    return 'bash';
  }

  if (shell.includes('zsh')) {
    return 'zsh';
  }

  if (shell.includes('fish')) {
    return 'fish';
  }

  // Check COMSPEC for Windows cmd (not supported)
  if (process.env.COMSPEC) {
    return null;
  }

  return null;
}

/**
 * Get completion filename for a shell
 * @param {string} shell - Target shell
 * @param {string} [cliName] - CLI name to use
 * @returns {string} Completion filename
 */
export function getCompletionFilename(shell, cliName = 'playground') {
  const normalizedShell = shell.toLowerCase();

  switch (normalizedShell) {
    case 'bash':
      return `${cliName}-completion.bash`;
    case 'zsh':
      return `_${cliName}`;
    case 'fish':
      return `${cliName}.fish`;
    case 'powershell':
      return `${cliName}-completion.ps1`;
    default:
      return `${cliName}-completion.${shell}`;
  }
}

// Re-export individual generators
export { generateBashCompletions, getBashInstallInstructions } from './bash.mjs';
export { generateZshCompletions, getZshInstallInstructions } from './zsh.mjs';
export { generateFishCompletions, getFishInstallInstructions } from './fish.mjs';
export { generatePowerShellCompletions, getPowerShellInstallInstructions } from './powershell.mjs';
