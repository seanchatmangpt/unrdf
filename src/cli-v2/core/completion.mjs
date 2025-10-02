/**
 * @file Shell Completion Generator
 * @module cli-v2/core/completion
 *
 * @description
 * Generates shell completion scripts for bash, zsh, and fish.
 * Reads completion files from the completions/ directory.
 */

import { readFileSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

/**
 * Generate shell completion script
 * @param {string} shell - Shell type (bash, zsh, fish)
 * @returns {string} Completion script content
 */
export function generateCompletion(shell) {
  const validShells = ['bash', 'zsh', 'fish'];

  if (!validShells.includes(shell)) {
    throw new Error(`Unsupported shell: ${shell}. Supported: ${validShells.join(', ')}`);
  }

  try {
    // Read completion file from completions directory
    const completionPath = join(__dirname, '..', '..', '..', 'completions', `${shell}-completion.${getExtension(shell)}`);
    return readFileSync(completionPath, 'utf-8');
  } catch (error) {
    throw new Error(`Failed to load ${shell} completion: ${error.message}`);
  }
}

/**
 * Get file extension for shell
 * @param {string} shell - Shell type
 * @returns {string} File extension
 */
function getExtension(shell) {
  const extensions = {
    bash: 'sh',
    zsh: 'zsh',
    fish: 'fish'
  };
  return extensions[shell];
}

/**
 * Get installation instructions for shell
 * @param {string} shell - Shell type
 * @returns {string} Installation instructions
 */
export function getInstallInstructions(shell) {
  const instructions = {
    bash: `# Add to ~/.bashrc:
source <(unrdf completion bash)

# Or save to file:
unrdf completion bash > ~/.bash_completion.d/unrdf
source ~/.bash_completion.d/unrdf`,

    zsh: `# Add to ~/.zshrc:
source <(unrdf completion zsh)

# Or save to completion directory:
unrdf completion zsh > ~/.zsh/completions/_unrdf
# Then add to ~/.zshrc:
fpath=(~/.zsh/completions $fpath)
autoload -Uz compinit && compinit`,

    fish: `# Install to fish completions directory:
unrdf completion fish > ~/.config/fish/completions/unrdf.fish

# Restart fish or run:
fish_update_completions`
  };

  return instructions[shell] || '';
}
