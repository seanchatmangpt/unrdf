/**
 * @file Shell Completion Generator
 * @module cli-v2/core/completion
 *
 * @description
 * Generates shell completion scripts for bash, zsh, and fish.
 * Reads completion files from the completions/ directory.
 */

// Embedded completion templates to avoid filesystem dependencies
const COMPLETION_TEMPLATES = {
  bash: `# bash completion for unrdf
_unrdf()
{
  local cur prev words cword
  _init_completion || return
  COMPREPLY=( $( compgen -W "graph hook policy sidecar store context plugin repl init completion --fast --help --version" -- "$cur" ) )
}
complete -F _unrdf unrdf
`,
  zsh: `#compdef unrdf
_unrdf() {
  local -a cmds
  cmds=(graph hook policy sidecar store context plugin repl init completion)
  _describe 'command' cmds
}
compdef _unrdf unrdf
`,
  fish: `complete -c unrdf -f -a "graph hook policy sidecar store context plugin repl init completion"
complete -c unrdf -l fast -d "Fast mode"
complete -c unrdf -l help -d "Show help"
complete -c unrdf -l version -d "Show version"`
};

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

  return COMPLETION_TEMPLATES[shell];
}

/**
 * Get file extension for shell
 * @param {string} shell - Shell type
 * @returns {string} File extension
 */
function getExtension(shell) {
  const extensions = { bash: 'sh', zsh: 'zsh', fish: 'fish' };
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
