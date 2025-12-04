/**
 * @fileoverview Fish Shell Completions Generator
 *
 * @description
 * Generates fish shell completion scripts for the playground CLI.
 * Uses fish's declarative completion system with descriptions
 * and condition-based completions.
 *
 * @module cli/completions/fish
 * @version 1.0.0
 * @license MIT
 */

/**
 * CLI command structure for completions
 * @type {Object}
 */
const CLI_STRUCTURE = {
  papers: {
    name: 'papers',
    description: 'Manage research papers',
    subcommands: {
      generate: {
        description: 'Generate paper from template',
        args: {
          '--family': {
            short: '-F',
            description: 'Paper family/structure type',
            choices: ['imrad', 'dsr', 'argument', 'contribution'],
          },
          '--title': { short: '-t', description: 'Paper title' },
          '--author': { short: '-a', description: 'Author name' },
          '--affiliation': { description: 'Author affiliation' },
          '--abstract': { description: 'Paper abstract' },
          '--output': { short: '-o', description: 'Output file path', type: 'file' },
          '--format': {
            short: '-f',
            description: 'Output format',
            choices: ['json', 'latex', 'yaml'],
          },
        },
      },
      list: {
        description: 'List available paper families',
        args: {
          '--verbose': { short: '-v', description: 'Show detailed information' },
          '--format': {
            short: '-f',
            description: 'Output format',
            choices: ['json', 'yaml', 'table'],
          },
        },
      },
      validate: {
        description: 'Validate paper structure',
        args: {
          '--strict': { description: 'Enable strict validation' },
          '--format': { short: '-f', description: 'Output format', choices: ['json', 'table'] },
        },
      },
    },
  },
  thesis: {
    name: 'thesis',
    description: 'Manage thesis documents',
    subcommands: {
      generate: {
        description: 'Generate thesis from template',
        args: {
          '--type': {
            short: '-T',
            description: 'Thesis type',
            choices: ['masters', 'phd', 'doctoral'],
          },
          '--title': { short: '-t', description: 'Thesis title' },
          '--author': { short: '-a', description: 'Author name' },
          '--advisor': { description: 'Advisor name' },
          '--institution': { description: 'Institution name' },
          '--department': { description: 'Department name' },
          '--output': { short: '-o', description: 'Output file path', type: 'file' },
          '--format': {
            short: '-f',
            description: 'Output format',
            choices: ['json', 'latex', 'yaml'],
          },
        },
      },
      list: {
        description: 'List available thesis types',
        args: {
          '--verbose': { short: '-v', description: 'Show detailed information' },
          '--format': {
            short: '-f',
            description: 'Output format',
            choices: ['json', 'yaml', 'table'],
          },
        },
      },
      schedule: {
        description: 'Manage thesis schedule',
        args: {
          '--milestone': { description: 'Milestone name' },
          '--date': { description: 'Target date' },
        },
      },
    },
  },
  config: {
    name: 'config',
    description: 'Manage CLI configuration',
    subcommands: {
      list: { description: 'List all configuration values', args: {} },
      get: { description: 'Get a configuration value', args: {} },
      set: { description: 'Set a configuration value', args: {} },
      reset: {
        description: 'Reset configuration to defaults',
        args: { '--all': { description: 'Reset all values' } },
      },
    },
  },
  meta: {
    name: 'meta',
    description: 'Metadata and introspection',
    subcommands: {
      sparql: {
        description: 'Execute SPARQL query',
        args: {
          '--format': {
            short: '-f',
            description: 'Output format',
            choices: ['json', 'table', 'csv'],
          },
          '--file': { description: 'Query file', type: 'file' },
        },
      },
      schema: {
        description: 'Show schema information',
        args: {
          '--format': {
            short: '-f',
            description: 'Output format',
            choices: ['json', 'yaml', 'turtle'],
          },
        },
      },
      version: { description: 'Show version information', args: {} },
      completions: {
        description: 'Generate shell completions',
        args: {
          '--shell': {
            description: 'Target shell',
            choices: ['bash', 'zsh', 'fish', 'powershell'],
          },
          '--output': { short: '-o', description: 'Output file', type: 'file' },
        },
      },
    },
  },
};

/**
 * Escape string for fish shell
 * @param {string} str - String to escape
 * @returns {string} Escaped string
 */
function escapeForFish(str) {
  return str.replace(/'/g, "\\'").replace(/"/g, '\\"');
}

/**
 * Generate fish completion script
 * @param {Object} [options] - Generation options
 * @param {string} [options.cliName] - CLI command name
 * @param {Object} [options.commands] - Command structure override
 * @returns {string} Fish completion script
 */
export function generateFishCompletions(options = {}) {
  const { cliName = 'playground', commands = CLI_STRUCTURE } = options;

  const commandNames = Object.keys(commands);

  let script = `# Fish completion for ${cliName} CLI
# Generated by papers-thesis-cli completions generator
#
# Installation:
#   1. Copy to ~/.config/fish/completions/${cliName}.fish
#   2. Or copy to /usr/local/share/fish/vendor_completions.d/${cliName}.fish

# Disable file completions by default
complete -c ${cliName} -f

# Helper function to check if we're in a specific command context
function __fish_${cliName}_using_command
    set -l cmd (commandline -opc)
    set -l expected_cmd $argv[1]

    if test (count $cmd) -gt 1
        if test $cmd[2] = $expected_cmd
            return 0
        end
    end
    return 1
end

# Helper function to check if we're in a subcommand context
function __fish_${cliName}_using_subcommand
    set -l cmd (commandline -opc)
    set -l expected_cmd $argv[1]
    set -l expected_subcmd $argv[2]

    if test (count $cmd) -gt 2
        if test $cmd[2] = $expected_cmd -a $cmd[3] = $expected_subcmd
            return 0
        end
    end
    return 1
end

# Helper function to check if no command is given yet
function __fish_${cliName}_needs_command
    set -l cmd (commandline -opc)
    if test (count $cmd) -eq 1
        return 0
    end
    return 1
end

# Helper function to check if command needs subcommand
function __fish_${cliName}_needs_subcommand
    set -l cmd (commandline -opc)
    set -l expected_cmd $argv[1]

    if test (count $cmd) -eq 2
        if test $cmd[2] = $expected_cmd
            return 0
        end
    end
    return 1
end

# =====================================
# Global options (available everywhere)
# =====================================

complete -c ${cliName} -s h -l help -d "Show help information"
complete -c ${cliName} -s v -l verbose -d "Enable verbose output"
complete -c ${cliName} -s q -l quiet -d "Suppress non-essential output"
complete -c ${cliName} -s f -l format -d "Output format" -xa "json json-pretty yaml table latex csv"
complete -c ${cliName} -s o -l output -d "Output file path" -r
complete -c ${cliName} -s c -l config -d "Configuration file" -r
complete -c ${cliName} -l version -d "Show version information"

# =====================================
# Main commands
# =====================================

`;

  // Generate main command completions
  for (const [cmdName, cmdDef] of Object.entries(commands)) {
    script += `complete -c ${cliName} -n "__fish_${cliName}_needs_command" -a "${cmdName}" -d "${escapeForFish(cmdDef.description)}"\n`;
  }

  script += `
# =====================================
# Subcommands and arguments
# =====================================

`;

  // Generate subcommand completions for each command
  for (const [cmdName, cmdDef] of Object.entries(commands)) {
    script += `# ${cmdName} subcommands\n`;

    for (const [subcmdName, subcmdDef] of Object.entries(cmdDef.subcommands || {})) {
      script += `complete -c ${cliName} -n "__fish_${cliName}_needs_subcommand ${cmdName}" -a "${subcmdName}" -d "${escapeForFish(subcmdDef.description)}"\n`;
    }

    script += '\n';

    // Generate argument completions for each subcommand
    for (const [subcmdName, subcmdDef] of Object.entries(cmdDef.subcommands || {})) {
      if (Object.keys(subcmdDef.args || {}).length === 0) continue;

      script += `# ${cmdName} ${subcmdName} arguments\n`;

      for (const [argName, argDef] of Object.entries(subcmdDef.args || {})) {
        const longOpt = argName.replace(/^--/, '');
        const shortOpt = argDef.short?.replace(/^-/, '');
        const desc = escapeForFish(argDef.description || 'Option');

        let completionLine = `complete -c ${cliName} -n "__fish_${cliName}_using_subcommand ${cmdName} ${subcmdName}"`;

        if (shortOpt) {
          completionLine += ` -s ${shortOpt}`;
        }
        completionLine += ` -l ${longOpt}`;
        completionLine += ` -d "${desc}"`;

        // Add choices or file completion
        if (argDef.choices) {
          completionLine += ` -xa "${argDef.choices.join(' ')}"`;
        } else if (argDef.type === 'file') {
          completionLine += ' -r';
        }

        script += completionLine + '\n';
      }

      script += '\n';
    }
  }

  // Add dynamic completion functions
  script += `# =====================================
# Dynamic completions (customizable)
# =====================================

# Function to get available paper families dynamically
function __fish_${cliName}_paper_families
    echo "imrad"
    echo "dsr"
    echo "argument"
    echo "contribution"
end

# Function to get available thesis types dynamically
function __fish_${cliName}_thesis_types
    echo "masters"
    echo "phd"
    echo "doctoral"
end

# Function to list available templates
function __fish_${cliName}_templates
    # Can be customized to load from filesystem or knowledge graph
    __fish_${cliName}_paper_families
    __fish_${cliName}_thesis_types
end
`;

  return script;
}

/**
 * Get installation instructions for fish completions
 * @param {string} [outputPath] - Path where completions are saved
 * @returns {string} Installation instructions
 */
export function getFishInstallInstructions(outputPath = 'playground.fish') {
  return `
Fish Completion Installation
============================

Option 1: User installation (recommended)
  mkdir -p ~/.config/fish/completions
  cp ${outputPath} ~/.config/fish/completions/playground.fish

Option 2: System-wide installation (requires sudo)
  sudo cp ${outputPath} /usr/local/share/fish/vendor_completions.d/

Option 3: Using fisher (if installed)
  # Create a fish plugin with completions

After installation, completions should be available immediately.
If not, restart fish or run:
  source ~/.config/fish/completions/playground.fish
`.trim();
}

export default generateFishCompletions;
