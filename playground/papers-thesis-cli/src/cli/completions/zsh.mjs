/**
 * @fileoverview Zsh Completions Generator
 *
 * @description
 * Generates zsh completion scripts for the playground CLI.
 * Uses zsh's powerful completion system with descriptions,
 * argument types, and contextual completions.
 *
 * @module cli/completions/zsh
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
            description: 'Paper family/structure type',
            choices: ['imrad', 'dsr', 'argument', 'contribution']
          },
          '-t': { alias: '--title' },
          '--title': { description: 'Paper title', type: 'string' },
          '-a': { alias: '--author' },
          '--author': { description: 'Author name', type: 'string' },
          '--affiliation': { description: 'Author affiliation', type: 'string' },
          '--abstract': { description: 'Paper abstract', type: 'string' },
          '-o': { alias: '--output' },
          '--output': { description: 'Output file path', type: 'file' },
          '-f': { alias: '--format' },
          '--format': { description: 'Output format', choices: ['json', 'latex', 'yaml'] }
        }
      },
      list: {
        description: 'List available paper families',
        args: {
          '-v': { alias: '--verbose' },
          '--verbose': { description: 'Show detailed information', type: 'boolean' },
          '-f': { alias: '--format' },
          '--format': { description: 'Output format', choices: ['json', 'yaml', 'table'] }
        }
      },
      validate: {
        description: 'Validate paper structure',
        args: {
          'path': { description: 'Path to paper file', type: 'file', positional: true },
          '--strict': { description: 'Enable strict validation', type: 'boolean' },
          '-f': { alias: '--format' },
          '--format': { description: 'Output format', choices: ['json', 'table'] }
        }
      }
    }
  },
  thesis: {
    name: 'thesis',
    description: 'Manage thesis documents',
    subcommands: {
      generate: {
        description: 'Generate thesis from template',
        args: {
          '--type': {
            description: 'Thesis type',
            choices: ['masters', 'phd', 'doctoral']
          },
          '-t': { alias: '--title' },
          '--title': { description: 'Thesis title', type: 'string' },
          '-a': { alias: '--author' },
          '--author': { description: 'Author name', type: 'string' },
          '--advisor': { description: 'Advisor name', type: 'string' },
          '--institution': { description: 'Institution name', type: 'string' },
          '--department': { description: 'Department name', type: 'string' },
          '-o': { alias: '--output' },
          '--output': { description: 'Output file path', type: 'file' },
          '-f': { alias: '--format' },
          '--format': { description: 'Output format', choices: ['json', 'latex', 'yaml'] }
        }
      },
      list: {
        description: 'List available thesis types',
        args: {
          '-v': { alias: '--verbose' },
          '--verbose': { description: 'Show detailed information', type: 'boolean' },
          '-f': { alias: '--format' },
          '--format': { description: 'Output format', choices: ['json', 'yaml', 'table'] }
        }
      },
      schedule: {
        description: 'Manage thesis schedule',
        args: {
          '--milestone': { description: 'Milestone name', type: 'string' },
          '--date': { description: 'Target date', type: 'string' }
        }
      }
    }
  },
  config: {
    name: 'config',
    description: 'Manage CLI configuration',
    subcommands: {
      list: {
        description: 'List all configuration values',
        args: {}
      },
      get: {
        description: 'Get a configuration value',
        args: {
          'key': { description: 'Configuration key', type: 'string', positional: true }
        }
      },
      set: {
        description: 'Set a configuration value',
        args: {
          'key': { description: 'Configuration key', type: 'string', positional: true },
          'value': { description: 'Configuration value', type: 'string', positional: true }
        }
      },
      reset: {
        description: 'Reset configuration to defaults',
        args: {
          '--all': { description: 'Reset all values', type: 'boolean' }
        }
      }
    }
  },
  meta: {
    name: 'meta',
    description: 'Metadata and introspection',
    subcommands: {
      sparql: {
        description: 'Execute SPARQL query',
        args: {
          'query': { description: 'SPARQL query string', type: 'string', positional: true },
          '-f': { alias: '--format' },
          '--format': { description: 'Output format', choices: ['json', 'table', 'csv'] },
          '--file': { description: 'Query file', type: 'file' }
        }
      },
      schema: {
        description: 'Show schema information',
        args: {
          '-f': { alias: '--format' },
          '--format': { description: 'Output format', choices: ['json', 'yaml', 'turtle'] }
        }
      },
      version: {
        description: 'Show version information',
        args: {}
      },
      completions: {
        description: 'Generate shell completions',
        args: {
          '--shell': { description: 'Target shell', choices: ['bash', 'zsh', 'fish', 'powershell'] },
          '-o': { alias: '--output' },
          '--output': { description: 'Output file', type: 'file' }
        }
      }
    }
  }
};

/**
 * Generate argument completion string for zsh
 * @param {string} name - Argument name
 * @param {Object} def - Argument definition
 * @returns {string} Zsh argument spec
 */
function formatZshArg(name, def) {
  // Handle aliases
  if (def.alias) {
    return null; // Skip aliases, they're handled with the main option
  }

  const desc = def.description || 'Option';
  const escapedDesc = desc.replace(/'/g, "''").replace(/:/g, '\\:');

  // Handle choices
  if (def.choices) {
    const choicesStr = def.choices.join(' ');
    if (name.startsWith('-')) {
      // Find short alias
      const shortOpt = Object.entries(CLI_STRUCTURE)
        .flatMap(([_, cmd]) => Object.entries(cmd.subcommands || {}))
        .flatMap(([_, sub]) => Object.entries(sub.args || {}))
        .find(([n, d]) => d.alias === name);

      if (shortOpt) {
        return `'(${shortOpt[0]} ${name})'{${shortOpt[0]},${name}}'[${escapedDesc}]:${name.replace(/-/g, '')}:(${choicesStr})'`;
      }
      return `'${name}[${escapedDesc}]:${name.replace(/-/g, '')}:(${choicesStr})'`;
    }
    return `':${name}:(${choicesStr})'`;
  }

  // Handle file type
  if (def.type === 'file') {
    if (name.startsWith('-')) {
      return `'${name}[${escapedDesc}]:file:_files'`;
    }
    return `':${name}:_files'`;
  }

  // Handle boolean
  if (def.type === 'boolean') {
    return `'${name}[${escapedDesc}]'`;
  }

  // Handle string
  if (name.startsWith('-')) {
    return `'${name}[${escapedDesc}]:${name.replace(/-/g, '')}:'`;
  }

  // Positional argument
  return `':${name}:'`;
}

/**
 * Generate zsh completion script
 * @param {Object} [options] - Generation options
 * @param {string} [options.cliName] - CLI command name
 * @param {Object} [options.commands] - Command structure override
 * @returns {string} Zsh completion script
 */
export function generateZshCompletions(options = {}) {
  const {
    cliName = 'playground',
    commands = CLI_STRUCTURE
  } = options;

  const commandDescriptions = Object.entries(commands)
    .map(([name, def]) => `'${name}:${def.description}'`)
    .join('\n        ');

  let script = `#compdef ${cliName}
# Zsh completion for ${cliName} CLI
# Generated by papers-thesis-cli completions generator
#
# Installation:
#   1. Copy to /usr/local/share/zsh/site-functions/_${cliName}
#   2. Or add to ~/.zsh/completions/_${cliName}
#   3. Ensure fpath includes the directory: fpath=(~/.zsh/completions $fpath)
#   4. Run: autoload -Uz compinit && compinit

_${cliName}() {
    local context state state_descr line
    typeset -A opt_args

    local -a commands
    commands=(
        ${commandDescriptions}
    )

    _arguments -C \\
        '(-h --help)'{-h,--help}'[Show help information]' \\
        '(-v --verbose)'{-v,--verbose}'[Enable verbose output]' \\
        '(-q --quiet)'{-q,--quiet}'[Suppress non-essential output]' \\
        '(-f --format)'{-f,--format}'[Output format]:format:(json json-pretty yaml table latex csv)' \\
        '(-o --output)'{-o,--output}'[Output file path]:file:_files' \\
        '(-c --config)'{-c,--config}'[Configuration file]:file:_files -g "*.{json,yaml,yml,mjs,js}"' \\
        '--version[Show version information]' \\
        '1:command:->command' \\
        '*::arg:->args'

    case "$state" in
        command)
            _describe -t commands '${cliName} command' commands
            ;;
        args)
            case $words[1] in
`;

  // Generate case for each command
  for (const [cmdName, cmdDef] of Object.entries(commands)) {
    script += `                ${cmdName})
                    _${cliName}_${cmdName}
                    ;;
`;
  }

  script += `            esac
            ;;
    esac
}

`;

  // Generate completion functions for each command
  for (const [cmdName, cmdDef] of Object.entries(commands)) {
    const subcommandDescriptions = Object.entries(cmdDef.subcommands || {})
      .map(([name, def]) => `'${name}:${def.description}'`)
      .join('\n            ');

    script += `# Completions for ${cmdName} command
_${cliName}_${cmdName}() {
    local -a subcommands
    subcommands=(
            ${subcommandDescriptions}
    )

    _arguments -C \\
        '1:subcommand:->subcommand' \\
        '*::arg:->args'

    case "$state" in
        subcommand)
            _describe -t subcommands '${cmdName} subcommand' subcommands
            ;;
        args)
            case $words[1] in
`;

    // Generate case for each subcommand
    for (const [subcmdName, subcmdDef] of Object.entries(cmdDef.subcommands || {})) {
      const args = Object.entries(subcmdDef.args || {})
        .map(([name, def]) => formatZshArg(name, def))
        .filter(Boolean)
        .join(' \\\n                    ');

      script += `                ${subcmdName})
                    _arguments \\
                    ${args || "':'"}
                    ;;
`;
    }

    script += `            esac
            ;;
    esac
}

`;
  }

  script += `# Hook into compinit
_${cliName} "$@"
`;

  return script;
}

/**
 * Get installation instructions for zsh completions
 * @param {string} [outputPath] - Path where completions are saved
 * @returns {string} Installation instructions
 */
export function getZshInstallInstructions(outputPath = '_playground') {
  return `
Zsh Completion Installation
===========================

Option 1: System-wide installation (requires sudo)
  sudo cp ${outputPath} /usr/local/share/zsh/site-functions/_playground

Option 2: User installation
  mkdir -p ~/.zsh/completions
  cp ${outputPath} ~/.zsh/completions/

  # Add to ~/.zshrc if not already present:
  fpath=(~/.zsh/completions $fpath)
  autoload -Uz compinit && compinit

Option 3: Oh-My-Zsh installation
  mkdir -p ~/.oh-my-zsh/completions
  cp ${outputPath} ~/.oh-my-zsh/completions/

After installation, restart your terminal or run:
  autoload -Uz compinit && compinit -i
`.trim();
}

export default generateZshCompletions;
