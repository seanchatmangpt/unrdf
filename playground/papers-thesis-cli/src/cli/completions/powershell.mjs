/**
 * @fileoverview PowerShell Completions Generator
 *
 * @description
 * Generates PowerShell completion scripts for the playground CLI.
 * Uses PowerShell's Register-ArgumentCompleter with tab completion
 * and tooltip support.
 *
 * @module cli/completions/powershell
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
            choices: ['imrad', 'dsr', 'argument', 'contribution'],
          },
          '--title': { description: 'Paper title', short: '-t' },
          '--author': { description: 'Author name', short: '-a' },
          '--affiliation': { description: 'Author affiliation' },
          '--abstract': { description: 'Paper abstract' },
          '--output': { description: 'Output file path', short: '-o', type: 'file' },
          '--format': {
            description: 'Output format',
            short: '-f',
            choices: ['json', 'latex', 'yaml'],
          },
        },
      },
      list: {
        description: 'List available paper families',
        args: {
          '--verbose': { description: 'Show detailed information', short: '-v' },
          '--format': {
            description: 'Output format',
            short: '-f',
            choices: ['json', 'yaml', 'table'],
          },
        },
      },
      validate: {
        description: 'Validate paper structure',
        args: {
          '--strict': { description: 'Enable strict validation' },
          '--format': { description: 'Output format', short: '-f', choices: ['json', 'table'] },
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
          '--type': { description: 'Thesis type', choices: ['masters', 'phd', 'doctoral'] },
          '--title': { description: 'Thesis title', short: '-t' },
          '--author': { description: 'Author name', short: '-a' },
          '--advisor': { description: 'Advisor name' },
          '--institution': { description: 'Institution name' },
          '--department': { description: 'Department name' },
          '--output': { description: 'Output file path', short: '-o', type: 'file' },
          '--format': {
            description: 'Output format',
            short: '-f',
            choices: ['json', 'latex', 'yaml'],
          },
        },
      },
      list: {
        description: 'List available thesis types',
        args: {
          '--verbose': { description: 'Show detailed information', short: '-v' },
          '--format': {
            description: 'Output format',
            short: '-f',
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
            description: 'Output format',
            short: '-f',
            choices: ['json', 'table', 'csv'],
          },
          '--file': { description: 'Query file', type: 'file' },
        },
      },
      schema: {
        description: 'Show schema information',
        args: {
          '--format': {
            description: 'Output format',
            short: '-f',
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
          '--output': { description: 'Output file', short: '-o', type: 'file' },
        },
      },
    },
  },
};

/**
 * Escape string for PowerShell
 * @param {string} str - String to escape
 * @returns {string} Escaped string
 */
function escapeForPowerShell(str) {
  return str.replace(/'/g, "''").replace(/`/g, '``');
}

/**
 * Generate PowerShell completion script
 * @param {Object} [options] - Generation options
 * @param {string} [options.cliName] - CLI command name
 * @param {Object} [options.commands] - Command structure override
 * @returns {string} PowerShell completion script
 */
export function generatePowerShellCompletions(options = {}) {
  const { cliName = 'playground', commands = CLI_STRUCTURE } = options;

  const commandNames = Object.keys(commands);

  let script = `# PowerShell completion for ${cliName} CLI
# Generated by papers-thesis-cli completions generator
#
# Installation:
#   1. Copy to your PowerShell profile directory
#   2. Or add to $PROFILE: . "/path/to/${cliName}-completion.ps1"
#   3. For all users: Copy to $PSHOME/Modules/

# Define completion data
$${cliName}Commands = @{
`;

  // Generate command data structure
  for (const [cmdName, cmdDef] of Object.entries(commands)) {
    script += `    '${cmdName}' = @{
        Description = '${escapeForPowerShell(cmdDef.description)}'
        Subcommands = @{
`;

    for (const [subcmdName, subcmdDef] of Object.entries(cmdDef.subcommands || {})) {
      script += `            '${subcmdName}' = @{
                Description = '${escapeForPowerShell(subcmdDef.description)}'
                Args = @{
`;

      for (const [argName, argDef] of Object.entries(subcmdDef.args || {})) {
        const escapedDesc = escapeForPowerShell(argDef.description || 'Option');
        script += `                    '${argName}' = @{
                        Description = '${escapedDesc}'
`;
        if (argDef.choices) {
          script += `                        Choices = @(${argDef.choices.map(c => `'${c}'`).join(', ')})
`;
        }
        if (argDef.short) {
          script += `                        Short = '${argDef.short}'
`;
        }
        if (argDef.type) {
          script += `                        Type = '${argDef.type}'
`;
        }
        script += `                    }
`;
      }

      script += `                }
            }
`;
    }

    script += `        }
    }
`;
  }

  script += `}

# Global options available for all commands
$${cliName}GlobalOptions = @(
    @{ Name = '-h'; Long = '--help'; Description = 'Show help information' }
    @{ Name = '-v'; Long = '--verbose'; Description = 'Enable verbose output' }
    @{ Name = '-q'; Long = '--quiet'; Description = 'Suppress non-essential output' }
    @{ Name = '-f'; Long = '--format'; Description = 'Output format'; Choices = @('json', 'json-pretty', 'yaml', 'table', 'latex', 'csv') }
    @{ Name = '-o'; Long = '--output'; Description = 'Output file path'; Type = 'file' }
    @{ Name = '-c'; Long = '--config'; Description = 'Configuration file'; Type = 'file' }
    @{ Name = ''; Long = '--version'; Description = 'Show version information' }
)

# Register argument completer
Register-ArgumentCompleter -CommandName ${cliName} -ScriptBlock {
    param($wordToComplete, $commandAst, $cursorPosition)

    $words = $commandAst.CommandElements | ForEach-Object { $_.Extent.Text }
    $wordsBeforeCursor = @()

    foreach ($word in $words) {
        if ($commandAst.CommandElements | Where-Object { $_.Extent.Text -eq $word -and $_.Extent.EndOffset -le $cursorPosition }) {
            $wordsBeforeCursor += $word
        }
    }

    $command = $null
    $subcommand = $null
    $lastArg = $null

    # Parse command context
    for ($i = 1; $i -lt $wordsBeforeCursor.Count; $i++) {
        $word = $wordsBeforeCursor[$i]
        if ($word -notlike '-*') {
            if (-not $command) {
                $command = $word
            } elseif (-not $subcommand) {
                $subcommand = $word
            }
        } else {
            $lastArg = $word
        }
    }

    $completions = @()

    # Check if we need to complete argument values
    if ($lastArg -and $command -and $subcommand) {
        $cmdData = $${cliName}Commands[$command]
        if ($cmdData -and $cmdData.Subcommands[$subcommand]) {
            $argData = $cmdData.Subcommands[$subcommand].Args[$lastArg]
            if ($argData -and $argData.Choices) {
                foreach ($choice in $argData.Choices) {
                    if ($choice -like "$wordToComplete*") {
                        $completions += [System.Management.Automation.CompletionResult]::new(
                            $choice,
                            $choice,
                            'ParameterValue',
                            $choice
                        )
                    }
                }
                return $completions
            }
        }

        # Check global options
        foreach ($opt in $${cliName}GlobalOptions) {
            if ($opt.Long -eq $lastArg -and $opt.Choices) {
                foreach ($choice in $opt.Choices) {
                    if ($choice -like "$wordToComplete*") {
                        $completions += [System.Management.Automation.CompletionResult]::new(
                            $choice,
                            $choice,
                            'ParameterValue',
                            $choice
                        )
                    }
                }
                return $completions
            }
        }
    }

    # Complete commands
    if (-not $command) {
        foreach ($cmd in $${cliName}Commands.Keys) {
            if ($cmd -like "$wordToComplete*") {
                $desc = $${cliName}Commands[$cmd].Description
                $completions += [System.Management.Automation.CompletionResult]::new(
                    $cmd,
                    $cmd,
                    'Command',
                    $desc
                )
            }
        }

        # Add global options
        foreach ($opt in $${cliName}GlobalOptions) {
            if ($opt.Long -like "$wordToComplete*") {
                $completions += [System.Management.Automation.CompletionResult]::new(
                    $opt.Long,
                    $opt.Long,
                    'ParameterName',
                    $opt.Description
                )
            }
            if ($opt.Name -and $opt.Name -like "$wordToComplete*") {
                $completions += [System.Management.Automation.CompletionResult]::new(
                    $opt.Name,
                    $opt.Name,
                    'ParameterName',
                    $opt.Description
                )
            }
        }

        return $completions
    }

    # Complete subcommands
    if ($command -and -not $subcommand -and $${cliName}Commands.ContainsKey($command)) {
        $cmdData = $${cliName}Commands[$command]
        foreach ($subcmd in $cmdData.Subcommands.Keys) {
            if ($subcmd -like "$wordToComplete*") {
                $desc = $cmdData.Subcommands[$subcmd].Description
                $completions += [System.Management.Automation.CompletionResult]::new(
                    $subcmd,
                    $subcmd,
                    'Command',
                    $desc
                )
            }
        }
        return $completions
    }

    # Complete arguments for subcommand
    if ($command -and $subcommand -and $wordToComplete -like '-*') {
        $cmdData = $${cliName}Commands[$command]
        if ($cmdData -and $cmdData.Subcommands[$subcommand]) {
            foreach ($arg in $cmdData.Subcommands[$subcommand].Args.Keys) {
                if ($arg -like "$wordToComplete*") {
                    $argData = $cmdData.Subcommands[$subcommand].Args[$arg]
                    $completions += [System.Management.Automation.CompletionResult]::new(
                        $arg,
                        $arg,
                        'ParameterName',
                        $argData.Description
                    )
                }
            }
        }

        # Add global options
        foreach ($opt in $${cliName}GlobalOptions) {
            if ($opt.Long -like "$wordToComplete*") {
                $completions += [System.Management.Automation.CompletionResult]::new(
                    $opt.Long,
                    $opt.Long,
                    'ParameterName',
                    $opt.Description
                )
            }
        }
    }

    return $completions
}

Write-Host "${cliName} completions loaded. Press Tab to use." -ForegroundColor Green
`;

  return script;
}

/**
 * Get installation instructions for PowerShell completions
 * @param {string} [outputPath] - Path where completions are saved
 * @returns {string} Installation instructions
 */
export function getPowerShellInstallInstructions(outputPath = 'playground-completion.ps1') {
  return `
PowerShell Completion Installation
==================================

Option 1: Add to your profile (recommended)
  # First, ensure profile exists
  if (!(Test-Path -Path $PROFILE)) {
      New-Item -ItemType File -Path $PROFILE -Force
  }

  # Add the completion script
  Add-Content -Path $PROFILE -Value ". '$PWD/${outputPath}'"

Option 2: Manual loading (per session)
  . "${outputPath}"

Option 3: System-wide installation
  # Copy to PowerShell modules directory
  Copy-Item ${outputPath} "$PSHOME/Modules/playground-completion.ps1"

After installation, restart PowerShell or run:
  . $PROFILE

Note: If you get an execution policy error, run:
  Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
`.trim();
}

export default generatePowerShellCompletions;
