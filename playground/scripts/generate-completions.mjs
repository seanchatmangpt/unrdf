#!/usr/bin/env node
/**
 * Generate shell completions for the playground CLI
 * @module scripts/generate-completions
 */

import { writeFileSync, mkdirSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Commands available in the CLI
 */
const COMMANDS = [
  'query',
  'parse',
  'validate',
  'render',
  'ontology',
  'hooks',
  'config',
];

/**
 * Generate bash completions
 * @returns {string} Bash completion script
 */
function generateBashCompletions() {
  return `# Bash completion for playground CLI
# Add to ~/.bashrc: source /path/to/playground-completion.bash

_playground_completions() {
    local cur prev commands
    COMPREPLY=()
    cur="\${COMP_WORDS[COMP_CWORD]}"
    prev="\${COMP_WORDS[COMP_CWORD-1]}"
    commands="${COMMANDS.join(' ')}"

    case "\${prev}" in
        playground)
            COMPREPLY=( $(compgen -W "\${commands} --help --version --verbose --config" -- "\${cur}") )
            return 0
            ;;
        query)
            COMPREPLY=( $(compgen -W "--sparql --file --format --output" -- "\${cur}") )
            return 0
            ;;
        parse)
            COMPREPLY=( $(compgen -W "--input --format --validate" -- "\${cur}") )
            return 0
            ;;
        validate)
            COMPREPLY=( $(compgen -W "--schema --strict --report" -- "\${cur}") )
            return 0
            ;;
        render)
            COMPREPLY=( $(compgen -W "--template --data --output" -- "\${cur}") )
            return 0
            ;;
        *)
            COMPREPLY=( $(compgen -f -- "\${cur}") )
            return 0
            ;;
    esac
}

complete -F _playground_completions playground
`;
}

/**
 * Generate zsh completions
 * @returns {string} Zsh completion script
 */
function generateZshCompletions() {
  return `#compdef playground
# Zsh completion for playground CLI

_playground() {
    local -a commands
    commands=(
        ${COMMANDS.map(c => `'${c}:${c} command'`).join('\n        ')}
    )

    _arguments -C \\
        '(-h --help)'{-h,--help}'[Show help]' \\
        '(-v --verbose)'{-v,--verbose}'[Enable verbose output]' \\
        '(-c --config)'{-c,--config}'[Configuration file]:file:_files' \\
        '--version[Show version]' \\
        '1:command:->command' \\
        '*::arg:->args'

    case "$state" in
        command)
            _describe -t commands 'playground command' commands
            ;;
        args)
            case $words[1] in
                query)
                    _arguments \\
                        '--sparql[SPARQL query]:query:' \\
                        '--file[Input file]:file:_files' \\
                        '--format[Output format]:format:(json table turtle)' \\
                        '--output[Output file]:file:_files'
                    ;;
                parse)
                    _arguments \\
                        '--input[Input file]:file:_files' \\
                        '--format[Input format]:format:(turtle rdfxml jsonld ntriples)' \\
                        '--validate[Validate input]'
                    ;;
                validate)
                    _arguments \\
                        '--schema[SHACL schema]:file:_files' \\
                        '--strict[Strict validation]' \\
                        '--report[Generate report]'
                    ;;
                render)
                    _arguments \\
                        '--template[Nunjucks template]:file:_files' \\
                        '--data[Data file]:file:_files' \\
                        '--output[Output file]:file:_files'
                    ;;
            esac
            ;;
    esac
}

_playground "$@"
`;
}

/**
 * Main function
 */
async function main() {
  const outputDir = join(__dirname, '..', 'output', 'completions');

  try {
    mkdirSync(outputDir, { recursive: true });

    // Generate bash completions
    const bashScript = generateBashCompletions();
    writeFileSync(join(outputDir, 'playground-completion.bash'), bashScript);
    console.log('Generated bash completions: output/completions/playground-completion.bash');

    // Generate zsh completions
    const zshScript = generateZshCompletions();
    writeFileSync(join(outputDir, '_playground'), zshScript);
    console.log('Generated zsh completions: output/completions/_playground');

    console.log('\nTo install bash completions:');
    console.log('  source output/completions/playground-completion.bash');
    console.log('\nTo install zsh completions:');
    console.log('  cp output/completions/_playground ~/.zsh/completions/');

  } catch (error) {
    console.error('Failed to generate completions:', error.message);
    process.exit(1);
  }
}

main();
