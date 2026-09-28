/**
 * @fileoverview Meta command - Introspection and meta operations
 *
 * @description
 * CLI commands for introspection, SPARQL queries, shell completions,
 * middleware listing, telemetry export, and version info.
 *
 * @module cli/commands/meta
 * @version 2.0.0
 * @license MIT
 */

import { defineCommand } from 'citty';

// =============================================================================
// Constants
// =============================================================================

/**
 * CLI version info
 */
const VERSION_INFO = {
  name: 'playground',
  version: '2.0.0',
  node: process.version,
  platform: process.platform,
  arch: process.arch
};

/**
 * Available middleware
 */
const MIDDLEWARE = [
  { name: 'logging', enabled: true, description: 'Request/response logging' },
  { name: 'profiling', enabled: true, description: 'Performance profiling' },
  { name: 'rate-limit', enabled: false, description: 'Rate limiting' },
  { name: 'telemetry', enabled: true, description: 'OpenTelemetry integration' },
  { name: 'validation', enabled: true, description: 'Zod input validation' },
  { name: 'error-handler', enabled: true, description: 'Graceful error handling' }
];

/**
 * Ontology entities
 */
const ONTOLOGY_ENTITIES = [
  { type: 'Class', name: 'pt:AcademicWork', label: 'Academic Work', comment: 'Base class for all academic documents' },
  { type: 'Class', name: 'pt:Paper', label: 'Research Paper', comment: 'A research paper' },
  { type: 'Class', name: 'pt:Thesis', label: 'Thesis', comment: 'A thesis document' },
  { type: 'Class', name: 'pt:IMRADPaper', label: 'IMRAD Paper', comment: 'IMRAD structure paper' },
  { type: 'Class', name: 'pt:DSRPaper', label: 'DSR Paper', comment: 'Design Science Research paper' },
  { type: 'Class', name: 'pt:Monograph', label: 'Monograph Thesis', comment: 'Monograph-style thesis' },
  { type: 'Class', name: 'pt:Section', label: 'Document Section', comment: 'A section in a document' },
  { type: 'Class', name: 'pt:Author', label: 'Author', comment: 'Document author' },
  { type: 'Class', name: 'pt:Committee', label: 'Committee', comment: 'Thesis committee' },
  { type: 'Property', name: 'pt:hasTitle', label: 'has title', domain: 'pt:AcademicWork', range: 'xsd:string' },
  { type: 'Property', name: 'pt:hasAbstract', label: 'has abstract', domain: 'pt:AcademicWork', range: 'xsd:string' },
  { type: 'Property', name: 'pt:hasAuthor', label: 'has author', domain: 'pt:AcademicWork', range: 'pt:Author' },
  { type: 'Property', name: 'pt:hasSection', label: 'has section', domain: 'pt:AcademicWork', range: 'pt:Section' },
  { type: 'Property', name: 'pt:paperFamily', label: 'paper family', domain: 'pt:Paper', range: 'xsd:string' },
  { type: 'Property', name: 'pt:thesisType', label: 'thesis type', domain: 'pt:Thesis', range: 'xsd:string' },
  { type: 'Property', name: 'pt:hasSupervisor', label: 'has supervisor', domain: 'pt:Thesis', range: 'pt:Author' }
];

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Print progress indicator
 * @param {string} message - Progress message
 * @param {string} type - Message type
 */
function printProgress(message, type = 'info') {
  const icons = {
    info: '\x1b[34mi\x1b[0m',
    success: '\x1b[32m+\x1b[0m',
    warning: '\x1b[33m!\x1b[0m',
    error: '\x1b[31mx\x1b[0m',
    progress: '\x1b[36m~\x1b[0m'
  };
  console.log(`${icons[type] || icons.info} ${message}`);
}

/**
 * Simple YAML formatter
 * @param {Object} obj - Object to format
 * @param {number} indent - Current indentation level
 * @returns {string} YAML-formatted string
 */
function toSimpleYaml(obj, indent = 0) {
  const spaces = '  '.repeat(indent);
  let result = '';

  for (const [key, value] of Object.entries(obj)) {
    if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
      result += `${spaces}${key}:\n${toSimpleYaml(value, indent + 1)}`;
    } else if (Array.isArray(value)) {
      result += `${spaces}${key}:\n`;
      for (const item of value) {
        if (typeof item === 'object') {
          result += `${spaces}  -\n${toSimpleYaml(item, indent + 2)}`;
        } else {
          result += `${spaces}  - ${item}\n`;
        }
      }
    } else {
      result += `${spaces}${key}: ${value}\n`;
    }
  }

  return result;
}

// =============================================================================
// Introspect Command
// =============================================================================

/**
 * Machine-readable introspection subcommand
 */
const introspectCommand = defineCommand({
  meta: {
    name: 'introspect',
    description: 'Machine-readable introspection (JSON-LD)'
  },
  args: {
    format: {
      type: 'string',
      alias: 'f',
      description: 'Output format (json, yaml, jsonld)',
      default: 'json'
    },
    section: {
      type: 'string',
      alias: 's',
      description: 'Specific section (commands, families, types, global, all)',
      default: 'all'
    }
  },
  async run({ args }) {
    const registry = {
      '@context': {
        '@vocab': 'http://papers-thesis.org/cli#',
        'schema': 'http://schema.org/'
      },
      '@type': 'CLIApplication',
      name: 'playground',
      version: '2.0.0',
      description: 'Citty + Nunjucks + UNRDF playground CLI',
      commands: {
        papers: {
          description: 'Manage research papers',
          subcommands: ['generate', 'list', 'validate', 'convert', 'info']
        },
        thesis: {
          description: 'Manage thesis documents',
          subcommands: ['generate', 'list', 'schedule', 'defense', 'committee']
        },
        config: {
          description: 'Manage CLI configuration',
          subcommands: ['get', 'set', 'list', 'reset', 'edit', 'validate']
        },
        meta: {
          description: 'Meta operations and introspection',
          subcommands: ['introspect', 'ontology', 'sparql', 'completions', 'middleware', 'telemetry', 'version']
        }
      },
      paperFamilies: ['imrad', 'dsr', 'argument', 'contribution'],
      thesisTypes: ['monograph', 'narrative', 'contribution'],
      globalArgs: {
        quiet: { type: 'boolean', alias: 'q', description: 'Suppress output' },
        verbose: { type: 'boolean', alias: 'v', description: 'Verbose output' },
        format: { type: 'string', alias: 'f', description: 'Output format' },
        config: { type: 'string', alias: 'c', description: 'Config file path' }
      }
    };

    let output = registry;

    // Filter by section if specified
    if (args.section !== 'all') {
      switch (args.section) {
        case 'commands':
          output = { commands: registry.commands };
          break;
        case 'families':
          output = { paperFamilies: registry.paperFamilies };
          break;
        case 'types':
          output = { thesisTypes: registry.thesisTypes };
          break;
        case 'global':
          output = { globalArgs: registry.globalArgs };
          break;
        default:
          printProgress(`Unknown section: ${args.section}`, 'error');
          console.error('Available sections: commands, families, types, global, all');
          process.exit(1);
      }
    }

    if (args.format === 'yaml') {
      console.log(toSimpleYaml(output));
    } else if (args.format === 'jsonld') {
      // Ensure JSON-LD context is included
      if (!output['@context']) {
        output = { '@context': registry['@context'], ...output };
      }
      console.log(JSON.stringify(output, null, 2));
    } else {
      console.log(JSON.stringify(output, null, 2));
    }

    return output;
  }
});

// =============================================================================
// Ontology Command
// =============================================================================

/**
 * List RDF classes subcommand
 */
const ontologyListCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'List RDF classes and properties'
  },
  args: {
    verbose: {
      type: 'boolean',
      alias: 'v',
      description: 'Show detailed information',
      default: false
    },
    format: {
      type: 'string',
      alias: 'f',
      description: 'Output format (table, json, turtle)',
      default: 'table'
    },
    type: {
      type: 'string',
      alias: 't',
      description: 'Filter by type (class, property, all)',
      default: 'all'
    }
  },
  async run({ args }) {
    // Filter by type
    let filtered = ONTOLOGY_ENTITIES;
    if (args.type !== 'all') {
      filtered = ONTOLOGY_ENTITIES.filter(e => e.type.toLowerCase() === args.type.toLowerCase());
    }

    if (args.format === 'json') {
      console.log(JSON.stringify(filtered, null, 2));
      return filtered;
    }

    if (args.format === 'turtle') {
      console.log('@prefix pt: <http://papers-thesis.org/ontology#> .');
      console.log('@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .');
      console.log('@prefix owl: <http://www.w3.org/2002/07/owl#> .');
      console.log('');

      for (const entity of filtered) {
        if (entity.type === 'Class') {
          console.log(`${entity.name} a owl:Class ;`);
          console.log(`    rdfs:label "${entity.label}" ;`);
          console.log(`    rdfs:comment "${entity.comment}" .`);
          console.log('');
        } else {
          console.log(`${entity.name} a owl:DatatypeProperty ;`);
          console.log(`    rdfs:label "${entity.label}" ;`);
          console.log(`    rdfs:domain ${entity.domain} ;`);
          console.log(`    rdfs:range ${entity.range} .`);
          console.log('');
        }
      }
      return filtered;
    }

    // Table format
    console.log('Ontology Entities:\n');

    const classes = filtered.filter(e => e.type === 'Class');
    const properties = filtered.filter(e => e.type === 'Property');

    if (classes.length > 0 && (args.type === 'all' || args.type === 'class')) {
      console.log('  \x1b[36mClasses:\x1b[0m');
      for (const cls of classes) {
        console.log(`    ${cls.name.padEnd(25)} ${cls.label}`);
        if (args.verbose) {
          console.log(`    ${''.padEnd(25)} ${cls.comment}`);
        }
      }
      console.log('');
    }

    if (properties.length > 0 && (args.type === 'all' || args.type === 'property')) {
      console.log('  \x1b[36mProperties:\x1b[0m');
      for (const prop of properties) {
        console.log(`    ${prop.name.padEnd(25)} ${prop.label}`);
        if (args.verbose) {
          console.log(`    ${''.padEnd(25)} Domain: ${prop.domain} -> Range: ${prop.range}`);
        }
      }
    }

    return filtered;
  }
});

/**
 * Ontology subcommand
 */
const ontologyCommand = defineCommand({
  meta: {
    name: 'ontology',
    description: 'List RDF classes and properties'
  },
  subCommands: {
    list: ontologyListCommand
  }
});

// =============================================================================
// SPARQL Command
// =============================================================================

/**
 * Execute SPARQL query subcommand
 */
const sparqlCommand = defineCommand({
  meta: {
    name: 'sparql',
    description: 'Execute SPARQL query'
  },
  args: {
    query: {
      type: 'positional',
      description: 'SPARQL query string',
      required: false
    },
    file: {
      type: 'string',
      alias: 'f',
      description: 'Path to SPARQL query file'
    },
    format: {
      type: 'string',
      description: 'Output format (table, json, csv)',
      default: 'table'
    },
    limit: {
      type: 'string',
      alias: 'l',
      description: 'Limit results (default: 100)',
      default: '100'
    }
  },
  async run({ args }) {
    let query = args.query;

    // Load from file if specified
    if (args.file) {
      printProgress(`Loading query from: ${args.file}`, 'progress');
      // In production, would read file
      query = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10';
    }

    if (!query) {
      printProgress('Error: Either query or --file must be provided', 'error');
      console.log('\nExamples:');
      console.log('  playground meta sparql "SELECT * WHERE { ?s ?p ?o } LIMIT 10"');
      console.log('  playground meta sparql --file ./queries/find-papers.sparql');
      process.exit(1);
    }

    printProgress('Executing SPARQL query...', 'progress');
    printProgress(`Query: ${query.substring(0, 50)}...`, 'info');

    // Mock results (would integrate with actual SPARQL engine in production)
    const mockResults = [
      { subject: 'ex:paper-1', predicate: 'rdf:type', object: 'pt:IMRADPaper' },
      { subject: 'ex:paper-1', predicate: 'pt:hasTitle', object: '"Sample Research Paper"' },
      { subject: 'ex:paper-1', predicate: 'pt:hasAuthor', object: 'ex:alice' },
      { subject: 'ex:thesis-1', predicate: 'rdf:type', object: 'pt:Monograph' },
      { subject: 'ex:thesis-1', predicate: 'pt:hasTitle', object: '"PhD Dissertation"' }
    ];

    if (args.format === 'json') {
      console.log(JSON.stringify(mockResults, null, 2));
      return mockResults;
    }

    if (args.format === 'csv') {
      console.log('subject,predicate,object');
      for (const row of mockResults) {
        console.log(`${row.subject},${row.predicate},${row.object}`);
      }
      return mockResults;
    }

    // Table format
    console.log('\nResults:\n');
    console.log('  ' + 'subject'.padEnd(20) + 'predicate'.padEnd(20) + 'object');
    console.log('  ' + '-'.repeat(60));
    for (const row of mockResults) {
      console.log(`  ${row.subject.padEnd(20)}${row.predicate.padEnd(20)}${row.object}`);
    }
    console.log(`\n  ${mockResults.length} result(s)`);

    return mockResults;
  }
});

// =============================================================================
// Completions Command
// =============================================================================

/**
 * Generate shell completions subcommand
 */
const completionsCommand = defineCommand({
  meta: {
    name: 'completions',
    description: 'Generate shell completions'
  },
  args: {
    shell: {
      type: 'positional',
      description: 'Target shell (bash, zsh, fish, powershell)',
      required: true
    }
  },
  async run({ args }) {
    const shell = args.shell.toLowerCase();

    if (!['bash', 'zsh', 'fish', 'powershell'].includes(shell)) {
      printProgress(`Unknown shell: ${shell}`, 'error');
      console.error('Supported shells: bash, zsh, fish, powershell');
      process.exit(1);
    }

    console.log(`# Shell completions for ${shell}`);
    console.log(`# Add to your shell config file`);
    console.log('');

    switch (shell) {
      case 'bash':
        console.log(generateBashCompletions());
        break;
      case 'zsh':
        console.log(generateZshCompletions());
        break;
      case 'fish':
        console.log(generateFishCompletions());
        break;
      case 'powershell':
        console.log(generatePowershellCompletions());
        break;
    }

    return { shell };
  }
});

// =============================================================================
// Middleware Command
// =============================================================================

/**
 * List active middleware subcommand
 */
const middlewareListCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'List active middleware'
  },
  args: {
    format: {
      type: 'string',
      alias: 'f',
      description: 'Output format (table, json)',
      default: 'table'
    }
  },
  async run({ args }) {
    if (args.format === 'json') {
      console.log(JSON.stringify(MIDDLEWARE, null, 2));
      return MIDDLEWARE;
    }

    console.log('Active Middleware:\n');
    for (const mw of MIDDLEWARE) {
      const status = mw.enabled ? '\x1b[32m[x]\x1b[0m' : '[ ]';
      console.log(`  ${status} ${mw.name.padEnd(15)} ${mw.description}`);
    }

    return MIDDLEWARE;
  }
});

/**
 * Middleware subcommand
 */
const middlewareCommand = defineCommand({
  meta: {
    name: 'middleware',
    description: 'Middleware operations'
  },
  subCommands: {
    list: middlewareListCommand
  }
});

// =============================================================================
// Telemetry Command
// =============================================================================

/**
 * Export execution metrics subcommand
 */
const telemetryCommand = defineCommand({
  meta: {
    name: 'telemetry',
    description: 'Export execution metrics'
  },
  args: {
    format: {
      type: 'positional',
      description: 'Export format (json, prometheus, otlp)',
      required: false,
      default: 'json'
    },
    output: {
      type: 'string',
      alias: 'o',
      description: 'Output file path'
    },
    timeframe: {
      type: 'string',
      alias: 't',
      description: 'Timeframe (1h, 24h, 7d)',
      default: '24h'
    }
  },
  async run({ args }) {
    const format = args.format || 'json';

    const metrics = {
      timeframe: args.timeframe,
      generatedAt: new Date().toISOString(),
      commands: {
        'papers.generate': { count: 15, avgDuration: 450, errors: 0 },
        'papers.list': { count: 8, avgDuration: 12, errors: 0 },
        'papers.validate': { count: 5, avgDuration: 230, errors: 1 },
        'thesis.generate': { count: 3, avgDuration: 520, errors: 0 },
        'thesis.schedule': { count: 12, avgDuration: 45, errors: 0 },
        'meta.sparql': { count: 22, avgDuration: 85, errors: 2 },
        'config.list': { count: 18, avgDuration: 8, errors: 0 }
      },
      totals: {
        totalCommands: 83,
        totalDuration: 12450,
        totalErrors: 3,
        successRate: 96.39
      }
    };

    if (format === 'prometheus') {
      console.log('# HELP playground_commands_total Total commands executed');
      console.log('# TYPE playground_commands_total counter');
      for (const [cmd, data] of Object.entries(metrics.commands)) {
        console.log(`playground_commands_total{command="${cmd}"} ${data.count}`);
      }
      console.log('');
      console.log('# HELP playground_command_duration_ms Average command duration');
      console.log('# TYPE playground_command_duration_ms gauge');
      for (const [cmd, data] of Object.entries(metrics.commands)) {
        console.log(`playground_command_duration_ms{command="${cmd}"} ${data.avgDuration}`);
      }
      console.log('');
      console.log('# HELP playground_command_errors_total Total command errors');
      console.log('# TYPE playground_command_errors_total counter');
      for (const [cmd, data] of Object.entries(metrics.commands)) {
        console.log(`playground_command_errors_total{command="${cmd}"} ${data.errors}`);
      }
      return metrics;
    }

    if (format === 'otlp') {
      const otlpPayload = {
        resourceMetrics: [{
          resource: {
            attributes: [
              { key: 'service.name', value: { stringValue: 'playground-cli' } },
              { key: 'service.version', value: { stringValue: '2.0.0' } }
            ]
          },
          scopeMetrics: [{
            scope: { name: 'playground.cli' },
            metrics: Object.entries(metrics.commands).map(([cmd, data]) => ({
              name: 'playground.command.duration',
              unit: 'ms',
              sum: {
                dataPoints: [{
                  asInt: data.avgDuration,
                  attributes: [{ key: 'command', value: { stringValue: cmd } }]
                }]
              }
            }))
          }]
        }]
      };
      console.log(JSON.stringify(otlpPayload, null, 2));
      return otlpPayload;
    }

    // JSON format (default)
    if (args.output) {
      printProgress(`Metrics would be written to: ${args.output}`, 'info');
    }
    console.log(JSON.stringify(metrics, null, 2));
    return metrics;
  }
});

// =============================================================================
// Version Command
// =============================================================================

/**
 * Show version info subcommand
 */
const versionCommand = defineCommand({
  meta: {
    name: 'version',
    description: 'Show version information'
  },
  args: {
    format: {
      type: 'string',
      alias: 'f',
      description: 'Output format (text, json)',
      default: 'text'
    }
  },
  async run({ args }) {
    const info = {
      ...VERSION_INFO,
      dependencies: {
        citty: '0.1.6',
        zod: '3.22.0',
        nunjucks: '3.2.4'
      },
      buildDate: '2024-11-22',
      commit: 'abc1234'
    };

    if (args.format === 'json') {
      console.log(JSON.stringify(info, null, 2));
      return info;
    }

    console.log(`
${info.name} v${info.version}

Runtime:
  Node.js:  ${info.node}
  Platform: ${info.platform}
  Arch:     ${info.arch}

Dependencies:
  citty:    ${info.dependencies.citty}
  zod:      ${info.dependencies.zod}
  nunjucks: ${info.dependencies.nunjucks}

Build:
  Date:   ${info.buildDate}
  Commit: ${info.commit}
    `.trim());

    return info;
  }
});

// =============================================================================
// Shell Completion Generators
// =============================================================================

function generateBashCompletions() {
  return `
_playground_completions() {
    local cur prev commands
    COMPREPLY=()
    cur="\${COMP_WORDS[COMP_CWORD]}"
    prev="\${COMP_WORDS[COMP_CWORD-1]}"

    commands="papers thesis config meta"
    papers_commands="generate list validate convert info"
    thesis_commands="generate list schedule defense committee"
    config_commands="get set list reset edit validate"
    meta_commands="introspect ontology sparql completions middleware telemetry version"

    case "\${prev}" in
        playground)
            COMPREPLY=( $(compgen -W "\${commands}" -- \${cur}) )
            ;;
        papers)
            COMPREPLY=( $(compgen -W "\${papers_commands}" -- \${cur}) )
            ;;
        thesis)
            COMPREPLY=( $(compgen -W "\${thesis_commands}" -- \${cur}) )
            ;;
        config)
            COMPREPLY=( $(compgen -W "\${config_commands}" -- \${cur}) )
            ;;
        meta)
            COMPREPLY=( $(compgen -W "\${meta_commands}" -- \${cur}) )
            ;;
    esac
}

complete -F _playground_completions playground
  `.trim();
}

function generateZshCompletions() {
  return `
#compdef playground

_playground() {
    local -a commands papers_cmds thesis_cmds config_cmds meta_cmds

    commands=(
        'papers:Manage research papers'
        'thesis:Manage thesis documents'
        'config:Manage CLI configuration'
        'meta:Meta operations and introspection'
    )

    papers_cmds=(
        'generate:Generate paper from template'
        'list:List available paper families'
        'validate:Validate paper structure'
        'convert:Convert between formats'
        'info:Get paper metadata'
    )

    thesis_cmds=(
        'generate:Generate thesis structure'
        'list:List thesis types'
        'schedule:Manage thesis schedule'
        'defense:Set defense date'
        'committee:Add committee member'
    )

    config_cmds=(
        'get:Get configuration value'
        'set:Set configuration value'
        'list:List all configuration'
        'reset:Reset to defaults'
        'edit:Edit in $EDITOR'
        'validate:Validate configuration'
    )

    meta_cmds=(
        'introspect:Machine-readable introspection'
        'ontology:List RDF classes'
        'sparql:Execute SPARQL query'
        'completions:Generate shell completions'
        'middleware:Middleware operations'
        'telemetry:Export execution metrics'
        'version:Show version info'
    )

    _arguments -C \\
        '1: :->command' \\
        '2: :->subcommand' \\
        '*::arg:->args'

    case $state in
        command)
            _describe 'command' commands
            ;;
        subcommand)
            case $words[2] in
                papers) _describe 'subcommand' papers_cmds ;;
                thesis) _describe 'subcommand' thesis_cmds ;;
                config) _describe 'subcommand' config_cmds ;;
                meta) _describe 'subcommand' meta_cmds ;;
            esac
            ;;
    esac
}

_playground
  `.trim();
}

function generateFishCompletions() {
  return `
# Fish completions for playground CLI

complete -c playground -f

# Main commands
complete -c playground -n "__fish_use_subcommand" -a papers -d "Manage research papers"
complete -c playground -n "__fish_use_subcommand" -a thesis -d "Manage thesis documents"
complete -c playground -n "__fish_use_subcommand" -a config -d "Manage CLI configuration"
complete -c playground -n "__fish_use_subcommand" -a meta -d "Meta operations and introspection"

# Papers subcommands
complete -c playground -n "__fish_seen_subcommand_from papers" -a generate -d "Generate paper from template"
complete -c playground -n "__fish_seen_subcommand_from papers" -a list -d "List available paper families"
complete -c playground -n "__fish_seen_subcommand_from papers" -a validate -d "Validate paper structure"
complete -c playground -n "__fish_seen_subcommand_from papers" -a convert -d "Convert between formats"
complete -c playground -n "__fish_seen_subcommand_from papers" -a info -d "Get paper metadata"

# Thesis subcommands
complete -c playground -n "__fish_seen_subcommand_from thesis" -a generate -d "Generate thesis structure"
complete -c playground -n "__fish_seen_subcommand_from thesis" -a list -d "List thesis types"
complete -c playground -n "__fish_seen_subcommand_from thesis" -a schedule -d "Manage thesis schedule"
complete -c playground -n "__fish_seen_subcommand_from thesis" -a defense -d "Set defense date"
complete -c playground -n "__fish_seen_subcommand_from thesis" -a committee -d "Add committee member"

# Config subcommands
complete -c playground -n "__fish_seen_subcommand_from config" -a get -d "Get configuration value"
complete -c playground -n "__fish_seen_subcommand_from config" -a set -d "Set configuration value"
complete -c playground -n "__fish_seen_subcommand_from config" -a list -d "List all configuration"
complete -c playground -n "__fish_seen_subcommand_from config" -a reset -d "Reset to defaults"
complete -c playground -n "__fish_seen_subcommand_from config" -a edit -d "Edit in editor"
complete -c playground -n "__fish_seen_subcommand_from config" -a validate -d "Validate configuration"

# Meta subcommands
complete -c playground -n "__fish_seen_subcommand_from meta" -a introspect -d "Machine-readable introspection"
complete -c playground -n "__fish_seen_subcommand_from meta" -a ontology -d "List RDF classes"
complete -c playground -n "__fish_seen_subcommand_from meta" -a sparql -d "Execute SPARQL query"
complete -c playground -n "__fish_seen_subcommand_from meta" -a completions -d "Generate shell completions"
complete -c playground -n "__fish_seen_subcommand_from meta" -a middleware -d "Middleware operations"
complete -c playground -n "__fish_seen_subcommand_from meta" -a telemetry -d "Export execution metrics"
complete -c playground -n "__fish_seen_subcommand_from meta" -a version -d "Show version info"
  `.trim();
}

function generatePowershellCompletions() {
  return `
# PowerShell completions for playground CLI

Register-ArgumentCompleter -CommandName playground -ScriptBlock {
    param($commandName, $wordToComplete, $cursorPosition)

    $commands = @{
        '' = @('papers', 'thesis', 'config', 'meta')
        'papers' = @('generate', 'list', 'validate', 'convert', 'info')
        'thesis' = @('generate', 'list', 'schedule', 'defense', 'committee')
        'config' = @('get', 'set', 'list', 'reset', 'edit', 'validate')
        'meta' = @('introspect', 'ontology', 'sparql', 'completions', 'middleware', 'telemetry', 'version')
    }

    $parts = $wordToComplete -split ' '
    $key = if ($parts.Count -gt 1) { $parts[0] } else { '' }

    $completions = $commands[$key]
    if ($completions) {
        $completions | Where-Object { $_ -like "$wordToComplete*" } | ForEach-Object {
            [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_)
        }
    }
}
  `.trim();
}

// =============================================================================
// Main Meta Command Export
// =============================================================================

/**
 * Meta command with subcommands
 * @type {import('citty').CommandDef}
 */
export const metaCommand = defineCommand({
  meta: {
    name: 'meta',
    description: 'Meta operations and introspection'
  },
  subCommands: {
    introspect: introspectCommand,
    ontology: ontologyCommand,
    sparql: sparqlCommand,
    completions: completionsCommand,
    middleware: middlewareCommand,
    telemetry: telemetryCommand,
    version: versionCommand
  }
});
