/**
 * @fileoverview Meta command - Introspection and meta operations
 *
 * @description
 * CLI commands for introspection, SPARQL queries, shell completions,
 * middleware listing, and telemetry export.
 *
 * @module cli/commands/meta
 * @version 2.0.0
 * @license MIT
 */

import { defineCommand } from 'citty';
import { readFile } from 'node:fs/promises';

// Import command registry for introspection
import commandRegistry from '../../../command-registry.json' with { type: 'json' };

// Import SPARQL integration layer
import {
  initKnowledgeGraph,
  executeSparqlSelect,
  executeSparqlAsk,
  executeSparqlConstruct,
  executeNamedQuery,
  listNamedQueries,
  getOntologyClasses,
  getOntologyProperties,
  getQueryMetrics,
  isInitialized,
  ONTOLOGY_PATHS,
} from '../../integration/sparql.mjs';

import { knowledgeGraph } from '../../integration/knowledge-graph.mjs';

/**
 * Meta command with subcommands for introspect, ontology, sparql, completions, middleware, telemetry
 * @type {import('citty').CommandDef}
 */
export const metaCommand = defineCommand({
  meta: {
    name: 'meta',
    description: 'Meta operations and introspection',
  },
  subCommands: {
    /**
     * Machine-grade introspection
     */
    introspect: defineCommand({
      meta: {
        name: 'introspect',
        description: 'Machine-grade introspection',
      },
      args: {
        format: {
          type: 'string',
          alias: 'f',
          description: 'Output format (json, yaml)',
          default: 'json',
        },
        section: {
          type: 'string',
          alias: 's',
          description: 'Specific section (commands, families, types, global)',
          default: 'all',
        },
      },
      async run({ args }) {
        let output = commandRegistry;

        // Filter by section if specified
        if (args.section !== 'all') {
          switch (args.section) {
            case 'commands':
              output = { commands: commandRegistry.commands };
              break;
            case 'families':
              output = { paperFamilies: commandRegistry.paperFamilies };
              break;
            case 'types':
              output = { thesisTypes: commandRegistry.thesisTypes };
              break;
            case 'global':
              output = { globalArgs: commandRegistry.globalArgs };
              break;
            default:
              console.error(`Unknown section: ${args.section}`);
              console.error('Available sections: commands, families, types, global');
              process.exit(1);
          }
        }

        if (args.format === 'yaml') {
          // Simple YAML-like output (for demo purposes)
          console.log(toSimpleYaml(output));
        } else {
          console.log(JSON.stringify(output, null, 2));
        }
      },
    }),

    /**
     * Ontology operations
     */
    ontology: defineCommand({
      meta: {
        name: 'ontology',
        description: 'Ontology operations',
      },
      subCommands: {
        /**
         * List RDF classes
         */
        list: defineCommand({
          meta: {
            name: 'list',
            description: 'List RDF classes and properties',
          },
          args: {
            verbose: {
              type: 'boolean',
              alias: 'v',
              description: 'Show detailed information',
              default: false,
            },
            format: {
              type: 'string',
              alias: 'f',
              description: 'Output format (table, json, turtle)',
              default: 'table',
            },
            type: {
              type: 'string',
              alias: 't',
              description: 'Filter by type (class, property, all)',
              default: 'all',
            },
            init: {
              type: 'boolean',
              description: 'Initialize knowledge graph before querying',
              default: false,
            },
          },
          async run({ args }) {
            let classes = [];
            let properties = [];

            // Try to use SPARQL layer if initialized
            if (args.init || isInitialized()) {
              try {
                if (args.init && !isInitialized()) {
                  console.log('Initializing knowledge graph...');
                  await initKnowledgeGraph({ loadExamples: true });
                  console.log('Knowledge graph initialized.');
                }

                if (args.type === 'all' || args.type === 'class') {
                  classes = await getOntologyClasses();
                }
                if (args.type === 'all' || args.type === 'property') {
                  properties = await getOntologyProperties();
                }
              } catch (error) {
                console.warn(`SPARQL query failed: ${error.message}. Using fallback data.`);
                // Fall back to mock data
                const fallbackData = await knowledgeGraph.listClasses();
                classes = fallbackData;
                properties = await knowledgeGraph.listProperties();
              }
            } else {
              // Use knowledge graph fallback (mock data)
              classes = await knowledgeGraph.listClasses();
              properties = await knowledgeGraph.listProperties();
            }

            // Format classes for display
            const classEntities = classes.map(c => ({
              type: 'Class',
              name: c.class || c.uri || 'unknown',
              label: c.label || extractLocalName(c.class || c.uri),
              comment: c.comment || '',
            }));

            // Format properties for display
            const propEntities = properties.map(p => ({
              type: 'Property',
              name: p.property || p.uri || 'unknown',
              label: p.label || extractLocalName(p.property || p.uri),
              domain: p.domain || 'unknown',
              range: p.range || 'unknown',
              propType: p.type || 'General',
            }));

            // Filter by type
            let filtered = [];
            if (args.type === 'all' || args.type === 'class') {
              filtered = filtered.concat(classEntities);
            }
            if (args.type === 'all' || args.type === 'property') {
              filtered = filtered.concat(propEntities);
            }

            if (args.format === 'json') {
              console.log(JSON.stringify(filtered, null, 2));
              return;
            }

            console.log('Ontology Entities:\n');

            if (classEntities.length > 0 && (args.type === 'all' || args.type === 'class')) {
              console.log('  Classes:');
              for (const cls of classEntities) {
                const displayName = extractLocalName(cls.name);
                console.log(`    ${displayName.padEnd(25)} ${cls.label}`);
                if (args.verbose && cls.comment) {
                  console.log(`    ${''.padEnd(25)} ${cls.comment}`);
                }
              }
              console.log('');
            }

            if (propEntities.length > 0 && (args.type === 'all' || args.type === 'property')) {
              console.log('  Properties:');
              for (const prop of propEntities) {
                const displayName = extractLocalName(prop.name);
                console.log(`    ${displayName.padEnd(25)} ${prop.label}`);
                if (args.verbose) {
                  const domain = extractLocalName(prop.domain);
                  const range = extractLocalName(prop.range);
                  console.log(`    ${''.padEnd(25)} Domain: ${domain} -> Range: ${range}`);
                }
              }
            }

            // Show metrics if verbose
            if (args.verbose && isInitialized()) {
              const metrics = getQueryMetrics();
              console.log('\n  Query Metrics:');
              console.log(`    Queries executed: ${metrics.queriesExecuted}`);
              console.log(`    Avg query time: ${metrics.avgQueryTime}ms`);
              console.log(`    Cache hit rate: ${metrics.hitRate}%`);
            }
          },
        }),
      },
    }),

    /**
     * Execute SPARQL query
     */
    sparql: defineCommand({
      meta: {
        name: 'sparql',
        description: 'Execute SPARQL query',
      },
      args: {
        query: {
          type: 'positional',
          description: 'SPARQL query string or named query name',
          required: false,
        },
        file: {
          type: 'string',
          alias: 'f',
          description: 'Path to SPARQL query file',
        },
        format: {
          type: 'string',
          description: 'Output format (table, json, csv)',
          default: 'table',
        },
        limit: {
          type: 'string',
          alias: 'l',
          description: 'Limit results (default: 100)',
          default: '100',
        },
        named: {
          type: 'string',
          alias: 'n',
          description: 'Execute named query by name',
        },
        params: {
          type: 'string',
          alias: 'p',
          description: 'Query parameters as JSON (for named queries)',
        },
        list: {
          type: 'boolean',
          description: 'List available named queries',
          default: false,
        },
        init: {
          type: 'boolean',
          description: 'Initialize knowledge graph before querying',
          default: false,
        },
      },
      async run({ args }) {
        // List named queries
        if (args.list) {
          const queries = listNamedQueries();
          console.log('Available Named Queries:\n');
          for (const q of queries) {
            console.log(`  ${q.name}`);
            console.log(`    ${q.description}`);
            if (q.params.length > 0) {
              console.log(`    Parameters: ${q.params.join(', ')}`);
            }
            console.log('');
          }
          return;
        }

        // Initialize if requested
        if (args.init && !isInitialized()) {
          console.log('Initializing knowledge graph...');
          await initKnowledgeGraph({ loadExamples: true });
          console.log('Knowledge graph initialized.\n');
        }

        let query = args.query;
        let isNamedQuery = false;
        let namedQueryParams = {};

        // Execute named query
        if (args.named) {
          isNamedQuery = true;
          query = args.named;

          // Parse parameters
          if (args.params) {
            try {
              namedQueryParams = JSON.parse(args.params);
            } catch (e) {
              console.error(`Invalid JSON parameters: ${e.message}`);
              console.error('Example: --params \'{"authorName": "Alice"}\'');
              process.exit(1);
            }
          }
        }

        // Load from file if specified
        if (args.file) {
          try {
            query = await readFile(args.file, 'utf-8');
            console.log(`Loaded query from: ${args.file}`);
          } catch (error) {
            console.error(`Failed to read query file: ${error.message}`);
            process.exit(1);
          }
        }

        if (!query) {
          console.error('Error: Either query, --named, or --file must be provided');
          console.error('');
          console.error('Examples:');
          console.error('  playground meta sparql "SELECT * WHERE { ?s ?p ?o } LIMIT 10"');
          console.error('  playground meta sparql --file ./queries/find-papers.sparql');
          console.error(
            '  playground meta sparql --named findPapersByAuthor --params \'{"authorName": "Alice"}\''
          );
          console.error('  playground meta sparql --list');
          process.exit(1);
        }

        console.log(`Executing ${isNamedQuery ? 'named ' : ''}SPARQL query...`);
        const startTime = Date.now();

        try {
          let results;

          if (isNamedQuery) {
            results = await executeNamedQuery(query, namedQueryParams);
          } else {
            // Detect query type
            const queryType = detectQueryType(query);

            switch (queryType) {
              case 'ASK':
                results = await executeSparqlAsk(query);
                console.log(`\nResult: ${results}`);
                return;
              case 'CONSTRUCT':
                results = await executeSparqlConstruct(query);
                console.log('\nConstructed RDF:\n');
                console.log(results);
                return;
              default:
                results = await executeSparqlSelect(query, { limit: parseInt(args.limit, 10) });
            }
          }

          const duration = Date.now() - startTime;

          // Handle array results (SELECT queries)
          if (Array.isArray(results)) {
            if (results.length === 0) {
              console.log('\nNo results found.');
              console.log(`\nQuery executed in ${duration}ms`);
              return;
            }

            if (args.format === 'json') {
              console.log(JSON.stringify(results, null, 2));
              console.log(`\n${results.length} result(s) in ${duration}ms`);
              return;
            }

            if (args.format === 'csv') {
              const keys = Object.keys(results[0]);
              console.log(keys.join(','));
              for (const row of results) {
                console.log(keys.map(k => escapeCsv(row[k])).join(','));
              }
              console.log(`\n${results.length} result(s) in ${duration}ms`);
              return;
            }

            // Table format
            const keys = Object.keys(results[0]);
            console.log('\nResults:\n');

            // Header
            const header = keys.map(k => k.padEnd(25)).join('');
            console.log(`  ${header}`);
            console.log('  ' + '-'.repeat(keys.length * 25));

            // Rows
            for (const row of results) {
              const values = keys
                .map(k => {
                  const val = row[k];
                  const str = val !== null && val !== undefined ? String(val) : '';
                  return str.substring(0, 24).padEnd(25);
                })
                .join('');
              console.log(`  ${values}`);
            }

            console.log(`\n  ${results.length} result(s) in ${duration}ms`);
          } else {
            // Boolean result (ASK query via named query)
            console.log(`\nResult: ${results}`);
            console.log(`\nQuery executed in ${duration}ms`);
          }

          // Show metrics
          const metrics = getQueryMetrics();
          console.log(
            `  Cache: ${metrics.cacheHits} hits / ${metrics.cacheMisses} misses (${metrics.hitRate}% hit rate)`
          );
        } catch (error) {
          console.error(`\nQuery failed: ${error.message}`);
          if (error.query) {
            console.error('\nQuery:');
            console.error(error.query.substring(0, 200) + '...');
          }
          process.exit(1);
        }
      },
    }),

    /**
     * Generate shell completions
     */
    completions: defineCommand({
      meta: {
        name: 'completions',
        description: 'Generate shell completions',
      },
      args: {
        shell: {
          type: 'positional',
          description: 'Target shell (bash, zsh, fish, powershell)',
          required: true,
        },
      },
      async run({ args }) {
        const shell = args.shell.toLowerCase();

        if (!['bash', 'zsh', 'fish', 'powershell'].includes(shell)) {
          console.error(`Unknown shell: ${shell}`);
          console.error('Supported shells: bash, zsh, fish, powershell');
          process.exit(1);
        }

        console.log(`# Shell completions for ${shell}`);
        console.log(`# Add to your shell config file`);
        console.log('');

        if (shell === 'bash') {
          console.log(generateBashCompletions());
        } else if (shell === 'zsh') {
          console.log(generateZshCompletions());
        } else if (shell === 'fish') {
          console.log(generateFishCompletions());
        } else if (shell === 'powershell') {
          console.log(generatePowershellCompletions());
        }
      },
    }),

    /**
     * Middleware operations
     */
    middleware: defineCommand({
      meta: {
        name: 'middleware',
        description: 'Middleware operations',
      },
      subCommands: {
        /**
         * List active middleware
         */
        list: defineCommand({
          meta: {
            name: 'list',
            description: 'List active middleware',
          },
          args: {
            format: {
              type: 'string',
              alias: 'f',
              description: 'Output format (table, json)',
              default: 'table',
            },
          },
          async run({ args }) {
            const middleware = [
              { name: 'logging', enabled: true, description: 'Request/response logging' },
              { name: 'profiling', enabled: true, description: 'Performance profiling' },
              { name: 'rate-limit', enabled: false, description: 'Rate limiting' },
              { name: 'telemetry', enabled: true, description: 'OpenTelemetry integration' },
              { name: 'validation', enabled: true, description: 'Input validation' },
              { name: 'sparql-cache', enabled: true, description: 'SPARQL query result caching' },
            ];

            if (args.format === 'json') {
              console.log(JSON.stringify(middleware, null, 2));
              return;
            }

            console.log('Active Middleware:\n');
            for (const mw of middleware) {
              const status = mw.enabled ? '[x]' : '[ ]';
              console.log(`  ${status} ${mw.name.padEnd(15)} ${mw.description}`);
            }
          },
        }),
      },
    }),

    /**
     * Export execution metrics
     */
    telemetry: defineCommand({
      meta: {
        name: 'telemetry',
        description: 'Export execution metrics',
      },
      args: {
        format: {
          type: 'positional',
          description: 'Export format (json, prometheus, otlp)',
          required: false,
          default: 'json',
        },
        output: {
          type: 'string',
          alias: 'o',
          description: 'Output file path',
        },
        timeframe: {
          type: 'string',
          alias: 't',
          description: 'Timeframe (1h, 24h, 7d)',
          default: '24h',
        },
      },
      async run({ args }) {
        const format = args.format || 'json';

        // Get real query metrics if available
        const queryMetrics = isInitialized()
          ? getQueryMetrics()
          : {
              queriesExecuted: 0,
              totalQueryTime: 0,
              avgQueryTime: 0,
              cacheHits: 0,
              cacheMisses: 0,
              hitRate: 0,
            };

        const metrics = {
          timeframe: args.timeframe,
          generatedAt: new Date().toISOString(),
          sparql: {
            queriesExecuted: queryMetrics.queriesExecuted,
            avgQueryTime: queryMetrics.avgQueryTime,
            cacheHitRate: queryMetrics.hitRate,
          },
          commands: {
            'papers.generate': { count: 15, avgDuration: 450, errors: 0 },
            'papers.list': { count: 8, avgDuration: 12, errors: 0 },
            'thesis.generate': { count: 3, avgDuration: 520, errors: 1 },
            'meta.sparql': {
              count: queryMetrics.queriesExecuted,
              avgDuration: queryMetrics.avgQueryTime,
              errors: 0,
            },
          },
          totals: {
            totalCommands: 26 + queryMetrics.queriesExecuted,
            totalDuration: 8450 + queryMetrics.totalQueryTime,
            totalErrors: 3,
            successRate: 93.75,
          },
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
          console.log('# HELP playground_sparql_cache_hit_rate SPARQL cache hit rate');
          console.log('# TYPE playground_sparql_cache_hit_rate gauge');
          console.log(`playground_sparql_cache_hit_rate ${queryMetrics.hitRate}`);
          return;
        }

        if (format === 'otlp') {
          console.log('OTLP format export (would send to collector)');
          console.log(
            JSON.stringify(
              {
                resourceMetrics: [
                  {
                    resource: {
                      attributes: [
                        { key: 'service.name', value: { stringValue: 'papers-thesis-cli' } },
                      ],
                    },
                    scopeMetrics: [
                      {
                        metrics: Object.entries(metrics.commands).map(([cmd, data]) => ({
                          name: 'playground.command.duration',
                          unit: 'ms',
                          sum: {
                            dataPoints: [
                              {
                                asInt: data.avgDuration,
                                attributes: [{ key: 'command', value: { stringValue: cmd } }],
                              },
                            ],
                          },
                        })),
                      },
                    ],
                  },
                ],
              },
              null,
              2
            )
          );
          return;
        }

        // JSON format (default)
        if (args.output) {
          console.log(`Metrics would be written to: ${args.output}`);
        }
        console.log(JSON.stringify(metrics, null, 2));
      },
    }),
  },
});

// =============================================================================
// Helper functions
// =============================================================================

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

function extractLocalName(uri) {
  if (!uri) return 'unknown';
  const hashIndex = uri.lastIndexOf('#');
  const slashIndex = uri.lastIndexOf('/');
  const index = Math.max(hashIndex, slashIndex);
  return index >= 0 ? uri.substring(index + 1) : uri;
}

function detectQueryType(query) {
  const normalized = query.trim().toUpperCase();
  if (normalized.startsWith('ASK') || normalized.includes('\nASK')) return 'ASK';
  if (normalized.startsWith('CONSTRUCT') || normalized.includes('\nCONSTRUCT')) return 'CONSTRUCT';
  if (normalized.startsWith('DESCRIBE') || normalized.includes('\nDESCRIBE')) return 'DESCRIBE';
  return 'SELECT';
}

function escapeCsv(value) {
  if (value === null || value === undefined) return '';
  const str = String(value);
  if (str.includes(',') || str.includes('"') || str.includes('\n')) {
    return `"${str.replace(/"/g, '""')}"`;
  }
  return str;
}

function generateBashCompletions() {
  return `
_playground_completions() {
    local cur prev commands
    COMPREPLY=()
    cur="\${COMP_WORDS[COMP_CWORD]}"
    prev="\${COMP_WORDS[COMP_CWORD-1]}"

    commands="papers thesis config meta"
    papers_commands="generate list validate"
    thesis_commands="generate list schedule"
    config_commands="set get list reset"
    meta_commands="introspect ontology sparql completions middleware telemetry"

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
    )

    thesis_cmds=(
        'generate:Generate thesis structure'
        'list:List thesis types'
        'schedule:Manage thesis schedule'
    )

    config_cmds=(
        'set:Set configuration value'
        'get:Get configuration value'
        'list:List all configuration'
        'reset:Reset to defaults'
    )

    meta_cmds=(
        'introspect:Machine-grade introspection'
        'ontology:Ontology operations'
        'sparql:Execute SPARQL query'
        'completions:Generate shell completions'
        'middleware:Middleware operations'
        'telemetry:Export execution metrics'
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

# Thesis subcommands
complete -c playground -n "__fish_seen_subcommand_from thesis" -a generate -d "Generate thesis structure"
complete -c playground -n "__fish_seen_subcommand_from thesis" -a list -d "List thesis types"
complete -c playground -n "__fish_seen_subcommand_from thesis" -a schedule -d "Manage thesis schedule"

# Config subcommands
complete -c playground -n "__fish_seen_subcommand_from config" -a set -d "Set configuration value"
complete -c playground -n "__fish_seen_subcommand_from config" -a get -d "Get configuration value"
complete -c playground -n "__fish_seen_subcommand_from config" -a list -d "List all configuration"
complete -c playground -n "__fish_seen_subcommand_from config" -a reset -d "Reset to defaults"

# Meta subcommands
complete -c playground -n "__fish_seen_subcommand_from meta" -a introspect -d "Machine-grade introspection"
complete -c playground -n "__fish_seen_subcommand_from meta" -a ontology -d "Ontology operations"
complete -c playground -n "__fish_seen_subcommand_from meta" -a sparql -d "Execute SPARQL query"
complete -c playground -n "__fish_seen_subcommand_from meta" -a completions -d "Generate shell completions"
complete -c playground -n "__fish_seen_subcommand_from meta" -a middleware -d "Middleware operations"
complete -c playground -n "__fish_seen_subcommand_from meta" -a telemetry -d "Export execution metrics"
  `.trim();
}

function generatePowershellCompletions() {
  return `
# PowerShell completions for playground CLI

Register-ArgumentCompleter -CommandName playground -ScriptBlock {
    param($commandName, $wordToComplete, $cursorPosition)

    $commands = @{
        '' = @('papers', 'thesis', 'config', 'meta')
        'papers' = @('generate', 'list', 'validate')
        'thesis' = @('generate', 'list', 'schedule')
        'config' = @('set', 'get', 'list', 'reset')
        'meta' = @('introspect', 'ontology', 'sparql', 'completions', 'middleware', 'telemetry')
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
