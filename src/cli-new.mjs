#!/usr/bin/env node

/**
 * @file UNRDF CLI - Rewritten Modular Implementation
 * @module cli-new
 *
 * @description
 * Production-ready CLI for UNRDF framework with clean separation of concerns,
 * comprehensive error handling, and full testability using citty-test-utils patterns.
 *
 * Key improvements:
 * - Modular command structure for maintainability
 * - Dependency injection for testability
 * - Comprehensive error handling with proper exit codes
 * - Configuration management with validation
 * - Hook coordination for swarm integration
 * - Performance optimized with the critical 20% focus
 */

import { defineCommand, runMain } from 'citty';
import { withContext } from './cli/utils/context-wrapper.mjs';
import { initializeTracer, shutdownTracer } from './cli/utils/otel-tracer.mjs';
import {
  parseCommand,
  queryCommand,
  validateCommand,
  hookEvalCommand,
  hookListCommand,
  hookCreateCommand,
  hookGetCommand,
  hookHistoryCommand,
  graphListCommand,
  graphGetCommand,
  graphCreateCommand,
  graphDeleteCommand,
  graphImportCommand,
  graphExportCommand,
  graphValidateCommand,
  graphStatsCommand,
  sidecarStatusCommand,
  sidecarHealthCommand,
  sidecarMetricsCommand,
  sidecarConfigGetCommand,
  sidecarConfigSetCommand,
  storeImportCommand,
  storeExportCommand,
  storeQueryCommand,
  policyApplyCommand,
  policyListCommand,
  policyGetCommand
} from './cli/commands/index.mjs';

/**
 * Main CLI command with subcommands
 */
const main = defineCommand({
  meta: {
    name: 'unrdf',
    version: '2.0.0',
    description: 'UNRDF - Opinionated composable framework for RDF knowledge operations'
  },
  subCommands: {
    /**
     * Hook commands - Knowledge hook management
     */
    hook: defineCommand({
      meta: {
        name: 'hook',
        description: 'Manage knowledge hooks'
      },
      subCommands: {
        eval: defineCommand({
          meta: { name: 'eval', description: 'Evaluate knowledge hook' },
          args: {
            hook: {
              type: 'positional',
              description: 'Hook file path',
              required: true
            },
            data: {
              type: 'string',
              description: 'RDF data file path'
            },
            format: {
              type: 'string',
              description: 'Output format (json, table)',
              default: 'table'
            },
            output: {
              type: 'string',
              description: 'Output file path'
            },
            verbose: {
              type: 'boolean',
              default: false
            }
          },
          run: withContext(hookEvalCommand, 'hook eval')
        }),
        list: defineCommand({
          meta: { name: 'list', description: 'List knowledge hooks' },
          args: {
            policyPack: {
              type: 'string',
              description: 'Filter by policy pack'
            },
            format: {
              type: 'string',
              description: 'Output format (json, table)',
              default: 'table'
            }
          },
          run: withContext(hookListCommand, 'hook list')
        }),
        create: defineCommand({
          meta: { name: 'create', description: 'Create hook from SPARQL query file or template' },
          args: {
            name: {
              type: 'positional',
              description: 'Hook name',
              required: true
            },
            type: {
              type: 'positional',
              description: 'Hook type (sparql-ask, shacl, threshold)',
              required: true
            },
            file: {
              type: 'string',
              description: 'SPARQL query file path (for sparql-ask/threshold hooks)'
            },
            query: {
              type: 'string',
              description: 'Inline SPARQL query for sparql-ask/threshold hooks'
            },
            shapes: {
              type: 'string',
              description: 'Shapes file for SHACL hooks'
            },
            threshold: {
              type: 'number',
              description: 'Threshold value'
            },
            operator: {
              type: 'string',
              description: 'Threshold operator (gt, lt, eq)'
            },
            phase: {
              type: 'string',
              description: 'Hook phase (before, after)',
              default: 'before'
            },
            description: {
              type: 'string',
              description: 'Hook description'
            },
            output: {
              type: 'string',
              description: 'Output file path'
            },
            verbose: {
              type: 'boolean',
              description: 'Show generated hook definition',
              default: false
            }
          },
          run: withContext(hookCreateCommand, 'hook create')
        }),
        get: defineCommand({
          meta: { name: 'get', description: 'Get hook details' },
          args: {
            id: {
              type: 'positional',
              description: 'Hook ID',
              required: true
            }
          },
          run: withContext(hookGetCommand, 'hook get')
        }),
        history: defineCommand({
          meta: { name: 'history', description: 'Show hook execution history' },
          args: {
            id: {
              type: 'positional',
              description: 'Hook ID',
              required: true
            },
            limit: {
              type: 'number',
              description: 'Limit results',
              default: 100
            },
            firedOnly: {
              type: 'boolean',
              description: 'Show only fired events',
              default: false
            }
          },
          run: withContext(hookHistoryCommand, 'hook history')
        })
      }
    }),

    /**
     * Graph commands - RDF graph management
     */
    graph: defineCommand({
      meta: {
        name: 'graph',
        description: 'Manage RDF graphs'
      },
      subCommands: {
        list: defineCommand({
          meta: { name: 'list', description: 'List graphs' },
          args: {
            format: {
              type: 'string',
              description: 'Output format (json, table)',
              default: 'table'
            }
          },
          run: withContext(graphListCommand, 'graph list')
        }),
        get: defineCommand({
          meta: { name: 'get', description: 'Get graph details' },
          args: {
            name: {
              type: 'positional',
              description: 'Graph name',
              required: true
            }
          },
          run: withContext(graphGetCommand, 'graph get')
        }),
        create: defineCommand({
          meta: { name: 'create', description: 'Create graph' },
          args: {
            name: {
              type: 'positional',
              description: 'Graph name',
              required: true
            },
            import: {
              type: 'string',
              description: 'Import initial data from file'
            }
          },
          run: withContext(graphCreateCommand, 'graph create')
        }),
        delete: defineCommand({
          meta: { name: 'delete', description: 'Delete graph' },
          args: {
            name: {
              type: 'positional',
              description: 'Graph name',
              required: true
            },
            force: {
              type: 'boolean',
              description: 'Skip confirmation',
              default: false
            },
            backup: {
              type: 'string',
              description: 'Backup directory'
            }
          },
          run: withContext(graphDeleteCommand, 'graph delete')
        }),
        import: defineCommand({
          meta: { name: 'import', description: 'Import data to graph' },
          args: {
            file: {
              type: 'positional',
              description: 'Input file path',
              required: true
            },
            mode: {
              type: 'string',
              description: 'Import mode (append, replace)',
              default: 'append'
            },
            validate: {
              type: 'boolean',
              description: 'Validate before import',
              default: false
            }
          },
          run: withContext(graphImportCommand, 'graph import')
        }),
        export: defineCommand({
          meta: { name: 'export', description: 'Export graph data' },
          args: {
            name: {
              type: 'positional',
              description: 'Graph name',
              required: true
            },
            output: {
              type: 'string',
              description: 'Output file path'
            }
          },
          run: withContext(graphExportCommand, 'graph export')
        }),
        validate: defineCommand({
          meta: { name: 'validate', description: 'Validate graph' },
          args: {
            name: {
              type: 'positional',
              description: 'Graph name',
              required: true
            },
            policyPack: {
              type: 'string',
              description: 'Policy pack name',
              default: 'default'
            },
            strict: {
              type: 'boolean',
              description: 'Strict validation mode',
              default: false
            },
            report: {
              type: 'string',
              description: 'Write report to file'
            }
          },
          run: withContext(graphValidateCommand, 'graph validate')
        }),
        stats: defineCommand({
          meta: { name: 'stats', description: 'Show graph statistics' },
          args: {
            name: {
              type: 'positional',
              description: 'Graph name',
              required: true
            },
            detailed: {
              type: 'boolean',
              description: 'Show detailed statistics',
              default: false
            }
          },
          run: withContext(graphStatsCommand, 'graph stats')
        })
      }
    }),

    /**
     * Sidecar commands - KGC sidecar management
     */
    sidecar: defineCommand({
      meta: {
        name: 'sidecar',
        description: 'Manage KGC sidecar'
      },
      subCommands: {
        status: defineCommand({
          meta: { name: 'status', description: 'Check sidecar status' },
          args: {},
          run: withContext(sidecarStatusCommand, 'sidecar status')
        }),
        health: defineCommand({
          meta: { name: 'health', description: 'Run health check' },
          args: {},
          run: withContext(sidecarHealthCommand, 'sidecar health')
        }),
        metrics: defineCommand({
          meta: { name: 'metrics', description: 'Show sidecar metrics' },
          args: {
            watch: {
              type: 'boolean',
              description: 'Watch metrics',
              default: false
            },
            interval: {
              type: 'number',
              description: 'Update interval (ms)',
              default: 5000
            },
            metrics: {
              type: 'string',
              description: 'Comma-separated metric names'
            }
          },
          run: withContext(sidecarMetricsCommand, 'sidecar metrics')
        }),
        config: defineCommand({
          meta: { name: 'config', description: 'Manage sidecar config' },
          subCommands: {
            get: defineCommand({
              meta: { name: 'get', description: 'Get config value' },
              args: {
                key: {
                  type: 'positional',
                  description: 'Config key (dot notation)'
                }
              },
              run: withContext(sidecarConfigGetCommand, 'sidecar config get')
            }),
            set: defineCommand({
              meta: { name: 'set', description: 'Set config value' },
              args: {
                key: {
                  type: 'positional',
                  description: 'Config key',
                  required: true
                },
                value: {
                  type: 'positional',
                  description: 'Config value',
                  required: true
                }
              },
              run: withContext(sidecarConfigSetCommand, 'sidecar config set')
            })
          }
        })
      }
    }),

    /**
     * Parse command - Parse RDF data from various formats
     */
    parse: defineCommand({
      meta: {
        name: 'parse',
        description: 'Parse RDF data from various formats'
      },
      args: {
        input: {
          type: 'positional',
          description: 'Input file path',
          required: true
        },
        format: {
          type: 'string',
          description: 'Input format (turtle, n-quads)',
          default: 'turtle'
        },
        output: {
          type: 'string',
          description: 'Output file path'
        },
        verbose: {
          type: 'boolean',
          description: 'Enable verbose output',
          default: false
        }
      },
      run: withContext(parseCommand, 'parse')
    }),

    /**
     * Query command - Execute SPARQL queries
     */
    query: defineCommand({
      meta: {
        name: 'query',
        description: 'Query RDF data with SPARQL'
      },
      args: {
        input: {
          type: 'positional',
          description: 'Input file path',
          required: true
        },
        query: {
          type: 'string',
          description: 'SPARQL query string'
        },
        'query-file': {
          type: 'string',
          description: 'Path to SPARQL query file'
        },
        format: {
          type: 'string',
          description: 'Output format (table, json, csv)',
          default: 'table'
        },
        output: {
          type: 'string',
          description: 'Output file path'
        },
        verbose: {
          type: 'boolean',
          description: 'Enable verbose output',
          default: false
        }
      },
      run: withContext(queryCommand, 'query')
    }),

    /**
     * Validate command - Validate RDF against SHACL shapes
     */
    validate: defineCommand({
      meta: {
        name: 'validate',
        description: 'Validate RDF data against SHACL shapes'
      },
      args: {
        data: {
          type: 'positional',
          description: 'Data file path',
          required: true
        },
        shape: {
          type: 'string',
          description: 'SHACL shapes file path',
          required: true
        },
        output: {
          type: 'string',
          description: 'Output file path for validation report'
        },
        verbose: {
          type: 'boolean',
          description: 'Enable verbose output',
          default: false
        }
      },
      run: withContext(validateCommand, 'validate')
    }),

    /**
     * Store commands - RDF store operations
     */
    store: defineCommand({
      meta: {
        name: 'store',
        description: 'Manage RDF store operations'
      },
      subCommands: {
        import: defineCommand({
          meta: { name: 'import', description: 'Import RDF data to store' },
          args: {
            file: {
              type: 'positional',
              description: 'Input file path',
              required: true
            },
            graph: {
              type: 'string',
              description: 'Target graph name',
              default: 'default'
            },
            verbose: {
              type: 'boolean',
              default: false
            }
          },
          run: withContext(storeImportCommand, 'store import')
        }),
        export: defineCommand({
          meta: { name: 'export', description: 'Export graph data' },
          args: {
            graph: {
              type: 'positional',
              description: 'Graph name',
              required: true
            },
            output: {
              type: 'string',
              description: 'Output file path'
            },
            verbose: {
              type: 'boolean',
              default: false
            }
          },
          run: withContext(storeExportCommand, 'store export')
        }),
        query: defineCommand({
          meta: { name: 'query', description: 'Execute SPARQL query' },
          args: {
            sparql: {
              type: 'positional',
              description: 'SPARQL query string',
              required: true
            },
            format: {
              type: 'string',
              description: 'Output format (table, json)',
              default: 'table'
            },
            verbose: {
              type: 'boolean',
              default: false
            }
          },
          run: withContext(storeQueryCommand, 'store query')
        })
      }
    }),

    /**
     * Policy commands - Policy pack management
     */
    policy: defineCommand({
      meta: {
        name: 'policy',
        description: 'Manage policy packs'
      },
      subCommands: {
        apply: defineCommand({
          meta: { name: 'apply', description: 'Apply policy pack' },
          args: {
            file: {
              type: 'positional',
              description: 'Policy pack manifest file',
              required: true
            },
            verbose: {
              type: 'boolean',
              default: false
            }
          },
          run: withContext(policyApplyCommand, 'policy apply')
        }),
        list: defineCommand({
          meta: { name: 'list', description: 'List policy packs' },
          args: {
            format: {
              type: 'string',
              description: 'Output format (json, table)',
              default: 'table'
            }
          },
          run: withContext(policyListCommand, 'policy list')
        }),
        get: defineCommand({
          meta: { name: 'get', description: 'Get policy pack details' },
          args: {
            id: {
              type: 'positional',
              description: 'Policy pack ID or name',
              required: true
            },
            verbose: {
              type: 'boolean',
              default: false
            }
          },
          run: withContext(policyGetCommand, 'policy get')
        })
      }
    })
  }
});

// Initialize OTEL before running CLI
await initializeTracer();

// Handle process exit to flush traces
process.on('exit', () => {
  // Synchronous shutdown attempt
  if (process.env.OTEL_DEBUG) {
    console.log('[OTEL] Process exiting, attempting flush...');
  }
});

process.on('SIGINT', async () => {
  await shutdownTracer();
  process.exit(0);
});

process.on('SIGTERM', async () => {
  await shutdownTracer();
  process.exit(0);
});

// Ensure traces are flushed after command execution
const originalRunMain = runMain;
async function runMainWithTracing(command) {
  try {
    await originalRunMain(command);
  } finally {
    // Give tracer time to flush spans
    await shutdownTracer();
  }
}

// Run the CLI with tracing
runMainWithTracing(main);
