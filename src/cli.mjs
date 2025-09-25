#!/usr/bin/env node

/**
 * UNRDF CLI - Production Command Line Interface
 * 
 * A comprehensive CLI for the UNRDF framework that exposes all composable
 * functionality through a clean command-line interface using Citty.
 * 
 * @fileoverview Production-ready CLI for UNRDF framework operations
 */

import { defineCommand, runMain } from 'citty';
import { initStore } from './index.mjs';
import { 
  useStore, useGraph, useTurtle, useValidator, 
  useZod, useJSONLD, useMetrics, useDelta, usePrefixes
} from './composables/index.mjs';
import {
  generateId, generateUUID, generateHashId,
  expandCurie, shrinkIri
} from './utils/index.mjs';
import { readFile, writeFile, access } from 'node:fs/promises';
import { resolve, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { defineHook, evaluateHook } from './hooks.mjs';
import * as hookCommands from './cli/knowledge-hooks.mjs';

const __dirname = dirname(fileURLToPath(import.meta.url));

/**
 * Helper function to wrap CLI commands with store context
 * @param {Function} commandFn - The command function to wrap
 * @returns {Function} Wrapped command function
 */
function withContext(commandFn) {
  return async (ctx) => {
    try {
      const config = await loadConfig();
      const runApp = initStore([], { baseIRI: config.baseIRI });
      await runApp(() => commandFn(ctx));
    } catch (error) {
      console.error(`❌ Error: ${error.message}`);
      process.exit(1);
    }
  };
}

/**
 * Load configuration from unrdf.config.mjs if it exists
 * @returns {Promise<Object>} Configuration object
 */
async function loadConfig() {
  const configPath = resolve(process.cwd(), 'unrdf.config.mjs');
  try {
    await access(configPath);
    const config = await import(configPath);
    return config.default || {};
  } catch {
    return {
      baseIRI: process.env.UNRDF_BASE_IRI || 'http://example.org/',
      prefixes: process.env.UNRDF_PREFIXES ? JSON.parse(process.env.UNRDF_PREFIXES) : {
        'ex': 'http://example.org/',
        'foaf': 'http://xmlns.com/foaf/0.1/',
        'schema': 'https://schema.org/',
        'rdf': 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
        'rdfs': 'http://www.w3.org/2000/01/rdf-schema#',
        'owl': 'http://www.w3.org/2002/07/owl#'
      },
      validation: {
        strict: true,
        validateOnLoad: true
      }
    };
  }
}

// Main CLI command
const main = defineCommand({
  meta: {
    name: 'unrdf',
    version: '1.0.0',
    description: 'UNRDF - Opinionated composable framework for RDF knowledge operations'
  },
  subCommands: {
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
        }
      },
      async run(ctx) {
        try {
          console.log('🔄 Parsing RDF data...');
          
          const config = await loadConfig();
          const runApp = initStore([], { baseIRI: config.baseIRI });
          
          await runApp(async () => {
            const store = useStore();
            const turtle = await useTurtle();
            const metrics = await useMetrics();
            
            // Read input data
            let inputData;
            try {
              inputData = await readFile(ctx.args.input, 'utf-8');
            } catch (error) {
              console.error(`❌ File not found: ${ctx.args.input}`);
              process.exit(1);
            }
            
            // Parse based on format
            let quads;
            const startTime = Date.now();
            
            const format = ctx.args.format || 'turtle';
            switch (format) {
              case 'turtle':
                quads = await turtle.parse(inputData);
                break;
            case 'json-ld':
              const jsonLd = useJSONLD();
              quads = await jsonLd.fromJSONLD(JSON.parse(inputData));
              break;
            case 'n-quads':
              const nquads = await import('./composables/use-n-quads.mjs');
              quads = await nquads.useNQuads().parse(inputData);
              break;
            default:
              console.error(`❌ Unsupported format: ${format}`);
              process.exit(1);
          }
          
          // Add to store
          store.add(...quads);
          
          const endTime = Date.now();
          const duration = endTime - startTime;
          
          console.log(`✅ Parsed ${quads.length} triples successfully in ${duration}ms`);
          
          if (ctx.args.stats) {
            const stats = metrics.analyzeQuads(quads);
            console.log('📊 Parse Summary:');
            console.log(`  - Triples: ${stats.tripleCount}`);
            console.log(`  - Subjects: ${stats.subjectCount}`);
            console.log(`  - Predicates: ${stats.predicateCount}`);
            console.log(`  - Objects: ${stats.objectCount}`);
            console.log(`  - Literals: ${stats.literalCount}`);
            console.log(`  - IRIs: ${stats.iriCount}`);
            console.log(`  - Blank nodes: ${stats.blankNodeCount}`);
          }
          
            if (ctx.args.output) {
              const serialized = await turtle.serialize();
              await writeFile(ctx.args.output, serialized);
              console.log(`📄 Output written to ${ctx.args.output}`);
            }
          });
          
        } catch (error) {
          console.error(`❌ Parse error: ${error.message}`);
          process.exit(1);
        }
      }
    }),

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
        }
      },
      async run(ctx) {
        try {
          console.log('🔍 Executing SPARQL query...');
          
          const config = await loadConfig();
          const runApp = initStore([], { baseIRI: config.baseIRI });
          
          await runApp(async () => {
            const store = useStore();
            const graph = useGraph();
            const turtle = await useTurtle();
          
          // Load data
          const inputData = await readFile(ctx.args.input, 'utf-8');
          const quads = await turtle.parse(inputData);
          store.add(...quads);
          
          // Get query
          let query;
          if (ctx.args.query) {
            query = ctx.args.query;
          } else if (ctx.args['query-file']) {
            query = await readFile(ctx.args['query-file'], 'utf-8');
          } else {
            console.error('❌ Query required: use --query or --query-file');
            process.exit(1);
          }
          
          // Execute query
          const results = await graph.select(query);
          
          // Format output
          let output;
          const format = ctx.args.format || 'table';
          switch (format) {
            case 'json':
              output = JSON.stringify(results, null, 2);
              break;
            case 'csv':
              if (results.length === 0) {
                output = '';
              } else {
                const headers = Object.keys(results[0]);
                output = headers.join(',') + '\n' + 
                        results.map(row => headers.map(h => row[h] || '').join(',')).join('\n');
              }
              break;
            case 'table':
            default:
              if (results.length === 0) {
                output = 'No results found.';
              } else {
                const headers = Object.keys(results[0]);
                const colWidths = headers.map(h => Math.max(h.length, ...results.map(r => String(r[h] || '').length)));
                output = headers.map((h, i) => h.padEnd(colWidths[i])).join(' | ') + '\n' +
                        headers.map((_, i) => '-'.repeat(colWidths[i])).join('-+-') + '\n' +
                        results.map(row => headers.map((h, i) => String(row[h] || '').padEnd(colWidths[i])).join(' | ')).join('\n');
              }
              break;
          }
          
            console.log(output);
            
            if (ctx.args.output) {
              await writeFile(ctx.args.output, output);
              console.log(`📄 Results written to ${ctx.args.output}`);
            }
          });
          
        } catch (error) {
          console.error(`❌ Query error: ${error.message}`);
          process.exit(1);
        }
      }
    }),

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
        }
      },
      async run(ctx) {
        try {
          console.log('✅ Validating RDF data...');
          
          const config = await loadConfig();
          const runApp = initStore([], { baseIRI: config.baseIRI });
          
          await runApp(async () => {
            const store = useStore();
            const turtle = await useTurtle();
            const validator = await useValidator();
          
          // Load data
          const dataContent = await readFile(ctx.args.data, 'utf-8');
          const dataQuads = await turtle.parse(dataContent);
          store.add(...dataQuads);
          
          // Load shapes
          const shapeContent = await readFile(ctx.args.shape, 'utf-8');
          const shapeQuads = await turtle.parse(shapeContent);
          
          // Validate
          const report = await validator.validate(store.store, shapeQuads);
          
          if (report.conforms) {
            console.log('✅ Validation passed');
          } else {
            console.log('❌ Validation failed');
            console.log(`Found ${report.results.length} violations:`);
            for (const result of report.results) {
              console.log(`  - ${result.message} (${result.severity})`);
            }
          }
          
            if (ctx.args.output) {
              await writeFile(ctx.args.output, JSON.stringify(report, null, 2));
              console.log(`📄 Validation report written to ${ctx.args.output}`);
            }
          });
          
        } catch (error) {
          console.error(`❌ Validation error: ${error.message}`);
          process.exit(1);
        }
      }
    }),

    convert: defineCommand({
      meta: {
        name: 'convert',
        description: 'Convert between RDF and structured data formats'
      },
      args: {
        input: {
          type: 'positional',
          description: 'Input file path',
          required: true
        }
      },
      async run(ctx) {
        try {
          console.log('🔄 Converting data...');
          
          const config = await loadConfig();
          const runApp = initStore([], { baseIRI: config.baseIRI });
          
          await runApp(async () => {
            const store = useStore();
            const turtle = await useTurtle();
            const jsonLd = useJsonLd();
          
          // Load input data
          const inputData = await readFile(ctx.args.input, 'utf-8');
          let quads;
          
          const fromFormat = ctx.args.from || 'turtle';
          switch (fromFormat) {
            case 'turtle':
              quads = await turtle.parse(inputData);
              break;
            case 'json-ld':
              quads = await jsonLd.fromJSONLD(JSON.parse(inputData));
              break;
            default:
              console.error(`❌ Unsupported source format: ${fromFormat}`);
              process.exit(1);
          }
          
          store.add(...quads);
          
          // Convert to target format
          let output;
          const toFormat = ctx.args.to;
          switch (toFormat) {
            case 'turtle':
              output = await turtle.serialize();
              break;
            case 'json-ld':
              output = JSON.stringify(await jsonLd.toJSONLD(store.store), null, 2);
              break;
            case 'n-quads':
              const nquads = await import('./composables/use-n-quads.mjs');
              output = await nquads.useNQuads().serialize(store.store);
              break;
            default:
              console.error(`❌ Unsupported target format: ${toFormat}`);
              process.exit(1);
          }
          
            console.log(output);
            
            if (ctx.args.output) {
              await writeFile(ctx.args.output, output);
              console.log(`📄 Converted data written to ${ctx.args.output}`);
            }
          });
          
        } catch (error) {
          console.error(`❌ Conversion error: ${error.message}`);
          process.exit(1);
        }
      }
    }),

    init: defineCommand({
      meta: {
        name: 'init',
        description: 'Initialize a new UNRDF project'
      },
      args: {
        name: {
          type: 'positional',
          description: 'Project name',
          required: true
        }
      },
      async run(ctx) {
        try {
          console.log(`🚀 Initializing UNRDF project: ${ctx.args.name}`);
          
          const projectDir = resolve(process.cwd(), ctx.args.name);
          const { mkdir, writeFile } = await import('node:fs/promises');
          
          await mkdir(projectDir, { recursive: true });
          
          // Create package.json
          const packageJson = {
            name: ctx.args.name,
            version: '1.0.0',
            description: `UNRDF project: ${ctx.args.name}`,
            type: 'module',
            scripts: {
              dev: 'unrdf parse data.ttl',
              build: 'unrdf convert data.ttl --to json-ld --output dist/data.jsonld'
            },
            dependencies: {
              'unrdf': '^1.0.0'
            }
          };
          
          await writeFile(
            resolve(projectDir, 'package.json'),
            JSON.stringify(packageJson, null, 2)
          );
          
          // Create unrdf.config.mjs
          const configContent = `export default {
  baseIRI: 'http://example.org/',
  prefixes: {
    'ex': 'http://example.org/',
    'foaf': 'http://xmlns.com/foaf/0.1/',
    'schema': 'https://schema.org/'
  },
  validation: {
    strict: true,
    validateOnLoad: true
  }
};`;
          
          await writeFile(resolve(projectDir, 'unrdf.config.mjs'), configContent);
          
          // Create sample data
          const sampleData = `@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:person1 a foaf:Person ;
  foaf:name "Alice" ;
  foaf:age 30 .

ex:person2 a foaf:Person ;
  foaf:name "Bob" ;
  foaf:age 25 .`;
          
          await writeFile(resolve(projectDir, 'data.ttl'), sampleData);
          
          console.log(`✅ Project initialized in ${projectDir}`);
          console.log('📁 Created files:');
          console.log('  - package.json');
          console.log('  - unrdf.config.mjs');
          console.log('  - data.ttl');
          
        } catch (error) {
          console.error(`❌ Initialization error: ${error.message}`);
          process.exit(1);
        }
      }
    }),

    id: defineCommand({
      meta: {
        name: 'id',
        description: 'Generate various types of IDs'
      },
      subCommands: {
        uuid: defineCommand({
          meta: {
            name: 'uuid',
            description: 'Generate UUIDs'
          },
          async run(ctx) {
            const count = ctx.args.count || 1;
            for (let i = 0; i < count; i++) {
              console.log(generateUUID());
            }
          }
        }),

        hash: defineCommand({
          meta: {
            name: 'hash',
            description: 'Generate hash-based IDs'
          },
          args: {
            input: {
              type: 'positional',
              description: 'Input string to hash',
              required: true
            }
          },
          async run(ctx) {
            const hashId = generateHashId(ctx.args.input);
            console.log(hashId);
          }
        }),

        generate: defineCommand({
          meta: {
            name: 'generate',
            description: 'Generate generic IDs'
          },
          async run(ctx) {
            const id = generateId(ctx.args.prefix);
            console.log(id);
          }
        })
      }
    }),

    store: defineCommand({
      meta: {
        name: 'store',
        description: 'Interact with RDF store'
      },
      subCommands: {
        stats: defineCommand({
          meta: {
            name: 'stats',
            description: 'Show store statistics'
          },
          args: {
            input: {
              type: 'positional',
              description: 'Input file path',
              required: true
            }
          },
          async run(ctx) {
            const store = useStore();
            const turtle = await useTurtle();
            const metrics = await useMetrics();
            
            const data = await readFile(ctx.args.input, 'utf-8');
            const quads = await turtle.parse(data);
            
            const stats = metrics.analyzeQuads(quads);
            console.log('📊 Store Statistics:');
            console.log(`  - Total triples: ${stats.tripleCount}`);
            console.log(`  - Unique subjects: ${stats.subjectCount}`);
            console.log(`  - Unique predicates: ${stats.predicateCount}`);
            console.log(`  - Unique objects: ${stats.objectCount}`);
            console.log(`  - Literals: ${stats.literalCount}`);
            console.log(`  - IRIs: ${stats.iriCount}`);
            console.log(`  - Blank nodes: ${stats.blankNodeCount}`);
          }
        }),

        clear: defineCommand({
          meta: {
            name: 'clear',
            description: 'Clear store contents'
          },
          async run() {
            const store = useStore();
            store.removeMatches();
            console.log('✅ Store cleared');
          }
        })
      }
    }),

    prefix: defineCommand({
      meta: {
        name: 'prefix',
        description: 'Manage namespace prefixes'
      },
      subCommands: {
        list: defineCommand({
          meta: {
            name: 'list',
            description: 'List all known prefixes'
          },
          async run() {
            const config = await loadConfig();
            const prefixes = usePrefixes(config.prefixes);
            
            console.log('📋 Known prefixes:');
            for (const [prefix, namespace] of Object.entries(config.prefixes)) {
              console.log(`  ${prefix}: ${namespace}`);
            }
          }
        }),

        expand: defineCommand({
          meta: {
            name: 'expand',
            description: 'Expand CURIEs to full IRIs'
          },
          args: {
            curie: {
              type: 'positional',
              description: 'CURIE to expand',
              required: true
            }
          },
          async run(ctx) {
            const config = await loadConfig();
            const expanded = expandCurie(ctx.args.curie, config.prefixes);
            console.log(`🔗 Expanded: ${expanded}`);
          }
        }),

        shrink: defineCommand({
          meta: {
            name: 'shrink',
            description: 'Shrink IRIs to CURIEs'
          },
          args: {
            iri: {
              type: 'positional',
              description: 'IRI to shrink',
              required: true
            }
          },
          async run(ctx) {
            const config = await loadConfig();
            const shrunk = shrinkIri(ctx.args.iri, config.prefixes);
            console.log(`🔗 Shrunk: ${shrunk}`);
          }
        })
      }
    }),

    metrics: defineCommand({
      meta: {
        name: 'metrics',
        description: 'Analyze RDF metrics'
      },
      args: {
        input: {
          type: 'positional',
          description: 'Input file path',
          required: true
        }
      },
      async run(ctx) {
        const metrics = await useMetrics();
        const turtle = await useTurtle();
        
        const data = await readFile(ctx.args.input, 'utf-8');
        const quads = await turtle.parse(data);
        
        const analysis = metrics.analyzeQuads(quads);
        
        console.log('📊 RDF Metrics:');
        console.log(`  - Total triples: ${analysis.tripleCount}`);
        console.log(`  - Unique subjects: ${analysis.subjectCount}`);
        console.log(`  - Unique predicates: ${analysis.predicateCount}`);
        console.log(`  - Unique objects: ${analysis.objectCount}`);
        console.log(`  - Literals: ${analysis.literalCount}`);
        console.log(`  - IRIs: ${analysis.iriCount}`);
        console.log(`  - Blank nodes: ${analysis.blankNodeCount}`);
        
        if (ctx.args.detailed) {
          console.log('\n🔍 Detailed Analysis:');
          console.log(`  - Average degree: ${analysis.averageDegree?.toFixed(2) || 'N/A'}`);
          console.log(`  - Density: ${analysis.density?.toFixed(4) || 'N/A'}`);
          if (analysis.topPredicates) {
            console.log(`  - Most common predicates:`);
            for (const [predicate, count] of analysis.topPredicates.slice(0, 5)) {
              console.log(`    - ${predicate}: ${count}`);
            }
          }
        }
      }
    }),

    delta: defineCommand({
      meta: {
        name: 'delta',
        description: 'Compare RDF datasets'
      },
      args: {
        source: {
          type: 'positional',
          description: 'Source file path',
          required: true
        },
        target: {
          type: 'positional',
          description: 'Target file path',
          required: true
        }
      },
      async run(ctx) {
        const delta = useDelta();
        const turtle = await useTurtle();

        const sourceData = await readFile(ctx.args.source, 'utf-8');
        const targetData = await readFile(ctx.args.target, 'utf-8');

        const sourceQuads = await turtle.parse(sourceData);
        const targetQuads = await turtle.parse(targetData);

        const diff = delta.diff(sourceQuads, targetQuads);

        console.log('📊 Dataset Comparison:');
        console.log(`  - Added triples: ${diff.added.length}`);
        console.log(`  - Removed triples: ${diff.removed.length}`);
        console.log(`  - Unchanged triples: ${diff.unchanged.length}`);

        if (diff.added.length > 0) {
          console.log('\n➕ Added triples:');
          for (const quad of diff.added.slice(0, 5)) {
            console.log(`  + ${quad.subject.value} ${quad.predicate.value} ${quad.object.value}`);
          }
          if (diff.added.length > 5) {
            console.log(`  ... and ${diff.added.length - 5} more`);
          }
        }

        if (diff.removed.length > 0) {
          console.log('\n➖ Removed triples:');
          for (const quad of diff.removed.slice(0, 5)) {
            console.log(`  - ${quad.subject.value} ${quad.predicate.value} ${quad.object.value}`);
          }
          if (diff.removed.length > 5) {
            console.log(`  ... and ${diff.removed.length - 5} more`);
          }
        }

        if (ctx.args.output) {
          const report = {
            summary: {
              added: diff.added.length,
              removed: diff.removed.length,
              unchanged: diff.unchanged.length
            },
            added: diff.added.map(q => ({
              subject: q.subject.value,
              predicate: q.predicate.value,
              object: q.object.value
            })),
            removed: diff.removed.map(q => ({
              subject: q.subject.value,
              predicate: q.predicate.value,
              object: q.object.value
            }))
          };
          await writeFile(ctx.args.output, JSON.stringify(report, null, 2));
          console.log(`\n📄 Delta report written to ${ctx.args.output}`);
        }
      }
    }),

    hook: defineCommand({
      meta: {
        name: 'hook',
        description: 'Knowledge Hooks management and evaluation'
      },
      subCommands: {
        eval: defineCommand({
          meta: {
            name: 'eval',
            description: 'Evaluate a Knowledge Hook'
          },
          args: {
            hook: {
              type: 'positional',
              description: 'Path to hook definition file or hook ID',
              required: true
            },
            data: {
              type: 'string',
              description: 'Path to RDF data directory or file'
            },
            persist: {
              type: 'boolean',
              description: 'Persist receipts and baselines',
              default: true
            },
            output: {
              type: 'string',
              description: 'Output format (json, jsonld, nquads, turtle)',
              default: 'json'
            }
          },
          async run(ctx) {
            await hookCommands.evalHook(ctx.args);
          }
        }),

        create: defineCommand({
          meta: {
            name: 'create',
            description: 'Create a new hook definition from template'
          },
          args: {
            output: {
              type: 'positional',
              description: 'Output file path',
              required: true
            },
            template: {
              type: 'string',
              description: 'Template type (health, compliance, drift)',
              default: 'health'
            }
          },
          async run(ctx) {
            await hookCommands.createHook(ctx.args);
          }
        }),

        templates: defineCommand({
          meta: {
            name: 'templates',
            description: 'List available hook templates'
          },
          async run() {
            hookCommands.listTemplates();
          }
        }),

        validate: defineCommand({
          meta: {
            name: 'validate',
            description: 'Validate a hook definition'
          },
          args: {
            hook: {
              type: 'positional',
              description: 'Path to hook definition file',
              required: true
            }
          },
          async run(ctx) {
            await hookCommands.validateHook(ctx.args);
          }
        }),

        plan: defineCommand({
          meta: {
            name: 'plan',
            description: 'Show evaluation plan for a hook (without execution)'
          },
          args: {
            hook: {
              type: 'positional',
              description: 'Path to hook definition file or hook ID',
              required: true
            }
          },
          async run(ctx) {
            try {
              const hookContent = await readFile(ctx.args.hook, 'utf-8');
              const hookConfig = JSON.parse(hookContent);
              const hook = defineHook(hookConfig);

              console.log('🔍 Hook Evaluation Plan:');
              console.log(`  Hook ID: ${hook.id}`);
              console.log(`  Name: ${hook.name || 'N/A'}`);
              console.log(`  Description: ${hook.description || 'N/A'}`);
              console.log(`  Combine Logic: ${hook.combine}`);
              console.log(`  Predicates: ${hook.predicates.length}`);

              console.log('\n🎯 Predicate Plan:');
              hook.predicates.forEach((pred, i) => {
                console.log(`  ${i + 1}. ${pred.kind}: ${JSON.stringify(pred.spec)}`);
              });

              console.log('\n📊 Query Plan:');
              if (hook.select) {
                console.log(`  Type: SPARQL SELECT`);
                console.log(`  Query: ${hook.select.substring(0, 100)}${hook.select.length > 100 ? '...' : ''}`);
              } else {
                console.log(`  Type: Direct ASK (no base query)`);
              }

            } catch (error) {
              console.error(`❌ Plan failed: ${error.message}`);
              process.exit(1);
            }
          }
        }),

        stats: defineCommand({
          meta: {
            name: 'stats',
            description: 'Show hook evaluation statistics'
          },
          args: {
            receipts: {
              type: 'string',
              description: 'Path to receipts directory'
            }
          },
          async run(ctx) {
            await hookCommands.showStats(ctx.args);
          }
        })
      }
    })
  }
});

// Run the CLI
runMain(main);