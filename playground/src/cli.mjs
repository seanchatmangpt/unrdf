#!/usr/bin/env node

/**
 * UNRDF CLI - Command Line Interface for RDF operations
 * 
 * This CLI demonstrates UNRDF functionality through command-line operations:
 * - Parse RDF data from various formats
 * - Query RDF graphs with SPARQL
 * - Validate RDF data against SHACL shapes
 * - Perform reasoning operations
 * - Convert between RDF and structured data
 */

import { createCommand, runMain } from 'citty';
import { useStore, useGraph, useTurtle, useValidator, useReasoner, useZod } from 'unrdf';
import { readFile, writeFile } from 'node:fs/promises';
import { z } from 'zod';

// Initialize the main command
const main = createCommand({
  meta: {
    name: 'unrdf-cli',
    version: '1.0.0',
    description: 'UNRDF Command Line Interface for RDF operations'
  },
  subCommands: {
    parse: parseCommand,
    query: queryCommand,
    validate: validateCommand,
    reason: reasonCommand,
    convert: convertCommand,
    help: helpCommand
  }
});

/**
 * Parse RDF data from various formats
 */
const parseCommand = createCommand({
  meta: {
    name: 'parse',
    description: 'Parse RDF data from various formats'
  },
  args: {
    input: {
      type: 'positional',
      description: 'Input file path or RDF data string',
      required: true
    },
    format: {
      type: 'string',
      description: 'Input format (turtle, n3, json-ld)',
      default: 'turtle'
    },
    output: {
      type: 'string',
      description: 'Output file path (optional)',
      alias: 'o'
    }
  },
  async run({ args }) {
    try {
      console.log('🔄 Parsing RDF data...');
      
      const store = useStore();
      const graph = useGraph(store);
      const turtle = useTurtle();
      
      // Read input data
      let inputData;
      try {
        inputData = await readFile(args.input, 'utf-8');
      } catch {
        // If file doesn't exist, treat as direct input
        inputData = args.input;
      }
      
      // Parse based on format
      let quads;
      switch (args.format.toLowerCase()) {
        case 'turtle':
        case 'ttl': {
          quads = await turtle.parse(inputData);
          break;
        }
        default: {
          throw new Error(`Unsupported format: ${args.format}`);
        }
      }
      
      await graph.addQuads(quads);
      
      console.log(`✅ Parsed ${quads.length} triples successfully`);
      
      // Output results
      if (args.output) {
        const serialized = await turtle.serialize(quads);
        await writeFile(args.output, serialized);
        console.log(`📄 Output written to ${args.output}`);
      } else {
        console.log('📊 Parse Summary:');
        console.log(`  - Triples: ${quads.length}`);
        console.log(`  - Subjects: ${new Set(quads.map(q => q.subject.value)).size}`);
        console.log(`  - Predicates: ${new Set(quads.map(q => q.predicate.value)).size}`);
      }
      
    } catch (error) {
      console.error('❌ Parse error:', error.message);
      process.exit(1);
    }
  }
});

/**
 * Query RDF data with SPARQL
 */
const queryCommand = createCommand({
  meta: {
    name: 'query',
    description: 'Query RDF data with SPARQL'
  },
  args: {
    input: {
      type: 'positional',
      description: 'Input RDF file path',
      required: true
    },
    query: {
      type: 'string',
      description: 'SPARQL query string',
      alias: 'q'
    },
    queryFile: {
      type: 'string',
      description: 'SPARQL query file path',
      alias: 'f'
    },
    format: {
      type: 'string',
      description: 'Output format (json, table, turtle)',
      default: 'table'
    }
  },
  async run({ args }) {
    try {
      console.log('🔍 Executing SPARQL query...');
      
      const store = useStore();
      const graph = useGraph(store);
      const turtle = useTurtle();
      
      // Load RDF data
      const inputData = await readFile(args.input, 'utf-8');
      const quads = await turtle.parse(inputData);
      await graph.addQuads(quads);
      
      // Get query
      let query;
      if (args.queryFile) {
        query = await readFile(args.queryFile, 'utf-8');
      } else if (args.query) {
        query = args.query;
      } else {
        throw new Error('Either --query or --query-file must be provided');
      }
      
      // Execute query
      const results = await graph.query(query);
      
      console.log('✅ Query executed successfully');
      
      // Format output
      switch (args.format.toLowerCase()) {
        case 'json': {
          const jsonResults = [];
          for await (const binding of results) {
            const obj = {};
            for (const [key, value] of binding) {
              obj[key] = value.value;
            }
            jsonResults.push(obj);
          }
          console.log(JSON.stringify(jsonResults, null, 2));
          break;
        }
          
        case 'table': {
          const tableResults = [];
          for await (const binding of results) {
            const row = {};
            for (const [key, value] of binding) {
              row[key] = value.value;
            }
            tableResults.push(row);
          }
          
          if (tableResults.length > 0) {
            const headers = Object.keys(tableResults[0]);
            console.log(headers.join('\t'));
            console.log(headers.map(() => '---').join('\t'));
            for (const row of tableResults) {
              console.log(headers.map(h => row[h] || '').join('\t'));
            }
          } else {
            console.log('No results found');
          }
          break;
        }
          
        default: {
          console.log('Query results:');
          for await (const binding of results) {
            console.log(binding.toString());
          }
        }
      }
      
    } catch (error) {
      console.error('❌ Query error:', error.message);
      process.exit(1);
    }
  }
});

/**
 * Validate RDF data against SHACL shapes
 */
const validateCommand = createCommand({
  meta: {
    name: 'validate',
    description: 'Validate RDF data against SHACL shapes'
  },
  args: {
    data: {
      type: 'positional',
      description: 'RDF data file path',
      required: true
    },
    shape: {
      type: 'string',
      description: 'SHACL shape file path',
      required: true
    },
    output: {
      type: 'string',
      description: 'Output file for validation report',
      alias: 'o'
    }
  },
  async run({ args }) {
    try {
      console.log('✅ Validating RDF data...');
      
      const store = useStore();
      const graph = useGraph(store);
      const turtle = useTurtle();
      const validator = useValidator();
      
      // Load data and shapes
      const dataContent = await readFile(args.data, 'utf-8');
      const shapeContent = await readFile(args.shape, 'utf-8');
      
      const dataQuads = await turtle.parse(dataContent);
      const shapeQuads = await turtle.parse(shapeContent);
      
      await graph.addQuads(dataQuads);
      
      // Validate
      const results = await validator.validate(dataQuads, shapeQuads);
      
      console.log(`📊 Validation Results:`);
      console.log(`  - Conforms: ${results.conforms}`);
      console.log(`  - Violations: ${results.results.length}`);
      
      if (results.results.length > 0) {
        console.log('\n❌ Validation Violations:');
        for (const result of results.results) {
          console.log(`  - Focus: ${result.focusNode.value}`);
          console.log(`    Message: ${result.resultMessage[0].value}`);
          console.log(`    Severity: ${result.severity.value}`);
          console.log('');
        }
      } else {
        console.log('\n✅ All data conforms to the SHACL shape!');
      }
      
      // Output report if requested
      if (args.output) {
        const report = {
          conforms: results.conforms,
          violations: results.results.length,
          results: results.results.map(r => ({
            focusNode: r.focusNode.value,
            message: r.resultMessage[0].value,
            severity: r.severity.value
          }))
        };
        await writeFile(args.output, JSON.stringify(report, null, 2));
        console.log(`📄 Validation report written to ${args.output}`);
      }
      
    } catch (error) {
      console.error('❌ Validation error:', error.message);
      process.exit(1);
    }
  }
});

/**
 * Perform reasoning operations
 */
const reasonCommand = createCommand({
  meta: {
    name: 'reason',
    description: 'Perform OWL reasoning on RDF data'
  },
  args: {
    input: {
      type: 'positional',
      description: 'Input RDF file path',
      required: true
    },
    output: {
      type: 'string',
      description: 'Output file for reasoned data',
      alias: 'o'
    }
  },
  async run({ args }) {
    try {
      console.log('🧠 Performing reasoning...');
      
      const store = useStore();
      const graph = useGraph(store);
      const turtle = useTurtle();
      const reasoner = useReasoner();
      
      // Load data
      const inputData = await readFile(args.input, 'utf-8');
      const quads = await turtle.parse(inputData);
      await graph.addQuads(quads);
      
      // Perform reasoning
      const reasonedQuads = await reasoner.reason(quads);
      
      console.log(`✅ Reasoning completed`);
      console.log(`  - Original triples: ${quads.length}`);
      console.log(`  - Inferred triples: ${reasonedQuads.length}`);
      console.log(`  - Total triples: ${quads.length + reasonedQuads.length}`);
      
      // Output reasoned data
      if (args.output) {
        const allQuads = [...quads, ...reasonedQuads];
        const serialized = await turtle.serialize(allQuads);
        await writeFile(args.output, serialized);
        console.log(`📄 Reasoned data written to ${args.output}`);
      }
      
    } catch (error) {
      console.error('❌ Reasoning error:', error.message);
      process.exit(1);
    }
  }
});

/**
 * Convert between RDF and structured data
 */
const convertCommand = createCommand({
  meta: {
    name: 'convert',
    description: 'Convert between RDF and structured data using Zod schemas'
  },
  args: {
    input: {
      type: 'positional',
      description: 'Input file path',
      required: true
    },
    schema: {
      type: 'string',
      description: 'Zod schema definition (JSON string)',
      required: true
    },
    direction: {
      type: 'string',
      description: 'Conversion direction (to-rdf, from-rdf)',
      default: 'to-rdf'
    },
    output: {
      type: 'string',
      description: 'Output file path',
      alias: 'o'
    }
  },
  async run({ args }) {
    try {
      console.log('🔄 Converting data...');
      
      const zodHelper = useZod();
      const turtle = useTurtle();
      
      // Parse schema
      const schemaDef = JSON.parse(args.schema);
      const schema = z.object(schemaDef);
      
      if (args.direction === 'to-rdf') {
        // Convert structured data to RDF
        const inputData = JSON.parse(await readFile(args.input, 'utf-8'));
        const validatedData = schema.parse(inputData);
        const quads = zodHelper.toRdf(validatedData, schema);
        
        console.log(`✅ Converted to ${quads.length} RDF triples`);
        
        if (args.output) {
          const serialized = await turtle.serialize(quads);
          await writeFile(args.output, serialized);
          console.log(`📄 RDF data written to ${args.output}`);
        }
      } else {
        // Convert RDF to structured data
        const inputData = await readFile(args.input, 'utf-8');
        const quads = await turtle.parse(inputData);
        const structuredData = zodHelper.fromRdf(quads, schema);
        
        console.log('✅ Converted to structured data');
        
        if (args.output) {
          await writeFile(args.output, JSON.stringify(structuredData, null, 2));
          console.log(`📄 Structured data written to ${args.output}`);
        } else {
          console.log(JSON.stringify(structuredData, null, 2));
        }
      }
      
    } catch (error) {
      console.error('❌ Conversion error:', error.message);
      process.exit(1);
    }
  }
});

/**
 * Show help information
 */
const helpCommand = createCommand({
  meta: {
    name: 'help',
    description: 'Show detailed help information'
  },
  args: {
    command: {
      type: 'positional',
      description: 'Command to get help for'
    }
  },
  async run({ args }) {
    console.log('📚 UNRDF CLI Help\n');
    
    if (args.command) {
      // Show specific command help
      switch (args.command) {
        case 'parse': {
          console.log('Parse RDF data from various formats\n');
          console.log('Usage: unrdf-cli parse <input> [options]\n');
          console.log('Options:');
          console.log('  --format <format>    Input format (turtle, n3, json-ld)');
          console.log('  -o, --output <file>  Output file path');
          break;
        }
        case 'query': {
          console.log('Query RDF data with SPARQL\n');
          console.log('Usage: unrdf-cli query <input> [options]\n');
          console.log('Options:');
          console.log('  -q, --query <query>     SPARQL query string');
          console.log('  -f, --query-file <file> SPARQL query file path');
          console.log('  --format <format>       Output format (json, table, turtle)');
          break;
        }
        default: {
          console.log(`Unknown command: ${args.command}`);
        }
      }
    } else {
      // Show general help
      console.log('Available commands:');
      console.log('  parse     Parse RDF data from various formats');
      console.log('  query     Query RDF data with SPARQL');
      console.log('  validate  Validate RDF data against SHACL shapes');
      console.log('  reason    Perform OWL reasoning on RDF data');
      console.log('  convert   Convert between RDF and structured data');
      console.log('  help      Show this help information\n');
      console.log('Use "unrdf-cli help <command>" for detailed help on a specific command.');
    }
  }
});

// Run the CLI
runMain(main);
