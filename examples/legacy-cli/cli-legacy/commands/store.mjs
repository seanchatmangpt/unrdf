/**
 * @file Store Commands
 * @module cli/commands/store
 *
 * @description
 * RDF store operations with OTEL instrumentation for cleanroom scenarios.
 */

import { readFile, writeFile } from 'node:fs/promises';
import {  Writer  } from '@unrdf/core/rdf/n3-justified-only';
import { QueryEngine } from '@comunica/query-sparql';
import { useStoreContext } from '../../context/index.mjs';
import { useTurtle } from '../../composables/index.mjs';
import { validateRequiredArgs, getArg } from '../utils/context-wrapper.mjs';
import { getTracer, printTraceInfo } from '../utils/otel-tracer.mjs';

/**
 * Persist store to disk
 * @param {Object} store - Store context
 * @returns {Promise<void>}
 */
async function persistStore(store) {
  const writer = new Writer({ format: 'N-Quads' });
  const quads = Array.from(store.store.getQuads());
  const serialized = writer.quadsToString(quads);
  await writeFile('.unrdf-store.nq', serialized);
}

/**
 * Import RDF data to store
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function storeImportCommand(ctx, _config) {
  const { args } = ctx;
  validateRequiredArgs(args, ['file']);

  const tracer = await getTracer();
  const span = tracer.startSpan('store.import', {
    attributes: {
      'cli.command': 'store import',
      file: args.file,
      graph: args.graph || 'default',
    },
  });

  const startTime = Date.now();

  try {
    const graphName = getArg(args, 'graph', 'default');

    const store = useStoreContext();

    // Ensure store context is initialized
    if (!store || !store.engine) {
      throw new Error('Store context not initialized. Please initialize the store first.');
    }

    const turtle = useTurtle();

    // Read and parse data
    const dataContent = await readFile(args.file, 'utf-8');
    span.addEvent('file.read', { size: dataContent.length });

    const parsedStore = turtle.parse(dataContent);

    // Convert store to array to get count
    const quads = Array.from(parsedStore);
    span.addEvent('data.parsed', { quads: quads.length });

    // Add to store
    store.add(...quads);

    // Persist store to disk for subsequent commands
    await persistStore(store);

    const duration = Date.now() - startTime;
    // Simple output for CLI compatibility
    console.log(`✅ Imported ${quads.length} triples to graph '${graphName}'`);

    span.setAttributes({
      'import.quads': quads.length,
      'import.duration_ms': duration,
      'import.success': true,
    });

    span.end();

    // Print trace information
    printTraceInfo('store import');
  } catch (error) {
    const duration = Date.now() - startTime;
    console.error(`❌ Import failed: ${error.message}`);

    span.setAttributes({
      'import.success': false,
      'import.duration_ms': duration,
      'error.message': error.message,
    });
    span.recordException(error);
    span.end();

    if (args.verbose) {
      console.error(error.stack);
    }
    process.exit(1);
  }
}

/**
 * Export graph data from store
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function storeExportCommand(ctx, _config) {
  const { args } = ctx;
  validateRequiredArgs(args, ['graph']);

  const tracer = await getTracer();
  const span = tracer.startSpan('store.export', {
    attributes: {
      'cli.command': 'store export',
      graph: args.graph,
    },
  });

  const startTime = Date.now();

  try {
    console.log(`📤 Exporting graph: ${args.graph}`);

    const store = useStoreContext();

    // Ensure store context is initialized
    if (!store || !store.engine) {
      throw new Error('Store context not initialized. Please initialize the store first.');
    }

    const turtle = useTurtle();

    const serialized = await turtle.serialize();
    span.addEvent('data.serialized', { size: serialized.length });

    if (args.output) {
      await writeFile(args.output, serialized);
      console.log(`✅ Exported to: ${args.output}`);
    } else {
      console.log(serialized);
    }

    const duration = Date.now() - startTime;
    span.setAttributes({
      'export.size': serialized.length,
      'export.duration_ms': duration,
      'export.success': true,
    });

    span.end();

    // Print trace information
    printTraceInfo('store export');
  } catch (error) {
    const duration = Date.now() - startTime;
    console.error(`❌ Export failed: ${error.message}`);

    span.setAttributes({
      'export.success': false,
      'export.duration_ms': duration,
      'error.message': error.message,
    });
    span.recordException(error);
    span.end();

    if (args.verbose) {
      console.error(error.stack);
    }
    process.exit(1);
  }
}

/**
 * Execute SPARQL query against store
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function storeQueryCommand(ctx, _config) {
  const { args } = ctx;

  // Get SPARQL query from positional argument or --query flag
  const sparqlQuery = args._?.[0] || args.query;

  if (!sparqlQuery) {
    console.error('❌ Missing required argument: SPARQL query');
    console.error('Usage: store query <sparql> [--format=json|table]');
    console.error('   or: store query --query="<sparql>" [--format=json|table]');
    console.error('Example: store query "SELECT * WHERE { ?s ?p ?o }" --format=table');
    process.exit(1);
  }

  const tracer = await getTracer();
  const span = tracer.startSpan('store.query', {
    attributes: {
      'cli.command': 'store query',
      'query.length': sparqlQuery.length,
    },
  });

  const startTime = Date.now();

  try {
    console.log(`🔍 Executing SPARQL query...`);

    const store = useStoreContext();

    // Check if store is empty
    if (store.store.size === 0) {
      console.error('⚠️  Store is empty. Did you run "store import" first?');
      process.exit(1);
    }

    // Execute query using Comunica directly
    // Note: Comunica expects the store directly in sources array, not wrapped in an object
    const queryEngine = new QueryEngine();
    const comunicaContext = {
      sources: [store.store],
    };

    // Determine query type
    const queryType = sparqlQuery
      .replace(/^PREFIX\s+[^\s]+\s+<[^>]+>\s*/gm, '')
      .trim()
      .toUpperCase()
      .split(/\s+/)[0];

    let result;
    if (queryType === 'SELECT') {
      const bindingsStream = await queryEngine.queryBindings(sparqlQuery, comunicaContext);
      const bindings = await bindingsStream.toArray();
      // Convert bindings to simple objects
      result = bindings.map(b => Object.fromEntries([...b].map(([k, v]) => [k.value, v.value])));
    } else if (queryType === 'ASK') {
      result = await queryEngine.queryBoolean(sparqlQuery, comunicaContext);
    } else if (queryType === 'CONSTRUCT') {
      const quadStream = await queryEngine.queryQuads(sparqlQuery, comunicaContext);
      const quads = await quadStream.toArray();
      result = { quads, isConstructQuery: true };
    } else {
      throw new Error(`Unsupported query type: ${queryType}`);
    }

    span.addEvent('query.executed');

    // Format output
    const format = getArg(args, 'format', 'table');

    // Determine query type from result
    const isSelectQuery = Array.isArray(result);
    const isBooleanQuery = typeof result === 'boolean';
    const isConstructQuery = result && result.isConstructQuery;

    if (format === 'json') {
      if (isSelectQuery) {
        console.log(JSON.stringify(result, null, 2));
      } else if (isBooleanQuery) {
        console.log(JSON.stringify({ result }, null, 2));
      } else if (isConstructQuery) {
        // Serialize the constructed quads
        const writer = new Writer({ format: 'Turtle' });
        const serialized = writer.quadsToString(result.quads);
        console.log(serialized);
      } else {
        console.log(JSON.stringify(result, null, 2));
      }
    } else if (isSelectQuery) {
      // Table format for SELECT results
      console.log('\nResults:');
      console.log('─'.repeat(80));

      if (result.length > 0) {
        // Print header
        const vars = Object.keys(result[0]);
        console.log(vars.join('\t'));
        console.log('─'.repeat(80));

        // Print rows
        for (const row of result) {
          const values = vars.map(v => row[v] || '');
          console.log(values.join('\t'));
        }

        console.log(`\n${result.length} results`);
      } else {
        console.log('No results');
      }
    } else if (isBooleanQuery) {
      console.log(`Result: ${result ? 'true' : 'false'}`);
    } else if (isConstructQuery) {
      // Table format for CONSTRUCT - show quads
      const writer = new Writer({ format: 'Turtle' });
      const serialized = writer.quadsToString(result.quads);
      console.log('\nConstructed graph:');
      console.log('─'.repeat(80));
      console.log(serialized);
    } else {
      console.log(JSON.stringify(result, null, 2));
    }

    const duration = Date.now() - startTime;

    // Determine result count for metrics
    let resultCount = 0;
    if (isSelectQuery) {
      resultCount = result.length;
    } else if (isBooleanQuery) {
      resultCount = 1;
    } else if (isConstructQuery) {
      resultCount = result.quads.length;
    }

    span.setAttributes({
      'query.type': queryType.toLowerCase(),
      'query.results': resultCount,
      'query.duration_ms': duration,
      'query.success': true,
    });

    span.end();

    // Print trace information
    printTraceInfo('store query');
  } catch (error) {
    const duration = Date.now() - startTime;
    console.error(`❌ Query failed: ${error.message}`);

    span.setAttributes({
      'query.success': false,
      'query.duration_ms': duration,
      'error.message': error.message,
    });
    span.recordException(error);
    span.end();

    if (args.verbose) {
      console.error(error.stack);
    }
    process.exit(1);
  }
}

/**
 * Export store command metadata
 */
export const storeCommandMeta = {
  name: 'store',
  description: 'Manage RDF store operations',
  subcommands: ['import', 'export', 'query'],
};
