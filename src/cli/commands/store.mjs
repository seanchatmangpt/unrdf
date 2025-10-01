/**
 * @file Store Commands
 * @module cli/commands/store
 *
 * @description
 * RDF store operations with OTEL instrumentation for cleanroom scenarios.
 */

import { readFile, writeFile } from 'node:fs/promises';
import { useStoreContext } from '../../context/index.mjs';
import { useTurtle } from '../../composables/index.mjs';
import { validateRequiredArgs, getArg } from '../utils/context-wrapper.mjs';
import { getTracer, printTraceInfo } from '../utils/otel-tracer.mjs';

/**
 * Import RDF data to store
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function storeImportCommand(ctx, config) {
  const { args } = ctx;
  validateRequiredArgs(args, ['file']);

  const tracer = await getTracer();
  const span = tracer.startSpan('store.import', {
    attributes: {
      'cli.command': 'store import',
      'file': args.file,
      'graph': args.graph || 'default'
    }
  });

  const startTime = Date.now();

  try {
    const graphName = getArg(args, 'graph', 'default');

    const store = useStoreContext();
    const turtle = await useTurtle();

    // Read and parse data
    const dataContent = await readFile(args.file, 'utf-8');
    span.addEvent('file.read', { size: dataContent.length });

    const parsedStore = turtle.parse(dataContent);

    // Convert store to array to get count
    const quads = Array.from(parsedStore);
    span.addEvent('data.parsed', { quads: quads.length });

    // Add to store
    store.add(...quads);

    const duration = Date.now() - startTime;
    // Simple output for CLI compatibility
    console.log(`✅ Imported ${quads.length} triples to graph '${graphName}'`);

    span.setAttributes({
      'import.quads': quads.length,
      'import.duration_ms': duration,
      'import.success': true
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
      'error.message': error.message
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
export async function storeExportCommand(ctx, config) {
  const { args } = ctx;
  validateRequiredArgs(args, ['graph']);

  const tracer = await getTracer();
  const span = tracer.startSpan('store.export', {
    attributes: {
      'cli.command': 'store export',
      'graph': args.graph
    }
  });

  const startTime = Date.now();

  try {
    console.log(`📤 Exporting graph: ${args.graph}`);

    const store = useStoreContext();
    const turtle = await useTurtle();

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
      'export.success': true
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
      'error.message': error.message
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
export async function storeQueryCommand(ctx, config) {
  const { args } = ctx;
  validateRequiredArgs(args, ['sparql']);

  const tracer = await getTracer();
  const span = tracer.startSpan('store.query', {
    attributes: {
      'cli.command': 'store query',
      'query.length': args.sparql.length
    }
  });

  const startTime = Date.now();

  try {
    console.log(`🔍 Executing SPARQL query...`);

    const store = useStoreContext();

    // Execute query
    const result = store.query(args.sparql);
    span.addEvent('query.executed');

    // Format output
    const format = getArg(args, 'format', 'table');
    if (format === 'json') {
      console.log(JSON.stringify(result, null, 2));
    } else if (result.type === 'select') {
      // Table format for SELECT
      console.log('\nResults:');
      console.log('─'.repeat(80));

      if (result.rows && result.rows.length > 0) {
        // Print header
        const vars = result.variables || Object.keys(result.rows[0]);
        console.log(vars.join('\t'));
        console.log('─'.repeat(80));

        // Print rows
        for (const row of result.rows) {
          const values = vars.map(v => row[v]?.value || '');
          console.log(values.join('\t'));
        }

        console.log(`\n${result.rows.length} results`);
      } else {
        console.log('No results');
      }
    } else if (result.type === 'ask') {
      console.log(`Result: ${result.result ? 'true' : 'false'}`);
    } else {
      console.log(JSON.stringify(result, null, 2));
    }

    const duration = Date.now() - startTime;
    span.setAttributes({
      'query.type': result.type,
      'query.results': result.rows?.length || 0,
      'query.duration_ms': duration,
      'query.success': true
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
      'error.message': error.message
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
  subcommands: ['import', 'export', 'query']
};
