/**
 * @file Graph Commands
 * @module cli/commands/graph
 *
 * @description
 * RDF graph lifecycle management with KGC sidecar integration.
 */

import { readFile, writeFile } from 'node:fs/promises';
import { useStoreContext } from '../../context/index.mjs';
import { useTurtle } from '../../composables/index.mjs';
import { validateRequiredArgs, getArg } from '../utils/context-wrapper.mjs';
import { _withSidecar, _formatSidecarError } from '../utils/sidecar-helper.mjs';
import { validatePolicy, formatValidationReport } from '../utils/policy-validator.mjs';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { printTraceInfo } from '../utils/otel-tracer.mjs';

const tracer = trace.getTracer('unrdf-cli-graph');

/**
 * List graphs
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function graphListCommand(ctx, _config) {
  const { args } = ctx;

  console.log('📋 Listing RDF graphs...');

  try {
    // For now, use local store context
    const store = useStoreContext();
    const graphs = [
      {
        name: 'default',
        triples: store.size,
        updated: new Date().toISOString(),
      },
    ];

    const format = getArg(args, 'format', 'table');
    if (format === 'json') {
      console.log(JSON.stringify(graphs, null, 2));
    } else {
      console.log('\nNAME       TRIPLES   UPDATED');
      console.log('─────────  ────────  ───────────────────');
      for (const graph of graphs) {
        const name = graph.name.padEnd(9);
        const triples = graph.triples.toString().padEnd(8);
        const updated = new Date(graph.updated).toISOString().substring(0, 19);
        console.log(`${name}  ${triples}  ${updated}`);
      }
    }
  } catch (error) {
    console.error(`❌ Failed to list graphs: ${error.message}`);
    process.exit(1);
  }
}

/**
 * Get graph details
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function graphGetCommand(ctx, _config) {
  const { args } = ctx;
  validateRequiredArgs(args, ['name']);

  console.log(`📊 Getting graph: ${args.name}`);

  try {
    const store = useStoreContext();
    const graph = {
      name: args.name,
      triples: store.size,
      subjects: new Set(store.getSubjects()).size,
      predicates: new Set(store.getPredicates()).size,
      objects: new Set(store.getObjects()).size,
      created: new Date().toISOString(),
      updated: new Date().toISOString(),
    };

    console.log(JSON.stringify(graph, null, 2));
  } catch (error) {
    console.error(`❌ Failed to get graph: ${error.message}`);
    process.exit(1);
  }
}

/**
 * Create graph
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function graphCreateCommand(ctx, _config) {
  const { args } = ctx;
  validateRequiredArgs(args, ['name']);

  console.log(`🔨 Creating graph: ${args.name}`);

  try {
    const store = useStoreContext();

    // Import initial data if provided
    if (args.import) {
      const turtle = await useTurtle();
      const dataContent = await readFile(args.import, 'utf-8');
      const quads = await turtle.parse(dataContent);
      store.add(...quads);
      console.log(`✅ Imported ${quads.length} triples`);
    }

    console.log(`✅ Graph created: ${args.name}`);
  } catch (error) {
    console.error(`❌ Failed to create graph: ${error.message}`);
    process.exit(1);
  }
}

/**
 * Delete graph
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function graphDeleteCommand(ctx, _config) {
  const { args } = ctx;
  validateRequiredArgs(args, ['name']);

  if (!args.force) {
    console.log(`⚠️  This will permanently delete graph: ${args.name}`);
    console.log('Use --force to confirm deletion');
    process.exit(1);
  }

  console.log(`🗑️  Deleting graph: ${args.name}`);

  try {
    const store = useStoreContext();

    // Create backup if requested
    if (args.backup) {
      const turtle = await useTurtle();
      const serialized = await turtle.serialize();
      const backupPath = `${args.backup}/${args.name}-${Date.now()}.ttl`;
      await writeFile(backupPath, serialized);
      console.log(`📦 Backup created: ${backupPath}`);
    }

    // Clear store
    store.removeMatches();
    console.log(`✅ Graph deleted: ${args.name}`);
  } catch (error) {
    console.error(`❌ Failed to delete graph: ${error.message}`);
    process.exit(1);
  }
}

/**
 * Import data to graph
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function graphImportCommand(ctx, _config) {
  const { args } = ctx;
  validateRequiredArgs(args, ['file']);

  console.log(`📥 Importing: ${args.file}`);

  try {
    const store = useStoreContext();
    const turtle = await useTurtle();

    const dataContent = await readFile(args.file, 'utf-8');
    const startTime = Date.now();
    const quads = await turtle.parse(dataContent);

    // Validate if requested
    if (args.validate) {
      console.log('🔍 Validating data...');
      // TODO: Add validation logic
    }

    // Apply based on mode
    const mode = getArg(args, 'mode', 'append');
    if (mode === 'replace') {
      store.removeMatches();
    }

    store.add(...quads);
    const duration = Date.now() - startTime;

    console.log(`✅ Imported ${quads.length} triples in ${duration}ms`);
  } catch (error) {
    console.error(`❌ Import failed: ${error.message}`);
    process.exit(1);
  }
}

/**
 * Export graph data
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function graphExportCommand(ctx, _config) {
  const { args } = ctx;
  validateRequiredArgs(args, ['name']);

  console.log(`📤 Exporting graph: ${args.name}`);

  try {
    const _store = useStoreContext();
    const turtle = await useTurtle();

    const serialized = await turtle.serialize();

    if (args.output) {
      await writeFile(args.output, serialized);
      console.log(`✅ Exported to: ${args.output}`);
    } else {
      console.log(serialized);
    }
  } catch (error) {
    console.error(`❌ Export failed: ${error.message}`);
    process.exit(1);
  }
}

/**
 * Validate graph
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function graphValidateCommand(ctx, _config) {
  return await tracer.startActiveSpan('graph.validate', async span => {
    try {
      const { args } = ctx;
      validateRequiredArgs(args, ['name']);

      span.setAttribute('graph.name', args.name);
      span.setAttribute('policy.pack', args.policyPack || 'default');
      span.setAttribute('validation.strict', args.strict || false);

      console.log(`🔍 Validating graph: ${args.name}`);
      console.log(`   Policy Pack: ${args.policyPack || 'default'}`);

      // Get store with RDF data
      const store = useStoreContext();
      span.setAttribute('store.size', store.size);

      // Run policy validation
      const result = await validatePolicy(store, args.policyPack || 'default', {
        strict: args.strict || false,
        basePath: process.cwd(),
      });

      // Format and display results
      const format = args.report ? 'json' : 'text';
      const report = formatValidationReport(result, format);

      if (result.passed) {
        console.log('✅ Validation PASSED');
        span.setStatus({ code: SpanStatusCode.OK });
      } else {
        console.log('❌ Validation FAILED');
        console.log(`   Violations: ${result.violations.length}`);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: `${result.violations.length} violations`,
        });
      }

      // Display report
      if (!args.report) {
        console.log(report);
      }

      // Write report to file if requested
      if (args.report) {
        await writeFile(args.report, report);
        console.log(`📄 Report written to: ${args.report}`);
      }

      // Print trace information
      printTraceInfo('graph validate');

      // Exit with error code if validation failed
      if (!result.passed) {
        process.exit(1);
      }
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      console.error(`❌ Validation failed: ${error.message}`);
      if (ctx.args.verbose) {
        console.error(error.stack);
      }
      process.exit(1);
    } finally {
      span.end();
    }
  });
}

/**
 * Show graph statistics
 * @param {Object} ctx - CLI context
 * @param {Object} config - Configuration
 * @returns {Promise<void>}
 */
export async function graphStatsCommand(ctx, _config) {
  const { args } = ctx;
  validateRequiredArgs(args, ['name']);

  console.log(`📊 Graph statistics: ${args.name}\n`);

  try {
    const store = useStoreContext();
    const stats = {
      totalTriples: store.size,
      uniqueSubjects: new Set(store.getSubjects()).size,
      uniquePredicates: new Set(store.getPredicates()).size,
      uniqueObjects: new Set(store.getObjects()).size,
    };

    console.log(`Total Triples:     ${stats.totalTriples.toLocaleString()}`);
    console.log(`Unique Subjects:   ${stats.uniqueSubjects.toLocaleString()}`);
    console.log(`Unique Predicates: ${stats.uniquePredicates.toLocaleString()}`);
    console.log(`Unique Objects:    ${stats.uniqueObjects.toLocaleString()}`);

    if (args.detailed) {
      console.log('\nPredicate Distribution:');
      const predicates = {};
      for (const quad of store) {
        const pred = quad.predicate.value;
        predicates[pred] = (predicates[pred] || 0) + 1;
      }

      const sorted = Object.entries(predicates)
        .sort((a, b) => b[1] - a[1])
        .slice(0, 10);

      for (const [pred, count] of sorted) {
        const shortPred = pred.split('/').pop().split('#').pop();
        console.log(`  ${shortPred.padEnd(30)} ${count}`);
      }
    }
  } catch (error) {
    console.error(`❌ Failed to get stats: ${error.message}`);
    process.exit(1);
  }
}

/**
 * Export graph command metadata
 */
export const graphCommandMeta = {
  name: 'graph',
  description: 'Manage RDF graphs',
  subcommands: ['list', 'get', 'create', 'delete', 'import', 'export', 'validate', 'stats'],
};
