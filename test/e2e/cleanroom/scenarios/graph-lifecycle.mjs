/**
 * @file P0 Scenario: Graph Lifecycle
 * @module scenarios/graph-lifecycle
 *
 * @description
 * Complete graph workflow: Create → Import → Query → Validate → Export
 * This is the most common workflow, representing 30% of real-world usage.
 */

import { ScenarioBuilder } from '../scenario-framework.mjs';
import assert from 'assert';
import { promises as fs } from 'fs';

/**
 * Graph Lifecycle scenario - P0 (Core Workflow)
 */
export const graphLifecycleScenario = new ScenarioBuilder()
  .name('Graph Lifecycle')
  .description('Create, import, query, validate, and export a complete RDF graph')
  .priority('P0')
  .tag('core')
  .tag('graph')
  .tag('workflow')

  // Step 1: Create graph
  .step({
    name: 'Create graph',
    command: 'node cli/unrdf.mjs graph create test-graph --base-iri=http://test.org/',
    expectedExitCode: 0,
    expectedOutput: /Graph created: test-graph/,
  })

  // Step 2: Import test data
  .step({
    name: 'Import RDF data',
    command: 'node cli/unrdf.mjs store import test/e2e/cleanroom/fixtures/test-data.ttl --graph=test-graph',
    expectedExitCode: 0,
    expectedOutput: /Imported \d+ triples?/,
  })

  // Step 3: Query graph
  .step({
    name: 'Query graph data',
    command: 'node cli/unrdf.mjs store query --query="SELECT * WHERE { ?s ?p ?o } LIMIT 10"',
    expectedExitCode: 0,
    expectedOutput: /\d+ results?/,
  })

  // Step 4: Validate graph
  .step({
    name: 'Validate graph integrity',
    command: 'node cli/unrdf.mjs graph validate test-graph --policy=default',
    expectedExitCode: 0,
    expectedOutput: /Validation passed|Valid/,
  })

  // Step 5: Export graph
  .step({
    name: 'Export graph to file',
    command: 'node cli/unrdf.mjs graph export test-graph --format=turtle --output=/tmp/output.ttl',
    expectedExitCode: 0,
  })

  // Expected OTEL spans
  .expectSpan('cli.graph.create')
  .expectSpan('sidecar.transaction.apply')
  .expectSpan('cli.store.import')
  .expectSpan('sidecar.store.add')
  .expectSpan('cli.store.query')
  .expectSpan('sidecar.query.execute')
  .expectSpan('cli.graph.validate')
  .expectSpan('sidecar.hook.evaluate')
  .expectSpan('cli.graph.export')
  .expectSpan('sidecar.store.serialize')

  // Assertions
  .assert(async (ctx) => {
    const traces = await ctx.getTraces();
    assert(traces.length > 0, 'Should have traces');

    const spans = await ctx.getSpans();
    assert(spans.length >= 10, 'Should have at least 10 spans');
  })

  .assert(async (ctx) => {
    // Verify output file exists
    try {
      await fs.access('/tmp/output.ttl');
      console.log('    ✓ Output file created successfully');
    } catch {
      throw new Error('Output file not created');
    }
  })

  .assert(async (ctx) => {
    // Verify all steps completed
    assert(ctx.results.length === 5, 'Should have 5 step results');

    for (const result of ctx.results) {
      assert(result.result.exitCode === 0 || result.result.success,
        `Step ${result.step} should succeed`);
    }
  })

  .build();

/**
 * Graph Lifecycle with Hooks scenario - P0 (Enhanced Workflow)
 */
export const graphLifecycleWithHooksScenario = new ScenarioBuilder()
  .name('Graph Lifecycle with Knowledge Hooks')
  .description('Complete graph workflow with pre/post hooks validation')
  .priority('P0')
  .tag('core')
  .tag('graph')
  .tag('hooks')

  // Step 1: Create graph
  .step({
    name: 'Create graph with hooks',
    command: 'node cli/unrdf.mjs graph create hooked-graph --base-iri=http://hooked.org/ --enable-hooks',
    expectedExitCode: 0,
  })

  // Step 2: Register validation hook
  .step({
    name: 'Register validation hook',
    command: 'node cli/unrdf.mjs hook create schema-validation --type=sparql-ask --file=test/e2e/cleanroom/fixtures/validation-hook.rq',
    expectedExitCode: 0,
  })

  // Step 3: Import data (triggers hooks)
  .step({
    name: 'Import data with hook evaluation',
    command: 'node cli/unrdf.mjs store import test/e2e/cleanroom/fixtures/test-data.ttl --graph=hooked-graph',
    expectedExitCode: 0,
  })

  // Step 4: Verify hook was triggered
  .step({
    name: 'Verify hook execution',
    command: 'node cli/unrdf.mjs hook history schema-validation --limit=1',
    expectedExitCode: 0,
    expectedOutput: /fired: (true|false)/,
  })

  // Expected OTEL spans
  .expectSpan('cli.graph.create')
  .expectSpan('cli.hook.create')
  .expectSpan('sidecar.hook.register')
  .expectSpan('cli.store.import')
  .expectSpan('sidecar.hook.evaluate')
  .expectSpan('sidecar.condition.evaluate')
  .expectSpan('cli.hook.history')

  .assert(async (ctx) => {
    const traces = await ctx.getTraces();
    assert(traces.length > 0, 'Should have traces with hook execution');

    const spans = await ctx.getSpans();
    const hookSpans = spans.filter(s =>
      s.operationName?.includes('hook') || s.name?.includes('hook')
    );
    assert(hookSpans.length >= 3, 'Should have hook-related spans');
  })

  .build();

/**
 * Concurrent Graph Operations scenario - P1 (Performance)
 */
export const concurrentGraphOpsScenario = new ScenarioBuilder()
  .name('Concurrent Graph Operations')
  .description('Multiple graph operations executed concurrently')
  .priority('P1')
  .tag('performance')
  .tag('graph')
  .tag('concurrent')

  // Create multiple graphs in parallel (simulated via sequential with timing)
  .step({
    name: 'Create graph 1',
    command: 'node cli/unrdf.mjs graph create concurrent-1 --base-iri=http://c1.org/',
    expectedExitCode: 0,
  })

  .step({
    name: 'Create graph 2',
    command: 'node cli/unrdf.mjs graph create concurrent-2 --base-iri=http://c2.org/',
    expectedExitCode: 0,
  })

  .step({
    name: 'Create graph 3',
    command: 'node cli/unrdf.mjs graph create concurrent-3 --base-iri=http://c3.org/',
    expectedExitCode: 0,
  })

  .expectSpan('cli.graph.create')
  .expectSpan('sidecar.transaction.apply')

  .assert(async (ctx) => {
    // Verify performance - all operations should complete quickly
    const duration = ctx.getDuration();
    assert(duration < 5000, `Concurrent operations took ${duration}ms, should be < 5000ms`);
  })

  .build();

export default {
  graphLifecycleScenario,
  graphLifecycleWithHooksScenario,
  concurrentGraphOpsScenario,
};
