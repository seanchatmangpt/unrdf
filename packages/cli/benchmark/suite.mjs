#!/usr/bin/env node

/**
 * @file unrdf CLI Scalability Benchmark Suite
 * @module benchmark/suite
 * @description Evidence-based benchmarks for the unrdf sync pipeline.
 * Measures ACTUAL performance across varying ontology sizes and rule counts.
 * No assumptions — only measurements.
 *
 * Usage:
 *   node benchmark/suite.mjs                  # Full benchmark suite
 *   node benchmark/suite.mjs --quick          # Fast run (small scales)
 *   node benchmark/suite.mjs --scale 50       # Test up to 50 rules
 *   node benchmark/suite.mjs --triples 10000  # Test with 10K triples
 *   node benchmark/suite.mjs --json           # Machine-readable JSON output
 */

import { mkdir, writeFile, rm, readFile, cp } from 'fs/promises';
import { existsSync } from 'fs';
import { join, resolve, dirname } from 'path';
import { tmpdir } from 'node:os';
import { performance } from 'node:perf_hooks';
import { loadOntology } from '../src/cli/commands/sync/ontology-loader.mjs';
import { executeSparqlQuery, buildPrefixDeclarations } from '../src/cli/commands/sync/sparql-executor.mjs';
import { renderTemplate } from '../src/cli/commands/sync/template-renderer.mjs';
import { runSync } from '../src/cli/commands/sync/orchestrator.mjs';
import { parseConfig } from '../src/cli/commands/sync/config-parser.mjs';

// ─── Configuration ──────────────────────────────────────────────────────────

const ARGS = parseArgs(process.argv.slice(2));

const SCALES = ARGS.quick
  ? [1, 5, 10]
  : [1, 5, 10, 20, 50, 100];

const TRIPLE_SIZES = ARGS.quick
  ? [100, 1000, 5000]
  : [100, 1000, 5000, 10000, 50000];

const ITERATIONS = ARGS.quick ? 2 : 3;

// ─── Utility Functions ──────────────────────────────────────────────────────

function parseArgs(argv) {
  const args = {};
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (arg === '--quick') args.quick = true;
    if (arg === '--json') args.json = true;
    if (arg === '--scale' && argv[i + 1]) { args.scale = parseInt(argv[i + 1]); i++; }
    if (arg === '--triples' && argv[i + 1]) { args.triples = parseInt(argv[i + 1]); i++; }
    if (arg === '--output' && argv[i + 1]) { args.output = argv[i + 1]; i++; }
  }
  return args;
}

function fmt(ms) {
  if (ms < 1) return (ms * 1000).toFixed(0) + 'us';
  if (ms < 1000) return ms.toFixed(1) + 'ms';
  if (ms < 60000) return (ms / 1000).toFixed(2) + 's';
  return (ms / 60000).toFixed(2) + 'm';
}

function fmtBytes(bytes) {
  if (bytes < 1024) return bytes + 'B';
  if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + 'KB';
  return (bytes / (1024 * 1024)).toFixed(1) + 'MB';
}

function getMemoryMB() {
  const mem = process.memoryUsage();
  return {
    rss: mem.rss / (1024 * 1024),
    heapUsed: mem.heapUsed / (1024 * 1024),
    heapTotal: mem.heapTotal / (1024 * 1024),
    external: mem.external / (1024 * 1024),
  };
}

function stats(values) {
  if (values.length === 0) return { mean: 0, median: 0, min: 0, max: 0, stddev: 0, p95: 0 };
  const sorted = [...values].sort((a, b) => a - b);
  const mean = sorted.reduce((a, b) => a + b, 0) / sorted.length;
  const variance = sorted.reduce((a, b) => a + (b - mean) ** 2, 0) / sorted.length;
  const p95idx = Math.ceil(sorted.length * 0.95) - 1;
  return {
    mean,
    median: sorted[Math.floor(sorted.length / 2)],
    min: sorted[0],
    max: sorted[sorted.length - 1],
    stddev: Math.sqrt(variance),
    p95: sorted[Math.max(0, p95idx)],
  };
}

// ─── Synthetic Data Generators ──────────────────────────────────────────────

/**
 * Generate synthetic N-Triples ontology.
 * Creates classes, properties, and instances with realistic RDF patterns.
 */
function generateOntology(tripleCount, baseUri = 'http://benchmark.org/') {
  const lines = [];

  // Schema triples (~20% of total): classes and properties
  const schemaCount = Math.max(10, Math.floor(tripleCount * 0.2));
  const classCount = Math.max(5, Math.floor(schemaCount / 3));
  const propCount = classCount * 2;

  for (let i = 0; i < classCount; i++) {
    lines.push(`<${baseUri}Class${i}> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2000/01/rdf-schema#Class> .`);
    lines.push(`<${baseUri}Class${i}> <http://www.w3.org/2000/01/rdf-schema#label> "Class ${i}" .`);
  }

  for (let i = 0; i < propCount; i++) {
    lines.push(`<${baseUri}prop${i}> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/1999/02/22-rdf-syntax-ns#Property> .`);
    lines.push(`<${baseUri}prop${i}> <http://www.w3.org/2000/01/rdf-schema#domain> <${baseUri}Class${i % classCount}> .`);
    lines.push(`<${baseUri}prop${i}> <http://www.w3.org/2000/01/rdf-schema#range> <http://www.w3.org/2001/XMLSchema#string> .`);
  }

  // Instance triples (~80% of total): typed instances with property values
  const instanceCount = tripleCount - lines.length;
  let instanceIdx = 0;

  while (lines.length < tripleCount) {
    const cls = instanceIdx % classCount;
    const inst = instanceIdx;

    // Type assertion
    lines.push(`<${baseUri}inst${inst}> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <${baseUri}Class${cls}> .`);

    // Property assertions (2-5 per instance)
    const propCount2 = 2 + (inst % 4);
    for (let p = 0; p < propCount2 && lines.length < tripleCount; p++) {
      const prop = (inst + p) % propCount;
      lines.push(`<${baseUri}inst${inst}> <${baseUri}prop${prop}> "value_${inst}_${p}" .`);
    }

    instanceIdx++;
  }

  return lines.slice(0, tripleCount).join('\n') + '\n';
}

/**
 * Generate synthetic SPARQL queries.
 * Each query targets a different pattern (SELECT with varying complexity).
 */
function generateQueries(ruleCount, baseUri = 'http://benchmark.org/') {
  const queries = [];
  const patterns = [
    // Simple type query
    (i) => `SELECT ?instance WHERE { ?instance a <${baseUri}Class${i % 5}> } LIMIT 100`,
    // Property value query
    (i) => `SELECT ?instance ?value WHERE { ?instance <${baseUri}prop${i % 10}> ?value } LIMIT 100`,
    // Join query (instance → class → label)
    (i) => `SELECT ?instance ?label WHERE { ?instance a <${baseUri}Class${i % 5}> . ?instance <${baseUri}prop${i % 10}> ?value . <${baseUri}Class${i % 5}> <http://www.w3.org/2000/01/rdf-schema#label> ?label } LIMIT 50`,
    // Filter query
    (i) => `SELECT ?instance WHERE { ?instance a <${baseUri}Class${i % 5}> . FILTER(STRSTARTS(STR(?instance), "${baseUri}inst")) } LIMIT 100`,
    // Count query pattern (returns results)
    (i) => `SELECT ?class (COUNT(?instance) AS ?count) WHERE { ?instance a ?class } GROUP BY ?class LIMIT 20`,
  ];

  for (let i = 0; i < ruleCount; i++) {
    const patternFn = patterns[i % patterns.length];
    queries.push({
      name: `rule-${i + 1}`,
      query: patternFn(i),
    });
  }

  return queries;
}

/**
 * Generate a synthetic Nunjucks template.
 * Templates range from simple to complex based on index.
 */
function generateTemplate(index, complexity = 'medium') {
  const templates = {
    simple: `---
description: "Benchmark template ${index}"
to: "output/rule-${index}.txt"
---
Generated by rule ${index}.
Results: {{ results | length }} items.
`,
    medium: `---
description: "Benchmark template ${index}"
to: "output/rule-${index}.md"
---
# Rule ${index} Output

{% for row in results %}
- {{ row.instance | default("N/A") }}{% if row.value %}: {{ row.value }}{% endif %}
{% endfor %}

Total: {{ results | length }} results.
`,
    complex: `---
description: "Benchmark template ${index}"
to: "output/rule-${index}.json"
---
{
  "rule": "rule-${index}",
  "generated_at": "{{ now | date("YYYY-MM-DD HH:mm:ss") }}",
  "results": [
  {%- for row in results %}
    {
      "instance": "{{ row.instance | default("") }}",
      "value": "{{ row.value | default("") }}"
    }{{ "," if not loop.last }}
  {%- endfor %}
  ],
  "count": {{ results | length }}
}
`,
  };

  return templates[complexity] || templates.medium;
}

/**
 * Generate an unrdf.toml config file with N rules.
 * All paths are relative to the config file's directory.
 */
function generateConfig(ruleCount, ontologyRelPath, templatesRelPath, outputRelPath) {
  let toml = `[project]\nname = "benchmark-${ruleCount}-rules"\n\n`;
  toml += `[ontology]\nsource = "${ontologyRelPath}"\n\n`;
  toml += `[generation]\noutput_dir = "${outputRelPath}"\ntemplates_dir = "${templatesRelPath}"\n\n`;

  const complexities = ['simple', 'medium', 'complex'];

  for (let i = 0; i < ruleCount; i++) {
    const complexity = complexities[i % 3];
    toml += `[[generation.rules]]\n`;
    toml += `name = "rule-${i + 1}"\n`;
    toml += `template = "${templatesRelPath}/template-${i + 1}.njk"\n`;
    toml += `output_file = "${outputRelPath}/rule-${i + 1}.${complexity === 'complex' ? 'json' : 'md'}"\n`;
    toml += `query = """SELECT ?instance WHERE { ?instance a <http://benchmark.org/Class${i % 5}> } LIMIT 100"""\n\n`;
  }

  return toml;
}

// ─── Benchmark Runners ──────────────────────────────────────────────────────

/**
 * Benchmark: Ontology loading time at various sizes.
 */
async function benchmarkOntologyLoading(benchDir) {
  console.log('\n═══ BENCHMARK 1: Ontology Loading ═══\n');

  const results = [];

  for (const tripleSize of TRIPLE_SIZES) {
    const timings = [];

    for (let iter = 0; iter < ITERATIONS; iter++) {
      const ontoFile = join(benchDir, `ontology-${tripleSize}-${iter}.nt`);
      const content = generateOntology(tripleSize);
      await writeFile(ontoFile, content, 'utf-8');

      const memBefore = getMemoryMB();
      const start = performance.now();

      // Suppress loader noise
      const _origLog = console.log;
      console.log = () => {};
      const result = await loadOntology({ source: ontoFile }, benchDir);
      console.log = _origLog;

      const elapsed = performance.now() - start;
      const memAfter = getMemoryMB();

      timings.push(elapsed);

      // Clean up for next iteration
      await rm(ontoFile, { force: true });
    }

    const s = stats(timings);
    const result = {
      tripleCount: tripleSize,
      iterations: ITERATIONS,
      timing: s,
      memory: {
        rssDelta: getMemoryMB().rss, // snapshot
      },
    };

    results.push(result);
    console.log(
      `  ${String(tripleSize).padStart(6)} triples: ` +
        `mean=${fmt(s.mean).padStart(10)} median=${fmt(s.median).padStart(10)} ` +
        `min=${fmt(s.min).padStart(10)} max=${fmt(s.max).padStart(10)} ` +
        `p95=${fmt(s.p95).padStart(10)}`
    );
  }

  return results;
}

/**
 * Benchmark: SPARQL query execution time.
 */
async function benchmarkSparqlQueries(benchDir) {
  console.log('\n═══ BENCHMARK 2: SPARQL Query Execution ═══\n');

  // Use a fixed medium ontology for query benchmarks
  const fixedTripleCount = ARGS.triples || 10000;
  const ontoFile = join(benchDir, 'query-bench-ontology.nt');
  await writeFile(ontoFile, generateOntology(fixedTripleCount), 'utf-8');

  // Suppress loader noise
  const origLog = console.log;
  console.log = () => {};
  const { store, prefixes } = await loadOntology({ source: ontoFile }, benchDir);
  console.log = origLog;
  console.log(`  Loaded ${fixedTripleCount} triples for query benchmark\n`);

  const results = [];

  for (const ruleCount of SCALES) {
    const queries = generateQueries(ruleCount);
    const timings = [];
    const resultCounts = [];

    for (const q of queries) {
      const start = performance.now();
      const queryResults = await executeSparqlQuery(store, q.query, prefixes);
      const elapsed = performance.now() - start;

      timings.push(elapsed);
      resultCounts.push(queryResults.length);
    }

    const s = stats(timings);
    const rcStats = stats(resultCounts);

    const result = {
      ruleCount,
      tripleCount: fixedTripleCount,
      queries: queries.length,
      timing: s,
      resultCounts: rcStats,
    };

    results.push(result);
    console.log(
      `  ${String(ruleCount).padStart(3)} queries: ` +
        `mean=${fmt(s.mean).padStart(10)} median=${fmt(s.median).padStart(10)} ` +
        `min=${fmt(s.min).padStart(10)} max=${fmt(s.max).padStart(10)} ` +
        `p95=${fmt(s.p95).padStart(10)} ` +
        `avg_results=${rcStats.mean.toFixed(1)}`
    );
  }

  await rm(ontoFile, { force: true });
  return results;
}

/**
 * Benchmark: Template rendering time.
 */
async function benchmarkTemplateRendering(benchDir) {
  console.log('\n═══ BENCHMARK 3: Template Rendering ═══\n');

  const complexities = ['simple', 'medium', 'complex'];
  const resultCounts = [0, 10, 50, 100]; // varying result set sizes
  const results = [];

  // Generate synthetic results
  function generateResults(count) {
    const results = [];
    for (let i = 0; i < count; i++) {
      results.push({
        instance: `http://benchmark.org/inst${i}`,
        value: `value_${i}`,
        _index: i,
        _meta: { termType: 'string' },
      });
    }
    return results;
  }

  for (const complexity of complexities) {
    for (const rc of resultCounts) {
      const timings = [];
      const templateFile = join(benchDir, `tmpl-bench-${complexity}-${rc}.njk`);
      await writeFile(templateFile, generateTemplate(0, complexity), 'utf-8');

      const sparqlResults = generateResults(rc);

      for (let iter = 0; iter < ITERATIONS; iter++) {
        const start = performance.now();
        await renderTemplate(templateFile, sparqlResults, {
          project: { name: 'benchmark' },
          prefixes: {},
        });
        const elapsed = performance.now() - start;
        timings.push(elapsed);
      }

      const s = stats(timings);
      const result = {
        complexity,
        resultCount: rc,
        iterations: ITERATIONS,
        timing: s,
      };

      results.push(result);
      console.log(
        `  ${complexity.padEnd(8)} ${String(rc).padStart(3)} results: ` +
          `mean=${fmt(s.mean).padStart(10)} median=${fmt(s.median).padStart(10)}`
      );

      await rm(templateFile, { force: true });
    }
  }

  return results;
}

/**
 * Benchmark: Full sync pipeline end-to-end.
 * This is the REAL test — how long does `unrdf sync` take?
 */
async function benchmarkFullPipeline(benchDir) {
  console.log('\n═══ BENCHMARK 4: Full Pipeline (unrdf sync) ═══\n');

  const tripleCount = ARGS.triples || 10000;
  const ontoDir = join(benchDir, 'ontology');
  const templatesDir = join(benchDir, 'templates');
  const outputDir = join(benchDir, 'output');

  await mkdir(ontoDir, { recursive: true });
  await mkdir(templatesDir, { recursive: true });
  await mkdir(outputDir, { recursive: true });

  // Write ontology
  const ontoFile = join(ontoDir, 'benchmark.nt');
  await writeFile(ontoFile, generateOntology(tripleCount), 'utf-8');

  const results = [];

  for (const ruleCount of SCALES) {
    // Clean output dir
    await rm(outputDir, { force: true, recursive: true });
    await mkdir(outputDir, { recursive: true });

    // Clean templates dir
    await rm(templatesDir, { force: true, recursive: true });
    await mkdir(templatesDir, { recursive: true });

    // Write templates
    const complexities = ['simple', 'medium', 'complex'];
    for (let i = 0; i < ruleCount; i++) {
      const complexity = complexities[i % 3];
      await writeFile(
        join(templatesDir, `template-${i + 1}.njk`),
        generateTemplate(i + 1, complexity),
        'utf-8'
      );
    }

    // Write config
    const configPath = join(benchDir, `config-${ruleCount}.toml`);
    const configContent = generateConfig(ruleCount, 'ontology', 'templates', 'output');
    await writeFile(configPath, configContent, 'utf-8');

    // Suppress console output from runSync during benchmark
    const origLog = console.log;
    const origError = console.error;
    console.log = () => {};
    console.error = () => {};

    // Measure full pipeline
    const timings = [];
    const fileCounts = [];
    const byteCounts = [];

    for (let iter = 0; iter < ITERATIONS; iter++) {
      // Clean output between iterations
      await rm(outputDir, { force: true, recursive: true });
      await mkdir(outputDir, { recursive: true });

      const memBefore = getMemoryMB();
      const start = performance.now();

      const result = await runSync({
        config: configPath,
        dryRun: false,
        verbose: false,
        force: true,
      });

      const elapsed = performance.now() - start;
      const memAfter = getMemoryMB();

      timings.push(elapsed);
      fileCounts.push(result.metrics?.filesGenerated || 0);
      byteCounts.push(result.metrics?.totalBytes || 0);
    }

    // Restore console
    console.log = origLog;
    console.error = origError;

    const s = stats(timings);
    const fcStats = stats(fileCounts);
    const bcStats = stats(byteCounts);
    const memSnap = getMemoryMB();

    const result = {
      ruleCount,
      tripleCount,
      iterations: ITERATIONS,
      timing: s,
      filesGenerated: fcStats,
      totalBytes: bcStats,
      memory: memSnap,
      throughput: {
        rulesPerSecond: ruleCount / (s.mean / 1000),
        bytesPerSecond: bcStats.mean / (s.mean / 1000),
      },
    };

    results.push(result);
    console.log(
      `  ${String(ruleCount).padStart(3)} rules (${String(tripleCount).padStart(6)} triples): ` +
        `total=${fmt(s.mean).padStart(10)} median=${fmt(s.median).padStart(10)} ` +
        `p95=${fmt(s.p95).padStart(10)} ` +
        `files=${fcStats.mean.toFixed(0).padStart(4)} ` +
        `bytes=${fmtBytes(bcStats.mean).padStart(8)} ` +
        `mem=${memSnap.heapUsed.toFixed(1).padStart(6)}MB ` +
        `rules/s=${result.throughput.rulesPerSecond.toFixed(1).padStart(6)}`
    );

    // Cleanup config
    await rm(configPath, { force: true });
  }

  return results;
}

/**
 * Benchmark: Memory scaling.
 * Track memory as we increase rule count with fixed ontology.
 */
async function benchmarkMemoryScaling(benchDir) {
  console.log('\n═══ BENCHMARK 5: Memory Scaling ═══\n');

  const tripleCount = ARGS.triples || 10000;
  const ontoDir = join(benchDir, 'mem-ontology');
  const templatesDir = join(benchDir, 'mem-templates');
  const outputDir = join(benchDir, 'mem-output');

  await mkdir(ontoDir, { recursive: true });
  await mkdir(templatesDir, { recursive: true });

  const ontoFile = join(ontoDir, 'benchmark.nt');
  await writeFile(ontoFile, generateOntology(tripleCount), 'utf-8');

  // Pre-generate all templates
  const complexities = ['simple', 'medium', 'complex'];
  const maxRules = SCALES[SCALES.length - 1];
  for (let i = 0; i < maxRules; i++) {
    await writeFile(
      join(templatesDir, `template-${i + 1}.njk`),
      generateTemplate(i + 1, complexities[i % 3]),
      'utf-8'
    );
  }

  const results = [];

  // Force GC if available
  if (global.gc) global.gc();
  const baselineMem = getMemoryMB();

  for (const ruleCount of SCALES) {
    await rm(outputDir, { force: true, recursive: true });
    await mkdir(outputDir, { recursive: true });

    const configPath = join(benchDir, `mem-config-${ruleCount}.toml`);
    await writeFile(configPath, generateConfig(ruleCount, 'mem-ontology', 'mem-templates', 'mem-output'), 'utf-8');

    if (global.gc) global.gc();
    const memBefore = getMemoryMB();

    // Suppress console output
    const origLog = console.log;
    const origError = console.error;
    console.log = () => {};
    console.error = () => {};

    await runSync({
      config: configPath,
      dryRun: false,
      verbose: false,
      force: true,
    });

    // Restore console
    console.log = origLog;
    console.error = origError;

    if (global.gc) global.gc();
    const memAfter = getMemoryMB();

    const result = {
      ruleCount,
      tripleCount,
      memoryBefore: memBefore,
      memoryAfter: memAfter,
      delta: {
        rss: memAfter.rss - memBefore.rss,
        heapUsed: memAfter.heapUsed - memBefore.heapUsed,
        heapTotal: memAfter.heapTotal - memBefore.heapTotal,
      },
    };

    results.push(result);
    console.log(
      `  ${String(ruleCount).padStart(3)} rules: ` +
        `heap=${memAfter.heapUsed.toFixed(1).padStart(6)}MB ` +
        `delta=${result.delta.heapUsed > 0 ? '+' : ''}${result.delta.heapUsed.toFixed(1).padStart(6)}MB ` +
        `rss=${memAfter.rss.toFixed(1).padStart(6)}MB ` +
        `heapTotal=${memAfter.heapTotal.toFixed(1).padStart(6)}MB`
    );

    await rm(configPath, { force: true });
  }

  return results;
}

// ─── Main Runner ────────────────────────────────────────────────────────────

async function main() {
  const startTotal = performance.now();
  const memStart = getMemoryMB();

  console.log('╔══════════════════════════════════════════════════════════╗');
  console.log('║           unrdf CLI Scalability Benchmark Suite        ║');
  console.log('╚══════════════════════════════════════════════════════════╝');
  console.log('');
  console.log(`  Scales:         ${SCALES.join(', ')} rules`);
  console.log(`  Triple sizes:   ${TRIPLE_SIZES.join(', ')} triples`);
  console.log(`  Iterations:     ${ITERATIONS}`);
  console.log(`  Quick mode:     ${ARGS.quick ? 'yes' : 'no'}`);
  console.log(`  Node.js:        ${process.version}`);
  console.log(`  Platform:       ${process.platform} ${process.arch}`);
  console.log(`  Memory (start): ${memStart.heapUsed.toFixed(1)}MB heap / ${memStart.rss.toFixed(1)}MB rss`);

  // Create temp benchmark directory
  const benchDir = join(tmpdir(), `unrdf-bench-${Date.now()}`);
  await mkdir(benchDir, { recursive: true });
  console.log(`  Benchmark dir:  ${benchDir}`);

  const benchmarkResults = {};

  try {
    // Run all benchmarks
    benchmarkResults.ontologyLoading = await benchmarkOntologyLoading(benchDir);
    benchmarkResults.sparqlQueries = await benchmarkSparqlQueries(benchDir);
    benchmarkResults.templateRendering = await benchmarkTemplateRendering(benchDir);
    benchmarkResults.fullPipeline = await benchmarkFullPipeline(benchDir);
    benchmarkResults.memoryScaling = await benchmarkMemoryScaling(benchDir);
  } finally {
    // Cleanup
    await rm(benchDir, { force: true, recursive: true });
    console.log(`\n  Cleaned up: ${benchDir}`);
  }

  const totalDuration = performance.now() - startTotal;
  const memEnd = getMemoryMB();

  // ─── Summary ────────────────────────────────────────────────────────────

  console.log('\n═══ SUMMARY ═══\n');

  // Pipeline scaling analysis
  const pipelineResults = benchmarkResults.fullPipeline;
  if (pipelineResults.length >= 2) {
    const first = pipelineResults[0];
    const last = pipelineResults[pipelineResults.length - 1];

    console.log('  Pipeline Scaling:');
    console.log(`    ${first.ruleCount} rules → ${fmt(first.timing.mean)}`);
    console.log(`    ${last.ruleCount} rules → ${fmt(last.timing.mean)}`);
    console.log(`    Ratio: ${(last.timing.mean / first.timing.mean).toFixed(2)}x slower`);

    // Detect scaling behavior
    const ratio = last.timing.mean / first.timing.mean;
    const ruleRatio = last.ruleCount / first.ruleCount;
    if (ratio < ruleRatio * 0.8) {
      console.log(`    Scaling: SUB-LINEAR (better than expected)`);
    } else if (ratio < ruleRatio * 1.2) {
      console.log(`    Scaling: LINEAR`);
    } else if (ratio < ruleRatio * 2.0) {
      console.log(`    Scaling: SUPER-LINEAR (some overhead)`);
    } else {
      console.log(`    Scaling: QUADRATIC+ (concerning — investigate)`);
    }
  }

  // Bottleneck analysis
  console.log('\n  Bottleneck Analysis (at 10 rules, 10K triples):');
  const pipeline10 = pipelineResults.find(r => r.ruleCount === 10);
  const query10 = benchmarkResults.sparqlQueries.find(r => r.ruleCount === 10);
  if (pipeline10 && query10) {
    const queryTotal = query10.timing.mean * query10.queries;
    const renderEst = pipeline10.timing.mean - queryTotal;
    console.log(`    Query total:    ${fmt(queryTotal).padStart(10)} (${((queryTotal / pipeline10.timing.mean) * 100).toFixed(0)}%)`);
    console.log(`    Render+IO est:  ${fmt(Math.max(0, renderEst)).padStart(10)} (${((Math.max(0, renderEst) / pipeline10.timing.mean) * 100).toFixed(0)}%)`);
    console.log(`    Pipeline total: ${fmt(pipeline10.timing.mean).padStart(10)}`);
  }

  console.log(`\n  Total benchmark time: ${fmt(totalDuration)}`);
  console.log(`  Memory: ${memStart.heapUsed.toFixed(1)}MB → ${memEnd.heapUsed.toFixed(1)}MB heap`);

  // ─── Output ─────────────────────────────────────────────────────────────

  const report = {
    metadata: {
      timestamp: new Date().toISOString(),
      nodeVersion: process.version,
      platform: `${process.platform} ${process.arch}`,
      scales: SCALES,
      tripleSizes: TRIPLE_SIZES,
      iterations: ITERATIONS,
      totalDuration,
    },
    benchmarks: benchmarkResults,
  };

  // Write JSON results
  const resultsDir = resolve(dirname(new URL(import.meta.url).pathname), 'results');
  await mkdir(resultsDir, { recursive: true });
  const resultFile = join(resultsDir, `benchmark-${Date.now()}.json`);
  await writeFile(resultFile, JSON.stringify(report, null, 2), 'utf-8');

  if (ARGS.json) {
    console.log('\n' + JSON.stringify(report, null, 2));
  } else {
    console.log(`\n  Results saved: ${resultFile}`);
  }

  return report;
}

main().catch(err => {
  console.error('Benchmark failed:', err);
  process.exit(1);
});
