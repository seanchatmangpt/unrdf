/**
 * @file RDF Visualization and Debugging Tools Example
 * @description Demonstrates graph visualization, query explanation, and graph inspection
 */

import { createStore, namedNode, literal } from '../packages/core/src/rdf/store.mjs';
import {
  toDOT,
  toMermaid,
  toASCII,
  toHTML,
  extractSubgraph,
} from '../packages/core/src/viz/graph-visualizer.mjs';
import {
  explainQuery,
  formatPlanAsTree,
  trackQueryStats,
} from '../packages/core/src/viz/query-explainer.mjs';
import {
  generateInspectionReport,
  getGraphStatistics,
  analyzeNamespaces,
  assessDataQuality,
} from '../packages/core/src/debug/rdf-inspector.mjs';
import { executeQuerySync } from '../packages/core/src/sparql/executor-sync.mjs';

// ============================================================================
// 1. Create Sample RDF Data
// ============================================================================

const store = createStore();

// Add FOAF data
store.add({
  subject: namedNode('http://example.org/alice'),
  predicate: namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
  object: namedNode('http://xmlns.com/foaf/0.1/Person'),
});

store.add({
  subject: namedNode('http://example.org/alice'),
  predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
  object: literal('Alice Smith'),
});

store.add({
  subject: namedNode('http://example.org/alice'),
  predicate: namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
  object: literal('Alice', 'en'),
});

store.add({
  subject: namedNode('http://example.org/alice'),
  predicate: namedNode('http://xmlns.com/foaf/0.1/knows'),
  object: namedNode('http://example.org/bob'),
});

store.add({
  subject: namedNode('http://example.org/bob'),
  predicate: namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
  object: namedNode('http://xmlns.com/foaf/0.1/Person'),
});

store.add({
  subject: namedNode('http://example.org/bob'),
  predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
  object: literal('Bob Johnson'),
});

console.log('Created sample RDF graph with 6 triples\n');

// ============================================================================
// 2. Graph Visualization
// ============================================================================

console.log('═══════════════════════════════════════════════════════════════');
console.log('GRAPH VISUALIZATION');
console.log('═══════════════════════════════════════════════════════════════\n');

// ASCII Art Visualization
console.log('ASCII Art Visualization:');
console.log(toASCII(store, { limit: 10 }));

// DOT Format (for Graphviz)
console.log('\nDOT Format (Graphviz):');
const dot = toDOT(store, { direction: 'LR', nodeShape: 'box' });
console.log(dot.split('\n').slice(0, 10).join('\n') + '\n...\n');

// Mermaid Diagram
console.log('Mermaid Diagram:');
const mermaid = toMermaid(store, { direction: 'TB' });
console.log(mermaid.split('\n').slice(0, 5).join('\n') + '\n...\n');

// HTML with D3.js (just show it's generated)
const html = toHTML(store, { width: 800, height: 600, theme: 'light' });
console.log(`HTML visualization generated (${html.length} bytes)\n`);

// Extract subgraph
console.log('Subgraph extraction (Alice and connections):');
const subgraph = extractSubgraph(store, {
  subject: 'http://example.org/alice',
  depth: 1,
});
console.log(`Extracted ${subgraph.length} triples\n`);

// ============================================================================
// 3. Query Explanation
// ============================================================================

console.log('═══════════════════════════════════════════════════════════════');
console.log('QUERY EXPLANATION');
console.log('═══════════════════════════════════════════════════════════════\n');

const query = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person ?name WHERE {
    ?person a foaf:Person .
    ?person foaf:name ?name .
    FILTER(STRLEN(?name) > 5)
  }
  LIMIT 10
`;

console.log('SPARQL Query:');
console.log(query);

console.log('\nExecution Plan:');
const plan = explainQuery(query, { includeOptimizations: true });
console.log(formatPlanAsTree(plan));

// ============================================================================
// 4. Query Performance Tracking
// ============================================================================

console.log('═══════════════════════════════════════════════════════════════');
console.log('QUERY PERFORMANCE TRACKING');
console.log('═══════════════════════════════════════════════════════════════\n');

const stats = await trackQueryStats(
  async () => executeQuerySync(store, query),
  query
);

console.log(`Execution Time:  ${stats.executionTime.toFixed(2)}ms`);
console.log(`Memory Delta:    ${(stats.memoryDelta / 1024).toFixed(2)} KB`);
console.log(`Result Count:    ${stats.resultCount} row(s)`);
console.log(`Success:         ${stats.success}`);
console.log();

// ============================================================================
// 5. Graph Inspection and Quality Assessment
// ============================================================================

console.log('═══════════════════════════════════════════════════════════════');
console.log('GRAPH INSPECTION & QUALITY ASSESSMENT');
console.log('═══════════════════════════════════════════════════════════════\n');

// Comprehensive report
const report = generateInspectionReport(store, { includeQuality: true });
console.log(report);

// Detailed statistics
console.log('\nDetailed Statistics:');
const stats2 = getGraphStatistics(store, {
  includeNamespaces: true,
  includeOrphans: true,
  includeQuality: true,
});
console.log(JSON.stringify(stats2, null, 2));

// Namespace analysis
console.log('\nNamespace Analysis:');
const namespaces = analyzeNamespaces(store);
for (const ns of namespaces.slice(0, 5)) {
  console.log(`  ${ns.namespace.padEnd(50)} ${ns.count} uses`);
}

// Data quality assessment
console.log('\nData Quality Assessment:');
const quality = assessDataQuality(store);
console.log(`  Overall Score:     ${quality.score}/100`);
console.log(`  Label Coverage:    ${quality.labelCoverage}%`);
console.log(`  Type Coverage:     ${quality.typeCoverage}%`);
console.log(`  Literal Quality:   ${quality.literalQuality}%`);

console.log('\n✓ Example completed successfully!');
