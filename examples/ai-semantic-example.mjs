/**
 * @file AI Semantic Analysis Example
 * @module examples/ai-semantic-example
 *
 * @description
 * Demonstrates the AI semantic analysis module for UNRDF 2028 Phase 1.
 * Shows semantic analysis, NL→SPARQL, embeddings, and anomaly detection.
 */

import { Store, DataFactory } from 'n3';
import {
  createSemanticAnalyzer,
  createNLPQueryBuilder,
  createEmbeddingsManager,
  createAnomalyDetector,
} from '../src/knowledge-engine/ai-semantic/index.mjs';
import { query } from '../src/knowledge-engine/query.mjs';

const { namedNode, literal, quad } = DataFactory;

// Helper to create URIs
const ex = name => namedNode(`http://example.org/${name}`);
const rdfs = name => namedNode(`http://www.w3.org/2000/01/rdf-schema#${name}`);
const rdf = name => namedNode(`http://www.w3.org/1999/02/22-rdf-syntax-ns#${name}`);

async function main() {
  console.log('🤖 AI Semantic Analysis Example\n');

  // Create store with example data
  const store = new Store();

  // Define ontology
  store.addQuad(quad(ex('Person'), rdf('type'), rdfs('Class')));
  store.addQuad(quad(ex('Person'), rdfs('label'), literal('Person')));
  store.addQuad(quad(ex('Organization'), rdf('type'), rdfs('Class')));
  store.addQuad(quad(ex('Organization'), rdfs('label'), literal('Organization')));

  // Add people
  const people = ['Alice', 'Bob', 'Charlie', 'David', 'Eve'];
  for (const person of people) {
    store.addQuad(quad(ex(person), rdf('type'), ex('Person')));
    store.addQuad(quad(ex(person), rdfs('label'), literal(person)));
  }

  // Add relationships
  store.addQuad(quad(ex('Alice'), ex('knows'), ex('Bob')));
  store.addQuad(quad(ex('Alice'), ex('knows'), ex('Charlie')));
  store.addQuad(quad(ex('Bob'), ex('knows'), ex('Charlie')));
  store.addQuad(quad(ex('Bob'), ex('knows'), ex('David')));
  store.addQuad(quad(ex('Charlie'), ex('knows'), ex('Eve')));

  // Add employment
  store.addQuad(quad(ex('Alice'), ex('worksFor'), ex('CompanyA')));
  store.addQuad(quad(ex('Bob'), ex('worksFor'), ex('CompanyA')));
  store.addQuad(quad(ex('Charlie'), ex('worksFor'), ex('CompanyB')));

  // Add companies
  store.addQuad(quad(ex('CompanyA'), rdf('type'), ex('Organization')));
  store.addQuad(quad(ex('CompanyA'), rdfs('label'), literal('Company A')));
  store.addQuad(quad(ex('CompanyB'), rdf('type'), ex('Organization')));
  store.addQuad(quad(ex('CompanyB'), rdfs('label'), literal('Company B')));

  // Add anomaly: David has no label (data quality issue)
  // Add anomaly: Eve has very few connections (potential outlier)
  // Add anomaly: No reverse 'knows' relationships (missing inverse)

  console.log(`📊 Created store with ${store.size} triples\n`);

  // =========================================================================
  // 1. SEMANTIC ANALYSIS
  // =========================================================================
  console.log('🔍 SEMANTIC ANALYSIS\n');

  const analyzer = createSemanticAnalyzer({
    maxConcepts: 20,
    minConceptFrequency: 1,
  });

  const analysis = await analyzer.analyze(store);

  console.log('📌 Key Concepts:');
  analysis.concepts.slice(0, 5).forEach(concept => {
    console.log(`  - ${concept.uri}`);
    console.log(
      `    Frequency: ${concept.frequency}, Centrality: ${concept.centrality.toFixed(3)}`
    );
  });

  console.log('\n📌 Top Relationships:');
  analysis.relationships.slice(0, 3).forEach(rel => {
    console.log(`  - ${rel.subject} --[${rel.predicate}]--> ${rel.object}`);
    console.log(`    Strength: ${rel.strength.toFixed(3)}`);
  });

  console.log('\n📌 Detected Patterns:');
  analysis.patterns.slice(0, 3).forEach(pattern => {
    console.log(`  - ${pattern.pattern}`);
    console.log(`    Count: ${pattern.count}, Confidence: ${pattern.confidence.toFixed(3)}`);
  });

  console.log('\n📌 Ontology Suggestions:');
  analysis.suggestions.slice(0, 3).forEach(suggestion => {
    console.log(`  [${suggestion.priority}] ${suggestion.type}: ${suggestion.description}`);
  });

  console.log('\n📌 Graph Statistics:');
  console.log(`  Triples: ${analysis.statistics.totalTriples}`);
  console.log(`  Unique Subjects: ${analysis.statistics.uniqueSubjects}`);
  console.log(`  Unique Predicates: ${analysis.statistics.uniquePredicates}`);
  console.log(`  Avg Degree: ${analysis.statistics.avgDegree.toFixed(2)}`);
  console.log(`  Density: ${analysis.statistics.density.toFixed(4)}`);
  console.log(`  Duration: ${analysis.duration}ms\n`);

  // =========================================================================
  // 2. NL → SPARQL TRANSLATION
  // =========================================================================
  console.log('💬 NATURAL LANGUAGE QUERY TRANSLATION\n');

  const nlpBuilder = createNLPQueryBuilder({
    enableLLM: false,
  });

  const queries = ['list all person', 'who is alice', 'how many person', 'show all organizations'];

  for (const nlQuery of queries) {
    console.log(`📝 Query: "${nlQuery}"`);

    const result = await nlpBuilder.buildQuery(nlQuery, store);

    console.log(`  Intent: ${result.intent}`);
    console.log(`  Confidence: ${result.confidence.toFixed(2)}`);
    console.log(`  Method: ${result.method}`);
    console.log(`  Duration: ${result.duration}ms`);
    console.log(`  SPARQL:\n    ${result.sparql.replace(/\n/g, '\n    ')}`);

    // Execute the query
    try {
      const queryResult = await query(store, result.sparql);
      console.log(`  Results: ${Array.isArray(queryResult) ? queryResult.length : 'N/A'} items`);
    } catch (error) {
      console.log(`  Execution: ${error.message}`);
    }

    console.log('');
  }

  // =========================================================================
  // 3. GRAPH EMBEDDINGS
  // =========================================================================
  console.log('🧠 GRAPH EMBEDDINGS\n');

  const embeddings = createEmbeddingsManager({
    embeddingDim: 64,
    algorithm: 'transe',
    epochs: 50,
    learningRate: 0.01,
  });

  console.log('⏳ Training embeddings (TransE)...');
  const embResult = await embeddings.generateEmbeddings(store);

  console.log(`✅ Training complete in ${embResult.duration}ms`);
  console.log(`  Entities: ${embResult.entities}`);
  console.log(`  Relations: ${embResult.relations}`);
  console.log(`  Dimension: ${embResult.dimension}`);
  console.log(`  Algorithm: ${embResult.algorithm}\n`);

  // Compute similarities
  console.log('📊 Entity Similarities:');
  const entities = ['Alice', 'Bob', 'Charlie', 'David', 'Eve'];
  const target = 'Alice';

  for (const entity of entities) {
    if (entity === target) continue;

    const similarity = embeddings.computeSimilarity(ex(target).value, ex(entity).value);

    console.log(`  ${target} ↔ ${entity}: ${similarity.toFixed(3)}`);
  }

  // Most similar to Alice
  const similarities = entities
    .filter(e => e !== target)
    .map(e => ({
      entity: e,
      similarity: embeddings.computeSimilarity(ex(target).value, ex(e).value),
    }))
    .sort((a, b) => b.similarity - a.similarity);

  console.log(
    `\n  Most similar to ${target}: ${similarities[0].entity} (${similarities[0].similarity.toFixed(3)})\n`
  );

  // =========================================================================
  // 4. ANOMALY DETECTION
  // =========================================================================
  console.log('🚨 ANOMALY DETECTION\n');

  const detector = createAnomalyDetector({
    enableStatistical: true,
    enableMLBased: true,
    minConfidence: 0.5,
  });

  const anomalies = await detector.detectAnomalies(store);

  console.log(`🔍 Found ${anomalies.statistics.total} anomalies in ${anomalies.duration}ms\n`);

  console.log('📊 By Severity:');
  for (const [severity, count] of Object.entries(anomalies.statistics.bySeverity)) {
    console.log(`  ${severity}: ${count}`);
  }

  console.log('\n📊 By Type:');
  for (const [type, count] of Object.entries(anomalies.statistics.byType)) {
    console.log(`  ${type}: ${count}`);
  }

  console.log('\n🔎 Top Anomalies:');
  anomalies.anomalies.slice(0, 5).forEach((anomaly, i) => {
    console.log(`\n  ${i + 1}. [${anomaly.severity}] ${anomaly.type}`);
    console.log(`     ${anomaly.description}`);
    console.log(`     Confidence: ${anomaly.confidence.toFixed(2)}`);
    if (anomaly.subject) console.log(`     Subject: ${anomaly.subject}`);
    if (anomaly.evidence) console.log(`     Evidence: ${anomaly.evidence.join(', ')}`);
  });

  // =========================================================================
  // 5. STATISTICS
  // =========================================================================
  console.log('\n\n📈 MODULE STATISTICS\n');

  console.log('Semantic Analyzer:');
  const analyzerStats = analyzer.getStats();
  console.log(`  Analyses: ${analyzerStats.analyses}`);
  console.log(`  Cache hits: ${analyzerStats.cacheHits}`);
  console.log(`  Cache hit rate: ${(analyzerStats.cacheHitRate * 100).toFixed(1)}%`);
  console.log(`  Avg duration: ${analyzerStats.avgDuration.toFixed(2)}ms`);

  console.log('\nNLP Query Builder:');
  const nlpStats = nlpBuilder.getStats();
  console.log(`  Queries: ${nlpStats.queries}`);
  console.log(`  Cache hits: ${nlpStats.cacheHits}`);
  console.log(`  Cache hit rate: ${(nlpStats.cacheHitRate * 100).toFixed(1)}%`);
  console.log(`  Avg duration: ${nlpStats.avgDuration.toFixed(2)}ms`);

  console.log('\nEmbeddings Manager:');
  const embStats = embeddings.getStats();
  console.log(`  Training sessions: ${embStats.trainingSessions}`);
  console.log(`  Entity embeddings: ${embStats.entityEmbeddings}`);
  console.log(`  Relation embeddings: ${embStats.relationEmbeddings}`);
  console.log(`  Cache hit rate: ${(embStats.cacheHitRate * 100).toFixed(1)}%`);

  console.log('\nAnomaly Detector:');
  const detectorStats = detector.getStats();
  console.log(`  Detections: ${detectorStats.detections}`);
  console.log(`  Anomalies found: ${detectorStats.anomaliesFound}`);
  console.log(`  Avg duration: ${detectorStats.avgDuration.toFixed(2)}ms`);

  console.log('\n✅ Example complete!\n');
}

// Run example
main().catch(console.error);
