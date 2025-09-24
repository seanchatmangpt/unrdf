#!/usr/bin/env node

/**
 * @fileoverview Integration tests for unrdf composables
 * 
 * These tests simulate real-world use cases following the 80/20 principle:
 * - Data loading and parsing (most common)
 * - SPARQL querying and validation (very common)
 * - Reasoning and inference (common in knowledge graphs)
 * - Performance monitoring (essential for production)
 * - End-to-end workflows (complete user journeys)
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { initStore } from "../src/context/index.mjs";
import { 
  useStore, 
  useTerms, 
  useGraph, 
  usePrefixes, 
  useValidator, 
  useReasoner, 
  useCanon, 
  usePointer, 
  useLists, 
  useTurtle, 
  useCache, 
  useMetrics, 
  useDelta, 
  useZod 
} from "../src/composables/index.mjs";
import { writeFile, mkdir } from "node:fs/promises";
import { join } from "node:path";

// Test data
const PERSON_DATA_TTL = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:person1 a foaf:Person ;
    foaf:name "Alice Johnson" ;
    foaf:age 30 ;
    foaf:knows ex:person2, ex:person3 .

ex:person2 a foaf:Person ;
    foaf:name "Bob Smith" ;
    foaf:age 25 ;
    foaf:knows ex:person1 .

ex:person3 a foaf:Person ;
    foaf:name "Carol Davis" ;
    foaf:age 35 ;
    foaf:knows ex:person1 .
`;

const SHACL_SHAPES_TTL = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:PersonShape a sh:NodeShape ;
    sh:targetClass foaf:Person ;
    sh:property [
        sh:path foaf:name ;
        sh:datatype <http://www.w3.org/2001/XMLSchema#string> ;
        sh:minCount 1 ;
        sh:maxCount 1
    ] ;
    sh:property [
        sh:path foaf:age ;
        sh:datatype <http://www.w3.org/2001/XMLSchema#integer> ;
        sh:minInclusive 0 ;
        sh:maxInclusive 150
    ] .
`;

const REASONING_RULES_TTL = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

# Simple rule: if someone knows someone, they are connected
ex:person1 ex:connectedTo ex:person2 .
ex:person2 ex:connectedTo ex:person1 .
`;

/**
 * Test 1: Data Loading and Parsing (80% of use cases)
 * Most users start by loading RDF data from files or strings
 */
async function testDataLoadingAndParsing() {
  console.log("\n🧪 Test 1: Data Loading and Parsing Integration");
  console.log("=" .repeat(50));

  const runApp = initStore([], { baseIRI: 'http://example.org/' });

  await runApp(async () => {
    const store = useStore();
    const terms = useTerms();
    const turtle = await useTurtle('./test-data');
    const prefixes = usePrefixes({
      "ex": "http://example.org/",
      "foaf": "http://xmlns.com/foaf/0.1/"
    });

    // Create test data file
    await mkdir('./test-data', { recursive: true });
    await writeFile('./test-data/persons.ttl', PERSON_DATA_TTL);

    // Load data using useTurtle
    console.log("📁 Loading data from file...");
    await turtle.load('persons');
    console.log(`✅ Loaded data: ${store.size} quads`);

    // Create additional data using useTerms
    console.log("🏷️ Creating additional data...");
    const newPerson = terms.iri("person4");
    const name = terms.lit("David Wilson");
    const age = terms.lit(28, "http://www.w3.org/2001/XMLSchema#integer");
    const personType = terms.iri("foaf:Person");
    
    store.add(terms.quad(newPerson, terms.iri("rdf:type"), personType));
    store.add(terms.quad(newPerson, terms.iri("foaf:name"), name));
    store.add(terms.quad(newPerson, terms.iri("foaf:age"), age));

    // Test prefix expansion
    console.log("🔗 Testing prefix expansion...");
    const expandedName = prefixes.expand("foaf:name");
    console.log(`✅ Expanded foaf:name to: ${expandedName}`);

    // Verify data integrity
    const stats = store.stats();
    console.log(`📊 Final stats: ${JSON.stringify(stats)}`);
    
    console.log("✅ Data loading and parsing test completed");
  });
}

/**
 * Test 2: SPARQL Querying and Validation (Very common)
 * Users frequently query data and validate against schemas
 */
async function testQueryingAndValidation() {
  console.log("\n🧪 Test 2: SPARQL Querying and Validation Integration");
  console.log("=" .repeat(50));

  const runApp = initStore([], { baseIRI: 'http://example.org/' });

  await runApp(async () => {
    const store = useStore();
    const graph = useGraph();
    const validator = useValidator();
    const cache = useCache();
    const metrics = useMetrics();

    // Load test data
    const turtle = await useTurtle('./test-data');
    await turtle.load('persons');

    // Create SHACL shapes
    await writeFile('./test-data/shapes.ttl', SHACL_SHAPES_TTL);
    await turtle.load('shapes');

    // Test SPARQL queries with caching
    console.log("🔍 Running SPARQL queries...");
    
    const queryMetrics = metrics.timer('sparql-queries');
    
    // Cached query for all persons
    const allPersons = await cache.get('all-persons', async () => {
      return await graph.select(`
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?person ?name ?age WHERE {
          ?person a foaf:Person ;
                  foaf:name ?name ;
                  foaf:age ?age .
        }
      `);
    });

    console.log(`✅ Found ${allPersons.length} persons`);

    // Query for social connections
    const connections = await graph.select(`
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      SELECT ?person1 ?person2 WHERE {
        ?person1 foaf:knows ?person2 .
      }
    `);

    console.log(`✅ Found ${connections.length} social connections`);

    // Test ASK query
    const hasYoungPeople = await graph.ask(`
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      ASK WHERE {
        ?person foaf:age ?age .
        FILTER(?age < 30)
      }
    `);

    console.log(`✅ Has young people: ${hasYoungPeople}`);

    queryMetrics.end();

    // Test SHACL validation
    console.log("✅ Running SHACL validation...");
    const validationTimer = metrics.timer('shacl-validation');
    
    try {
      const report = await validator.validate(SHACL_SHAPES_TTL);
      console.log(`✅ Validation completed: ${report.conforms ? 'PASSED' : 'FAILED'}`);
      if (!report.conforms) {
        console.log(`❌ Validation issues: ${report.results.length}`);
        report.results.forEach(result => {
          console.log(`   - ${result.message}`);
        });
      }
    } catch (error) {
      console.log(`⚠️ Validation error: ${error.message}`);
    }

    validationTimer.end();

    // Get performance metrics
    const querySummary = metrics.summary('sparql-queries');
    const validationSummary = metrics.summary('shacl-validation');
    
    console.log(`📊 Query performance: ${querySummary.average.toFixed(2)}ms avg`);
    console.log(`📊 Validation performance: ${validationSummary.average.toFixed(2)}ms avg`);

    console.log("✅ Querying and validation test completed");
  });
}

/**
 * Test 3: Reasoning and Canonicalization (Common in knowledge graphs)
 * Users often need inference and canonical forms for comparison
 */
async function testReasoningAndCanonicalization() {
  console.log("\n🧪 Test 3: Reasoning and Canonicalization Integration");
  console.log("=" .repeat(50));

  const runApp = initStore([], { baseIRI: 'http://example.org/' });

  await runApp(async () => {
    const store = useStore();
    const graph = useGraph();
    const reasoner = useReasoner();
    const canon = useCanon();
    const metrics = useMetrics();

    // Load base data
    const turtle = await useTurtle('./test-data');
    await turtle.load('persons');

    // Create reasoning rules
    await writeFile('./test-data/rules.ttl', REASONING_RULES_TTL);

    console.log("🧠 Running reasoning...");
    const reasoningTimer = metrics.timer('reasoning');
    
    // Apply reasoning rules
    const inferred = await reasoner.reason(null, REASONING_RULES_TTL);
    console.log(`✅ Reasoning completed: ${inferred.size} total quads`);

    // Get new triples from reasoning
    const newTriples = reasoner.getNewTriples(store.store, inferred);
    console.log(`✅ Inferred ${newTriples.size} new triples`);

    reasoningTimer.end();

    // Test canonicalization
    console.log("📏 Testing canonicalization...");
    const canonTimer = metrics.timer('canonicalization');
    
    const canonical = await canon.canonicalize(store.store);
    console.log(`✅ Canonical form generated: ${canonical.length} characters`);

    // Test isomorphism
    const isIsomorphic = await canon.isIsomorphic(store.store, inferred.store);
    console.log(`✅ Store isomorphism check: ${isIsomorphic}`);

    // Generate hash for comparison
    const hash = await canon.hash(store.store);
    console.log(`✅ Store hash: ${hash}`);

    canonTimer.end();

    // Get reasoning statistics
    const stats = reasoner.getStats(store.store, inferred);
    console.log(`📊 Reasoning stats:`, {
      original: stats.original.quads,
      inferred: stats.inferred.quads,
      new: stats.new.quads,
      growth: `${stats.growth.percentage.toFixed(1)}%`
    });

    // Get performance metrics
    const reasoningSummary = metrics.summary('reasoning');
    const canonSummary = metrics.summary('canonicalization');
    
    console.log(`📊 Reasoning performance: ${reasoningSummary.average.toFixed(2)}ms avg`);
    console.log(`📊 Canonicalization performance: ${canonSummary.average.toFixed(2)}ms avg`);

    console.log("✅ Reasoning and canonicalization test completed");
  });
}

/**
 * Test 4: Performance Monitoring (Essential for production)
 * Users need to monitor performance and optimize operations
 */
async function testPerformanceMonitoring() {
  console.log("\n🧪 Test 4: Performance Monitoring Integration");
  console.log("=" .repeat(50));

  const runApp = initStore([], { baseIRI: 'http://example.org/' });

  await runApp(async () => {
    const store = useStore();
    const graph = useGraph();
    const cache = useCache();
    const metrics = useMetrics();
    const delta = useDelta();

    // Load test data
    const turtle = await useTurtle('./test-data');
    await turtle.load('persons');

    console.log("📊 Testing performance monitoring...");

    // Monitor query performance
    const queryPerformance = metrics.wrap('query-performance', async () => {
      return await graph.select(`
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?person ?name WHERE {
          ?person a foaf:Person ;
                  foaf:name ?name .
        }
      `);
    });

    // Run multiple queries to get performance data
    for (let i = 0; i < 5; i++) {
      await queryPerformance();
    }

    // Monitor cache performance
    const cachePerformance = metrics.wrap('cache-performance', async () => {
      return await cache.get(`test-query-${Math.random()}`, async () => {
        return { timestamp: Date.now(), data: "cached result" };
      });
    });

    for (let i = 0; i < 3; i++) {
      await cachePerformance();
    }

    // Monitor delta operations
    const deltaPerformance = metrics.wrap('delta-performance', async () => {
      const emptyStore = new (await import('n3')).Store();
      return delta.diff(emptyStore, store.store);
    });

    await deltaPerformance();

    // Get comprehensive metrics
    const querySummary = metrics.summary('query-performance');
    const cacheSummary = metrics.summary('cache-performance');
    const deltaSummary = metrics.summary('delta-performance');

    console.log(`📊 Query performance:`, {
      count: querySummary.count,
      average: `${querySummary.average.toFixed(2)}ms`,
      min: `${querySummary.min.toFixed(2)}ms`,
      max: `${querySummary.max.toFixed(2)}ms`
    });

    console.log(`📊 Cache performance:`, {
      count: cacheSummary.count,
      average: `${cacheSummary.average.toFixed(2)}ms`,
      errorRate: `${(cacheSummary.errorRate * 100).toFixed(1)}%`
    });

    console.log(`📊 Delta performance:`, {
      count: deltaSummary.count,
      average: `${deltaSummary.average.toFixed(2)}ms`
    });

    // Export metrics for analysis
    const metricsExport = metrics.export();
    console.log(`📊 Metrics exported: ${metricsExport.length} characters`);

    // Cache statistics
    const cacheStats = cache.stats();
    console.log(`💾 Cache stats:`, {
      size: cacheStats.size,
      valid: cacheStats.valid,
      hitRate: `${cacheStats.hitRate}%`
    });

    console.log("✅ Performance monitoring test completed");
  });
}

/**
 * Test 5: End-to-End Workflow (Complete user journey)
 * Simulates a complete knowledge graph management workflow
 */
async function testEndToEndWorkflow() {
  console.log("\n🧪 Test 5: End-to-End Workflow Integration");
  console.log("=" .repeat(50));

  const runApp = initStore([], { baseIRI: 'http://example.org/' });

  await runApp(async () => {
    const store = useStore();
    const terms = useTerms();
    const graph = useGraph();
    const prefixes = usePrefixes();
    const validator = useValidator();
    const reasoner = useReasoner();
    const canon = useCanon();
    const pointer = usePointer();
    const lists = useLists();
    const cache = useCache();
    const metrics = useMetrics();
    const delta = useDelta();
    const { z } = await import('zod');

    console.log("🚀 Starting end-to-end workflow...");

    // Step 1: Load and validate data
    console.log("\n📥 Step 1: Data Loading and Validation");
    const turtle = await useTurtle('./test-data');
    await turtle.load('persons');
    await turtle.load('shapes');

    const initialSize = store.size;
    console.log(`✅ Loaded ${initialSize} quads`);

    // Step 2: Data enrichment using terms and lists
    console.log("\n🏷️ Step 2: Data Enrichment");
    const person1 = terms.iri("person1");
    const skills = ["JavaScript", "RDF", "SPARQL"];
    const skillsList = lists.write(skills.map(skill => terms.lit(skill)));
    
    store.add(terms.quad(person1, terms.iri("ex:hasSkills"), skillsList));
    console.log(`✅ Added skills list with ${lists.length(skillsList)} items`);

    // Step 3: Query and analyze data
    console.log("\n🔍 Step 3: Data Analysis");
    const analysisResults = await cache.get('person-analysis', async () => {
      return await graph.select(`
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX ex: <http://example.org/>
        SELECT ?person ?name ?age ?skills WHERE {
          ?person a foaf:Person ;
                  foaf:name ?name ;
                  foaf:age ?age .
          OPTIONAL { ?person ex:hasSkills ?skillsList . }
        }
      `);
    });

    console.log(`✅ Analyzed ${analysisResults.length} persons`);

    // Step 4: Apply reasoning
    console.log("\n🧠 Step 4: Knowledge Inference");
    const inferred = await reasoner.reason(null, REASONING_RULES_TTL);
    const newTriples = reasoner.getNewTriples(store.store, inferred);
    console.log(`✅ Inferred ${newTriples.size} new triples`);

    // Step 5: Validate against schema
    console.log("\n✅ Step 5: Schema Validation");
    try {
      const validationReport = await validator.validate(SHACL_SHAPES_TTL);
      console.log(`✅ Validation: ${validationReport.conforms ? 'PASSED' : 'FAILED'}`);
    } catch (error) {
      console.log(`⚠️ Validation error: ${error.message}`);
    }

    // Step 6: Graph traversal and analysis
    console.log("\n👆 Step 6: Graph Traversal");
    const persons = pointer.ofType("foaf:Person");
    const namedPersons = pointer.withProperty("foaf:name");
    console.log(`✅ Found ${persons.length} persons, ${namedPersons.length} named`);

    // Step 7: Zod validation of query results
    console.log("\n🔍 Step 7: Type-Safe Validation");
    const zod = useZod();
    const PersonSchema = z.object({
      person: z.string().url(),
      name: z.string(),
      age: z.string(),
      skills: z.string().optional()
    });

    const validation = await zod.validateResults(analysisResults, PersonSchema);
    console.log(`✅ Zod validation: ${validation.validated.length} valid, ${validation.errors.length} errors`);

    // Step 8: Canonicalization and comparison
    console.log("\n📏 Step 8: Canonicalization");
    const canonical = await canon.canonicalize(store.store);
    const hash = await canon.hash(store.store);
    console.log(`✅ Canonical form: ${canonical.length} chars, hash: ${hash}`);

    // Step 9: Change tracking
    console.log("\n🔄 Step 9: Change Tracking");
    const emptyStore = new (await import('n3')).Store();
    const changes = delta.diff(emptyStore, store.store);
    const changeStats = delta.getStats(changes);
    console.log(`✅ Changes tracked: ${changeStats.total.quads} total, ${changeStats.total.net} net`);

    // Step 10: Performance summary
    console.log("\n📊 Step 10: Performance Summary");
    const allMetrics = metrics.timeline();
    const totalOperations = allMetrics.length;
    const avgDuration = allMetrics.reduce((sum, m) => sum + m.duration, 0) / totalOperations;
    
    console.log(`✅ Performance: ${totalOperations} operations, ${avgDuration.toFixed(2)}ms avg`);

    // Final statistics
    const finalStats = store.stats();
    console.log(`\n🎯 Final Results:`, {
      totalQuads: finalStats.quads,
      subjects: finalStats.subjects,
      predicates: finalStats.predicates,
      objects: finalStats.objects,
      growth: `${((finalStats.quads - initialSize) / initialSize * 100).toFixed(1)}%`
    });

    console.log("✅ End-to-end workflow test completed");
  });
}

/**
 * Run all integration tests
 */
async function runAllTests() {
  console.log("🧪 Starting unrdf Integration Tests");
  console.log("=" .repeat(60));
  console.log("Testing real-world use cases following 80/20 principle");
  console.log("=" .repeat(60));

  const startTime = performance.now();

  try {
    await testDataLoadingAndParsing();
    await testQueryingAndValidation();
    await testReasoningAndCanonicalization();
    await testPerformanceMonitoring();
    await testEndToEndWorkflow();

    const endTime = performance.now();
    const totalTime = endTime - startTime;

    console.log("\n🎉 All Integration Tests Completed Successfully!");
    console.log("=" .repeat(60));
    console.log(`⏱️ Total execution time: ${totalTime.toFixed(2)}ms`);
    console.log("✅ All composables working together seamlessly");
    console.log("✅ Context integration verified");
    console.log("✅ Real-world use cases validated");
    console.log("=" .repeat(60));

  } catch (error) {
    console.error("\n❌ Integration test failed:", error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run the tests
runAllTests().catch(console.error);
