/**
 * @file Example demonstrating the Knowledge Engine usage.
 * @module knowledge-engine-example
 * 
 * This example shows how to use the Knowledge Engine for:
 * - Parsing and serialization
 * - SPARQL querying
 * - SHACL validation
 * - N3 reasoning
 * - Canonicalization
 * - Transaction management with hooks
 */

import {
  parseTurtle,
  toTurtle,
  query,
  validateShacl,
  reason,
  TransactionManager,
  createKnowledgeEngine
} from './knowledge-engine.mjs';
import { DataFactory, Store } from 'n3';

const { namedNode, quad } = DataFactory;

/**
 * Main example function demonstrating all Knowledge Engine capabilities.
 */
async function main() {
  console.log('🚀 Knowledge Engine Example\n');

  // =================================================================
  // == 1. Basic Parsing and Serialization
  // =================================================================
  console.log('1. Parsing and Serialization');
  console.log('============================');

  const ttl = `
    @prefix ex: <http://example.org/> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
    
    ex:alice a ex:Person ;
      ex:name "Alice" ;
      ex:age 30 ;
      ex:knows ex:bob .
    
    ex:bob a ex:Person ;
      ex:name "Bob" ;
      ex:age 25 ;
      ex:knows ex:alice .
  `;

  let store = await parseTurtle(ttl);
  console.log("Initial store size:", store.size);
  console.log("Serialized Turtle:");
  console.log(await toTurtle(store));
  console.log();

  // =================================================================
  // == 2. SPARQL Querying
  // =================================================================
  console.log('2. SPARQL Querying');
  console.log('==================');

  // SELECT query
  const selectResults = await query(store, `
    PREFIX ex: <http://example.org/>
    SELECT ?name ?age WHERE { 
      ?person ex:name ?name ;
              ex:age ?age .
    }
  `);
  console.log("SELECT query results:", selectResults);

  // ASK query
  const askResult = await query(store, `
    PREFIX ex: <http://example.org/>
    ASK WHERE { 
      ex:alice ex:knows ex:bob 
    }
  `);
  console.log("ASK query result:", askResult);

  // CONSTRUCT query
  const constructResult = await query(store, `
    PREFIX ex: <http://example.org/>
    CONSTRUCT {
      ?person ex:type ex:Person .
    } WHERE {
      ?person a ex:Person .
    }
  `);
  console.log("CONSTRUCT query result size:", constructResult.size);
  console.log();

  // =================================================================
  // == 3. SHACL Validation
  // =================================================================
  console.log('3. SHACL Validation');
  console.log('===================');

  const shapesTtl = `
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix ex: <http://example.org/> .
    @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
    
    ex:PersonShape a sh:NodeShape ;
      sh:targetClass ex:Person ;
      sh:property [
        sh:path ex:name ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string
      ] ;
      sh:property [
        sh:path ex:age ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0
      ] .
  `;

  const validationReport = await validateShacl(store, shapesTtl);
  console.log("SHACL validation report:");
  console.log("- Conforms:", validationReport.conforms);
  console.log("- Results count:", validationReport.results.length);
  
  if (validationReport.results.length > 0) {
    console.log("- Validation results:");
    validationReport.results.forEach((result, i) => {
      console.log(`  ${i + 1}. ${result.message} (${result.focusNode})`);
    });
  }
  console.log();

  // =================================================================
  // == 4. N3 Reasoning
  // =================================================================
  console.log('4. N3 Reasoning');
  console.log('===============');

  const rulesTtl = `
    @prefix ex: <http://example.org/> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
    
    # Rule: If A knows B, then B knows A (symmetric relationship)
    { ?x ex:knows ?y } => { ?y ex:knows ?x } .
    
    # Rule: If someone is a Person, they have a type
    { ?x a ex:Person } => { ?x ex:type ex:Person } .
  `;

  const reasonedStore = await reason(store, rulesTtl);
  console.log("Original store size:", store.size);
  console.log("Reasoned store size:", reasonedStore.size);
  console.log("New quads added:", reasonedStore.size - store.size);
  
  // Show the new quads
  const newQuads = reasonedStore.getQuads().filter(q => 
    !store.getQuads().some(original => original.equals(q))
  );
  console.log("Newly inferred quads:");
  newQuads.forEach(quad => {
    console.log(`  ${quad.subject.value} ${quad.predicate.value} ${quad.object.value}`);
  });
  console.log();

  // =================================================================
  // == 5. Canonicalization
  // =================================================================
  console.log('5. Canonicalization');
  console.log('===================');

  const { canonicalize, isIsomorphic } = await import('./knowledge-engine.mjs');
  
  const canonical = await canonicalize(store);
  console.log("Canonical N-Quads length:", canonical.length);
  console.log("First 200 characters of canonical form:");
  console.log(canonical.substring(0, 200) + "...");
  
  // Test isomorphism
  const storeCopy = new Store(store.getQuads());
  const isIsomorphicResult = await isIsomorphic(store, storeCopy);
  console.log("Store is isomorphic to its copy:", isIsomorphicResult);
  console.log();

  // =================================================================
  // == 6. Transaction Management with Hooks
  // =================================================================
  console.log('6. Transaction Management with Hooks');
  console.log('====================================');

  const tx = new TransactionManager();

  // Add a pre-hook that vetoes transactions adding "eve"
  tx.addHook({
    id: "no-eve",
    mode: "pre",
    condition: async (store, delta) => {
      const hasEve = delta.additions.some(q => 
        q.object.value.includes("eve") || q.subject.value.includes("eve")
      );
      return !hasEve;
    },
    effect: "veto"
  });

  // Add a post-hook that logs successful commits
  tx.addHook({
    id: "log-commit",
    mode: "post",
    condition: async () => true,
    effect: async (store, delta) => {
      console.log(`Post-hook: Successfully committed ${delta.additions.length} additions, ${delta.removals.length} removals`);
    }
  });

  // Transaction 1: Should be vetoed (adds "eve")
  console.log("Transaction 1: Adding eve (should be vetoed)");
  let delta = {
    additions: [
      quad(namedNode("http://example.org/bob"), namedNode("http://example.org/knows"), namedNode("http://example.org/eve"))
    ],
    removals: []
  };
  
  let result1 = await tx.apply(store, delta);
  console.log("Receipt 1:");
  console.log("- Committed:", result1.receipt.committed);
  console.log("- Hook results:", result1.receipt.hookResults.map(r => `${r.hookId}: ${r.result}`));
  console.log("- Store size unchanged:", result1.store.size === store.size);
  console.log();

  // Transaction 2: Should succeed (adds "carol")
  console.log("Transaction 2: Adding carol (should succeed)");
  delta = {
    additions: [
      quad(namedNode("http://example.org/bob"), namedNode("http://example.org/knows"), namedNode("http://example.org/carol"))
    ],
    removals: []
  };
  
  let result2 = await tx.apply(store, delta);
  console.log("Receipt 2:");
  console.log("- Committed:", result2.receipt.committed);
  console.log("- Hook results:", result2.receipt.hookResults.map(r => `${r.hookId}: ${r.result}`));
  console.log("- Store size increased:", result2.store.size > store.size);
  console.log();

  // =================================================================
  // == 7. Using the Complete Knowledge Engine
  // =================================================================
  console.log('7. Complete Knowledge Engine');
  console.log('============================');

  const engine = createKnowledgeEngine({
    baseIRI: 'http://example.org/',
    strictMode: true
  });

  // Parse with engine
  const engineStore = await engine.parseTurtle(ttl);
  console.log("Engine parsed store size:", engineStore.size);

  // Query with engine
  const engineResults = await engine.query(engineStore, `
    PREFIX ex: <http://example.org/>
    SELECT ?name WHERE { 
      ?person ex:name ?name 
    }
  `);
  console.log("Engine query results:", engineResults);

  // Validate with engine
  const engineValidation = await engine.validateShacl(engineStore, shapesTtl);
  console.log("Engine validation conforms:", engineValidation.conforms);

  // Create transaction manager with engine
  const engineTx = await engine.createTransactionManager();
  console.log("Engine transaction manager created with", engineTx.getStats().totalHooks, "hooks");
  console.log();

  // =================================================================
  // == 8. Error Handling Examples
  // =================================================================
  console.log('8. Error Handling');
  console.log('=================');

  try {
    // Invalid Turtle
    await parseTurtle("invalid turtle syntax");
  } catch (error) {
    console.log("Caught parsing error:", error.message);
  }

  try {
    // Invalid SPARQL
    await query(store, "INVALID SPARQL QUERY");
  } catch (error) {
    console.log("Caught query error:", error.message);
  }

  try {
    // Invalid store for validation
    await validateShacl(null, shapesTtl);
  } catch (error) {
    console.log("Caught validation error:", error.message);
  }

  console.log();
  console.log('✅ Knowledge Engine example completed successfully!');
}

/**
 * Run the example with error handling.
 */
async function runExample() {
  try {
    await main();
  } catch (error) {
    console.error('❌ Example failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run the example if this file is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runExample();
}

export { main, runExample };
