/**
 * Production README Example: Quick Start
 * Tests basic API using actual production exports
 */

import {
  createStoreContext,
  parseTurtle,
  query,
  asNamedNode,
  asLiteral,
  RdfEngine,
} from "unrdf";

console.log("🧪 Testing Quick Start with Production API...\n");

try {
  // 1. Initialize engine and store
  const engine = new RdfEngine();
  const storeContext = createStoreContext();

  // 2. Add RDF data using real Turtle syntax
  const turtleData = `
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .
    @prefix ex: <http://example.org/> .
    
    ex:alice a foaf:Person ;
        foaf:name "Alice" .
        
    ex:bob a foaf:Person ;
        foaf:name "Bob" .
  `;

  const store = await parseTurtle(turtleData);

  // 3. Query the data using SPARQL
  const results = await query(
    store,
    `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?name WHERE {
      ?person foaf:name ?name .
    }
  `,
  );

  console.log("Query results:", JSON.stringify(results, null, 2));

  // Verify expected output
  const hasAlice =
    results.some((r) => r.name && r.name.value === "Alice") ||
    results.some((r) => r.name && r.name === "Alice");
  const hasBob =
    results.some((r) => r.name && r.name.value === "Bob") ||
    results.some((r) => r.name && r.name === "Bob");

  if (hasAlice && hasBob && results.length === 2) {
    console.log("✅ Quick Start Production Test PASSED");
    console.log("   - Engine initialization successful");
    console.log("   - Turtle parsing working");
    console.log("   - SPARQL query execution successful");
    console.log("   - Found both Alice and Bob");
  } else {
    console.log("❌ Quick Start Production Test FAILED");
    console.log("   - Expected: Alice and Bob found");
    console.log("   - Got:", results.length, "results");
    console.log("   - First result:", JSON.stringify(results[0]));
  }

  process.exit(hasAlice && hasBob ? 0 : 1);
} catch (error) {
  console.log("❌ Quick Start Production Test FAILED with error:");
  console.error(error);
  process.exit(1);
}
