/**
 * Production README Example: Simple Knowledge Graph
 * Tests parseTurtle + SPARQL query with prefixes using real APIs
 */

import { parseTurtle, query, createStoreContext, RdfEngine } from "unrdf";

console.log("🧪 Testing Knowledge Graph with Production API...\n");

try {
  const engine = new RdfEngine();
  const storeContext = createStoreContext();

  // Parse and load data
  const ttl = `
    @prefix ex: <http://example.org/> .
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .

    ex:alice a foaf:Person ;
             foaf:name "Alice" ;
             foaf:knows ex:bob .

    ex:bob a foaf:Person ;
           foaf:name "Bob" ;
           foaf:knows ex:alice .
  `;

  const store = await parseTurtle(ttl);

  // Query social network
  const friends = await query(
    store,
    `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX ex: <http://example.org/>
    SELECT ?person ?name ?friend ?friendName
    WHERE {
      ?person foaf:knows ?friend .
      ?person foaf:name ?name .
      ?friend foaf:name ?friendName .
    }
  `,
  );

  console.log("Query results:", JSON.stringify(friends, null, 2));

  // Verify expected output
  const hasAliceKnowsBob = friends.some(
    (r) =>
      (r.name === "Alice" || r.name?.value === "Alice") &&
      (r.friendName === "Bob" || r.friendName?.value === "Bob"),
  );
  const hasBobKnowsAlice = friends.some(
    (r) =>
      (r.name === "Bob" || r.name?.value === "Bob") &&
      (r.friendName === "Alice" || r.friendName?.value === "Alice"),
  );

  if ((hasAliceKnowsBob || hasBobKnowsAlice) && friends.length >= 1) {
    console.log("✅ Knowledge Graph Production Test PASSED");
    console.log("   - Turtle parsing successful");
    console.log("   - SPARQL query with prefixes executed");
    console.log("   - Found friendship relationships");
    console.log(`   - Found ${friends.length} relationships`);
  } else {
    console.log("❌ Knowledge Graph Production Test FAILED");
    console.log("   - Expected: Friendship relationships");
    console.log("   - Got:", friends.length, "relationships");
  }

  process.exit(
    (hasAliceKnowsBob || hasBobKnowsAlice) && friends.length >= 1 ? 0 : 1,
  );
} catch (error) {
  console.log("❌ Knowledge Graph Production Test FAILED with error:");
  console.error(error);
  process.exit(1);
}
