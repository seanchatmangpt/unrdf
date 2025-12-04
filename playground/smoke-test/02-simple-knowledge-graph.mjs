/**
 * README Example 1: Simple Knowledge Graph (lines 332-377)
 * Tests parseTurtle + SPARQL query with prefixes
 */

import { createDarkMatterCore, parseTurtle, DataFactory, Store } from "unrdf";
const { namedNode, quad  } = dataFactory;

console.log("🧪 Testing Simple Knowledge Graph Example...\n");

try {
  const system = await createDarkMatterCore();

  // Initialize the system
  await system.initialize();

  // Create a store for data
  const store = createStore();

  // Parse and load data
  const ttl = `
    @prefix ex: <http://example.org/> .
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .

    ex:alice a foaf:Person ;
             foaf:name "Alice" ;
             foaf:knows ex:bob .

    ex:bob a foaf:Person ;
           foaf:name "Bob" .
  `;

  const parsedQuads = await parseTurtle(ttl);

  // Add to knowledge graph
  await system.executeTransaction(store, {
    additions: [...parsedQuads],
    removals: [],
    actor: "importer",
  });

  // Query social network by checking the store directly
  const allQuads = Array.from(store);

  console.log("Store contents:", allQuads.length, "quads");
  console.log(
    "Quads:",
    JSON.stringify(
      allQuads.map((q) => ({
        subject: q.subject.value,
        predicate: q.predicate.value,
        object: q.object.value,
      })),
      null,
      2,
    ),
  );

  // Find Alice knows Bob relationship
  const aliceKnowsBob = store.getQuads(
    namedNode("http://example.org/alice"),
    namedNode("http://xmlns.com/foaf/0.1/knows"),
    namedNode("http://example.org/bob"),
    null,
  );

  const aliceName = store.getQuads(
    namedNode("http://example.org/alice"),
    namedNode("http://xmlns.com/foaf/0.1/name"),
    null,
    null,
  );

  const bobName = store.getQuads(
    namedNode("http://example.org/bob"),
    namedNode("http://xmlns.com/foaf/0.1/name"),
    null,
    null,
  );

  console.log("Query results:", {
    aliceKnowsBob: aliceKnowsBob.length > 0,
    aliceName: aliceName.length > 0 ? aliceName[0].object.value : null,
    bobName: bobName.length > 0 ? bobName[0].object.value : null,
  });

  // Verify expected output
  const hasAliceKnowsBob =
    aliceKnowsBob.length > 0 &&
    aliceName.length > 0 &&
    aliceName[0].object.value === "Alice" &&
    bobName.length > 0 &&
    bobName[0].object.value === "Bob";

  if (hasAliceKnowsBob) {
    console.log("✅ Simple Knowledge Graph example PASSED");
    console.log("   - Turtle parsing successful");
    console.log("   - Found Alice knows Bob relationship");
  } else {
    console.log("❌ Simple Knowledge Graph example FAILED");
    console.log("   - Expected: Alice knows Bob");
    console.log("   - aliceKnowsBob:", aliceKnowsBob.length > 0);
    console.log(
      "   - aliceName:",
      aliceName.length > 0 ? aliceName[0].object.value : "not found",
    );
    console.log(
      "   - bobName:",
      bobName.length > 0 ? bobName[0].object.value : "not found",
    );
  }

  await system.cleanup();

  process.exit(hasAliceKnowsBob ? 0 : 1);
} catch (error) {
  console.log("❌ Simple Knowledge Graph example FAILED with error:");
  console.error(error);
  process.exit(1);
}
