/**
 * README Example: Quick Start (lines 64-100)
 * Tests basic createDarkMatterCore + query functionality
 */

import { createDarkMatterCore, DataFactory, Store } from "unrdf";
const { namedNode, quad, literal } = DataFactory;

console.log("🧪 Testing Quick Start Example...\n");

try {
  // 1. Create the knowledge engine
  const system = await createDarkMatterCore();

  // Initialize the system
  await system.initialize();

  // 2. Create a store for data
  const store = new Store();

  // 3. Add some RDF data
  await system.executeTransaction(store, {
    additions: [
      quad(
        namedNode("http://example.org/alice"),
        namedNode("http://xmlns.com/foaf/0.1/name"),
        literal("Alice"),
      ),
      quad(
        namedNode("http://example.org/alice"),
        namedNode("http://xmlns.com/foaf/0.1/knows"),
        namedNode("http://example.org/bob"),
      ),
    ],
    removals: [],
    actor: "system",
  });

  // 3. Verify data was added to store
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

  // Find Alice's name
  const nameQuads = store.getQuads(
    namedNode("http://example.org/alice"),
    namedNode("http://xmlns.com/foaf/0.1/name"),
    null,
    null,
  );

  console.log(
    "Query results:",
    JSON.stringify(
      nameQuads.map((q) => ({
        name: { value: q.object.value },
      })),
      null,
      2,
    ),
  );

  // Verify expected output
  const resultArray = nameQuads.map((q) => ({
    name: { value: q.object.value },
  }));
  const hasAlice = resultArray.some((r) => r.name && r.name.value === "Alice");

  if (hasAlice && resultArray.length === 1) {
    console.log("✅ Quick Start example PASSED");
    console.log("   - Query executed successfully");
    console.log('   - Returned expected result: { name: "Alice" }');
  } else {
    console.log("❌ Quick Start example FAILED");
    console.log('   - Expected: [{ name: "Alice" }]');
    console.log("   - Got:", resultArray);
  }

  // 4. Cleanup
  await system.cleanup();

  process.exit(hasAlice && resultArray.length === 1 ? 0 : 1);
} catch (error) {
  console.log("❌ Quick Start example FAILED with error:");
  console.error(error);
  process.exit(1);
}
