/**
 * @fileoverview Example demonstrating the new unctx-based store context
 * 
 * This example shows how to use the new context-based approach where
 * there's only one store by default, managed through unctx.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

/**
 * @fileoverview Example demonstrating the new unctx-based store context
 * 
 * This example shows how to use the new context-based approach where
 * there's only one store by default, managed through unctx.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { initStore, useStore, useGraph, useTerms } from "../src/index.mjs";

// Example 1: Basic usage with context
console.log("=== Example 1: Basic Context Usage ===");

const runApp = initStore([], { baseIRI: 'http://example.org/' });

runApp(() => {
  // Get the store from context
  const store = useStore();
  const terms = useTerms();
  const graph = useGraph();
  
  // Create some RDF data
  const subject = terms.iri("person1");
  const predicate = terms.iri("name");
  const object = terms.lit("John Doe");
  const quad = terms.quad(subject, predicate, object);
  
  // Add to the store
  store.add(quad);
  
  console.log(`Store size: ${store.size}`);
  console.log(`Store stats:`, store.stats());
  
  // Query the data
  graph.select(`
    PREFIX ex: <http://example.org/>
    SELECT ?s ?p ?o WHERE { ?s ?p ?o }
  `).then(results => {
    console.log("Query results:", results);
  });
});

// Example 2: Multiple composables sharing the same store
console.log("\n=== Example 2: Shared Store Context ===");

const runApp2 = initStore();

runApp2(() => {
  const store1 = useStore();
  const store2 = useStore();
  const graph1 = useGraph();
  const graph2 = useGraph();
  
  // These are all the same instance
  console.log("store1 === store2:", store1 === store2);
  console.log("graph1.store === graph2.store:", graph1.store === graph2.store);
  
  // Add data through one composable
  const terms = useTerms();
  const quad = terms.quad(
    terms.iri("person2"),
    terms.iri("age"),
    terms.lit("30", "http://www.w3.org/2001/XMLSchema#integer")
  );
  
  store1.add(quad);
  
  // Query through another composable
  console.log("Store size after adding:", store2.size);
});

// Example 3: Error handling when context is not initialized
console.log("\n=== Example 3: Error Handling ===");

try {
  // This will fail because we're not in a context
  const store = useStore();
} catch (error) {
  console.log("Expected error:", error.message);
}

console.log("\n=== Context-based unrdf is ready! ===");
console.log("Key benefits:");
console.log("- Single store by default");
console.log("- Automatic context management");
console.log("- Consistent state across composables");
console.log("- Better error handling");
console.log("- Follows unctx best practices");
