import { describe, expect, it, beforeEach } from "vitest";
import { useGraph, useStore, initStore } from "../../src/index.mjs";

/**
 * @fileoverview Tests for useGraph composable
 * 
 * Tests the graph operations functionality with context support
 */

describe("useGraph", () => {
  it("should create graph interface from context", async () => {
    const runApp = initStore([], { baseIRI: "http://example.org/" });
    
    await runApp(async () => {
      const store = useStore();
      const q = store.quad(
        store.namedNode("http://example.org/s"),
        store.namedNode("http://example.org/p"),
        store.literal("o")
      );
      store.add(q);

      const graph = useGraph();
      
      // Assert
      expect(graph.store).toBe(store.store);
      expect(typeof graph.select).toBe("function");
      expect(typeof graph.ask).toBe("function");
      expect(typeof graph.update).toBe("function");
      expect(typeof graph.construct).toBe("function");
      expect(typeof graph.validate).toBe("function");
      expect(typeof graph.serialize).toBe("function");
      expect(typeof graph.stats).toBe("function");
    });
  });

  it("should execute SPARQL SELECT queries", async () => {
    const runApp = initStore([], { baseIRI: "http://example.org/" });
    
    await runApp(async () => {
      const store = useStore();
      const q = store.quad(
        store.namedNode("http://example.org/s"),
        store.namedNode("http://example.org/p"),
        store.literal("o")
      );
      store.add(q);

      const graph = useGraph();
      const query = "SELECT ?s ?p ?o WHERE { ?s ?p ?o }";
      
      // Act
      const result = await graph.select(query);
      
      // Assert
      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThan(0);
    });
  });

  it("should execute SPARQL ASK queries", async () => {
    const runApp = initStore([], { baseIRI: "http://example.org/" });
    
    await runApp(async () => {
      const store = useStore();
      const q = store.quad(
        store.namedNode("http://example.org/s"),
        store.namedNode("http://example.org/p"),
        store.literal("o")
      );
      store.add(q);

      const graph = useGraph();
      const query = "ASK WHERE { ?s ?p ?o }";
      
      // Act
      const result = await graph.ask(query);
      
      // Assert
      expect(typeof result).toBe("boolean");
      expect(result).toBe(true);
    });
  });
});
