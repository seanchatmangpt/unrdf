/**
 * @fileoverview Tests for useGraph composable
 * Tests SPARQL queries, graph operations, and traversal utilities
 */

import { describe, it, expect, beforeEach } from "vitest";
import { useGraph } from "../../src/composables/use-graph.mjs";
import { initStore, useStoreContext } from "../../src/context/index.mjs";
import { Store } from "n3";

describe("useGraph composable", () => {
  let runApp;

  beforeEach(() => {
    runApp = initStore();
  });

  it("provides graph operations interface", async () => {
    await runApp(async () => {
      const graph = useGraph();
      
      expect(graph).toBeDefined();
      expect(typeof graph.query).toBe("function");
      expect(typeof graph.select).toBe("function");
      expect(typeof graph.ask).toBe("function");
      expect(typeof graph.construct).toBe("function");
      expect(typeof graph.update).toBe("function");
      expect(typeof graph.size).toBe("number");
    });
  });

  it("executes SPARQL UPDATE operations", async () => {
    await runApp(async () => {
      const graph = useGraph();
      
      const result = await graph.update(`
        PREFIX ex: <https://example.org/>
        INSERT DATA {
          ex:test ex:property "value" .
        }
      `);
      
      expect(result).toBeDefined();
      expect(result.ok).toBe(true);
    });
  });

  it("handles invalid SPARQL queries gracefully", async () => {
    await runApp(async () => {
      const graph = useGraph();
      
      await expect(async () => {
        await graph.select("INVALID SPARQL SYNTAX");
      }).rejects.toThrow();
    });
  });

  it("provides access to underlying store size", async () => {
    await runApp(async () => {
      const graph = useGraph();
      
      expect(typeof graph.size).toBe("number");
      expect(graph.size).toBeGreaterThanOrEqual(0);
    });
  });

  it("maintains store consistency during operations", async () => {
    await runApp(async () => {
      const graph = useGraph();
      const storeContext = useStoreContext();
      
      console.log("Initial store size:", graph.size);
      
      // Add data directly to the store first
      storeContext.add(storeContext.quad(
        storeContext.namedNode("https://example.org/test"),
        storeContext.namedNode("https://example.org/property"),
        storeContext.literal("direct value")
      ));
      
      console.log("Store size after direct add:", graph.size);
      
      // Now try UPDATE operation
      const result = await graph.update(`
        PREFIX ex: <https://example.org/>
        INSERT DATA {
          ex:test ex:property "value" .
        }
      `);
      
      console.log("Update result:", result);
      console.log("Final store size:", graph.size);
      
      expect(result.ok).toBe(true);
      expect(graph.size).toBeGreaterThan(0);
    });
  });

  it("handles concurrent queries", async () => {
    await runApp(async () => {
      const graph = useGraph();
      
      // Execute multiple queries concurrently
      const promises = [
        graph.update(`PREFIX ex: <https://example.org/> INSERT DATA { ex:test1 ex:prop "val1" . }`),
        graph.update(`PREFIX ex: <https://example.org/> INSERT DATA { ex:test2 ex:prop "val2" . }`),
        graph.update(`PREFIX ex: <https://example.org/> INSERT DATA { ex:test3 ex:prop "val3" . }`)
      ];
      
      const results = await Promise.all(promises);
      
      expect(results).toHaveLength(3);
      expect(results[0].ok).toBe(true);
      expect(results[1].ok).toBe(true);
      expect(results[2].ok).toBe(true);
    });
  });
});