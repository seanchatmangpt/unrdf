/**
 * @fileoverview RDF engine edge cases and error handling tests
 * 
 * Tests edge cases and error handling including:
 * - Timeout handling
 * - Malformed input handling
 * - Empty JSON-LD handling
 * - Malformed rules handling
 * - Canonicalization failure handling
 * - Metrics callback errors
 * - Performance and deterministic behavior
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, test, expect, beforeEach, vi } from "vitest";
import { RdfEngine } from "../../../src/engines/rdf-engine.mjs";
import { initStore } from "../../../src/context/index.mjs";

describe("RdfEngine Edge Cases and Error Handling", () => {
  let engine;
  let runApp;

  beforeEach(() => {
    // Initialize a fresh store context for each test
    runApp = initStore([], { 
      baseIRI: "http://example.org/",
      deterministic: true,
      timeoutMs: 5000
    });
  });

  describe("Edge Cases and Error Handling", () => {
    test("should handle timeout in operations", async () => {
      await runApp(async () => {
        const shortTimeoutEngine = new RdfEngine({ timeoutMs: 1 });
        
        // Add some data first to make canonicalization take longer
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
          ex:Jane ex:name "Jane Smith" .
          ex:Bob ex:name "Bob Wilson" .
        `;
        shortTimeoutEngine.parseTurtle(ttl);
        
        // Test a different operation that might timeout
        const { Store } = await import("n3");
        const dataStore = new Store();
        const rulesStore = new Store();
        
        // Add a complex rule that might take time to process
        const complexRule = shortTimeoutEngine.quad(
          shortTimeoutEngine.namedNode("http://example.org/rule"),
          shortTimeoutEngine.namedNode("http://example.org/complex"),
          shortTimeoutEngine.literal("complex rule")
        );
        rulesStore.add(complexRule);
        
        // Add more data to make reasoning take longer
        const dataQuad = shortTimeoutEngine.quad(
          shortTimeoutEngine.namedNode("http://example.org/data"),
          shortTimeoutEngine.namedNode("http://example.org/prop"),
          shortTimeoutEngine.literal("data")
        );
        dataStore.add(dataQuad);
        
        // Add a complex rule that might take time to process
        const complexRule2 = shortTimeoutEngine.quad(
          shortTimeoutEngine.namedNode("http://example.org/rule2"),
          shortTimeoutEngine.namedNode("http://example.org/complex2"),
          shortTimeoutEngine.literal("complex rule 2")
        );
        rulesStore.add(complexRule2);
        
        // This should timeout quickly
        await expect(
          shortTimeoutEngine.reason(dataStore, rulesStore)
        ).rejects.toThrow("timeout");
      });
    });

    test("should handle malformed Turtle gracefully", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const malformedTtl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human
          // Missing period
        `;
        
        // Should throw for malformed Turtle
        expect(() => {
          engine.parseTurtle(malformedTtl);
        }).toThrow();
      });
    });

    test("should handle empty JSON-LD", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const emptyJsonld = {
          "@context": { "ex": "http://example.org/" },
          "@id": "ex:empty",
          "@type": "ex:Empty"
        };
        
        const store = await engine.fromJSONLD(emptyJsonld);
        
        expect(store.size).toBeGreaterThan(0);
        expect(store).toBe(engine.getStore());
      });
    });

    test("should handle reasoning with malformed rules", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const { Store } = await import("n3");
        const dataStore = new Store();
        const rulesStore = new Store();
        
        // Add malformed rule
        const malformedRule = engine.quad(
          engine.namedNode("http://example.org/rule"),
          engine.namedNode("http://example.org/malformed"),
          engine.literal("invalid")
        );
        rulesStore.add(malformedRule);
        
        // Should handle gracefully
        const result = await engine.reason(dataStore, rulesStore);
        
        expect(result).toBeDefined();
      });
    });

    test("should handle canonicalization failure gracefully", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        // Create a store that might cause canonicalization issues
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          _:b1 ex:name "John" .
          _:b2 ex:name "Jane" .
        `;
        engine.parseTurtle(ttl);
        
        // Should not throw even if canonicalization fails
        const canonical = await engine.canonicalize();
        
        expect(typeof canonical).toBe("string");
      });
    });

    test("should handle metrics callback errors", async () => {
      await runApp(async () => {
        const errorMetric = vi.fn(() => {
          throw new Error("Metrics error");
        });
        
        const engineWithErrorMetric = new RdfEngine({ onMetric: errorMetric });
        
        // Should not throw even if metrics callback fails
        const stats = engineWithErrorMetric.getStats();
        
        expect(stats).toBeDefined();
      });
    });
  });

  describe("Performance and Deterministic Behavior", () => {
    test("should produce deterministic results", async () => {
      await runApp(async () => {
        engine = new RdfEngine({ deterministic: true });
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
          ex:Jane ex:name "Jane Smith" .
        `;
        engine.parseTurtle(ttl);
        
        const result1 = await engine.query("SELECT ?s ?p ?o WHERE { ?s ?p ?o }");
        const result2 = await engine.query("SELECT ?s ?p ?o WHERE { ?s ?p ?o }");
        
        expect(result1.results).toEqual(result2.results);
      });
    });

    test("should handle non-deterministic mode", async () => {
      await runApp(async () => {
        const nonDeterministicEngine = new RdfEngine({ deterministic: false });
        
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
          ex:Jane ex:name "Jane Smith" .
        `;
        nonDeterministicEngine.parseTurtle(ttl);
        
        const result = await nonDeterministicEngine.query("SELECT ?s ?p ?o WHERE { ?s ?p ?o }");
        
        expect(result.type).toBe("select");
        expect(result.results.length).toBeGreaterThan(0);
      });
    });
  });
});
