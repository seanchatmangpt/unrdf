/**
 * @fileoverview RDF engine set operations and utility tests
 * 
 * Tests set operations and utility functionality including:
 * - Union of stores
 * - Difference between stores
 * - Intersection of stores
 * - Blank node skolemization
 * - Store statistics
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, test, expect, beforeEach } from "vitest";
import { RdfEngine } from "../../../src/engines/rdf-engine.mjs";
import { initStore } from "../../../src/context/index.mjs";

describe("RdfEngine Set Operations and Utilities", () => {
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

  describe("Set Operations and Utilities", () => {
    test("should perform union of stores", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const { Store } = await import("n3");
        const store1 = new Store();
        const store2 = new Store();
        
        const q1 = engine.quad(
          engine.namedNode("http://example.org/s1"),
          engine.namedNode("http://example.org/p1"),
          engine.literal("o1")
        );
        const q2 = engine.quad(
          engine.namedNode("http://example.org/s2"),
          engine.namedNode("http://example.org/p2"),
          engine.literal("o2")
        );
        
        store1.add(q1);
        store2.add(q2);
        
        const union = engine.union(store1, store2);
        
        expect(union.size).toBe(2);
      });
    });

    test("should perform difference of stores", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const { Store } = await import("n3");
        const store1 = new Store();
        const store2 = new Store();
        
        const q1 = engine.quad(
          engine.namedNode("http://example.org/s1"),
          engine.namedNode("http://example.org/p1"),
          engine.literal("o1")
        );
        const q2 = engine.quad(
          engine.namedNode("http://example.org/s2"),
          engine.namedNode("http://example.org/p2"),
          engine.literal("o2")
        );
        
        store1.add(q1);
        store1.add(q2);
        store2.add(q1);
        
        const difference = engine.difference(store1, store2);
        
        expect(difference.size).toBe(1);
        expect(difference.has(q2)).toBe(true);
        expect(difference.has(q1)).toBe(false);
      });
    });

    test("should perform intersection of stores", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const { Store } = await import("n3");
        const store1 = new Store();
        const store2 = new Store();
        
        const q1 = engine.quad(
          engine.namedNode("http://example.org/s1"),
          engine.namedNode("http://example.org/p1"),
          engine.literal("o1")
        );
        const q2 = engine.quad(
          engine.namedNode("http://example.org/s2"),
          engine.namedNode("http://example.org/p2"),
          engine.literal("o2")
        );
        
        store1.add(q1, q2);
        store2.add(q1);
        
        const intersection = engine.intersection(store1, store2);
        
        expect(intersection.size).toBe(1);
      });
    });

    test("should skolemize blank nodes", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const { Store } = await import("n3");
        const store = new Store();
        
        const q = engine.quad(
          engine.blankNode("b1"),
          engine.namedNode("http://example.org/p"),
          engine.blankNode("b2")
        );
        
        store.add(q);
        
        const skolemized = engine.skolemize(store);
        
        expect(skolemized.size).toBe(1);
        
        // Check that blank nodes were replaced with named nodes
        const quad = [...skolemized][0];
        expect(quad.subject.termType).toBe("NamedNode");
        expect(quad.object.termType).toBe("NamedNode");
      });
    });

    test("should get store statistics", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
          ex:Jane ex:name "Jane Smith" .
        `;
        engine.parseTurtle(ttl);
        
        const stats = engine.getStats();
        
        expect(stats.quads).toBeGreaterThan(0);
        expect(stats.subjects).toBeGreaterThan(0);
        expect(stats.predicates).toBeGreaterThan(0);
        expect(stats.objects).toBeGreaterThan(0);
        expect(stats.graphs).toBeGreaterThan(0);
      });
    });
  });
});
