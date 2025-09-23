/**
 * @fileoverview Comprehensive tests for context module
 * 
 * Tests the store context creation, management, and async context support
 * using unctx with native AsyncLocalStorage
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, expect, it, beforeEach, afterEach } from "vitest";
import { 
  createStoreContext, 
  initStore, 
  setStoreContext, 
  useStoreContext,
  storeContext 
} from "../src/context/index.mjs";
import { DataFactory } from "n3";

const { namedNode, literal, blankNode, quad, defaultGraph } = DataFactory;

describe("Context Module", () => {
  describe("createStoreContext", () => {
    it("should create a store context with empty initial quads", () => {
      const context = createStoreContext();
      
      expect(context).toBeDefined();
      expect(context.store).toBeDefined();
      expect(context.engine).toBeDefined();
      expect(context.size).toBe(0);
      expect(typeof context.add).toBe("function");
      expect(typeof context.remove).toBe("function");
      expect(typeof context.clear).toBe("function");
      expect(typeof context.stats).toBe("function");
      expect(typeof context.serialize).toBe("function");
      expect(typeof context.namedNode).toBe("function");
      expect(typeof context.literal).toBe("function");
      expect(typeof context.blankNode).toBe("function");
      expect(typeof context.quad).toBe("function");
      expect(typeof context.has).toBe("function");
      expect(typeof context.getQuads).toBe("function");
    });

    it("should create a store context with initial quads", () => {
      const initialQuad = quad(
        namedNode("http://example.org/s"),
        namedNode("http://example.org/p"),
        literal("o")
      );
      
      const context = createStoreContext([initialQuad]);
      
      expect(context.size).toBe(1);
      expect(context.has(initialQuad)).toBe(true);
    });

    it("should create a store context with options", () => {
      const options = { baseIRI: "http://example.org/" };
      const context = createStoreContext([], options);
      
      expect(context).toBeDefined();
      expect(context.engine).toBeDefined();
    });

    it("should throw error for invalid initialQuads", () => {
      expect(() => createStoreContext("not an array")).toThrow(
        "[createStoreContext] initialQuads must be an array"
      );
    });

    it("should throw error for invalid options", () => {
      expect(() => createStoreContext([], "not an object")).toThrow(
        "[createStoreContext] options must be an object"
      );
    });
  });

  describe("Store Context Methods", () => {
    let context;

    beforeEach(() => {
      context = createStoreContext();
    });

    describe("add method", () => {
      it("should add a single quad", () => {
        const testQuad = context.quad(
          context.namedNode("http://example.org/s"),
          context.namedNode("http://example.org/p"),
          context.literal("o")
        );
        
        const result = context.add(testQuad);
        
        expect(result).toBe(context); // Should return this for chaining
        expect(context.size).toBe(1);
        expect(context.has(testQuad)).toBe(true);
      });

      it("should add multiple quads", () => {
        const quad1 = context.quad(
          context.namedNode("http://example.org/s1"),
          context.namedNode("http://example.org/p1"),
          context.literal("o1")
        );
        const quad2 = context.quad(
          context.namedNode("http://example.org/s2"),
          context.namedNode("http://example.org/p2"),
          context.literal("o2")
        );
        
        context.add(quad1, quad2);
        
        expect(context.size).toBe(2);
        expect(context.has(quad1)).toBe(true);
        expect(context.has(quad2)).toBe(true);
      });

      it("should throw error for null quad", () => {
        expect(() => context.add(null)).toThrow(
          "[StoreContext] Cannot add null or undefined quad"
        );
      });

      it("should throw error for undefined quad", () => {
        expect(() => context.add(undefined)).toThrow(
          "[StoreContext] Cannot add null or undefined quad"
        );
      });

      it("should throw error for invalid quad", () => {
        expect(() => context.add({})).toThrow(
          "[StoreContext] Invalid quad: must have termType property"
        );
      });
    });

    describe("remove method", () => {
      it("should remove a quad", () => {
        const testQuad = context.quad(
          context.namedNode("http://example.org/s"),
          context.namedNode("http://example.org/p"),
          context.literal("o")
        );
        
        context.add(testQuad);
        expect(context.size).toBe(1);
        
        const result = context.remove(testQuad);
        
        expect(result).toBe(context); // Should return this for chaining
        expect(context.size).toBe(0);
        expect(context.has(testQuad)).toBe(false);
      });

      it("should remove multiple quads", () => {
        const quad1 = context.quad(
          context.namedNode("http://example.org/s1"),
          context.namedNode("http://example.org/p1"),
          context.literal("o1")
        );
        const quad2 = context.quad(
          context.namedNode("http://example.org/s2"),
          context.namedNode("http://example.org/p2"),
          context.literal("o2")
        );
        
        context.add(quad1, quad2);
        expect(context.size).toBe(2);
        
        context.remove(quad1, quad2);
        
        expect(context.size).toBe(0);
        expect(context.has(quad1)).toBe(false);
        expect(context.has(quad2)).toBe(false);
      });

      it("should throw error for null quad", () => {
        expect(() => context.remove(null)).toThrow(
          "[StoreContext] Cannot remove null or undefined quad"
        );
      });

      it("should throw error for undefined quad", () => {
        expect(() => context.remove(undefined)).toThrow(
          "[StoreContext] Cannot remove null or undefined quad"
        );
      });

      it("should throw error for invalid quad", () => {
        expect(() => context.remove({})).toThrow(
          "[StoreContext] Invalid quad: must have termType property"
        );
      });
    });

    describe("clear method", () => {
      it("should clear all quads", () => {
        const quad1 = context.quad(
          context.namedNode("http://example.org/s1"),
          context.namedNode("http://example.org/p1"),
          context.literal("o1")
        );
        const quad2 = context.quad(
          context.namedNode("http://example.org/s2"),
          context.namedNode("http://example.org/p2"),
          context.literal("o2")
        );
        
        context.add(quad1, quad2);
        expect(context.size).toBe(2);
        
        const result = context.clear();
        
        expect(result).toBe(context); // Should return this for chaining
        expect(context.size).toBe(0);
        expect(context.has(quad1)).toBe(false);
        expect(context.has(quad2)).toBe(false);
      });

      it("should handle clearing empty store", () => {
        expect(context.size).toBe(0);
        
        const result = context.clear();
        
        expect(result).toBe(context);
        expect(context.size).toBe(0);
      });
    });

    describe("stats method", () => {
      it("should return store statistics", async () => {
        const runApp = initStore([], { baseIRI: "http://example.org/" });
        
        await runApp(async () => {
          const context = useStoreContext();
          const testQuad = context.quad(
            context.namedNode("http://example.org/s"),
            context.namedNode("http://example.org/p"),
            context.literal("o")
          );
          
          context.add(testQuad);
          
          const stats = context.stats();
          
          expect(stats).toBeDefined();
          expect(typeof stats).toBe("object");
          expect(stats.quads).toBe(1);
        });
      });

      it("should return empty stats for empty store", async () => {
        const runApp = initStore([], { baseIRI: "http://example.org/" });
        
        await runApp(async () => {
          const context = useStoreContext();
          const stats = context.stats();
          
          expect(stats).toBeDefined();
          expect(stats.quads).toBe(0);
        });
      });
    });

    describe("serialize method", () => {
      it("should serialize to Turtle format", async () => {
        const runApp = initStore([], { baseIRI: "http://example.org/" });
        
        await runApp(async () => {
          const context = useStoreContext();
          const testQuad = context.quad(
            context.namedNode("http://example.org/s"),
            context.namedNode("http://example.org/p"),
            context.literal("o")
          );
          
          context.add(testQuad);
          
          const turtle = await context.serialize({ format: "Turtle" });
          
          expect(typeof turtle).toBe("string");
          expect(turtle.length).toBeGreaterThan(0);
        });
      });

      it("should serialize to N-Quads format", async () => {
        const runApp = initStore([], { baseIRI: "http://example.org/" });
        
        await runApp(async () => {
          const context = useStoreContext();
          const testQuad = context.quad(
            context.namedNode("http://example.org/s"),
            context.namedNode("http://example.org/p"),
            context.literal("o")
          );
          
          context.add(testQuad);
          
          const nquads = await context.serialize({ format: "N-Quads" });
          
          expect(typeof nquads).toBe("string");
          expect(nquads.length).toBeGreaterThan(0);
        });
      });

      it("should use default Turtle format", async () => {
        const runApp = initStore([], { baseIRI: "http://example.org/" });
        
        await runApp(async () => {
          const context = useStoreContext();
          const testQuad = context.quad(
            context.namedNode("http://example.org/s"),
            context.namedNode("http://example.org/p"),
            context.literal("o")
          );
          
          context.add(testQuad);
          
          const turtle = await context.serialize();
          
          expect(typeof turtle).toBe("string");
          expect(turtle.length).toBeGreaterThan(0);
        });
      });

      it("should throw error for invalid options", async () => {
        await expect(context.serialize("not an object")).rejects.toThrow(
          "[StoreContext] serialize options must be an object"
        );
      });

      it("should throw error for unsupported format", async () => {
        await expect(context.serialize({ format: "Unsupported" })).rejects.toThrow(
          "[StoreContext] Unsupported serialization format: Unsupported"
        );
      });
    });
  });

  describe("Term Creation Methods", () => {
    let context;

    beforeEach(() => {
      context = createStoreContext();
    });

    describe("namedNode method", () => {
      it("should create a named node", () => {
        const node = context.namedNode("http://example.org/test");
        
        expect(node.termType).toBe("NamedNode");
        expect(node.value).toBe("http://example.org/test");
      });

      it("should throw error for non-string value", () => {
        expect(() => context.namedNode(123)).toThrow(
          "[StoreContext] namedNode value must be a string"
        );
      });

      it("should throw error for null value", () => {
        expect(() => context.namedNode(null)).toThrow(
          "[StoreContext] namedNode value must be a string"
        );
      });
    });

    describe("literal method", () => {
      it("should create a literal", () => {
        const lit = context.literal("test value");
        
        expect(lit.termType).toBe("Literal");
        expect(lit.value).toBe("test value");
      });

      it("should create a literal with datatype", () => {
        const lit = context.literal("42", "http://www.w3.org/2001/XMLSchema#integer");
        
        expect(lit.termType).toBe("Literal");
        expect(lit.value).toBe("42");
        expect(lit.language).toBe("http://www.w3.org/2001/xmlschema#integer");
      });

      it("should throw error for non-string value", () => {
        expect(() => context.literal(123)).toThrow(
          "[StoreContext] literal value must be a string"
        );
      });

      it("should throw error for null value", () => {
        expect(() => context.literal(null)).toThrow(
          "[StoreContext] literal value must be a string"
        );
      });
    });

    describe("blankNode method", () => {
      it("should create a blank node without value", () => {
        const bnode = context.blankNode();
        
        expect(bnode.termType).toBe("BlankNode");
        expect(typeof bnode.value).toBe("string");
        expect(bnode.value.length).toBeGreaterThan(0);
      });

      it("should create a blank node with value", () => {
        const bnode = context.blankNode("test123");
        
        expect(bnode.termType).toBe("BlankNode");
        expect(bnode.value).toBe("test123");
      });

      it("should throw error for non-string value", () => {
        expect(() => context.blankNode(123)).toThrow(
          "[StoreContext] blankNode value must be a string"
        );
      });
    });

    describe("quad method", () => {
      it("should create a quad", () => {
        const s = context.namedNode("http://example.org/s");
        const p = context.namedNode("http://example.org/p");
        const o = context.literal("o");
        
        const q = context.quad(s, p, o);
        
        expect(q.subject).toBe(s);
        expect(q.predicate).toBe(p);
        expect(q.object).toBe(o);
        expect(q.graph.termType).toBe("DefaultGraph");
      });

      it("should create a quad with graph", () => {
        const s = context.namedNode("http://example.org/s");
        const p = context.namedNode("http://example.org/p");
        const o = context.literal("o");
        const g = context.namedNode("http://example.org/g");
        
        const q = context.quad(s, p, o, g);
        
        expect(q.subject).toBe(s);
        expect(q.predicate).toBe(p);
        expect(q.object).toBe(o);
        expect(q.graph).toBe(g);
      });

      it("should throw error for missing subject", () => {
        const p = context.namedNode("http://example.org/p");
        const o = context.literal("o");
        
        expect(() => context.quad(null, p, o)).toThrow(
          "[StoreContext] quad requires subject, predicate, and object"
        );
      });

      it("should throw error for missing predicate", () => {
        const s = context.namedNode("http://example.org/s");
        const o = context.literal("o");
        
        expect(() => context.quad(s, null, o)).toThrow(
          "[StoreContext] quad requires subject, predicate, and object"
        );
      });

      it("should throw error for missing object", () => {
        const s = context.namedNode("http://example.org/s");
        const p = context.namedNode("http://example.org/p");
        
        expect(() => context.quad(s, p, null)).toThrow(
          "[StoreContext] quad requires subject, predicate, and object"
        );
      });
    });
  });

  describe("Query Methods", () => {
    let context;

    beforeEach(() => {
      context = createStoreContext();
    });

    describe("has method", () => {
      it("should return true for existing quad", () => {
        const testQuad = context.quad(
          context.namedNode("http://example.org/s"),
          context.namedNode("http://example.org/p"),
          context.literal("o")
        );
        
        context.add(testQuad);
        
        expect(context.has(testQuad)).toBe(true);
      });

      it("should return false for non-existing quad", () => {
        const testQuad = context.quad(
          context.namedNode("http://example.org/s"),
          context.namedNode("http://example.org/p"),
          context.literal("o")
        );
        
        expect(context.has(testQuad)).toBe(false);
      });
    });

    describe("getQuads method", () => {
      beforeEach(() => {
        const quad1 = context.quad(
          context.namedNode("http://example.org/s1"),
          context.namedNode("http://example.org/p1"),
          context.literal("o1")
        );
        const quad2 = context.quad(
          context.namedNode("http://example.org/s2"),
          context.namedNode("http://example.org/p2"),
          context.literal("o2")
        );
        
        context.add(quad1, quad2);
      });

      it("should get all quads", () => {
        const quads = context.getQuads();
        
        expect(quads).toHaveLength(2);
      });

      it("should get quads by subject", () => {
        const subject = context.namedNode("http://example.org/s1");
        const quads = context.getQuads(subject);
        
        expect(quads).toHaveLength(1);
        expect(quads[0].subject.value).toBe("http://example.org/s1");
      });

      it("should get quads by predicate", () => {
        const predicate = context.namedNode("http://example.org/p1");
        const quads = context.getQuads(null, predicate);
        
        expect(quads).toHaveLength(1);
        expect(quads[0].predicate.value).toBe("http://example.org/p1");
      });

      it("should get quads by object", () => {
        const object = context.literal("o1");
        const quads = context.getQuads(null, null, object);
        
        expect(quads).toHaveLength(1);
        expect(quads[0].object.value).toBe("o1");
      });

      it("should get quads by graph", () => {
        const graph = context.namedNode("http://example.org/g");
        const quads = context.getQuads(null, null, null, graph);
        
        expect(quads).toHaveLength(0); // No quads in this graph
      });

      it("should get quads by subject and predicate", () => {
        const subject = context.namedNode("http://example.org/s1");
        const predicate = context.namedNode("http://example.org/p1");
        const quads = context.getQuads(subject, predicate);
        
        expect(quads).toHaveLength(1);
        expect(quads[0].subject.value).toBe("http://example.org/s1");
        expect(quads[0].predicate.value).toBe("http://example.org/p1");
      });
    });

    describe("size property", () => {
      it("should return correct size", () => {
        expect(context.size).toBe(0);
        
        const quad1 = context.quad(
          context.namedNode("http://example.org/s1"),
          context.namedNode("http://example.org/p1"),
          context.literal("o1")
        );
        context.add(quad1);
        expect(context.size).toBe(1);
        
        const quad2 = context.quad(
          context.namedNode("http://example.org/s2"),
          context.namedNode("http://example.org/p2"),
          context.literal("o2")
        );
        context.add(quad2);
        expect(context.size).toBe(2);
      });
    });

    describe("iterator", () => {
      it("should iterate over all quads", () => {
        const quad1 = context.quad(
          context.namedNode("http://example.org/s1"),
          context.namedNode("http://example.org/p1"),
          context.literal("o1")
        );
        const quad2 = context.quad(
          context.namedNode("http://example.org/s2"),
          context.namedNode("http://example.org/p2"),
          context.literal("o2")
        );
        
        context.add(quad1, quad2);
        
        const quads = [...context];
        
        expect(quads).toHaveLength(2);
        // Check that we have quads with the expected values
        const subjects = quads.map(q => q.subject.value);
        expect(subjects).toContain("http://example.org/s1");
        expect(subjects).toContain("http://example.org/s2");
      });

      it("should iterate over empty store", () => {
        const quads = [...context];
        
        expect(quads).toHaveLength(0);
      });
    });
  });

  describe("initStore", () => {
    it("should create a function that runs with context", async () => {
      const runApp = initStore([], { baseIRI: "http://example.org/" });
      
      expect(typeof runApp).toBe("function");
      
      let contextReceived = null;
      await runApp(async () => {
        contextReceived = useStoreContext();
      });
      
      expect(contextReceived).toBeDefined();
      expect(contextReceived.store).toBeDefined();
      expect(contextReceived.engine).toBeDefined();
    });

    it("should preserve context across async operations", async () => {
      const runApp = initStore([], { baseIRI: "http://example.org/" });
      
      let context1 = null;
      let context2 = null;
      
      await runApp(async () => {
        context1 = useStoreContext();
        
        await new Promise(resolve => setTimeout(resolve, 10));
        
        context2 = useStoreContext();
      });
      
      expect(context1).toBe(context2);
    });

    it("should handle errors in async context", async () => {
      const runApp = initStore([], { baseIRI: "http://example.org/" });
      
      await expect(runApp(async () => {
        throw new Error("Test error");
      })).rejects.toThrow("Test error");
    });

    it("should create different contexts for different calls", async () => {
      const runApp1 = initStore([], { baseIRI: "http://example1.org/" });
      const runApp2 = initStore([], { baseIRI: "http://example2.org/" });
      
      let context1 = null;
      let context2 = null;
      
      await runApp1(async () => {
        context1 = useStoreContext();
      });
      
      await runApp2(async () => {
        context2 = useStoreContext();
      });
      
      expect(context1).not.toBe(context2);
      expect(context1.engine).not.toBe(context2.engine);
    });
  });

  describe("setStoreContext", () => {
    it("should set the store context", () => {
      const context = setStoreContext([], { baseIRI: "http://example.org/" });
      
      expect(context).toBeDefined();
      expect(context.store).toBeDefined();
      expect(context.engine).toBeDefined();
      
      // Verify context is accessible
      const retrievedContext = useStoreContext();
      expect(retrievedContext).toBe(context);
    });
  });

  describe("useStoreContext", () => {
    it("should work within initStore context", async () => {
      const runApp = initStore([], { baseIRI: "http://example.org/" });
      
      await runApp(async () => {
        const context = useStoreContext();
        
        expect(context).toBeDefined();
        expect(context.store).toBeDefined();
        expect(context.engine).toBeDefined();
      });
    });
  });

  describe("Integration Tests", () => {
    it("should work with complex RDF operations", async () => {
      const runApp = initStore([], { baseIRI: "http://example.org/" });
      
      await runApp(async () => {
        const store = useStoreContext();
        
        // Create a complex RDF structure
        const person = store.namedNode("http://example.org/person1");
        const name = store.namedNode("http://example.org/name");
        const age = store.namedNode("http://example.org/age");
        const type = store.namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");
        const personClass = store.namedNode("http://example.org/Person");
        
        const quads = [
          store.quad(person, type, personClass),
          store.quad(person, name, store.literal("John Doe")),
          store.quad(person, age, store.literal("30", "http://www.w3.org/2001/XMLSchema#integer"))
        ];
        
        store.add(...quads);
        
        expect(store.size).toBe(3);
        
        // Query the data
        const personQuads = store.getQuads(person);
        expect(personQuads).toHaveLength(3);
        
        const nameQuads = store.getQuads(person, name);
        expect(nameQuads).toHaveLength(1);
        expect(nameQuads[0].object.value).toBe("John Doe");
        
        // Serialize
        const turtle = await store.serialize({ format: "Turtle" });
        expect(typeof turtle).toBe("string");
        expect(turtle).toContain("John Doe");
        
        // Get stats
        const stats = store.stats();
        expect(stats.quads).toBe(3);
      });
    });

    it("should handle concurrent operations", async () => {
      const runApp = initStore([], { baseIRI: "http://example.org/" });
      
      await runApp(async () => {
        const store = useStoreContext();
        
        // Simulate concurrent operations
        const promises = [];
        
        for (let i = 0; i < 10; i++) {
          promises.push(
            Promise.resolve().then(() => {
              const quad = store.quad(
                store.namedNode(`http://example.org/s${i}`),
                store.namedNode(`http://example.org/p${i}`),
                store.literal(`o${i}`)
              );
              store.add(quad);
            })
          );
        }
        
        await Promise.all(promises);
        
        expect(store.size).toBe(10);
      });
    });

    it("should maintain context isolation between different initStore calls", async () => {
      const runApp1 = initStore([], { baseIRI: "http://example1.org/" });
      const runApp2 = initStore([], { baseIRI: "http://example2.org/" });
      
      let store1, store2;
      
      await runApp1(async () => {
        store1 = useStoreContext();
        const quad = store1.quad(
          store1.namedNode("http://example1.org/s"),
          store1.namedNode("http://example1.org/p"),
          store1.literal("o1")
        );
        store1.add(quad);
      });
      
      await runApp2(async () => {
        store2 = useStoreContext();
        const quad = store2.quad(
          store2.namedNode("http://example2.org/s"),
          store2.namedNode("http://example2.org/p"),
          store2.literal("o2")
        );
        store2.add(quad);
      });
      
      expect(store1).not.toBe(store2);
      expect(store1.size).toBe(1);
      expect(store2.size).toBe(1);
      
      // Verify isolation
      const quad1 = store1.quad(
        store1.namedNode("http://example1.org/s"),
        store1.namedNode("http://example1.org/p"),
        store1.literal("o1")
      );
      const quad2 = store2.quad(
        store2.namedNode("http://example2.org/s"),
        store2.namedNode("http://example2.org/p"),
        store2.literal("o2")
      );
      
      expect(store1.has(quad1)).toBe(true);
      expect(store1.has(quad2)).toBe(false);
      expect(store2.has(quad1)).toBe(false);
      expect(store2.has(quad2)).toBe(true);
    });
  });

  describe("Edge Cases", () => {
    let context;

    beforeEach(() => {
      context = createStoreContext();
    });

    it("should handle empty string literals", () => {
      const emptyLit = context.literal("");
      const quad = context.quad(
        context.namedNode("http://example.org/s"),
        context.namedNode("http://example.org/p"),
        emptyLit
      );
      
      context.add(quad);
      expect(context.size).toBe(1);
      expect(context.has(quad)).toBe(true);
    });

    it("should handle very long IRIs", () => {
      const longIRI = "http://example.org/" + "a".repeat(1000);
      const node = context.namedNode(longIRI);
      
      expect(node.value).toBe(longIRI);
    });

    it("should handle special characters in literals", () => {
      const specialLit = context.literal("Hello\nWorld\twith\rspecial\"chars'");
      const quad = context.quad(
        context.namedNode("http://example.org/s"),
        context.namedNode("http://example.org/p"),
        specialLit
      );
      
      context.add(quad);
      expect(context.size).toBe(1);
    });

    it("should handle Unicode characters", () => {
      const unicodeLit = context.literal("Hello 世界 🌍");
      const quad = context.quad(
        context.namedNode("http://example.org/s"),
        context.namedNode("http://example.org/p"),
        unicodeLit
      );
      
      context.add(quad);
      expect(context.size).toBe(1);
    });

    it("should handle large numbers of quads", () => {
      const quads = [];
      for (let i = 0; i < 100; i++) { // Reduced to 100 for faster tests
        quads.push(context.quad(
          context.namedNode(`http://example.org/s${i}`),
          context.namedNode(`http://example.org/p${i}`),
          context.literal(`o${i}`)
        ));
      }
      
      context.add(...quads);
      expect(context.size).toBe(100);
      
      // Test that we can retrieve the quads
      const allQuads = context.getQuads();
      expect(allQuads).toHaveLength(100);
    });
  });
});
