/**
 * @fileoverview Core RDF engine tests
 * 
 * Tests basic engine functionality including:
 * - Constructor and configuration
 * - Store access
 * - Term creation methods
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, test, expect, beforeEach, vi } from "vitest";
import { RdfEngine } from "../../../src/engines/rdf-engine.mjs";
import { initStore } from "../../../src/context/index.mjs";

describe("RdfEngine Core", () => {
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

  describe("Constructor and Configuration", () => {
    test("should create engine with default options", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        
        expect(engine).toBeDefined();
        expect(engine.baseIRI).toBe("http://example.org/");
        expect(engine.deterministic).toBe(true);
        expect(engine.timeoutMs).toBe(30_000);
        expect(engine.onMetric).toBeNull();
        expect(engine.log).toBe(console);
        expect(engine.engine).toBeDefined();
        expect(engine.$rdf).toBeDefined();
      });
    });

    test("should create engine with custom options", async () => {
      const customLogger = { log: vi.fn(), error: vi.fn(), warn: vi.fn() };
      const customMetric = vi.fn();
      
      await runApp(async () => {
        engine = new RdfEngine({
          baseIRI: "http://custom.org/",
          deterministic: false,
          timeoutMs: 10000,
          onMetric: customMetric,
          logger: customLogger
        });
        
        expect(engine.baseIRI).toBe("http://custom.org/");
        expect(engine.deterministic).toBe(false);
        expect(engine.timeoutMs).toBe(10000);
        expect(engine.onMetric).toBe(customMetric);
        expect(engine.log).toBe(customLogger);
      });
    });

    test("should handle invalid timeout values", async () => {
      await runApp(async () => {
        engine = new RdfEngine({
          timeoutMs: "invalid"
        });
        
        expect(engine.timeoutMs).toBe(30_000);
      });
    });
  });

  describe("Store Access", () => {
    test("should get store from context", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const store = engine.getStore();
        
        expect(store).toBeDefined();
        expect(typeof store.add).toBe("function");
        expect(typeof store.size).toBe("number");
      });
    });

    test("should get store context", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const context = engine.getStoreContext();
        
        expect(context).toBeDefined();
        expect(context.store).toBeDefined();
        expect(context.engine).toBeDefined();
      });
    });

    test("should throw error when store context not initialized", () => {
      // Test without context
      expect(() => {
        engine = new RdfEngine();
        engine.getStore();
      }).toThrow();
    });
  });

  describe("Term Creation Methods", () => {
    beforeEach(async () => {
      await runApp(async () => {
        engine = new RdfEngine();
      });
    });

    test("should create named nodes", () => {
      const node = engine.namedNode("http://example.org/test");
      
      expect(node.termType).toBe("NamedNode");
      expect(node.value).toBe("http://example.org/test");
    });

    test("should create literals", () => {
      const literal1 = engine.literal("hello");
      const literal2 = engine.literal("world", "en");
      const literal3 = engine.literal("42", engine.namedNode("http://www.w3.org/2001/XMLSchema#integer"));
      
      expect(literal1.termType).toBe("Literal");
      expect(literal1.value).toBe("hello");
      
      expect(literal2.termType).toBe("Literal");
      expect(literal2.value).toBe("world");
      expect(literal2.language).toBe("en");
      
      expect(literal3.termType).toBe("Literal");
      expect(literal3.value).toBe("42");
      expect(literal3.datatype.value).toBe("http://www.w3.org/2001/XMLSchema#integer");
    });

    test("should create blank nodes", () => {
      const bnode1 = engine.blankNode();
      const bnode2 = engine.blankNode("test123");
      
      expect(bnode1.termType).toBe("BlankNode");
      expect(bnode1.value).toBeDefined();
      
      expect(bnode2.termType).toBe("BlankNode");
      expect(bnode2.value).toBe("test123");
    });

    test("should create quads", () => {
      const s = engine.namedNode("http://example.org/s");
      const p = engine.namedNode("http://example.org/p");
      const o = engine.literal("o");
      const g = engine.namedNode("http://example.org/g");
      
      const quad1 = engine.quad(s, p, o);
      const quad2 = engine.quad(s, p, o, g);
      
      expect(quad1.subject).toBe(s);
      expect(quad1.predicate).toBe(p);
      expect(quad1.object).toBe(o);
      expect(quad1.graph.termType).toBe("DefaultGraph");
      
      expect(quad2.subject).toBe(s);
      expect(quad2.predicate).toBe(p);
      expect(quad2.object).toBe(o);
      expect(quad2.graph).toBe(g);
    });
  });
});
