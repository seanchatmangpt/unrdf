/**
 * @fileoverview RDF engine reasoning and JSON-LD tests
 * 
 * Tests reasoning and JSON-LD functionality including:
 * - N3 reasoning with empty stores
 * - Reasoning with data but no rules
 * - Reasoning with rules
 * - JSON-LD conversion to/from store
 * - JSON-LD with context and frames
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, test, expect, beforeEach } from "vitest";
import { RdfEngine } from "../../../src/engines/rdf-engine.mjs";
import { initStore } from "../../../src/context/index.mjs";

describe("RdfEngine Reasoning and JSON-LD", () => {
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

  describe("Graph Manipulation", () => {
    test("should get Clownface pointer", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
        `;
        engine.parseTurtle(ttl);
        
        const pointer = engine.getClownface();
        
        expect(pointer).toBeDefined();
        expect(typeof pointer.out).toBe("function");
        expect(typeof pointer.in).toBe("function");
      });
    });
  });

  describe("Reasoning", () => {
    test("should perform reasoning with empty stores", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const { Store } = await import("n3");
        const dataStore = new Store();
        const rulesStore = new Store();
        
        const result = await engine.reason(dataStore, rulesStore);
        
        expect(result.size).toBe(0);
      });
    });

    test("should perform reasoning with data but no rules", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
        `;
        engine.parseTurtle(ttl);
        const dataStore = engine.getStore();
        
        const { Store } = await import("n3");
        const rulesStore = new Store();
        
        const result = await engine.reason(dataStore, rulesStore);
        
        expect(result.size).toBe(dataStore.size);
      });
    });

    test("should perform reasoning with rules", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const dataTtl = `
          @prefix ex: <http://example.org/> .
          @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
          ex:John a ex:Person .
          ex:Person rdfs:subClassOf ex:Human .
        `;
        engine.parseTurtle(dataTtl);
        const dataStore = engine.getStore();
        
        const rulesTtl = `
          @prefix ex: <http://example.org/> .
          @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
          
          ex:Person rdfs:subClassOf ex:Human .
          ex:John a ex:Person .
        `;
        const rulesStore = engine.parseTurtle(rulesTtl);
        
        const result = await engine.reason(dataStore, rulesStore);
        
        expect(result.size).toBeGreaterThanOrEqual(dataStore.size);
      });
    });
  });

  describe("JSON-LD I/O", () => {
    test("should convert store to JSON-LD", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
        `;
        engine.parseTurtle(ttl);
        const store = engine.getStore();
        
        const jsonld = await engine.toJSONLD(store);
        
        expect(jsonld).toBeDefined();
        expect(jsonld["@context"]).toBeDefined();
        expect(Array.isArray(jsonld["@graph"])).toBe(true);
      });
    });

    test("should convert store to JSON-LD with context", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
        `;
        engine.parseTurtle(ttl);
        const store = engine.getStore();
        
        const context = {
          "ex": "http://example.org/",
          "name": "ex:name"
        };
        
        const jsonld = await engine.toJSONLD(store, { context });
        
        expect(jsonld["@context"]).toBeDefined();
      });
    });

    test("should convert store to JSON-LD with frame", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
        `;
        engine.parseTurtle(ttl);
        const store = engine.getStore();
        
        const frame = {
          "@type": "ex:Human"
        };
        
        const jsonld = await engine.toJSONLD(store, { frame });
        
        expect(jsonld).toBeDefined();
      });
    });

    test("should convert JSON-LD to store", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const jsonldData = {
          "@context": { "ex": "http://example.org/" },
          "@id": "ex:person",
          "@type": "ex:Person",
          "ex:name": "John Doe"
        };
        
        const store = await engine.fromJSONLD(jsonldData);
        
        expect(store.size).toBeGreaterThan(0);
        expect(store).toBe(engine.getStore());
      });
    });
  });
});
