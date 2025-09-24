/**
 * @fileoverview RDF engine SPARQL query and update tests
 * 
 * Tests SPARQL functionality including:
 * - SELECT queries
 * - ASK queries
 * - CONSTRUCT queries
 * - DESCRIBE queries
 * - INSERT updates
 * - DELETE updates
 * - Query limits and error handling
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, test, expect, beforeEach } from "vitest";
import { RdfEngine } from "../../../src/engines/rdf-engine.mjs";
import { initStore } from "../../../src/context/index.mjs";

describe("RdfEngine SPARQL Query and Update", () => {
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

  describe("SPARQL Query and Update", () => {
    test("should execute SELECT queries", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
          ex:Jane ex:name "Jane Smith" .
        `;
        engine.parseTurtle(ttl);
        
        const query = "SELECT ?s ?p ?o WHERE { ?s ?p ?o }";
        
        const result = await engine.query(query);
        
        expect(result.type).toBe("select");
        expect(result.variables).toBeDefined();
        expect(Array.isArray(result.results)).toBe(true);
        expect(result.results.length).toBeGreaterThan(0);
      });
    });

    test("should execute ASK queries", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
          ex:Jane ex:name "Jane Smith" .
        `;
        engine.parseTurtle(ttl);
        
        const query = "ASK WHERE { ?s <http://example.org/name> ?o }";
        
        const result = await engine.query(query);
        
        expect(result.type).toBe("ask");
        expect(typeof result.boolean).toBe("boolean");
        expect(result.boolean).toBe(true);
      });
    });

    test("should execute CONSTRUCT queries", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
          ex:Jane ex:name "Jane Smith" .
        `;
        engine.parseTurtle(ttl);
        
        const query = `
          CONSTRUCT { ?s <http://example.org/hasName> ?o }
          WHERE { ?s <http://example.org/name> ?o }
        `;
        
        const result = await engine.query(query);
        
        expect(result.type).toBe("construct");
        expect(result.store).toBeDefined();
        expect(result.quads).toBeDefined();
      });
    });

    test("should execute DESCRIBE queries", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
          ex:Jane ex:name "Jane Smith" .
        `;
        engine.parseTurtle(ttl);
        
        const query = "DESCRIBE <http://example.org/John>";
        
        const result = await engine.query(query);
        
        expect(result.type).toBe("describe");
        expect(result.store).toBeDefined();
        expect(result.quads).toBeDefined();
      });
    });

    test("should execute INSERT updates", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
          ex:Jane ex:name "Jane Smith" .
        `;
        engine.parseTurtle(ttl);
        
        const update = `
          INSERT DATA {
            <http://example.org/Bob> <http://example.org/name> "Bob Wilson" .
          }
        `;
        
        const result = await engine.query(update);
        
        expect(result.type).toBe("update");
        expect(result.ok).toBe(true);
        
        // Verify the data was inserted
        const selectResult = await engine.query(
          "SELECT ?name WHERE { <http://example.org/Bob> <http://example.org/name> ?name }"
        );
        expect(selectResult.results.length).toBeGreaterThan(0);
      });
    });

    test("should execute DELETE updates", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
          ex:Jane ex:name "Jane Smith" .
        `;
        engine.parseTurtle(ttl);
        
        const update = `
          DELETE DATA {
            <http://example.org/John> <http://example.org/name> "John Doe" .
          }
        `;
        
        const result = await engine.query(update);
        
        expect(result.type).toBe("update");
        expect(result.ok).toBe(true);
      });
    });

    test("should handle query limits", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
          ex:Jane ex:name "Jane Smith" .
        `;
        engine.parseTurtle(ttl);
        
        const query = "SELECT ?s ?p ?o WHERE { ?s ?p ?o }";
        
        const result = await engine.query(query, { limit: 1 });
        
        expect(result.type).toBe("select");
        expect(result.results.length).toBeLessThanOrEqual(1);
      });
    });

    test("should throw error for invalid queries", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        
        await expect(
          engine.query("")
        ).rejects.toThrow("query: non-empty SPARQL required");
        
        await expect(
          engine.query("INVALID QUERY")
        ).rejects.toThrow("query: unknown query type");
      });
    });
  });
});
