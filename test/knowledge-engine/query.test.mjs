/**
 * @fileoverview Unit tests for knowledge-engine/query.mjs
 * Tests SPARQL SELECT, ASK, CONSTRUCT, UPDATE operations
 */

import { describe, it, expect, beforeEach } from "vitest";
import { Store, DataFactory } from "n3";
import {
  query,
  select,
  ask,
  construct,
  describe as describeQuery,
  update,
  getQueryStats
} from "../../src/knowledge-engine/query.mjs";

const { namedNode, literal, quad } = DataFactory;

describe("query.mjs", () => {
  let testStore;

  beforeEach(() => {
    testStore = new Store();
    // Add test data
    testStore.addQuad(
      namedNode("http://example.org/alice"),
      namedNode("http://example.org/name"),
      literal("Alice")
    );
    testStore.addQuad(
      namedNode("http://example.org/alice"),
      namedNode("http://example.org/age"),
      literal("30", namedNode("http://www.w3.org/2001/XMLSchema#integer"))
    );
    testStore.addQuad(
      namedNode("http://example.org/alice"),
      namedNode("http://example.org/knows"),
      namedNode("http://example.org/bob")
    );
    testStore.addQuad(
      namedNode("http://example.org/bob"),
      namedNode("http://example.org/name"),
      literal("Bob")
    );
    testStore.addQuad(
      namedNode("http://example.org/bob"),
      namedNode("http://example.org/age"),
      literal("25", namedNode("http://www.w3.org/2001/XMLSchema#integer"))
    );
    testStore.addQuad(
      namedNode("http://example.org/alice"),
      namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
      namedNode("http://example.org/Person")
    );
    testStore.addQuad(
      namedNode("http://example.org/bob"),
      namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
      namedNode("http://example.org/Person")
    );
  });

  describe("query", () => {
    it("should execute SELECT query", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        SELECT ?name ?age WHERE {
          ?person ex:name ?name ;
                  ex:age ?age .
        }
      `;

      const results = await query(testStore, sparql);
      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBe(2);
      expect(results[0]).toHaveProperty("name");
      expect(results[0]).toHaveProperty("age");
    });

    it("should execute ASK query", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        ASK WHERE {
          ex:alice ex:knows ex:bob .
        }
      `;

      const result = await query(testStore, sparql);
      expect(typeof result).toBe("boolean");
      expect(result).toBe(true);
    });

    it("should execute CONSTRUCT query", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        CONSTRUCT {
          ?person ex:type ex:Person .
        } WHERE {
          ?person a ex:Person .
        }
      `;

      const result = await query(testStore, sparql);
      expect(result).toBeInstanceOf(Store);
      expect(result.size).toBe(2);
    });

    it("should execute DESCRIBE query", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        DESCRIBE ex:alice
      `;

      const result = await query(testStore, sparql);
      expect(result).toBeInstanceOf(Store);
      expect(result.size).toBeGreaterThan(0);
    });

    it("should execute UPDATE query", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        INSERT DATA {
          ex:alice ex:email "alice@example.org" .
        }
      `;

      const result = await update(testStore, sparql);
      expect(result).toBeInstanceOf(Store);
      
      // Check that the data was added
      const checkQuery = `
        PREFIX ex: <http://example.org/>
        ASK WHERE {
          ex:alice ex:email "alice@example.org" .
        }
      `;
      const exists = await query(result, checkQuery);
      expect(exists).toBe(true);
    });

    it("should handle query with options", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        SELECT ?name WHERE {
          ?person ex:name ?name .
        } LIMIT 1
      `;

      const results = await query(testStore, sparql, { limit: 1 });
      expect(results.length).toBeLessThanOrEqual(1);
    });

    it("should throw error for invalid store", async () => {
      const sparql = "SELECT * WHERE { ?s ?p ?o }";
      
      await expect(query(null, sparql)).rejects.toThrow("query: store must be a valid Store instance");
      await expect(query("invalid", sparql)).rejects.toThrow("query: store must be a valid Store instance");
    });

    it("should throw error for invalid SPARQL", async () => {
      await expect(query(testStore, "")).rejects.toThrow("query: sparql must be a non-empty string");
      await expect(query(testStore, null)).rejects.toThrow("query: sparql must be a non-empty string");
      await expect(query(testStore, "INVALID SPARQL")).rejects.toThrow();
    });
  });

  describe("select", () => {
    it("should execute SELECT query and return array", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        SELECT ?name WHERE {
          ?person ex:name ?name .
        }
      `;

      const results = await select(testStore, sparql);
      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBe(2);
      expect(results[0]).toHaveProperty("name");
    });

    it("should handle SELECT with aggregation", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        SELECT (COUNT(?person) as ?count) WHERE {
          ?person a ex:Person .
        }
      `;

      const results = await select(testStore, sparql);
      expect(Array.isArray(results)).toBe(true);
      expect(results[0]).toHaveProperty("count");
    });

    it("should handle SELECT with ORDER BY", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        SELECT ?name ?age WHERE {
          ?person ex:name ?name ;
                  ex:age ?age .
        } ORDER BY ?age
      `;

      const results = await select(testStore, sparql);
      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBe(2);
    });

    it("should handle SELECT with LIMIT", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        SELECT ?name WHERE {
          ?person ex:name ?name .
        } LIMIT 1
      `;

      const results = await select(testStore, sparql);
      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBe(1);
    });
  });

  describe("ask", () => {
    it("should return true for existing pattern", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        ASK WHERE {
          ex:alice ex:knows ex:bob .
        }
      `;

      const result = await ask(testStore, sparql);
      expect(result).toBe(true);
    });

    it("should return false for non-existing pattern", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        ASK WHERE {
          ex:alice ex:knows ex:charlie .
        }
      `;

      const result = await ask(testStore, sparql);
      expect(result).toBe(false);
    });

    it("should handle complex ASK queries", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        ASK WHERE {
          ?person a ex:Person ;
                  ex:name ?name ;
                  ex:age ?age .
          FILTER(?age > 20)
        }
      `;

      const result = await ask(testStore, sparql);
      expect(typeof result).toBe("boolean");
    });
  });

  describe("construct", () => {
    it("should construct new triples", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        CONSTRUCT {
          ?person ex:type ex:Person .
        } WHERE {
          ?person a ex:Person .
        }
      `;

      const result = await construct(testStore, sparql);
      expect(result).toBeInstanceOf(Store);
      expect(result.size).toBe(2);
    });

    it("should construct with multiple patterns", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        CONSTRUCT {
          ?person ex:hasName ?name ;
                  ex:hasAge ?age .
        } WHERE {
          ?person ex:name ?name ;
                  ex:age ?age .
        }
      `;

      const result = await construct(testStore, sparql);
      expect(result).toBeInstanceOf(Store);
      expect(result.size).toBe(4); // 2 people × 2 properties
    });

    it("should handle empty CONSTRUCT result", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        CONSTRUCT {
          ?person ex:type ex:NonExistent .
        } WHERE {
          ?person a ex:NonExistent .
        }
      `;

      const result = await construct(testStore, sparql);
      expect(result).toBeInstanceOf(Store);
      expect(result.size).toBe(0);
    });
  });

  describe("describe", () => {
    it("should describe a specific resource", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        DESCRIBE ex:alice
      `;

      const result = await describe(testStore, sparql);
      expect(result).toHaveProperty("type");
      expect(result.type).toBe("collector");
    });

    it("should describe multiple resources", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        DESCRIBE ex:alice ex:bob
      `;

      const result = await describe(testStore, sparql);
      expect(result).toHaveProperty("type");
      expect(result.type).toBe("collector");
    });

    it("should describe with WHERE clause", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        DESCRIBE ?person WHERE {
          ?person a ex:Person .
        }
      `;

      const result = await describe(testStore, sparql);
      expect(result).toHaveProperty("type");
      expect(result.type).toBe("collector");
    });
  });

  describe("update", () => {
    it("should insert data", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        INSERT DATA {
          ex:alice ex:email "alice@example.org" .
        }
      `;

      const result = await update(testStore, sparql);
      expect(result).toBeInstanceOf(Store);
      
      // Verify insertion
      const checkQuery = `
        PREFIX ex: <http://example.org/>
        ASK WHERE {
          ex:alice ex:email "alice@example.org" .
        }
      `;
      const exists = await ask(result, checkQuery);
      expect(exists).toBe(true);
    });

    it("should delete data", async () => {
      // First insert some data
      const storeWithTemp = await update(testStore, `
        PREFIX ex: <http://example.org/>
        INSERT DATA {
          ex:alice ex:temp "temp value" .
        }
      `);

      // Then delete it
      const sparql = `
        PREFIX ex: <http://example.org/>
        DELETE DATA {
          ex:alice ex:temp "temp value" .
        }
      `;

      const result = await update(storeWithTemp, sparql);
      expect(result).toBeInstanceOf(Store);
      
      // Verify deletion
      const checkQuery = `
        PREFIX ex: <http://example.org/>
        ASK WHERE {
          ex:alice ex:temp "temp value" .
        }
      `;
      const exists = await ask(result, checkQuery);
      expect(exists).toBe(false);
    });

    it("should insert with WHERE clause", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        INSERT {
          ?person ex:hasName ?name .
        } WHERE {
          ?person ex:name ?name .
        }
      `;

      const result = await update(testStore, sparql);
      expect(result).toBeInstanceOf(Store);
      
      // Verify insertion
      const checkQuery = `
        PREFIX ex: <http://example.org/>
        ASK WHERE {
          ?person ex:hasName ?name .
        }
      `;
      const exists = await ask(result, checkQuery);
      expect(exists).toBe(true);
    });

    it("should delete with WHERE clause", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        DELETE {
          ?person ex:age ?age .
        } WHERE {
          ?person ex:age ?age .
          FILTER(?age > 25)
        }
      `;

      const result = await update(testStore, sparql);
      expect(result).toBeInstanceOf(Store);
      
      // Verify deletion (Bob's age should be gone, Alice's should remain)
      const checkQuery = `
        PREFIX ex: <http://example.org/>
        SELECT ?name ?age WHERE {
          ?person ex:name ?name ;
                  ex:age ?age .
        }
      `;
      const results = await select(result, checkQuery);
      expect(results.length).toBe(1); // Only Alice should remain
      expect(results[0].name).toBe("Alice");
    });
  });

  describe("getQueryStats", () => {
    it("should return query statistics", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        SELECT ?name WHERE {
          ?person ex:name ?name .
        }
      `;

      const stats = await getQueryStats(testStore, sparql);
      expect(stats).toHaveProperty("duration");
      expect(stats).toHaveProperty("resultCount");
      expect(stats).toHaveProperty("success");
      expect(typeof stats.duration).toBe("number");
      expect(typeof stats.resultCount).toBe("number");
      expect(typeof stats.success).toBe("boolean");
    });

    it("should handle different query types", async () => {
      const selectStats = await getQueryStats(testStore, "SELECT * WHERE { ?s ?p ?o }");
      expect(selectStats).toHaveProperty("duration");
      expect(selectStats).toHaveProperty("resultCount");

      const askStats = await getQueryStats(testStore, "ASK WHERE { ?s ?p ?o }");
      expect(askStats).toHaveProperty("duration");
      expect(askStats).toHaveProperty("resultCount");

      const constructStats = await getQueryStats(testStore, "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }");
      expect(constructStats).toHaveProperty("duration");
      expect(constructStats).toHaveProperty("resultCount");
    });
  });

  describe("edge cases", () => {
    it("should handle empty store", async () => {
      const emptyStore = new Store();
      const sparql = "SELECT * WHERE { ?s ?p ?o }";
      
      const results = await select(emptyStore, sparql);
      expect(results).toEqual([]);
    });

    it("should handle queries with no results", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        SELECT ?name WHERE {
          ?person ex:name ?name .
          FILTER(?name = "NonExistent")
        }
      `;

      const results = await select(testStore, sparql);
      expect(results).toEqual([]);
    });

    it("should handle large result sets", async () => {
      // Add many triples
      const largeStore = new Store();
      for (let i = 0; i < 1000; i++) {
        largeStore.addQuad(
          namedNode(`http://example.org/resource${i}`),
          namedNode("http://example.org/hasValue"),
          literal(`value${i}`)
        );
      }

      const sparql = "SELECT ?s ?o WHERE { ?s <http://example.org/hasValue> ?o }";
      const results = await select(largeStore, sparql);
      expect(results.length).toBe(1000);
    });

    it("should handle concurrent queries", async () => {
      const queries = [
        "SELECT ?name WHERE { ?person <http://example.org/name> ?name }",
        "ASK WHERE { ?person a <http://example.org/Person> }",
        "CONSTRUCT { ?person <http://example.org/type> <http://example.org/Person> } WHERE { ?person a <http://example.org/Person> }"
      ];

      const promises = queries.map(sparql => query(testStore, sparql));
      const results = await Promise.all(promises);
      
      expect(results).toHaveLength(3);
      expect(Array.isArray(results[0])).toBe(true);
      expect(typeof results[1]).toBe("boolean");
      expect(results[2]).toBeInstanceOf(Store);
    });

    it("should handle queries with special characters", async () => {
      // Add data with special characters
      testStore.addQuad(
        namedNode("http://example.org/test"),
        namedNode("http://example.org/value"),
        literal("Special chars: \"quotes\", \n newlines, \t tabs")
      );

      const sparql = `
        PREFIX ex: <http://example.org/>
        SELECT ?value WHERE {
          ex:test ex:value ?value .
        }
      `;

      const results = await select(testStore, sparql);
      expect(results.length).toBe(1);
      expect(results[0].value).toContain("quotes");
    });

    it("should handle queries with language tags", async () => {
      // Add data with language tags
      testStore.addQuad(
        namedNode("http://example.org/test"),
        namedNode("http://example.org/name"),
        literal("Test", "en")
      );
      testStore.addQuad(
        namedNode("http://example.org/test"),
        namedNode("http://example.org/name"),
        literal("Prueba", "es")
      );

      const sparql = `
        PREFIX ex: <http://example.org/>
        SELECT ?name WHERE {
          ex:test ex:name ?name .
        }
      `;

      const results = await select(testStore, sparql);
      expect(results.length).toBe(2);
    });

    it("should handle queries with datatypes", async () => {
      const sparql = `
        PREFIX ex: <http://example.org/>
        SELECT ?age WHERE {
          ?person ex:age ?age .
        }
      `;

      const results = await select(testStore, sparql);
      expect(results.length).toBe(1);
      expect(results[0].age).toBe("30");
    });
  });
});
