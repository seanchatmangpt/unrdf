/**
 * @fileoverview RDF engine SHACL validation tests
 * 
 * Tests SHACL validation functionality including:
 * - Data validation against shapes
 * - Store input validation
 * - Validation error handling
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, test, expect, beforeEach } from "vitest";
import { RdfEngine } from "../../../src/engines/rdf-engine.mjs";
import { initStore } from "../../../src/context/index.mjs";

describe("RdfEngine SHACL Validation", () => {
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

  describe("SHACL Validation", () => {
    test("should validate data against shapes", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const dataTtl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
        `;
        engine.parseTurtle(dataTtl);
        const dataStore = engine.getStore();
        
        const shapesTtl = `
          @prefix ex: <http://example.org/> .
          @prefix sh: <http://www.w3.org/ns/shacl#> .
          @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
          
          ex:PersonShape a sh:NodeShape ;
            sh:targetClass ex:Human ;
            sh:property [
              sh:path ex:name ;
              sh:datatype xsd:string ;
              sh:minCount 1
            ] .
        `;
        
        const result = await engine.validateShacl(dataStore, shapesTtl);
        
        expect(result).toHaveProperty("conforms");
        expect(result).toHaveProperty("results");
        expect(Array.isArray(result.results)).toBe(true);
      });
    });

    test("should validate with Store input", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const dataTtl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
        `;
        engine.parseTurtle(dataTtl);
        const dataStore = engine.getStore();
        
        const shapesTtl = `
          @prefix ex: <http://example.org/> .
          @prefix sh: <http://www.w3.org/ns/shacl#> .
          
          ex:PersonShape a sh:NodeShape ;
            sh:targetClass ex:Human .
        `;
        const shapesStore = engine.parseTurtle(shapesTtl);
        
        const result = await engine.validateShacl(dataStore, shapesStore);
        
        expect(result).toHaveProperty("conforms");
        expect(result).toHaveProperty("results");
      });
    });

    test("should validate and throw on failure", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const dataTtl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
        `;
        engine.parseTurtle(dataTtl);
        const dataStore = engine.getStore();
        
        const shapesTtl = `
          @prefix ex: <http://example.org/> .
          @prefix sh: <http://www.w3.org/ns/shacl#> .
          
          ex:PersonShape a sh:NodeShape ;
            sh:targetClass ex:Human ;
            sh:property [
              sh:path ex:name ;
              sh:minCount 1
            ] .
        `;
        
        await expect(
          engine.validateShaclOrThrow(dataStore, shapesTtl)
        ).rejects.toThrow("SHACL validation failed");
      });
    });
  });
});
