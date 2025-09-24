/**
 * @fileoverview Tests for useValidator composable with context architecture
 * 
 * Tests the SHACL validation functionality using the context system
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, expect, it, beforeEach } from "vitest";
import { useValidator } from "../../src/composables/use-validator.mjs";
import { useStore } from "../../src/composables/use-store.mjs";
import { initStore } from "../../src/context/index.mjs";
import { Store, DataFactory } from "n3";

const { namedNode, literal, quad } = DataFactory;

describe("useValidator with Context", () => {
  let runApp;

  beforeEach(() => {
    // Create test data
    const testQuads = [
      quad(
        namedNode("http://example.org/person1"),
        namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
        namedNode("http://xmlns.com/foaf/0.1/Person")
      ),
      quad(
        namedNode("http://example.org/person1"),
        namedNode("http://xmlns.com/foaf/0.1/name"),
        literal("John Doe")
      )
    ];
    
    runApp = initStore(testQuads, { baseIRI: "http://example.org/" });
  });

  it("should create validator interface with context", async () => {
    await runApp(() => {
      // Act
      const validator = useValidator();
      
      // Assert
      expect(typeof validator.validate).toBe("function");
      expect(typeof validator.validateOrThrow).toBe("function");
      expect(typeof validator.validateStore).toBe("function");
      expect(typeof validator.validateStoreOrThrow).toBe("function");
      expect(typeof validator.summarize).toBe("function");
      expect(validator.engine).toBeDefined();
    });
  });

  it("should validate context store against SHACL shapes", async () => {
    await runApp(async () => {
      // Arrange
      const validator = useValidator();
      const shapesTurtle = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        
        [] a sh:NodeShape ;
           sh:targetClass foaf:Person ;
           sh:property [
             sh:path foaf:name ;
             sh:minCount 1 ;
             sh:maxCount 1 ;
           ] .
      `;
      
      // Act
      const report = await validator.validate(shapesTurtle);
      
      // Assert
      expect(report).toBeDefined();
      expect(typeof report.conforms).toBe("boolean");
      expect(Array.isArray(report.results)).toBe(true);
    });
  });

  it("should validate specific store against SHACL shapes", async () => {
    await runApp(async () => {
      // Arrange
      const validator = useValidator();
      const dataStore = useStore();
      const shapesStore = new Store([
        quad(
          namedNode("http://example.org/PersonShape"),
          namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
          namedNode("http://www.w3.org/ns/shacl#NodeShape")
        )
      ]);
      
      // Act
      const report = await validator.validateStore(dataStore, shapesStore);
      
      // Assert
      expect(report).toBeDefined();
      expect(typeof report.conforms).toBe("boolean");
      expect(Array.isArray(report.results)).toBe(true);
    });
  });

  it("should summarize validation results", async () => {
    await runApp(async () => {
      // Arrange
      const validator = useValidator();
      const shapesTurtle = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        
        [] a sh:NodeShape ;
           sh:targetClass foaf:Person ;
           sh:property [
             sh:path foaf:name ;
             sh:minCount 1 ;
             sh:maxCount 1 ;
           ] .
      `;
      
      // Act
      const report = await validator.validate(shapesTurtle);
      const summary = validator.summarize(report);
      
      // Assert
      expect(summary).toBeDefined();
      expect(typeof summary.conforms).toBe("boolean");
      expect(typeof summary.totalResults).toBe("number");
      expect(typeof summary.errorCount).toBe("number");
      expect(typeof summary.warningCount).toBe("number");
      expect(typeof summary.infoCount).toBe("number");
      expect(typeof summary.hasErrors).toBe("boolean");
      expect(typeof summary.hasWarnings).toBe("boolean");
      expect(typeof summary.hasInfo).toBe("boolean");
    });
  });

  it("should filter validation results by severity", async () => {
    await runApp(async () => {
      // Arrange
      const validator = useValidator();
      const shapesTurtle = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        
        [] a sh:NodeShape ;
           sh:targetClass foaf:Person ;
           sh:property [
             sh:path foaf:name ;
             sh:minCount 1 ;
             sh:maxCount 1 ;
           ] .
      `;
      
      // Act
      const report = await validator.validate(shapesTurtle);
      const violations = validator.filterBySeverity(report, "http://www.w3.org/ns/shacl#Violation");
      
      // Assert
      expect(Array.isArray(violations)).toBe(true);
    });
  });

  it("should group validation results by focus node", async () => {
    await runApp(async () => {
      // Arrange
      const validator = useValidator();
      const shapesTurtle = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        
        [] a sh:NodeShape ;
           sh:targetClass foaf:Person ;
           sh:property [
             sh:path foaf:name ;
             sh:minCount 1 ;
             sh:maxCount 1 ;
           ] .
      `;
      
      // Act
      const report = await validator.validate(shapesTurtle);
      const grouped = validator.groupByFocusNode(report);
      
      // Assert
      expect(grouped).toBeInstanceOf(Map);
    });
  });

  it("should group validation results by property path", async () => {
    await runApp(async () => {
      // Arrange
      const validator = useValidator();
      const shapesTurtle = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        
        [] a sh:NodeShape ;
           sh:targetClass foaf:Person ;
           sh:property [
             sh:path foaf:name ;
             sh:minCount 1 ;
             sh:maxCount 1 ;
           ] .
      `;
      
      // Act
      const report = await validator.validate(shapesTurtle);
      const grouped = validator.groupByPath(report);
      
      // Assert
      expect(grouped).toBeInstanceOf(Map);
    });
  });

  it("should throw error when context is not initialized", () => {
    // Act & Assert
    expect(() => {
      useValidator();
    }).toThrow();
  });

  it("should work with different timeout settings", async () => {
    await runApp(() => {
      // Act
      const validator = useValidator({ timeoutMs: 60000 });
      
      // Assert
      expect(validator.engine).toBeDefined();
      expect(typeof validator.validate).toBe("function");
    });
  });

  it("should handle empty shapes gracefully", async () => {
    await runApp(async () => {
      // Arrange
      const validator = useValidator();
      
      // Act - Pass null for shapes to avoid parsing empty string
      const report = await validator.validate(null);
      
      // Assert
      expect(report).toBeDefined();
      expect(typeof report.conforms).toBe("boolean");
    });
  });
});