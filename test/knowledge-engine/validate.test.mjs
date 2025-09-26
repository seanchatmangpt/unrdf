/**
 * @fileoverview Unit tests for knowledge-engine/validate.mjs
 * Tests SHACL validation functionality
 */

import { describe, it, expect, beforeEach } from "vitest";
import { Store, DataFactory } from "n3";
import {
  validateShacl,
  validateShaclMultiple,
  formatValidationReport,
  hasValidationErrors,
  getValidationErrors,
  getValidationWarnings
} from "../../src/knowledge-engine/validate.mjs";

const { namedNode, literal, quad } = DataFactory;

describe("validate.mjs", () => {
  let testStore;
  let validShapes;
  let invalidShapes;

  beforeEach(() => {
    testStore = new Store();
    // Add valid test data
    testStore.addQuad(
      namedNode("http://example.org/alice"),
      namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
      namedNode("http://example.org/Person")
    );
    testStore.addQuad(
      namedNode("http://example.org/alice"),
      namedNode("http://example.org/name"),
      literal("Alice")
    );
    testStore.addQuad(
      namedNode("http://example.org/alice"),
      namedNode("http://example.org/email"),
      literal("alice@example.org")
    );
    testStore.addQuad(
      namedNode("http://example.org/alice"),
      namedNode("http://example.org/age"),
      literal("30", namedNode("http://www.w3.org/2001/XMLSchema#integer"))
    );

    // Valid SHACL shapes
    validShapes = `
      @prefix sh: <http://www.w3.org/ns/shacl#> .
      @prefix ex: <http://example.org/> .
      @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
      
      ex:PersonShape a sh:NodeShape ;
        sh:targetClass ex:Person ;
        sh:property [
          sh:path ex:name ;
          sh:minCount 1 ;
          sh:maxCount 1 ;
          sh:datatype xsd:string
        ] ;
        sh:property [
          sh:path ex:email ;
          sh:minCount 1 ;
          sh:maxCount 1 ;
          sh:datatype xsd:string ;
          sh:pattern "^[^\\s@]+@[^\\s@]+\\.[^\\s@]+$"
        ] ;
        sh:property [
          sh:path ex:age ;
          sh:minCount 0 ;
          sh:maxCount 1 ;
          sh:datatype xsd:integer ;
          sh:minInclusive 0
        ] .
    `;

    // Invalid SHACL shapes (will cause validation errors)
    invalidShapes = `
      @prefix sh: <http://www.w3.org/ns/shacl#> .
      @prefix ex: <http://example.org/> .
      @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
      
      ex:PersonShape a sh:NodeShape ;
        sh:targetClass ex:Person ;
        sh:property [
          sh:path ex:name ;
          sh:minCount 2 ;
          sh:maxCount 1 ;
          sh:datatype xsd:string
        ] ;
        sh:property [
          sh:path ex:email ;
          sh:minCount 1 ;
          sh:maxCount 1 ;
          sh:datatype xsd:integer
        ] .
    `;
  });

  describe("validateShacl", () => {
    it("should validate conforming data", () => {
      const report = validateShacl(testStore, validShapes);
      
      expect(report).toHaveProperty("conforms");
      expect(report).toHaveProperty("results");
      expect(Array.isArray(report.results)).toBe(true);
      expect(report.conforms).toBe(true);
      expect(report.results.length).toBe(0);
    });

    it("should detect validation errors", () => {
      const report = validateShacl(testStore, invalidShapes);
      
      expect(report).toHaveProperty("conforms");
      expect(report).toHaveProperty("results");
      expect(Array.isArray(report.results)).toBe(true);
      expect(report.conforms).toBe(false);
      expect(report.results.length).toBeGreaterThan(0);
    });

    it("should validate with Store shapes", () => {
      const shapesStore = new Store();
      // Add shapes to store
      const shapesTtl = validShapes;
      const parser = new (await import('n3')).Parser();
      const shapesQuads = parser.parse(shapesTtl);
      shapesStore.addQuads(shapesQuads);

      const report = validateShacl(testStore, shapesStore);
      
      expect(report).toHaveProperty("conforms");
      expect(report).toHaveProperty("results");
      expect(report.conforms).toBe(true);
    });

    it("should handle validation options", () => {
      const report = validateShacl(testStore, validShapes, {
        strict: true,
        includeDetails: true
      });
      
      expect(report).toHaveProperty("conforms");
      expect(report).toHaveProperty("results");
      expect(report.conforms).toBe(true);
    });

    it("should throw error for invalid store", () => {
      expect(() => {
        validateShacl(null, validShapes);
      }).toThrow("validateShacl: store must be a valid Store instance");

      expect(() => {
        validateShacl("invalid", validShapes);
      }).toThrow("validateShacl: store must be a valid Store instance");
    });

    it("should throw error for missing shapes", () => {
      expect(() => {
        validateShacl(testStore, null);
      }).toThrow("validateShacl: shapes must be provided");

      expect(() => {
        validateShacl(testStore, "");
      }).toThrow("validateShacl: shapes must be provided");
    });

    it("should handle empty store", () => {
      const emptyStore = new Store();
      const report = validateShacl(emptyStore, validShapes);
      
      expect(report).toHaveProperty("conforms");
      expect(report).toHaveProperty("results");
      expect(report.conforms).toBe(true);
      expect(report.results.length).toBe(0);
    });

    it("should handle complex validation scenarios", () => {
      // Add more complex data
      const complexStore = new Store();
      complexStore.addQuad(
        namedNode("http://example.org/person1"),
        namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
        namedNode("http://example.org/Person")
      );
      complexStore.addQuad(
        namedNode("http://example.org/person1"),
        namedNode("http://example.org/name"),
        literal("John Doe")
      );
      complexStore.addQuad(
        namedNode("http://example.org/person1"),
        namedNode("http://example.org/email"),
        literal("john.doe@example.org")
      );
      complexStore.addQuad(
        namedNode("http://example.org/person1"),
        namedNode("http://example.org/age"),
        literal("25", namedNode("http://www.w3.org/2001/XMLSchema#integer"))
      );

      const report = validateShacl(complexStore, validShapes);
      expect(report).toHaveProperty("conforms");
      expect(report).toHaveProperty("results");
    });
  });

  describe("validateShaclMultiple", () => {
    it("should validate against multiple shape sets", () => {
      const shapesList = [validShapes, invalidShapes];
      const reports = validateShaclMultiple(testStore, shapesList);
      
      expect(Array.isArray(reports)).toBe(true);
      expect(reports.length).toBe(2);
      expect(reports[0].conforms).toBe(true);
      expect(reports[1].conforms).toBe(false);
    });

    it("should handle empty shapes list", () => {
      const reports = validateShaclMultiple(testStore, []);
      expect(Array.isArray(reports)).toBe(true);
      expect(reports.length).toBe(0);
    });

    it("should handle mixed valid and invalid shapes", () => {
      const mixedShapes = [
        validShapes,
        invalidShapes,
        validShapes
      ];
      
      const reports = validateShaclMultiple(testStore, mixedShapes);
      expect(reports.length).toBe(3);
      expect(reports[0].conforms).toBe(true);
      expect(reports[1].conforms).toBe(false);
      expect(reports[2].conforms).toBe(true);
    });

    it("should handle validation options", () => {
      const shapesList = [validShapes];
      const reports = validateShaclMultiple(testStore, shapesList, {
        strict: true,
        includeDetails: true
      });
      
      expect(Array.isArray(reports)).toBe(true);
      expect(reports.length).toBe(1);
      expect(reports[0].conforms).toBe(true);
    });
  });

  describe("formatValidationReport", () => {
    it("should format valid report", () => {
      const report = validateShacl(testStore, validShapes);
      const formatted = formatValidationReport(report);
      
      expect(typeof formatted).toBe("string");
      expect(formatted.length).toBeGreaterThan(0);
      expect(formatted).toContain("conforms");
    });

    it("should format invalid report", () => {
      const report = validateShacl(testStore, invalidShapes);
      const formatted = formatValidationReport(report);
      
      expect(typeof formatted).toBe("string");
      expect(formatted.length).toBeGreaterThan(0);
      expect(formatted).toContain("conforms");
    });

    it("should format with custom options", () => {
      const report = validateShacl(testStore, validShapes);
      const formatted = formatValidationReport(report, {
        includeDetails: true,
        format: "json"
      });
      
      expect(typeof formatted).toBe("string");
    });

    it("should handle empty report", () => {
      const emptyReport = { conforms: true, results: [] };
      const formatted = formatValidationReport(emptyReport);
      
      expect(typeof formatted).toBe("string");
      expect(formatted).toContain("conforms");
    });
  });

  describe("hasValidationErrors", () => {
    it("should return false for valid report", () => {
      const report = validateShacl(testStore, validShapes);
      const hasErrors = hasValidationErrors(report);
      
      expect(hasErrors).toBe(false);
    });

    it("should return true for invalid report", () => {
      const report = validateShacl(testStore, invalidShapes);
      const hasErrors = hasValidationErrors(report);
      
      expect(hasErrors).toBe(true);
    });

    it("should handle empty report", () => {
      const emptyReport = { conforms: true, results: [] };
      const hasErrors = hasValidationErrors(emptyReport);
      
      expect(hasErrors).toBe(false);
    });
  });

  describe("getValidationErrors", () => {
    it("should return empty array for valid report", () => {
      const report = validateShacl(testStore, validShapes);
      const errors = getValidationErrors(report);
      
      expect(Array.isArray(errors)).toBe(true);
      expect(errors.length).toBe(0);
    });

    it("should return errors for invalid report", () => {
      const report = validateShacl(testStore, invalidShapes);
      const errors = getValidationErrors(report);
      
      expect(Array.isArray(errors)).toBe(true);
      expect(errors.length).toBeGreaterThan(0);
    });

    it("should filter only error-level results", () => {
      const report = validateShacl(testStore, invalidShapes);
      const errors = getValidationErrors(report);
      
      if (errors.length > 0) {
        errors.forEach(error => {
          expect(error).toHaveProperty("severity");
          expect(error.severity).toBe("http://www.w3.org/ns/shacl#Violation");
        });
      }
    });
  });

  describe("getValidationWarnings", () => {
    it("should return empty array for valid report", () => {
      const report = validateShacl(testStore, validShapes);
      const warnings = getValidationWarnings(report);
      
      expect(Array.isArray(warnings)).toBe(true);
      expect(warnings.length).toBe(0);
    });

    it("should return warnings for report with warnings", () => {
      const report = validateShacl(testStore, invalidShapes);
      const warnings = getValidationWarnings(report);
      
      expect(Array.isArray(warnings)).toBe(true);
      // Note: The actual warnings depend on the SHACL implementation
    });

    it("should filter only warning-level results", () => {
      const report = validateShacl(testStore, invalidShapes);
      const warnings = getValidationWarnings(report);
      
      if (warnings.length > 0) {
        warnings.forEach(warning => {
          expect(warning).toHaveProperty("severity");
          expect(warning.severity).toBe("http://www.w3.org/ns/shacl#Warning");
        });
      }
    });
  });

  describe("edge cases", () => {
    it("should handle malformed SHACL shapes", () => {
      const malformedShapes = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        
        ex:PersonShape a sh:NodeShape ;
          sh:targetClass ex:Person ;
          sh:property [
            sh:path ex:name ;
            sh:minCount "invalid" ;  // Invalid value
            sh:maxCount 1
          ] .
      `;

      expect(() => {
        validateShacl(testStore, malformedShapes);
      }).toThrow();
    });

    it("should handle very large validation reports", () => {
      // Create a large store
      const largeStore = new Store();
      for (let i = 0; i < 1000; i++) {
        largeStore.addQuad(
          namedNode(`http://example.org/person${i}`),
          namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
          namedNode("http://example.org/Person")
        );
        largeStore.addQuad(
          namedNode(`http://example.org/person${i}`),
          namedNode("http://example.org/name"),
          literal(`Person ${i}`)
        );
      }

      const report = validateShacl(largeStore, validShapes);
      expect(report).toHaveProperty("conforms");
      expect(report).toHaveProperty("results");
    });

    it("should handle concurrent validations", async () => {
      const promises = [
        Promise.resolve(validateShacl(testStore, validShapes)),
        Promise.resolve(validateShacl(testStore, invalidShapes)),
        Promise.resolve(validateShacl(testStore, validShapes))
      ];

      const reports = await Promise.all(promises);
      expect(reports).toHaveLength(3);
      expect(reports[0].conforms).toBe(true);
      expect(reports[1].conforms).toBe(false);
      expect(reports[2].conforms).toBe(true);
    });

    it("should handle validation with blank nodes", () => {
      const storeWithBlanks = new Store();
      storeWithBlanks.addQuad(
        namedNode("http://example.org/person"),
        namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
        namedNode("http://example.org/Person")
      );
      storeWithBlanks.addQuad(
        namedNode("http://example.org/person"),
        namedNode("http://example.org/address"),
        namedNode("_:address1")
      );
      storeWithBlanks.addQuad(
        namedNode("_:address1"),
        namedNode("http://example.org/street"),
        literal("123 Main St")
      );

      const shapesWithBlanks = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        
        ex:PersonShape a sh:NodeShape ;
          sh:targetClass ex:Person ;
          sh:property [
            sh:path ex:address ;
            sh:node ex:AddressShape
          ] .
        
        ex:AddressShape a sh:NodeShape ;
          sh:property [
            sh:path ex:street ;
            sh:minCount 1
          ] .
      `;

      const report = validateShacl(storeWithBlanks, shapesWithBlanks);
      expect(report).toHaveProperty("conforms");
      expect(report).toHaveProperty("results");
    });

    it("should handle validation with different datatypes", () => {
      const storeWithTypes = new Store();
      storeWithTypes.addQuad(
        namedNode("http://example.org/test"),
        namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
        namedNode("http://example.org/TestClass")
      );
      storeWithTypes.addQuad(
        namedNode("http://example.org/test"),
        namedNode("http://example.org/intValue"),
        literal("42", namedNode("http://www.w3.org/2001/XMLSchema#integer"))
      );
      storeWithTypes.addQuad(
        namedNode("http://example.org/test"),
        namedNode("http://example.org/floatValue"),
        literal("3.14", namedNode("http://www.w3.org/2001/XMLSchema#float"))
      );
      storeWithTypes.addQuad(
        namedNode("http://example.org/test"),
        namedNode("http://example.org/dateValue"),
        literal("2024-01-01", namedNode("http://www.w3.org/2001/XMLSchema#date"))
      );

      const typeShapes = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        
        ex:TestShape a sh:NodeShape ;
          sh:targetClass ex:TestClass ;
          sh:property [
            sh:path ex:intValue ;
            sh:datatype xsd:integer
          ] ;
          sh:property [
            sh:path ex:floatValue ;
            sh:datatype xsd:float
          ] ;
          sh:property [
            sh:path ex:dateValue ;
            sh:datatype xsd:date
          ] .
      `;

      const report = validateShacl(storeWithTypes, typeShapes);
      expect(report).toHaveProperty("conforms");
      expect(report).toHaveProperty("results");
    });
  });
});
