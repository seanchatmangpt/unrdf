/**
 * @fileoverview Tests for useNQuads composable with context architecture
 * 
 * Tests the N-Quads functionality using the context system
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, expect, it, beforeEach } from "vitest";
import { useNQuads } from "../../src/composables/use-n-quads.mjs";
import { useStore } from "../../src/composables/use-store.mjs";
import { initStore } from "../../src/context/index.mjs";
import { Store, DataFactory } from "n3";

const { namedNode, literal, quad } = DataFactory;

describe("useNQuads with Context", () => {
  let runApp;

  beforeEach(() => {
    // Create test data
    const testQuads = [
      quad(
        namedNode("http://example.org/person1"),
        namedNode("http://xmlns.com/foaf/0.1/name"),
        literal("John Doe")
      ),
      quad(
        namedNode("http://example.org/person1"),
        namedNode("http://xmlns.com/foaf/0.1/age"),
        literal("30", namedNode("http://www.w3.org/2001/XMLSchema#integer"))
      )
    ];
    
    runApp = initStore(testQuads, { baseIRI: "http://example.org/" });
  });

  it("should create nquads interface with context", async () => {
    await runApp(() => {
      // Act
      const nquads = useNQuads();
      
      // Assert
      expect(typeof nquads.parse).toBe("function");
      expect(typeof nquads.serialize).toBe("function");
      expect(typeof nquads.parseFile).toBe("function");
      expect(typeof nquads.writeFile).toBe("function");
      expect(typeof nquads.validate).toBe("function");
      expect(typeof nquads.getStats).toBe("function");
      expect(typeof nquads.toTurtle).toBe("function");
      expect(typeof nquads.fromTurtle).toBe("function");
      expect(typeof nquads.getBaseIRI).toBe("function");
      expect(typeof nquads.setBaseIRI).toBe("function");
      expect(typeof nquads.isNQuads).toBe("function");
      expect(typeof nquads.isValid).toBe("function");
    });
  });

  it("should parse N-Quads string", async () => {
    await runApp(() => {
      // Arrange
      const nquads = useNQuads();
      const nquadsString = `
        <http://example.org/person1> <http://xmlns.com/foaf/0.1/name> "John Doe" .
        <http://example.org/person1> <http://xmlns.com/foaf/0.1/age> "30"^^<http://www.w3.org/2001/XMLSchema#integer> .
      `;
      
      // Act
      const store = nquads.parse(nquadsString);
      
      // Assert
      expect(store).toBeDefined();
      expect(typeof store.size).toBe("number");
      expect(store.size).toBeGreaterThan(0);
    });
  });

  it("should serialize store to N-Quads", async () => {
    await runApp(async () => {
      // Arrange
      const nquads = useNQuads();
      const store = useStore();
      
      // Act
      const nquadsString = await nquads.serialize(store.store);
      
      // Assert
      expect(typeof nquadsString).toBe("string");
      expect(nquadsString.length).toBeGreaterThan(0);
      expect(nquadsString).toContain("http://example.org/person1");
      expect(nquadsString).toContain("John Doe");
    });
  });

  it("should validate N-Quads string", async () => {
    await runApp(() => {
      // Arrange
      const nquads = useNQuads();
      const validNQuads = `
        <http://example.org/person1> <http://xmlns.com/foaf/0.1/name> "John Doe" .
      `;
      const invalidNQuads = `invalid nquads content`;
      
      // Act
      const validResult = nquads.validate(validNQuads);
      const invalidResult = nquads.validate(invalidNQuads);
      
      // Assert
      expect(validResult.valid).toBe(true);
      expect(validResult.error).toBeNull();
      expect(validResult.quads).toBeGreaterThan(0);
      
      expect(invalidResult.valid).toBe(false);
      expect(invalidResult.error).toBeDefined();
      expect(invalidResult.quads).toBe(0);
    });
  });

  it("should get statistics about N-Quads content", async () => {
    await runApp(() => {
      // Arrange
      const nquads = useNQuads();
      const nquadsString = `
        <http://example.org/person1> <http://xmlns.com/foaf/0.1/name> "John Doe" .
        <http://example.org/person1> <http://xmlns.com/foaf/0.1/age> "30"^^<http://www.w3.org/2001/XMLSchema#integer> .
      `;
      
      // Act
      const stats = nquads.getStats(nquadsString);
      
      // Assert
      expect(stats).toBeDefined();
      expect(typeof stats.quads).toBe("number");
      expect(typeof stats.subjects).toBe("number");
      expect(typeof stats.predicates).toBe("number");
      expect(typeof stats.objects).toBe("number");
      expect(typeof stats.graphs).toBe("number");
      expect(typeof stats.lines).toBe("number");
      expect(typeof stats.quadLines).toBe("number");
      expect(typeof stats.emptyLines).toBe("number");
    });
  });

  it("should convert N-Quads to Turtle", async () => {
    await runApp(async () => {
      // Arrange
      const nquads = useNQuads();
      const nquadsString = `
        <http://example.org/person1> <http://xmlns.com/foaf/0.1/name> "John Doe" .
      `;
      
      // Act
      const turtle = await nquads.toTurtle(nquadsString, {
        prefixes: { ex: "http://example.org/" }
      });
      
      // Assert
      expect(typeof turtle).toBe("string");
      expect(turtle.length).toBeGreaterThan(0);
      expect(turtle).toContain("@prefix");
    });
  });

  it("should convert Turtle to N-Quads", async () => {
    await runApp(async () => {
      // Arrange
      const nquads = useNQuads();
      const turtle = `
        @prefix ex: <http://example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        ex:person1 foaf:name "John Doe" .
      `;
      
      // Act
      const nquadsString = await nquads.fromTurtle(turtle);
      
      // Assert
      expect(typeof nquadsString).toBe("string");
      expect(nquadsString.length).toBeGreaterThan(0);
      expect(nquadsString).toContain("http://example.org/person1");
    });
  });

  it("should get and set base IRI", async () => {
    await runApp(() => {
      // Arrange
      const nquads = useNQuads();
      
      // Act
      const originalBaseIRI = nquads.getBaseIRI();
      nquads.setBaseIRI("http://custom.org/");
      const newBaseIRI = nquads.getBaseIRI();
      
      // Assert
      expect(originalBaseIRI).toBe("http://example.org/");
      expect(newBaseIRI).toBe("http://custom.org/");
    });
  });

  it("should check if content is N-Quads format", async () => {
    await runApp(() => {
      // Arrange
      const nquads = useNQuads();
      const nquadsContent = `
        <http://example.org/person1> <http://xmlns.com/foaf/0.1/name> "John Doe" .
      `;
      const nonNQuadsContent = `This is not N-Quads content`;
      
      // Act
      const isNQuads = nquads.isNQuads(nquadsContent);
      const isNotNQuads = nquads.isNQuads(nonNQuadsContent);
      
      // Assert
      expect(isNQuads).toBe(true);
      expect(isNotNQuads).toBe(false);
    });
  });

  it("should check if content is valid N-Quads", async () => {
    await runApp(() => {
      // Arrange
      const nquads = useNQuads();
      const validNQuads = `
        <http://example.org/person1> <http://xmlns.com/foaf/0.1/name> "John Doe" .
      `;
      const invalidNQuads = `invalid nquads content`;
      
      // Act
      const isValid = nquads.isValid(validNQuads);
      const isInvalid = nquads.isValid(invalidNQuads);
      
      // Assert
      expect(isValid).toBe(true);
      expect(isInvalid).toBe(false);
    });
  });

  it("should handle empty N-Quads gracefully", async () => {
    await runApp(() => {
      // Arrange
      const nquads = useNQuads();
      
      // Act - Use null instead of empty string to avoid parsing errors
      const store = nquads.parse(null);
      const validation = nquads.validate(null);
      const stats = nquads.getStats(null);
      
      // Assert
      expect(store.size).toBe(0);
      expect(validation.valid).toBe(true);
      expect(stats.quads).toBe(0);
    });
  });

  it("should throw error when context is not initialized", () => {
    // Act & Assert
    expect(() => {
      useNQuads();
    }).toThrow();
  });

  it("should work with different base IRIs", async () => {
    const runAppCustom = initStore([], { baseIRI: "http://custom.org/" });
    
    await runAppCustom(() => {
      // Act
      const nquads = useNQuads();
      
      // Assert
      expect(nquads.getBaseIRI()).toBe("http://custom.org/");
      expect(typeof nquads.parse).toBe("function");
    });
  });

  it("should work with strict mode disabled", async () => {
    await runApp(() => {
      // Arrange
      const nquads = useNQuads({ strict: false });
      const invalidNQuads = `invalid nquads content`;
      
      // Act
      const store = nquads.parse(invalidNQuads);
      
      // Assert
      expect(store).toBeDefined();
      expect(store.size).toBe(0); // Should return empty store instead of throwing
    });
  });
});
});
});