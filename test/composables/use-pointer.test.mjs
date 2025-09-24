/**
 * @fileoverview Tests for usePointer composable with context architecture
 * 
 * Tests the Clownface graph traversal functionality using the context system
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, expect, it, beforeEach } from "vitest";
import { usePointer } from "../../src/composables/use-pointer.mjs";
import { useStore } from "../../src/composables/use-store.mjs";
import { initStore } from "../../src/context/index.mjs";
import { Store, DataFactory } from "n3";

const { namedNode, literal, quad } = DataFactory;

describe("usePointer with Context", () => {
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
      ),
      quad(
        namedNode("http://example.org/person1"),
        namedNode("http://xmlns.com/foaf/0.1/knows"),
        namedNode("http://example.org/person2")
      ),
      quad(
        namedNode("http://example.org/person2"),
        namedNode("http://xmlns.com/foaf/0.1/name"),
        literal("Jane Smith")
      )
    ];
    
    runApp = initStore(testQuads, { baseIRI: "http://example.org/" });
  });

  it("should create pointer interface with context", async () => {
    await runApp(() => {
      // Act
      const pointer = usePointer();
      
      // Assert
      expect(typeof pointer.node).toBe("function");
      expect(typeof pointer.blankNode).toBe("function");
      expect(typeof pointer.namedNode).toBe("function");
      expect(typeof pointer.literal).toBe("function");
      expect(typeof pointer.all).toBe("function");
      expect(typeof pointer.subjects).toBe("function");
      expect(typeof pointer.predicates).toBe("function");
      expect(typeof pointer.objects).toBe("function");
      expect(typeof pointer.ofType).toBe("function");
      expect(typeof pointer.withProperty).toBe("function");
      expect(typeof pointer.withValue).toBe("function");
      expect(typeof pointer.getClownface).toBe("function");
      expect(typeof pointer.getStore).toBe("function");
      expect(typeof pointer.getEngine).toBe("function");
      expect(typeof pointer.getBaseIRI).toBe("function");
      expect(typeof pointer.withBaseIRI).toBe("function");
      expect(typeof pointer.getStats).toBe("function");
      expect(typeof pointer.isEmpty).toBe("function");
      expect(typeof pointer.size).toBe("function");
    });
  });

  it("should get pointer to specific node", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act
      const nodePointer = pointer.node("person1");
      
      // Assert
      expect(nodePointer).toBeDefined();
      expect(typeof nodePointer.value).toBe("string");
    });
  });

  it("should get pointer to blank node", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act
      const bnodePointer = pointer.blankNode("test");
      
      // Assert
      expect(bnodePointer).toBeDefined();
      expect(typeof bnodePointer.value).toBe("string");
    });
  });

  it("should get pointer to named node", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act
      const namedPointer = pointer.namedNode("http://example.org/test");
      
      // Assert
      expect(namedPointer).toBeDefined();
      expect(namedPointer.value).toBe("http://example.org/test");
    });
  });

  it("should get pointer to literal", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act
      const literalPointer = pointer.literal("test value");
      
      // Assert
      expect(literalPointer).toBeDefined();
      expect(literalPointer.value).toBe("test value");
    });
  });

  it("should get all nodes in graph", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act
      const allNodes = pointer.all();
      
      // Assert
      expect(allNodes).toBeDefined();
      expect(typeof allNodes.length).toBe("number");
    });
  });

  it("should get all subjects in graph", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act - Use the clownface instance directly
      const clownface = pointer.getClownface();
      const subjects = clownface.subjects();
      
      // Assert
      expect(subjects).toBeDefined();
      expect(typeof subjects.length).toBe("number");
    });
  });

  it("should get all predicates in graph", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act - Use the clownface instance directly
      const clownface = pointer.getClownface();
      const predicates = clownface.predicates();
      
      // Assert
      expect(predicates).toBeDefined();
      expect(typeof predicates.length).toBe("number");
    });
  });

  it("should get all objects in graph", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act - Use the clownface instance directly
      const clownface = pointer.getClownface();
      const objects = clownface.objects();
      
      // Assert
      expect(objects).toBeDefined();
      expect(typeof objects.length).toBe("number");
    });
  });

  it("should get nodes of specific type", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act
      const persons = pointer.ofType("foaf:Person");
      
      // Assert
      expect(persons).toBeDefined();
      expect(typeof persons.length).toBe("number");
    });
  });

  it("should get nodes with specific property", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act
      const namedNodes = pointer.withProperty("foaf:name");
      
      // Assert
      expect(namedNodes).toBeDefined();
      expect(typeof namedNodes.length).toBe("number");
    });
  });

  it("should get nodes with specific property value", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act
      const johnNodes = pointer.withValue("foaf:name", "John Doe");
      
      // Assert
      expect(johnNodes).toBeDefined();
      expect(typeof johnNodes.length).toBe("number");
    });
  });

  it("should get underlying Clownface instance", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act
      const clownface = pointer.getClownface();
      
      // Assert
      expect(clownface).toBeDefined();
      expect(typeof clownface.namedNode).toBe("function");
    });
  });

  it("should get underlying store", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act
      const store = pointer.getStore();
      
      // Assert
      expect(store).toBeDefined();
      expect(typeof store.size).toBe("number");
    });
  });

  it("should get underlying engine", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act
      const engine = pointer.getEngine();
      
      // Assert
      expect(engine).toBeDefined();
      expect(typeof engine.getClownface).toBe("function");
    });
  });

  it("should get base IRI", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act
      const baseIRI = pointer.getBaseIRI();
      
      // Assert
      expect(baseIRI).toBe("http://example.org/");
    });
  });

  it("should create pointer with different base IRI", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act
      const newPointer = pointer.withBaseIRI("http://other.org/");
      
      // Assert
      expect(newPointer).toBeDefined();
      expect(newPointer.getBaseIRI()).toBe("http://other.org/");
    });
  });

  it("should get graph statistics", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act
      const stats = pointer.getStats();
      
      // Assert
      expect(stats).toBeDefined();
      expect(typeof stats.quads).toBe("number");
      expect(typeof stats.subjects).toBe("number");
      expect(typeof stats.predicates).toBe("number");
      expect(typeof stats.objects).toBe("number");
    });
  });

  it("should check if graph is empty", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act
      const isEmpty = pointer.isEmpty();
      
      // Assert
      expect(typeof isEmpty).toBe("boolean");
      expect(isEmpty).toBe(false); // We have test data
    });
  });

  it("should get graph size", async () => {
    await runApp(() => {
      // Arrange
      const pointer = usePointer();
      
      // Act
      const size = pointer.size();
      
      // Assert
      expect(typeof size).toBe("number");
      expect(size).toBeGreaterThan(0);
    });
  });

  it("should throw error when context is not initialized", () => {
    // Act & Assert
    expect(() => {
      usePointer();
    }).toThrow();
  });

  it("should work with different base IRIs", async () => {
    const runAppCustom = initStore([], { baseIRI: "http://custom.org/" });
    
    await runAppCustom(() => {
      // Act
      const pointer = usePointer();
      
      // Assert
      expect(pointer.getBaseIRI()).toBe("http://custom.org/");
      expect(typeof pointer.node).toBe("function");
    });
  });
});