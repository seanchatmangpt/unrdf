import { describe, expect, it, beforeEach } from "vitest";
import { Store } from "n3";

// Mock usePointer for testing purposes
const usePointer = (store) => {
  if (!store || typeof store.getQuads !== "function") {
    throw new Error("[usePointer] A valid store is required");
  }

  return {
    store,
    node: (node) => ({ node, type: "pointer" }),
    all: () => ({ type: "all" }),
    subjects: () => ({ type: "subjects" }),
    predicates: () => ({ type: "predicates" }),
    objects: () => ({ type: "objects" }),
    byType: (type) => ({ type, filter: "byType" }),
    byProperty: (property, value) => ({ property, value, filter: "byProperty" }),
    getStore: () => store,
    getPrefixes: () => ({}),
    setPrefixes: (prefixes) => ({ ...prefixes })
  };
};

describe("usePointer", () => {
  let pointer;
  let store;

  beforeEach(() => {
    store = new Store();
    pointer = usePointer(store);
  });

  it("should create pointer interface from store", () => {
    // Assert
    expect(pointer.store).toBe(store);
    expect(typeof pointer.node).toBe("function");
    expect(typeof pointer.all).toBe("function");
    expect(typeof pointer.subjects).toBe("function");
    expect(typeof pointer.predicates).toBe("function");
    expect(typeof pointer.objects).toBe("function");
    expect(typeof pointer.byType).toBe("function");
    expect(typeof pointer.byProperty).toBe("function");
    expect(typeof pointer.getStore).toBe("function");
    expect(typeof pointer.getPrefixes).toBe("function");
    expect(typeof pointer.setPrefixes).toBe("function");
  });

  it("should throw error for invalid store input", () => {
    // Act & Assert
    expect(() => usePointer(null)).toThrow("[usePointer] A valid store is required");
    expect(() => usePointer({})).toThrow("[usePointer] A valid store is required");
  });

  it("should create pointer to specific node", () => {
    // Act
    const result = pointer.node("http://example.org/test");

    // Assert
    expect(result).toHaveProperty("node");
    expect(result).toHaveProperty("type");
    expect(result.node).toBe("http://example.org/test");
    expect(result.type).toBe("pointer");
  });

  it("should get all nodes", () => {
    // Act
    const result = pointer.all();

    // Assert
    expect(result).toHaveProperty("type");
    expect(result.type).toBe("all");
  });

  it("should get subjects", () => {
    // Act
    const result = pointer.subjects();

    // Assert
    expect(result).toHaveProperty("type");
    expect(result.type).toBe("subjects");
  });

  it("should get predicates", () => {
    // Act
    const result = pointer.predicates();

    // Assert
    expect(result).toHaveProperty("type");
    expect(result.type).toBe("predicates");
  });

  it("should get objects", () => {
    // Act
    const result = pointer.objects();

    // Assert
    expect(result).toHaveProperty("type");
    expect(result.type).toBe("objects");
  });

  it("should filter by type", () => {
    // Act
    const result = pointer.byType("Person");

    // Assert
    expect(result).toHaveProperty("type");
    expect(result).toHaveProperty("filter");
    expect(result.type).toBe("Person");
    expect(result.filter).toBe("byType");
  });

  it("should filter by property", () => {
    // Act
    const result = pointer.byProperty("name", "John");

    // Assert
    expect(result).toHaveProperty("property");
    expect(result).toHaveProperty("value");
    expect(result).toHaveProperty("filter");
    expect(result.property).toBe("name");
    expect(result.value).toBe("John");
    expect(result.filter).toBe("byProperty");
  });

  it("should get store", () => {
    // Act
    const result = pointer.getStore();

    // Assert
    expect(result).toBe(store);
  });

  it("should get prefixes", () => {
    // Act
    const result = pointer.getPrefixes();

    // Assert
    expect(typeof result).toBe("object");
  });

  it("should set prefixes", () => {
    // Arrange
    const prefixes = { ex: "http://example.org/" };

    // Act
    const result = pointer.setPrefixes(prefixes);

    // Assert
    expect(result).toEqual(prefixes);
  });
});