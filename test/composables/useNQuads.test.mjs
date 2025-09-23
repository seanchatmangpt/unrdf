import { describe, expect, it, beforeEach } from "vitest";
import { useNQuads } from "../../src/composables/useNQuads.mjs";
import { useStore } from "../../src/composables/useStore.mjs";
import { Store } from "n3";

describe("useNQuads", () => {
  let nquads;
  let store;

  beforeEach(() => {
    nquads = useNQuads();
    store = useStore();
  });

  it("should create N-Quads interface", () => {
    // Assert
    expect(typeof nquads.parse).toBe("function");
    expect(typeof nquads.serialize).toBe("function");
    expect(typeof nquads.parseFile).toBe("function");
    expect(typeof nquads.writeFile).toBe("function");
    expect(typeof nquads.validate).toBe("function");
    expect(typeof nquads.getBaseIRI).toBe("function");
    expect(typeof nquads.setBaseIRI).toBe("function");
    expect(typeof nquads.isNQuads).toBe("function");
    expect(typeof nquads.getStats).toBe("function");
  });

  it("should parse N-Quads string", async () => {
    // Arrange
    const nquadsString = "<http://example.org/s> <http://example.org/p> <http://example.org/o> .";

    // Act
    const result = await nquads.parse(nquadsString);

    // Assert
    expect(result).toBeInstanceOf(Store);
  });

  it("should parse N-Quads with custom base IRI", async () => {
    // Arrange
    const nquadsString = "<http://example.org/s> <http://example.org/p> <http://example.org/o> .";

    // Act
    const result = await nquads.parse(nquadsString, { baseIRI: 'http://my.org/' });

    // Assert
    expect(result).toBeInstanceOf(Store);
  });

  it("should write store to N-Quads", async () => {
    // Act
    const result = await nquads.serialize(store);

    // Assert
    expect(typeof result).toBe("string");
    // Note: Empty store will produce empty string
  });

  it("should write store with pretty printing", async () => {
    // Act
    const result = await nquads.serialize(store, { deterministic: true });

    // Assert
    expect(typeof result).toBe("string");
  });

  it("should parse N-Quads from file", async () => {
    // Arrange
    const fs = await import("fs");
    const testContent = "<http://example.org/s> <http://example.org/p> <http://example.org/o> .\n";
    fs.writeFileSync("./test.nq", testContent);

    try {
      // Act
      const result = await nquads.parseFile('./test.nq');

      // Assert
      expect(result).toBeInstanceOf(Store);
      expect(result.size).toBe(1);
    } finally {
      // Cleanup
      fs.unlinkSync("./test.nq");
    }
  });

  it("should parse file with custom base IRI", async () => {
    // Arrange
    const fs = await import("fs");
    const testContent = "<http://example.org/s> <http://example.org/p> <http://example.org/o> .\n";
    fs.writeFileSync("./test.nq", testContent);
    const customNQuads = useNQuads({ baseIRI: "http://my.org/" });

    try {
      // Act
      const result = await customNQuads.parseFile('./test.nq');

      // Assert
      expect(result).toBeInstanceOf(Store);
      expect(result.size).toBe(1);
    } finally {
      // Cleanup
      fs.unlinkSync("./test.nq");
    }
  });

  it("should write N-Quads to file", async () => {
    // Act
    const result = await nquads.writeFile(store.store, './test.nq');

    // Assert
    expect(result).toEqual({
      path: './test.nq',
      bytes: 0,
      quads: 0
    });
  });

  it("should write file with pretty printing", async () => {
    // Act
    const result = await nquads.writeFile(store.store, './test.nq', { pretty: true });

    // Assert
    expect(result).toEqual({
      path: './test.nq',
      bytes: 0,
      quads: 0
    });
  });

  it("should validate N-Quads string", async () => {
    // Arrange
    const validNQuads = "<http://example.org/s> <http://example.org/p> <http://example.org/o> .";
    const invalidNQuads = "invalid n-quads content";

    // Act
    const validResult = await nquads.validate(validNQuads);
    const invalidResult = await nquads.validate(invalidNQuads);

    // Assert
    expect(validResult).toEqual({
      valid: true,
      error: null,
      quads: 1
    });
    expect(invalidResult).toEqual({
      valid: false,
      error: expect.any(String),
      quads: 0
    });
  });

  it("should get base IRI", () => {
    // Act
    const result = nquads.getBaseIRI();

    // Assert
    expect(result).toBe('http://example.org/');
  });

  it("should set base IRI", () => {
    // Act
    nquads.setBaseIRI('http://my.org/');

    // Assert
    expect(nquads.getBaseIRI()).toBe('http://my.org/');
  });

  it("should detect N-Quads content", () => {
    // Act & Assert
    expect(nquads.isNQuads("<http://example.org/s> <http://example.org/p> <http://example.org/o> .")).toBe(true);
    expect(nquads.isNQuads("This is not N-Quads")).toBe(false);
  });

  it("should get N-Quads statistics", () => {
    // Arrange
    const nquadsString = "<http://example.org/s> <http://example.org/p> <http://example.org/o> .";

    // Act
    const result = nquads.getStats(nquadsString);

    // Assert
    expect(result).toEqual({
      quads: 1,
      subjects: 1,
      predicates: 1,
      objects: 1,
      graphs: 1,
      lines: 1,
      quadLines: 1,
      emptyLines: 0
    });
  });
});