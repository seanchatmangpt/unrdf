import { describe, expect, it, beforeEach } from "vitest";
import { Store } from "n3";
import { useTurtleFS } from "../../src/composables/useTurtleFS.mjs";

describe("useTurtleFS", () => {
  let turtle;

  beforeEach(async () => {
    turtle = await useTurtleFS("./test-graph", {
      baseIRI: 'http://example.org/',
      autoLoad: false,
      validateOnLoad: false
    });
  });

  it("should create TurtleFS interface", () => {
    // Assert
    expect(typeof turtle.getGraphDir).toBe("function");
    expect(typeof turtle.loadAll).toBe("function");
    expect(typeof turtle.load).toBe("function");
    expect(typeof turtle.save).toBe("function");
    expect(typeof turtle.list).toBe("function");
    expect(typeof turtle.exists).toBe("function");
    expect(typeof turtle.stats).toBe("function");
    expect(typeof turtle.delete).toBe("function");
    expect(typeof turtle.copy).toBe("function");
    expect(typeof turtle.move).toBe("function");
    expect(typeof turtle.getBaseIRI).toBe("function");
    expect(typeof turtle.setBaseIRI).toBe("function");
  });

  it("should get graph directory", () => {
    // Act
    const result = turtle.getGraphDir();

    // Assert
    expect(result).toBe("./test-graph");
  });

  it("should load all files", async () => {
    // Act
    const result = await turtle.loadAll();

    // Assert
    expect(result).toBeInstanceOf(Store);
  });

  it("should load all files recursively", async () => {
    // Act
    const result = await turtle.loadAll({ recursive: true });

    // Assert
    expect(result).toBeInstanceOf(Store);
  });

  it("should load a specific file", async () => {
    // Act
    const result = await turtle.load("test-file");

    // Assert
    expect(result).toBeInstanceOf(Store);
  });

  it("should load a file with custom base IRI", async () => {
    // Act
    const result = await turtle.load("test-file", { baseIRI: "http://custom.org/" });

    // Assert
    expect(result).toBeInstanceOf(Store);
  });

  it("should save a store to file", async () => {
    // Arrange
    const store = new Store();

    // Act
    const result = await turtle.save("test-file", store);

    // Assert
    expect(result).toHaveProperty("path");
    expect(result).toHaveProperty("size");
  });

  it("should save a store with prefixes", async () => {
    // Arrange
    const store = new Store();
    const prefixes = { ex: "http://example.org/" };

    // Act
    const result = await turtle.save("test-file", store, { prefixes });

    // Assert
    expect(result).toHaveProperty("path");
    expect(result).toHaveProperty("size");
  });

  it("should list files", async () => {
    // Act
    const result = await turtle.list();

    // Assert
    expect(Array.isArray(result)).toBe(true);
  });

  it("should list files recursively", async () => {
    // Act
    const result = await turtle.list({ recursive: true });

    // Assert
    expect(Array.isArray(result)).toBe(true);
  });

  it("should list files with custom extension", async () => {
    // Act
    const result = await turtle.list({ extension: ".rdf" });

    // Assert
    expect(Array.isArray(result)).toBe(true);
  });

  it("should check if file exists", async () => {
    // Act
    const result = await turtle.exists("non-existent-file");

    // Assert
    expect(typeof result).toBe("boolean");
  });

  it("should get file statistics", async () => {
    // Act & Assert (should not throw for non-existent file)
    await expect(turtle.stats("non-existent-file")).resolves.toBeDefined();
  });

  it("should delete a file", async () => {
    // Act
    const result = await turtle.delete("non-existent-file");

    // Assert
    expect(typeof result).toBe("boolean");
  });

  it("should copy a file", async () => {
    // Act
    const result = await turtle.copy("source", "destination");

    // Assert
    expect(result).toHaveProperty("source");
    expect(result).toHaveProperty("destination");
  });

  it("should move a file", async () => {
    // Act
    const result = await turtle.move("source", "destination");

    // Assert
    expect(result).toHaveProperty("source");
    expect(result).toHaveProperty("destination");
  });

  it("should get base IRI", () => {
    // Act
    const result = turtle.getBaseIRI();

    // Assert
    expect(result).toBe("http://example.org/");
  });

  it("should set base IRI", () => {
    // Act
    turtle.setBaseIRI("http://my.org/");

    // Assert
    expect(turtle.getBaseIRI()).toBe("http://my.org/");
  });
});