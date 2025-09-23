import { describe, expect, it, beforeEach } from "vitest";
import { useIRIs } from "../../src/composables/useIRIs.mjs";

describe("useIRIs", () => {
  let iris;

  beforeEach(() => {
    iris = useIRIs({
      uriMaps: {
        'graph://': './graph/',
        'data://': './data/'
      },
      baseIRI: 'http://example.org/'
    });
  });

  it("should create IRIs interface", () => {
    // Assert
    expect(typeof iris.resolve).toBe("function");
    expect(typeof iris.map).toBe("function");
    expect(typeof iris.maps).toBe("function");
    expect(typeof iris.hasMapping).toBe("function");
    expect(typeof iris.getMapping).toBe("function");
    expect(typeof iris.removeMapping).toBe("function");
    expect(typeof iris.clearMappings).toBe("function");
    expect(typeof iris.getBaseIRI).toBe("function");
    expect(typeof iris.setBaseIRI).toBe("function");
    expect(typeof iris.isAbsolute).toBe("function");
    expect(typeof iris.isRelative).toBe("function");
    expect(typeof iris.getScheme).toBe("function");
    expect(typeof iris.getPath).toBe("function");
    expect(typeof iris.size).toBe("function");
  });

  it("should resolve custom URI schemes", () => {
    // Act
    const result = iris.resolve("graph://my-graph.ttl");

    // Assert
    expect(result).toBe("./graph/my-graph.ttl");
  });

  it("should resolve relative IRIs", () => {
    // Act
    const result1 = iris.resolve("#fragment");
    const result2 = iris.resolve("/path");

    // Assert
    expect(result1).toBe("http://example.org/#fragment");
    expect(result2).toBe("http://example.org//path");
  });

  it("should resolve absolute IRIs", () => {
    // Act
    const result = iris.resolve("http://example.org/test");

    // Assert
    expect(result).toBe("http://example.org/test");
  });

  it("should resolve default base IRI", () => {
    // Act
    const result = iris.resolve("test");

    // Assert
    expect(result).toBe("http://example.org/test");
  });

  it("should map prefixes to paths", () => {
    // Act
    iris.map("ex", "./examples/");

    // Assert
    expect(iris.hasMapping("ex")).toBe(true);
    expect(iris.getMapping("ex")).toBe("./examples/");
  });

  it("should get all mappings", () => {
    // Act
    const result = iris.maps();

    // Assert
    expect(result).toHaveProperty("graph://");
    expect(result).toHaveProperty("data://");
    expect(result["graph://"]).toBe("./graph/");
    expect(result["data://"]).toBe("./data/");
  });

  it("should check if prefix is mapped", () => {
    // Act & Assert
    expect(iris.hasMapping("graph")).toBe(true);
    expect(iris.hasMapping("unknown")).toBe(false);
  });

  it("should get mapping for prefix", () => {
    // Act
    const result = iris.getMapping("graph");

    // Assert
    expect(result).toBe("./graph/");
  });

  it("should remove prefix mapping", () => {
    // Act
    const removed = iris.removeMapping("graph");

    // Assert
    expect(removed).toBe(true);
    expect(iris.hasMapping("graph")).toBe(false);
  });

  it("should clear all mappings", () => {
    // Act
    iris.clearMappings();

    // Assert
    expect(iris.size()).toBe(0);
  });

  it("should get base IRI", () => {
    // Act
    const result = iris.getBaseIRI();

    // Assert
    expect(result).toBe("http://example.org/");
  });

  it("should set base IRI", () => {
    // Act
    iris.setBaseIRI("http://my.org/");

    // Assert
    expect(iris.getBaseIRI()).toBe("http://my.org/");
  });

  it("should check if URI is absolute", () => {
    // Act & Assert
    expect(iris.isAbsolute("http://example.org/test")).toBe(true);
    expect(iris.isAbsolute("https://example.org/test")).toBe(true);
    expect(iris.isAbsolute("file:///path/to/file")).toBe(true);
    expect(iris.isAbsolute("graph://test")).toBe(true);
    expect(iris.isAbsolute("relative/path")).toBe(false);
  });

  it("should check if URI is relative", () => {
    // Act & Assert
    expect(iris.isRelative("relative/path")).toBe(true);
    expect(iris.isRelative("#fragment")).toBe(true);
    expect(iris.isRelative("http://example.org/test")).toBe(false);
  });

  it("should get URI scheme", () => {
    // Act & Assert
    expect(iris.getScheme("http://example.org/test")).toBe("http");
    expect(iris.getScheme("https://example.org/test")).toBe("https");
    expect(iris.getScheme("graph://test")).toBe("graph");
    expect(iris.getScheme("no-scheme")).toBeUndefined();
  });

  it("should get URI path", () => {
    // Act & Assert
    expect(iris.getPath("http://example.org/test")).toBe("//example.org/test");
    expect(iris.getPath("graph://test")).toBe("test");
    expect(iris.getPath("no-scheme")).toBe("no-scheme");
  });

  it("should get number of mappings", () => {
    // Act
    const result = iris.size();

    // Assert
    expect(result).toBe(2); // graph:// and data://
  });
});