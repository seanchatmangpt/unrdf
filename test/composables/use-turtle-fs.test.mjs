/**
 * @fileoverview Tests for useTurtleFS composable with context architecture
 * 
 * Tests the Turtle file system functionality using the context system
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, expect, it, beforeEach, afterEach } from "vitest";
import { useTurtleFS } from "../../src/composables/use-turtle-fs.mjs";
import { useStore } from "../../src/composables/use-store.mjs";
import { initStore } from "../../src/context/index.mjs";
import { Store, DataFactory } from "n3";
import { writeFile, mkdir, rm } from "node:fs/promises";
import { join } from "node:path";

const { namedNode, literal, quad } = DataFactory;

describe("useTurtleFS with Context", () => {
  let runApp;
  let testDir;

  beforeEach(async () => {
    // Create test directory
    testDir = join(process.cwd(), "test-temp-turtle");
    await mkdir(testDir, { recursive: true });
    
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

  afterEach(async () => {
    // Clean up test directory
    try {
      await rm(testDir, { recursive: true, force: true });
    } catch (error) {
      // Ignore cleanup errors
    }
  });

  it("should create turtleFS interface with context", async () => {
    await runApp(async () => {
      // Act
      const turtleFS = await useTurtleFS(testDir);
      
      // Assert
      expect(typeof turtleFS.loadAll).toBe("function");
      expect(typeof turtleFS.load).toBe("function");
      expect(typeof turtleFS.save).toBe("function");
      expect(typeof turtleFS.list).toBe("function");
      expect(typeof turtleFS.exists).toBe("function");
      expect(typeof turtleFS.stats).toBe("function");
      expect(typeof turtleFS.delete).toBe("function");
      expect(typeof turtleFS.copy).toBe("function");
      expect(typeof turtleFS.move).toBe("function");
      expect(typeof turtleFS.getBaseIRI).toBe("function");
      expect(typeof turtleFS.setBaseIRI).toBe("function");
      expect(turtleFS.engine).toBeDefined();
    });
  });

  it("should get graph directory path", async () => {
    await runApp(async () => {
      // Act
      const turtleFS = await useTurtleFS(testDir);
      
      // Assert
      expect(turtleFS.getGraphDir()).toBe(testDir);
    });
  });

  it("should list files in directory", async () => {
    await runApp(async () => {
      // Arrange
      const turtleFS = await useTurtleFS(testDir);
      
      // Create a test file
      const testFile = join(testDir, "test.ttl");
      await writeFile(testFile, `
        @prefix ex: <http://example.org/> .
        ex:test a ex:Test .
      `);
      
      // Act
      const files = await turtleFS.list();
      
      // Assert
      expect(Array.isArray(files)).toBe(true);
      expect(files).toContain("test.ttl");
    });
  });

  it("should check if file exists", async () => {
    await runApp(async () => {
      // Arrange
      const turtleFS = await useTurtleFS(testDir);
      
      // Create a test file
      const testFile = join(testDir, "test.ttl");
      await writeFile(testFile, `
        @prefix ex: <http://example.org/> .
        ex:test a ex:Test .
      `);
      
      // Act
      const exists = await turtleFS.exists("test.ttl");
      const notExists = await turtleFS.exists("nonexistent.ttl");
      
      // Assert
      expect(exists).toBe(true);
      expect(notExists).toBe(false);
    });
  });

  it("should get file statistics", async () => {
    await runApp(async () => {
      // Arrange
      const turtleFS = await useTurtleFS(testDir);
      
      // Create a test file
      const testFile = join(testDir, "test.ttl");
      const content = `
        @prefix ex: <http://example.org/> .
        ex:test a ex:Test .
      `;
      await writeFile(testFile, content);
      
      // Act
      const stats = await turtleFS.stats("test.ttl");
      
      // Assert
      expect(stats).toBeDefined();
      expect(typeof stats.size).toBe("number");
      expect(typeof stats.mtime).toBe("object");
      expect(typeof stats.ctime).toBe("object");
      expect(typeof stats.isFile).toBe("boolean");
      expect(typeof stats.isDirectory).toBe("boolean");
      expect(stats.isFile).toBe(true);
      expect(stats.isDirectory).toBe(false);
    });
  });

  it("should load a specific file", async () => {
    await runApp(async () => {
      // Arrange
      const turtleFS = await useTurtleFS(testDir);
      
      // Create a test file
      const testFile = join(testDir, "test.ttl");
      await writeFile(testFile, `
        @prefix ex: <http://example.org/> .
        ex:test a ex:Test .
      `);
      
      // Act
      const store = await turtleFS.load("test.ttl");
      
      // Assert
      expect(store).toBeDefined();
      expect(typeof store.size).toBe("number");
      expect(store.size).toBeGreaterThan(0);
    });
  });

  it("should save a store to file", async () => {
    await runApp(async () => {
      // Arrange
      const turtleFS = await useTurtleFS(testDir);
      const store = useStore();
      
      // Act
      await turtleFS.save("test-output.ttl", store.store);
      
      // Assert
      const exists = await turtleFS.exists("test-output.ttl");
      expect(exists).toBe(true);
      
      const stats = await turtleFS.stats("test-output.ttl");
      expect(stats.size).toBeGreaterThan(0);
    });
  });

  it("should copy a file", async () => {
    await runApp(async () => {
      // Arrange
      const turtleFS = await useTurtleFS(testDir);
      
      // Create a test file
      const testFile = join(testDir, "test.ttl");
      await writeFile(testFile, `
        @prefix ex: <http://example.org/> .
        ex:test a ex:Test .
      `);
      
      // Act
      await turtleFS.copy("test.ttl", "test-copy.ttl");
      
      // Assert
      const originalExists = await turtleFS.exists("test.ttl");
      const copyExists = await turtleFS.exists("test-copy.ttl");
      
      expect(originalExists).toBe(true);
      expect(copyExists).toBe(true);
    });
  });

  it("should move a file", async () => {
    await runApp(async () => {
      // Arrange
      const turtleFS = await useTurtleFS(testDir);
      
      // Create a test file
      const testFile = join(testDir, "test.ttl");
      await writeFile(testFile, `
        @prefix ex: <http://example.org/> .
        ex:test a ex:Test .
      `);
      
      // Act
      await turtleFS.move("test.ttl", "test-moved.ttl");
      
      // Assert
      const originalExists = await turtleFS.exists("test.ttl");
      const movedExists = await turtleFS.exists("test-moved.ttl");
      
      expect(originalExists).toBe(false);
      expect(movedExists).toBe(true);
    });
  });

  it("should delete a file", async () => {
    await runApp(async () => {
      // Arrange
      const turtleFS = await useTurtleFS(testDir);
      
      // Create a test file
      const testFile = join(testDir, "test.ttl");
      await writeFile(testFile, `
        @prefix ex: <http://example.org/> .
        ex:test a ex:Test .
      `);
      
      // Act
      await turtleFS.delete("test.ttl");
      
      // Assert
      const exists = await turtleFS.exists("test.ttl");
      expect(exists).toBe(false);
    });
  });

  it("should get and set base IRI", async () => {
    await runApp(async () => {
      // Arrange
      const turtleFS = await useTurtleFS(testDir);
      
      // Act
      const originalBaseIRI = turtleFS.getBaseIRI();
      turtleFS.setBaseIRI("http://custom.org/");
      const newBaseIRI = turtleFS.getBaseIRI();
      
      // Assert
      expect(originalBaseIRI).toBe("http://example.org/");
      expect(newBaseIRI).toBe("http://custom.org/");
    });
  });

  it("should throw error when context is not initialized", async () => {
    // Act & Assert
    await expect(async () => {
      await useTurtleFS(testDir);
    }).rejects.toThrow();
  });

  it("should work with different base IRIs", async () => {
    const runAppCustom = initStore([], { baseIRI: "http://custom.org/" });
    
    await runAppCustom(async () => {
      // Act
      const turtleFS = await useTurtleFS(testDir);
      
      // Assert
      expect(turtleFS.getBaseIRI()).toBe("http://custom.org/");
      expect(turtleFS.engine).toBeDefined();
    });
  });

  it("should handle recursive file listing", async () => {
    await runApp(async () => {
      // Arrange
      const turtleFS = await useTurtleFS(testDir);
      
      // Create nested directory structure
      const subDir = join(testDir, "subdir");
      await mkdir(subDir, { recursive: true });
      
      // Create files in subdirectory
      await writeFile(join(subDir, "test.ttl"), `
        @prefix ex: <http://example.org/> .
        ex:test a ex:Test .
      `);
      
      // Act
      const files = await turtleFS.list({ recursive: true });
      
      // Assert
      expect(Array.isArray(files)).toBe(true);
      expect(files.some(f => f.includes("subdir/test.ttl"))).toBe(true);
    });
  });
});