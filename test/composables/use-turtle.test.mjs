/**
 * @fileoverview Tests for useTurtle composable
 * Tests Turtle file I/O operations
 */

import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { useTurtle } from "../../src/composables/use-turtle.mjs";
import { initStore } from "../../src/context/index.mjs";
import { readFile, writeFile, mkdir, rm, readdir } from "node:fs/promises";
import { join } from "node:path";

describe("useTurtle composable", () => {
  let runApp;
  let testDir;

  beforeEach(async () => {
    runApp = initStore();
    testDir = join(process.cwd(), "test-turtle-temp");
    await mkdir(testDir, { recursive: true });
  });

  afterEach(async () => {
    try {
      await rm(testDir, { recursive: true, force: true });
    } catch (error) {
      // Ignore cleanup errors
    }
  });

  it("provides turtle file operations interface", async () => {
    await runApp(async () => {
      const turtle = await useTurtle(testDir);
      
      expect(turtle).toBeDefined();
      expect(typeof turtle.loadAll).toBe("function");
      expect(typeof turtle.load).toBe("function");
      expect(typeof turtle.save).toBe("function");
      expect(typeof turtle.listFiles).toBe("function");
      expect(typeof turtle.parse).toBe("function");
      expect(typeof turtle.serialize).toBe("function");
    });
  });

  it("creates directory if it doesn't exist", async () => {
    await runApp(async () => {
      const newDir = join(testDir, "new-subdir");
      const turtle = await useTurtle(newDir);
      
      expect(turtle.graphDir).toBe(newDir);
    });
  });

  it("saves and loads turtle files", async () => {
    await runApp(async () => {
      const turtle = await useTurtle(testDir);
      
      // Create some test data
      const testTurtle = `
        @prefix ex: <http://example.org/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        
        ex:alice rdf:type ex:Person ;
                 ex:name "Alice" .
      `;
      
      // Parse and save
      const store = await turtle.parse(testTurtle, { addToStore: true });
      await turtle.save("test-file");
      
      // Verify file was created
      const savedContent = await readFile(join(testDir, "test-file.ttl"), "utf8");
      expect(savedContent).toContain("Alice");
      
      // Load the file
      const loadedStore = await turtle.load("test-file", { merge: false });
      expect(loadedStore.size).toBeGreaterThan(0);
    });
  });

  it("handles empty directory gracefully", async () => {
    await runApp(async () => {
      const turtle = await useTurtle(testDir);
      
      const result = await turtle.loadAll();
      expect(result.loaded).toBe(0);
      expect(result.files).toEqual([]);
    });
  });

  it("lists files in directory", async () => {
    await runApp(async () => {
      const turtle = await useTurtle(testDir);
      
      // Create a test file
      await writeFile(join(testDir, "test.ttl"), "@prefix ex: <http://example.org/> . ex:test ex:value \"test\" .", "utf8");
      
      const files = await turtle.listFiles();
      expect(files).toContain("test.ttl");
    });
  });

  it("parses turtle strings", async () => {
    await runApp(async () => {
      const turtle = await useTurtle(testDir);
      
      const testTurtle = `
        @prefix ex: <http://example.org/> .
        ex:alice ex:name "Alice" .
      `;
      
      const store = await turtle.parse(testTurtle);
      expect(store.size).toBeGreaterThan(0);
    });
  });

  it("serializes store to turtle", async () => {
    await runApp(async () => {
      const turtle = await useTurtle(testDir);
      
      // Create some test data
      const testTurtle = `
        @prefix ex: <http://example.org/> .
        ex:alice ex:name "Alice" .
      `;
      
      const store = await turtle.parse(testTurtle, { addToStore: true });
      const serialized = await turtle.serialize();
      
      expect(serialized).toContain("Alice");
    });
  });

  it("handles save with backup option", async () => {
    await runApp(async () => {
      const turtle = await useTurtle(testDir);
      
      // Create initial file
      const testTurtle = `@prefix ex: <http://example.org/> . ex:alice ex:name "Alice" .`;
      const store = await turtle.parse(testTurtle, { addToStore: true });
      await turtle.save("backup-test");
      
      // Save again with backup
      await turtle.save("backup-test", { createBackup: true });
      
      // Check if backup was created
      const files = await turtle.listFiles();
      const allFiles = await readdir(testDir);
      expect(allFiles).toContain("backup-test.ttl.backup");
    });
  });

  it("handles load default file", async () => {
    await runApp(async () => {
      const turtle = await useTurtle(testDir);
      
      // Create default.ttl
      const testTurtle = `@prefix ex: <http://example.org/> . ex:default ex:value "default" .`;
      const store = await turtle.parse(testTurtle, { addToStore: true });
      await turtle.save("default");
      
      // Load default
      const loadedStore = await turtle.loadDefault();
      expect(loadedStore).toBeDefined();
      expect(loadedStore.size).toBeGreaterThan(0);
    });
  });

  it("returns null for missing default file", async () => {
    await runApp(async () => {
      const turtle = await useTurtle(testDir);
      
      const loadedStore = await turtle.loadDefault();
      expect(loadedStore).toBeNull();
    });
  });

  it("provides access to engine and store", async () => {
    await runApp(async () => {
      const turtle = await useTurtle(testDir);
      
      expect(turtle.engine).toBeDefined();
      expect(turtle.store).toBeDefined();
    });
  });
});
