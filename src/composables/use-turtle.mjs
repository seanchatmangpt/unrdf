/**
 * @fileoverview useTurtle composable - Turtle file I/O operations
 * 
 * This composable provides file system operations for Turtle files.
 * It handles loading, saving, and managing .ttl files with automatic
 * parsing and serialization to/from N3.Store.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { readdir, readFile, writeFile, mkdir, stat } from "node:fs/promises";
import { join, dirname } from "node:path";
import { useStoreContext } from "../context/index.mjs";

/**
 * Create a Turtle file system composable
 * 
 * @param {string} [graphDir='./graph'] - Directory containing Turtle files
 * @param {Object} [options] - Turtle options
 * @param {string} [options.baseIRI] - Base IRI for parsing
 * @param {boolean} [options.autoLoad=true] - Automatically load all .ttl files
 * @param {boolean} [options.validateOnLoad=true] - Validate files on load
 * @returns {Promise<Object>} Turtle file system interface
 * 
 * @example
 * const turtle = await useTurtle('./my-graph');
 * 
 * // Load all .ttl files
 * await turtle.loadAll();
 * 
 * // Save a specific graph
 * await turtle.save('my-graph', store);
 * 
 * // Load a specific file
 * const store = await turtle.load('my-graph');
 */
export async function useTurtle(graphDir = "./graph", options = {}) {
  const {
    baseIRI = "http://example.org/",
    autoLoad = true,
    validateOnLoad = true
  } = options;

  // Get the engine from context
  const storeContext = useStoreContext();
  const engine = storeContext.engine;
  
  // Ensure directory exists
  try {
    await mkdir(graphDir, { recursive: true });
  } catch (error) {
    if (error.code !== "EEXIST") {
      throw error;
    }
  }

  return {
    /**
     * The underlying store instance
     * @type {Object}
     */
    get store() {
      return storeContext;
    },

    /**
     * The graph directory path
     * @type {string}
     */
    get graphDir() {
      return graphDir;
    },

    /**
     * The RDF engine instance
     * @type {RdfEngine}
     */
    get engine() {
      return engine;
    },

    /**
     * Load all .ttl files from the graph directory
     * @param {Object} [options] - Load options
     * @param {boolean} [options.merge=true] - Merge with existing store
     * @param {boolean} [options.validate] - Validate files on load
     * @returns {Promise<Object>} Load result
     */
    async loadAll(options = {}) {
      const { merge = true, validate = validateOnLoad } = options;
      
      try {
        const files = await readdir(graphDir);
        const ttlFiles = files.filter(f => f.endsWith(".ttl"));
        
        if (ttlFiles.length === 0) {
          console.log(`No .ttl files found in ${graphDir}`);
          return { loaded: 0, files: [] };
        }

        const loadedFiles = [];
        
        for (const fileName of ttlFiles) {
          try {
            const filePath = join(graphDir, fileName);
            const content = await readFile(filePath, "utf8");
            
            if (validate) {
              // Basic validation - try to parse
              engine.parseTurtle(content, { baseIRI });
            }
            
            if (!merge) {
              storeContext.store.clear();
            }
            
            const parsedStore = engine.parseTurtle(content, { baseIRI });
            for (const quad of parsedStore) {
              storeContext.store.add(quad);
            }
            
            loadedFiles.push(fileName);
            console.log(`✅ Loaded: ${fileName}`);
          } catch (error) {
            console.warn(`⚠️ Failed to load ${fileName}: ${error.message}`);
          }
        }
        
        console.log(`📁 Loaded ${loadedFiles.length} files from ${graphDir}`);
        return { loaded: loadedFiles.length, files: loadedFiles };
      } catch (error) {
        if (error.code === "ENOENT") {
          console.log(`📁 Graph directory ${graphDir} doesn't exist yet`);
          return { loaded: 0, files: [] };
        }
        throw error;
      }
    },

    /**
     * Load a specific Turtle file
     * @param {string} fileName - Name of the file (without .ttl extension)
     * @param {Object} [options] - Load options
     * @param {boolean} [options.merge=true] - Merge with existing store
     * @param {boolean} [options.validate] - Validate file on load
     * @returns {Promise<Store>} Loaded store
     */
    async load(fileName, options = {}) {
      const { merge = true, validate = validateOnLoad } = options;
      const filePath = join(graphDir, `${fileName}.ttl`);
      
      try {
        const content = await readFile(filePath, "utf8");
        
        if (validate) {
          engine.parseTurtle(content, { baseIRI });
        }
        
        const parsedStore = engine.parseTurtle(content, { baseIRI });
        
        if (!merge) {
          storeContext.clear();
        }
        
        for (const quad of parsedStore) {
          storeContext.store.add(quad);
        }
        
        console.log(`✅ Loaded: ${fileName}.ttl`);
        return parsedStore;
      } catch (error) {
        if (error.code === "ENOENT") {
          throw new Error(`File not found: ${fileName}.ttl`);
        }
        throw error;
      }
    },

    /**
     * Save the current store to a Turtle file
     * @param {string} fileName - Name of the file (without .ttl extension)
     * @param {Object} [options] - Save options
     * @param {Object} [options.prefixes] - Prefix mappings
     * @param {boolean} [options.createBackup=false] - Create backup of existing file
     * @returns {Promise<Object>} Save result
     */
    async save(fileName, options = {}) {
      const { prefixes, createBackup = false } = options;
      const filePath = join(graphDir, `${fileName}.ttl`);
      
      try {
        // Create backup if requested and file exists
        if (createBackup) {
          try {
            await stat(filePath);
            const backupPath = `${filePath}.backup`;
            const content = await readFile(filePath, "utf8");
            await writeFile(backupPath, content, "utf8");
            console.log(`📋 Created backup: ${fileName}.ttl.backup`);
          } catch {
            // File doesn't exist, no backup needed
          }
        }
        
        const turtleContent = await engine.serializeTurtle(storeContext.store, { prefixes });
        await writeFile(filePath, turtleContent, "utf8");
        
        const stats = await stat(filePath);
        console.log(`💾 Saved: ${fileName}.ttl (${stats.size} bytes)`);
        
        return { path: filePath, bytes: stats.size };
      } catch (error) {
        console.error(`❌ Failed to save ${fileName}.ttl:`, error.message);
        throw error;
      }
    },

    /**
     * Save the current store to default.ttl
     * @param {Object} [options] - Save options
     * @returns {Promise<Object>} Save result
     */
    async saveDefault(options = {}) {
      return this.save("default", { ...options, createBackup: true });
    },

    /**
     * Load default.ttl file
     * @param {Object} [options] - Load options
     * @returns {Promise<Store|null>} Loaded store or null if not found
     */
    async loadDefault(options = {}) {
      try {
        return await this.load("default", options);
      } catch (error) {
        if (error.message.includes("File not found")) {
          console.log(`ℹ️ No default.ttl file found in ${graphDir}`);
          return null;
        }
        throw error;
      }
    },

    /**
     * List all .ttl files in the graph directory
     * @returns {Promise<Array<string>>} Array of file names
     */
    async listFiles() {
      try {
        const files = await readdir(graphDir);
        const ttlFiles = files.filter(f => f.endsWith(".ttl"));
        console.log(`📁 Found ${ttlFiles.length} .ttl files in ${graphDir}`);
        return ttlFiles;
      } catch (error) {
        if (error.code === "ENOENT") {
          return [];
        }
        throw error;
      }
    },

    /**
     * Get statistics about the current store
     * @returns {Object} Store statistics
     */
    stats() {
      return storeContext.stats();
    },

    /**
     * Clear the current store
     * @returns {void}
     */
    clear() {
      storeContext.clear();
    },

    /**
     * Parse a Turtle string into a new store
     * @param {string} ttl - Turtle string
     * @param {Object} [options] - Parse options
     * @param {boolean} [options.addToStore=false] - Add parsed data to context store
     * @returns {Promise<Store>} Parsed store
     */
    async parse(ttl, options = {}) {
      const { addToStore = false, ...parseOptions } = options;
      const parsedStore = engine.parseTurtle(ttl, { baseIRI, ...parseOptions });
      
      if (addToStore) {
        for (const quad of parsedStore) {
          storeContext.store.add(quad);
        }
      }
      
      return parsedStore;
    },

    /**
     * Serialize the current store to Turtle
     * @param {Object} [options] - Serialization options
     * @param {Object} [options.prefixes] - Prefix mappings
     * @returns {Promise<string>} Turtle string
     */
    async serialize(options = {}) {
      return await engine.serializeTurtle(storeContext.store, options);
    }
  };
}
