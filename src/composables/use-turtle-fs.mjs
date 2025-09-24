/**
 * @fileoverview useTurtleFS composable - Turtle file system operations
 * 
 * This composable provides file system operations for Turtle files.
 * It handles loading, saving, and managing .ttl files with automatic
 * parsing and serialization to/from N3.Store.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { readdir, readFile, writeFile, mkdir, stat, unlink, copyFile } from "node:fs/promises";
import { join, dirname, basename, extname } from "node:path";
import { useStoreContext } from "../context/index.mjs";
import { useStore } from "./use-store.mjs";

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
 * // Initialize store context first
 * const runApp = initStore();
 * 
 * runApp(async () => {
 *   const turtle = await useTurtleFS('./my-graph');
 *   
 *   // Load all .ttl files into context store
 *   await turtle.loadAll();
 *   
 *   // Save a specific graph
 *   await turtle.save('my-graph', store);
 *   
 *   // Load a specific file
 *   const store = await turtle.load('my-graph');
 * });
 * 
 * @throws {Error} If store context is not initialized
 */
export async function useTurtleFS(graphDir = "./graph", options = {}) {
  let {
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
      throw new Error(`[useTurtleFS] Failed to create directory: ${error.message}`);
    }
  }

  return {
    /**
     * The underlying RDF engine
     * @type {RdfEngine}
     */
    get engine() {
      return engine;
    },

    /**
     * Get the graph directory path
     * @returns {string} Graph directory path
     */
    getGraphDir() {
      return graphDir;
    },

    /**
     * Load all Turtle files from the directory
     * @param {Object} [opts] - Load options
     * @param {boolean} [opts.recursive=false] - Load files recursively
     * @returns {Promise<Store>} Combined store with all loaded data
     */
    async loadAll(opts = {}) {
      const { recursive = false } = opts;
      const storeContext = useStoreContext();
      
      try {
        const files = await this.list({ recursive });
        
        for (const file of files) {
          if (file.endsWith('.ttl')) {
            const fileStore = await this.load(file);
            storeContext.add([...fileStore]);
          }
        }
        
        return storeContext;
      } catch (error) {
        throw new Error(`[useTurtleFS] Failed to load all files: ${error.message}`);
      }
    },

    /**
     * Load a specific Turtle file
     * @param {string} filename - Name of the file to load
     * @param {Object} [opts] - Load options
     * @param {string} [opts.baseIRI] - Override base IRI
     * @returns {Promise<Store>} Store containing the loaded data
     */
    async load(filename, opts = {}) {
      const { baseIRI: fileBaseIRI } = opts;
      const filePath = join(graphDir, filename);
      
      try {
        const content = await readFile(filePath, 'utf-8');
        const store = await engine.parseTurtle(content, fileBaseIRI || baseIRI);
        
        if (validateOnLoad && // Basic validation - check for parse errors
          store.size === 0 && content.trim().length > 0) {
            throw new Error(`[useTurtleFS] File appears to be empty or invalid: ${filename}`);
          }
        
        return store;
      } catch (error) {
        throw new Error(`[useTurtleFS] Failed to load file ${filename}: ${error.message}`);
      }
    },

    /**
     * Save a store to a Turtle file
     * @param {string} filename - Name of the file to save
     * @param {Store} store - Store to save
     * @param {Object} [opts] - Save options
     * @param {Object} [opts.prefixes] - Prefix mappings for serialization
     * @returns {Promise<void>}
     */
    async save(filename, store, opts = {}) {
      if (!store || typeof store.getQuads !== "function") {
        throw new Error("[useTurtleFS] A valid store is required");
      }

      const { prefixes = {} } = opts;
      const filePath = join(graphDir, filename);
      
      try {
        // Ensure directory exists
        await mkdir(dirname(filePath), { recursive: true });
        
        // Serialize to Turtle
        const content = await engine.serializeTurtle(store, { prefixes });
        
        // Write to file
        await writeFile(filePath, content, 'utf-8');
      } catch (error) {
        throw new Error(`[useTurtleFS] Failed to save file ${filename}: ${error.message}`);
      }
    },

    /**
     * List all files in the graph directory
     * @param {Object} [opts] - List options
     * @param {boolean} [opts.recursive=false] - List files recursively
     * @param {string} [opts.extension='.ttl'] - File extension to filter by
     * @returns {Promise<Array>} Array of file paths
     */
    async list(opts = {}) {
      const { recursive = false, extension = '.ttl' } = opts;
      
      try {
        const files = await this._listFiles(graphDir, recursive, extension);
        return files.map(file => file.replace(graphDir + '/', ''));
      } catch (error) {
        throw new Error(`[useTurtleFS] Failed to list files: ${error.message}`);
      }
    },

    /**
     * Check if a file exists
     * @param {string} filename - Name of the file to check
     * @returns {Promise<boolean>} True if file exists
     */
    async exists(filename) {
      try {
        const filePath = join(graphDir, filename);
        await stat(filePath);
        return true;
      } catch {
        return false;
      }
    },

    /**
     * Get file statistics
     * @param {string} filename - Name of the file
     * @returns {Promise<Object>} File statistics
     */
    async stats(filename) {
      try {
        const filePath = join(graphDir, filename);
        const stats = await stat(filePath);
        
        return {
          size: stats.size,
          mtime: stats.mtime,
          ctime: stats.ctime,
          isFile: stats.isFile(),
          isDirectory: stats.isDirectory()
        };
      } catch (error) {
        throw new Error(`[useTurtleFS] Failed to get stats for ${filename}: ${error.message}`);
      }
    },

    /**
     * Delete a file
     * @param {string} filename - Name of the file to delete
     * @returns {Promise<void>}
     */
    async delete(filename) {
      try {
        const filePath = join(graphDir, filename);
        await unlink(filePath);
      } catch (error) {
        throw new Error(`[useTurtleFS] Failed to delete file ${filename}: ${error.message}`);
      }
    },

    /**
     * Copy a file
     * @param {string} source - Source filename
     * @param {string} target - Target filename
     * @returns {Promise<void>}
     */
    async copy(source, target) {
      try {
        const sourcePath = join(graphDir, source);
        const targetPath = join(graphDir, target);
        
        const content = await readFile(sourcePath, 'utf-8');
        await writeFile(targetPath, content, 'utf-8');
      } catch (error) {
        throw new Error(`[useTurtleFS] Failed to copy file ${source} to ${target}: ${error.message}`);
      }
    },

    /**
     * Move a file
     * @param {string} source - Source filename
     * @param {string} target - Target filename
     * @returns {Promise<void>}
     */
    async move(source, target) {
      try {
        await this.copy(source, target);
        await this.delete(source);
      } catch (error) {
        throw new Error(`[useTurtleFS] Failed to move file ${source} to ${target}: ${error.message}`);
      }
    },

    /**
     * Get the base IRI
     * @returns {string} Base IRI
     */
    getBaseIRI() {
      return baseIRI;
    },

    /**
     * Set the base IRI
     * @param {string} newBaseIRI - New base IRI
     * @returns {Object} This composable instance
     */
    setBaseIRI(newBaseIRI) {
      baseIRI = newBaseIRI;
      return this;
    },

    /**
     * List files recursively
     * @param {string} dir - Directory to list
     * @param {boolean} recursive - Whether to recurse
     * @param {string} extension - File extension to filter by
     * @returns {Promise<Array>} Array of file paths
     * @private
     */
    async _listFiles(dir, recursive, extension) {
      const files = [];
      
      try {
        const entries = await readdir(dir, { withFileTypes: true });
        
        for (const entry of entries) {
          const fullPath = join(dir, entry.name);
          
          if (entry.isDirectory() && recursive) {
            const subFiles = await this._listFiles(fullPath, recursive, extension);
            files.push(...subFiles);
          } else if (entry.isFile() && entry.name.endsWith(extension)) {
            files.push(fullPath);
          }
        }
      } catch (error) {
        // Directory might not exist, return empty array
        if (error.code === 'ENOENT') {
          return [];
        }
        throw error;
      }
      
      return files;
    }
  };
}
