/**
 * @file Test Base Infrastructure
 * @module test-base
 *
 * @description
 * Base class for all knowledge engine tests providing common setup,
 * teardown, and utility methods.
 */

import { KnowledgeHookManager } from '../../../packages/knowledge-engine/knowledge-hook-manager.mjs';
import { createStore } from '@unrdf/oxigraph';
import { join } from 'path';
import { tmpdir } from 'os';
import { mkdir, rm } from 'fs/promises';

/**
 * Base class for knowledge engine tests
 */
export class TestBase {
  /**
   *
   */
  constructor() {
    this.tempDir = null;
    this.manager = null;
    this.testStore = null;
    this.setupComplete = false;
  }

  /**
   * Setup test environment
   * @param {Object} options - Setup options
   * @param {string} [options.testName] - Name for the test directory
   * @param {Object} [options.managerConfig] - Configuration for KnowledgeHookManager
   */
  async setup(options = {}) {
    const { testName = 'test', managerConfig = {} } = options;

    // Create temporary directory
    this.tempDir = await this.createTempDir(testName);

    // Create test store
    this.testStore = createStore();

    // Create manager with temp directory as base path
    this.manager = new KnowledgeHookManager({
      basePath: this.tempDir,
      ...managerConfig,
    });

    this.setupComplete = true;
  }

  /**
   * Teardown test environment
   */
  async teardown() {
    if (!this.setupComplete) return;

    try {
      // Clean up temporary directory
      await this.cleanup();
    } catch (error) {
      // Ignore cleanup errors in tests
      console.warn('Test cleanup warning:', error.message);
    } finally {
      this.tempDir = null;
      this.manager = null;
      this.testStore = null;
      this.setupComplete = false;
    }
  }

  /**
   * Create a temporary directory for the test
   * @param {string} testName - Name for the test directory
   * @returns {Promise<string>} Path to the temporary directory
   */
  async createTempDir(testName) {
    const tempDir = join(
      tmpdir(),
      `unrdf-${testName}-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`
    );
    await mkdir(tempDir, { recursive: true });
    return tempDir;
  }

  /**
   * Clean up temporary directory
   */
  async cleanup() {
    if (this.tempDir) {
      await rm(this.tempDir, { recursive: true, force: true });
    }
  }

  /**
   * Get the test manager
   * @returns {KnowledgeHookManager} The test manager
   */
  getManager() {
    if (!this.setupComplete) {
      throw new Error('Test setup not complete. Call setup() first.');
    }
    return this.manager;
  }

  /**
   * Get the test store
   * @returns {Store} The test store
   */
  getStore() {
    if (!this.setupComplete) {
      throw new Error('Test setup not complete. Call setup() first.');
    }
    return this.testStore;
  }

  /**
   * Get the temporary directory path
   * @returns {string} Path to the temporary directory
   */
  getTempDir() {
    if (!this.setupComplete) {
      throw new Error('Test setup not complete. Call setup() first.');
    }
    return this.tempDir;
  }

  /**
   * Create a path within the temporary directory
   * @param {string} filename - Filename to create path for
   * @returns {string} Full path to the file
   */
  getTempPath(filename) {
    return join(this.getTempDir(), filename);
  }
}
