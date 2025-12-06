/**
 * @file CLI Store Instance - Simple local store for CLI commands
 * @module cli/utils/store-instance
 *
 * 80/20 Implementation: Local store without sidecar integration
 * Provides immediate value while keeping implementation simple
 */

import { createStore } from '@unrdf/core';
import { OxigraphStore } from '@unrdf/oxigraph';

/**
 * Global store instance (singleton pattern)
 * Simple in-memory store for CLI operations
 */
let storeInstance = null;

/**
 * Get or create store instance
 * @returns {Object} UnrdfStore instance
 */
export function getStore() {
  if (!storeInstance) {
    storeInstance = createStore();
  }
  return storeInstance;
}

/**
 * Reset store instance (for testing)
 */
export function resetStore() {
  storeInstance = null;
}

/**
 * Get data factory for creating RDF terms
 * @returns {Object} Data factory methods
 */
export function getDataFactory() {
  return OxigraphStore.getDataFactory();
}

export default {
  getStore,
  resetStore,
  getDataFactory
};
