/**
 * @fileoverview Shared in-memory state for data API (Nitro runtime)
 */

// Singleton data source registry
export const dataStore = globalThis.__unrdf_dataStore || new Map()

// Persist on global to ensure singleton across hot reloads in dev
globalThis.__unrdf_dataStore = dataStore


