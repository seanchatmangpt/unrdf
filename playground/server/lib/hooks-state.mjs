/**
 * @fileoverview Shared in-memory state for hooks API (Nitro runtime)
 */

// Singleton registries for hooks and their evaluation results
export const hookRegistry = globalThis.__unrdf_hookRegistry || new Map()
export const hookResults = globalThis.__unrdf_hookResults || new Map()

// Persist on global to ensure singletons across hot reloads in dev
globalThis.__unrdf_hookRegistry = hookRegistry
globalThis.__unrdf_hookResults = hookResults


