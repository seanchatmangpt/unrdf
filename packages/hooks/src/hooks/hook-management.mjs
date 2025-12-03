/**
 * @file Hook registry and management utilities for UNRDF Knowledge Hooks.
 * @module hooks/hook-management
 */

import { z } from 'zod';
import { HookSchema } from './define-hook.mjs';

/**
 * @typedef {import('./define-hook.mjs').Hook} Hook
 * @typedef {import('./define-hook.mjs').HookTrigger} HookTrigger
 */

/**
 * Hook registry for managing registered hooks.
 * @typedef {Object} HookRegistry
 * @property {Map<string, Hook>} hooks - Map of hook name to hook
 * @property {Map<HookTrigger, Set<string>>} triggerIndex - Index of trigger to hook names
 */

/* ========================================================================= */
/* Zod Schemas                                                              */
/* ========================================================================= */

export const HookRegistrySchema = z.object({
  hooks: z.instanceof(Map),
  triggerIndex: z.instanceof(Map),
});

/* ========================================================================= */
/* Public API                                                               */
/* ========================================================================= */

/**
 * Create a new hook registry.
 *
 * @returns {HookRegistry} - New empty registry
 *
 * @example
 * const registry = createHookRegistry();
 * registerHook(registry, myHook);
 */
export function createHookRegistry() {
  return {
    hooks: new Map(),
    triggerIndex: new Map(),
  };
}

/**
 * Register a hook in the registry.
 *
 * @param {HookRegistry} registry - Hook registry
 * @param {Hook} hook - Hook to register
 * @throws {Error} - If hook with same name already exists
 *
 * @example
 * registerHook(registry, defineHook({
 *   name: 'validate-iri',
 *   trigger: 'before-add',
 *   validate: (quad) => quad.subject.termType === 'NamedNode'
 * }));
 */
export function registerHook(registry, hook) {
  const validatedRegistry = HookRegistrySchema.parse(registry);
  const validatedHook = HookSchema.parse(hook);

  if (validatedRegistry.hooks.has(validatedHook.name)) {
    throw new Error(`Hook already registered: ${validatedHook.name}`);
  }

  validatedRegistry.hooks.set(validatedHook.name, validatedHook);

  if (!validatedRegistry.triggerIndex.has(validatedHook.trigger)) {
    validatedRegistry.triggerIndex.set(validatedHook.trigger, new Set());
  }
  validatedRegistry.triggerIndex.get(validatedHook.trigger).add(validatedHook.name);
}

/**
 * Unregister a hook from the registry.
 *
 * @param {HookRegistry} registry - Hook registry
 * @param {string} name - Hook name to remove
 * @returns {boolean} - True if hook was removed, false if not found
 *
 * @example
 * unregisterHook(registry, 'validate-iri');
 */
export function unregisterHook(registry, name) {
  const validatedRegistry = HookRegistrySchema.parse(registry);

  const hook = validatedRegistry.hooks.get(name);
  if (!hook) {
    return false;
  }

  validatedRegistry.hooks.delete(name);

  const triggerSet = validatedRegistry.triggerIndex.get(hook.trigger);
  if (triggerSet) {
    triggerSet.delete(name);
    if (triggerSet.size === 0) {
      validatedRegistry.triggerIndex.delete(hook.trigger);
    }
  }

  return true;
}

/**
 * Get a hook by name.
 *
 * @param {HookRegistry} registry - Hook registry
 * @param {string} name - Hook name
 * @returns {Hook | undefined} - Hook if found, undefined otherwise
 */
export function getHook(registry, name) {
  const validatedRegistry = HookRegistrySchema.parse(registry);
  return validatedRegistry.hooks.get(name);
}

/**
 * List all registered hooks.
 *
 * @param {HookRegistry} registry - Hook registry
 * @returns {Hook[]} - Array of all registered hooks
 */
export function listHooks(registry) {
  const validatedRegistry = HookRegistrySchema.parse(registry);
  return Array.from(validatedRegistry.hooks.values());
}

/**
 * Get hooks by trigger type.
 *
 * @param {HookRegistry} registry - Hook registry
 * @param {HookTrigger} trigger - Trigger type
 * @returns {Hook[]} - Array of hooks for this trigger
 */
export function getHooksByTrigger(registry, trigger) {
  const validatedRegistry = HookRegistrySchema.parse(registry);

  const hookNames = validatedRegistry.triggerIndex.get(trigger);
  if (!hookNames) {
    return [];
  }

  const hooks = [];
  for (const name of hookNames) {
    const hook = validatedRegistry.hooks.get(name);
    if (hook) {
      hooks.push(hook);
    }
  }
  return hooks;
}

/**
 * Check if a hook is registered.
 *
 * @param {HookRegistry} registry - Hook registry
 * @param {string} name - Hook name
 * @returns {boolean} - True if hook is registered
 */
export function hasHook(registry, name) {
  const validatedRegistry = HookRegistrySchema.parse(registry);
  return validatedRegistry.hooks.has(name);
}

/**
 * Clear all hooks from registry.
 *
 * @param {HookRegistry} registry - Hook registry
 */
export function clearHooks(registry) {
  const validatedRegistry = HookRegistrySchema.parse(registry);
  validatedRegistry.hooks.clear();
  validatedRegistry.triggerIndex.clear();
}

/**
 * Get registry statistics.
 *
 * @param {HookRegistry} registry - Hook registry
 * @returns {Object} - Registry statistics
 * @property {number} totalHooks - Total number of registered hooks
 * @property {Record<string, number>} byTrigger - Count of hooks by trigger type
 */
export function getRegistryStats(registry) {
  const validatedRegistry = HookRegistrySchema.parse(registry);

  const byTrigger = {};
  for (const [trigger, names] of validatedRegistry.triggerIndex) {
    byTrigger[trigger] = names.size;
  }

  return {
    totalHooks: validatedRegistry.hooks.size,
    byTrigger,
  };
}
