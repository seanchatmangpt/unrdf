/**
 * useDelta Composable - Undo/Redo Delta Management
 *
 * Track RDF store changes with undo/redo capabilities.
 * Maintains a history of deltas for state restoration.
 *
 * @module composables/use-delta
 */

import { ref, computed } from 'vue';
import { z } from 'zod';

/**
 * Delta schema - represents a change to the store
 */
const DeltaSchema = z
  .object({
    additions: z.array(z.any()).optional().default([]),
    deletions: z.array(z.any()).optional().default([]),
    timestamp: z.number().optional(),
    metadata: z.record(z.any()).optional(),
  })
  .strict();

/**
 * Options schema for useDelta
 */
const UseDeltaOptionsSchema = z
  .object({
    maxDepth: z.number().optional().default(50),
    trackMetadata: z.boolean().optional().default(false),
  })
  .strict();

/**
 * Track store changes with undo/redo support
 *
 * @param {import('vue').Ref<object>} storeRef - RDF store reference
 * @param {object} [options={}] - Configuration options
 * @param {number} [options.maxDepth=50] - Maximum undo/redo stack depth
 * @param {boolean} [options.trackMetadata=false] - Include metadata in deltas
 * @returns {{
 *   deltas: import('vue').Ref<Array>,
 *   push: (delta: object) => void,
 *   undo: () => void,
 *   redo: () => void,
 *   canUndo: import('vue').ComputedRef<boolean>,
 *   canRedo: import('vue').ComputedRef<boolean>,
 *   clear: () => void
 * }} Delta management state and methods
 * @example
 * const { push, undo, redo, canUndo } = useDelta(store)
 * push({ additions: [quad1], deletions: [quad2] })
 * if (canUndo.value) undo()
 */
export function useDelta(storeRef, options = {}) {
  const opts = UseDeltaOptionsSchema.parse(options);

  // Undo/redo stacks
  const undoStack = ref([]);
  const redoStack = ref([]);

  // All deltas (read-only history)
  const deltas = computed(() => [...undoStack.value]);

  // Computed capabilities
  const canUndo = computed(() => undoStack.value.length > 0);
  const canRedo = computed(() => redoStack.value.length > 0);

  /**
   * Push a new delta onto the undo stack
   *
   * @param {object} delta - Delta object with additions/deletions
   */
  function push(delta) {
    const validated = DeltaSchema.parse(delta);

    // Add timestamp if tracking metadata
    if (opts.trackMetadata && !validated.timestamp) {
      validated.timestamp = Date.now();
    }

    // Push to undo stack
    undoStack.value.push(validated);

    // Enforce max depth
    if (undoStack.value.length > opts.maxDepth) {
      undoStack.value.shift();
    }

    // Clear redo stack on new change
    redoStack.value = [];
  }

  /**
   * Undo the last delta
   */
  function undo() {
    if (!canUndo.value) return;

    const delta = undoStack.value.pop();
    const store = storeRef.value;

    if (!store) return;

    try {
      // Reverse the delta: remove additions, restore deletions
      for (const quad of delta.additions || []) {
        store.delete(quad);
      }
      for (const quad of delta.deletions || []) {
        store.add(quad);
      }

      // Move to redo stack
      redoStack.value.push(delta);
    } catch (err) {
      // Re-add to undo stack on failure
      undoStack.value.push(delta);
      throw err;
    }
  }

  /**
   * Redo the last undone delta
   */
  function redo() {
    if (!canRedo.value) return;

    const delta = redoStack.value.pop();
    const store = storeRef.value;

    if (!store) return;

    try {
      // Replay the delta: add additions, remove deletions
      for (const quad of delta.additions || []) {
        store.add(quad);
      }
      for (const quad of delta.deletions || []) {
        store.delete(quad);
      }

      // Move back to undo stack
      undoStack.value.push(delta);
    } catch (err) {
      // Re-add to redo stack on failure
      redoStack.value.push(delta);
      throw err;
    }
  }

  /**
   * Clear all deltas and reset stacks
   */
  function clear() {
    undoStack.value = [];
    redoStack.value = [];
  }

  return {
    deltas,
    push,
    undo,
    redo,
    canUndo,
    canRedo,
    clear,
  };
}
