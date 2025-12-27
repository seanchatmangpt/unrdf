/**
 * @file app/composables/useKnowledgeHooks.mjs
 * @description Knowledge Hooks composable for managing CRUD operations
 */

import { ref } from 'vue'
import {
  KnowledgeHookSchema,
  CreateHookSchema,
  UpdateHookSchema,
  HookListResponseSchema,
  HookEvaluationResultSchema
} from '~/schemas/hooks.mjs'

/**
 * Knowledge Hooks composable - Internal implementation
 * @returns {{
 *   hooks: import('vue').Ref<Array<Object>>,
 *   currentHook: import('vue').Ref<Object|null>,
 *   loading: import('vue').Ref<boolean>,
 *   error: import('vue').Ref<Error|null>,
 *   fetchHooks: Function,
 *   fetchHook: Function,
 *   createHook: Function,
 *   updateHook: Function,
 *   deleteHook: Function,
 *   evaluateHook: Function,
 *   total: import('vue').Ref<number>
 * }}
 */
const _useKnowledgeHooks = () => {
  /** @type {import('vue').Ref<Array<Object>>} */
  const hooks = ref([])

  /** @type {import('vue').Ref<Object|null>} */
  const currentHook = ref(null)

  /** @type {import('vue').Ref<number>} */
  const total = ref(0)

  /** @type {import('vue').Ref<boolean>} */
  const loading = ref(false)

  /** @type {import('vue').Ref<Error|null>} */
  const error = ref(null)

  /**
   * Fetch all knowledge hooks
   * @param {Object} [options] - Query options
   * @param {number} [options.limit] - Number of hooks to fetch
   * @param {number} [options.offset] - Offset for pagination
   * @param {string} [options.phase] - Filter by phase (pre|post|invariant)
   * @param {boolean} [options.disabled] - Filter by disabled status
   * @returns {Promise<void>}
   */
  async function fetchHooks(options = {}) {
    loading.value = true
    error.value = null

    try {
      const data = await $fetch('/api/hooks/list', {
        query: options
      })

      // Validate response with Zod
      const validated = HookListResponseSchema.parse(data)
      hooks.value = validated.hooks
      total.value = validated.total
    } catch (err) {
      error.value = err
      console.error('[useKnowledgeHooks] Failed to fetch hooks:', err)
      hooks.value = []
      total.value = 0
    } finally {
      loading.value = false
    }
  }

  /**
   * Fetch a single knowledge hook by ID
   * @param {string} id - Hook ID
   * @returns {Promise<Object|null>}
   */
  async function fetchHook(id) {
    loading.value = true
    error.value = null

    try {
      const data = await $fetch(`/api/hooks/${id}`)

      // Validate response with Zod
      const validated = KnowledgeHookSchema.parse(data)
      currentHook.value = validated
      return validated
    } catch (err) {
      error.value = err
      console.error(`[useKnowledgeHooks] Failed to fetch hook ${id}:`, err)
      currentHook.value = null
      return null
    } finally {
      loading.value = false
    }
  }

  /**
   * Create a new knowledge hook
   * @param {Object} hookData - Hook data to create
   * @returns {Promise<Object|null>}
   */
  async function createHook(hookData) {
    loading.value = true
    error.value = null

    try {
      // Validate input with Zod
      const validatedInput = CreateHookSchema.parse(hookData)

      const data = await $fetch('/api/hooks/register', {
        method: 'POST',
        body: validatedInput
      })

      // Validate response
      const validated = KnowledgeHookSchema.parse(data)

      // Add to local cache
      hooks.value = [...hooks.value, validated]
      total.value += 1

      return validated
    } catch (err) {
      error.value = err
      console.error('[useKnowledgeHooks] Failed to create hook:', err)
      return null
    } finally {
      loading.value = false
    }
  }

  /**
   * Update an existing knowledge hook
   * @param {string} id - Hook ID
   * @param {Object} updates - Partial hook data to update
   * @returns {Promise<Object|null>}
   */
  async function updateHook(id, updates) {
    loading.value = true
    error.value = null

    try {
      // Validate input with Zod
      const validatedInput = UpdateHookSchema.parse({ id, ...updates })

      const data = await $fetch(`/api/hooks/${id}`, {
        method: 'PUT',
        body: validatedInput
      })

      // Validate response
      const validated = KnowledgeHookSchema.parse(data)

      // Update local cache
      const index = hooks.value.findIndex(h => h.id === id)
      if (index !== -1) {
        hooks.value[index] = validated
      }

      if (currentHook.value?.id === id) {
        currentHook.value = validated
      }

      return validated
    } catch (err) {
      error.value = err
      console.error(`[useKnowledgeHooks] Failed to update hook ${id}:`, err)
      return null
    } finally {
      loading.value = false
    }
  }

  /**
   * Delete a knowledge hook
   * @param {string} id - Hook ID to delete
   * @returns {Promise<boolean>}
   */
  async function deleteHook(id) {
    loading.value = true
    error.value = null

    try {
      await $fetch(`/api/hooks/${id}`, {
        method: 'DELETE'
      })

      // Remove from local cache
      hooks.value = hooks.value.filter(h => h.id !== id)
      total.value -= 1

      if (currentHook.value?.id === id) {
        currentHook.value = null
      }

      return true
    } catch (err) {
      error.value = err
      console.error(`[useKnowledgeHooks] Failed to delete hook ${id}:`, err)
      return false
    } finally {
      loading.value = false
    }
  }

  /**
   * Evaluate a knowledge hook
   * @param {string} id - Hook ID to evaluate
   * @param {Object} [context] - Optional context data for evaluation
   * @returns {Promise<Object|null>}
   */
  async function evaluateHook(id, context = {}) {
    loading.value = true
    error.value = null

    try {
      const data = await $fetch('/api/hooks/evaluate', {
        method: 'POST',
        body: {
          hookId: id,
          context
        }
      })

      // Validate response
      const validated = HookEvaluationResultSchema.parse(data)
      return validated
    } catch (err) {
      error.value = err
      console.error(`[useKnowledgeHooks] Failed to evaluate hook ${id}:`, err)
      return null
    } finally {
      loading.value = false
    }
  }

  return {
    hooks,
    currentHook,
    total,
    loading,
    error,
    fetchHooks,
    fetchHook,
    createHook,
    updateHook,
    deleteHook,
    evaluateHook
  }
}

/**
 * Knowledge Hooks composable with shared state
 * @type {() => {
 *   hooks: import('vue').Ref<Array<Object>>,
 *   currentHook: import('vue').Ref<Object|null>,
 *   loading: import('vue').Ref<boolean>,
 *   error: import('vue').Ref<Error|null>,
 *   fetchHooks: Function,
 *   fetchHook: Function,
 *   createHook: Function,
 *   updateHook: Function,
 *   deleteHook: Function,
 *   evaluateHook: Function,
 *   total: import('vue').Ref<number>
 * }}
 */
export const useKnowledgeHooks = createSharedComposable(_useKnowledgeHooks)
