/**
 * @file useKnowledgeHooks Composable Tests
 * @description London School TDD tests for core hooks management logic
 * Focus on testing collaborations and contracts with external services
 */

import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { useKnowledgeHooks } from '~/composables/useKnowledgeHooks.mjs'

// Mock $fetch and useFetch for API interactions
vi.mock('#app', () => ({
  useRuntimeConfig: vi.fn(() => ({
    public: {
      apiBase: 'http://localhost:3000/api'
    }
  })),
  useFetch: vi.fn(),
  useNuxtApp: vi.fn(() => ({
    $fetch: vi.fn()
  }))
}))

describe('useKnowledgeHooks Composable', () => {
  let mockFetch

  beforeEach(() => {
    vi.clearAllMocks()
    mockFetch = vi.fn()
    global.$fetch = mockFetch
  })

  afterEach(() => {
    vi.restoreAllMocks()
  })

  describe('Hook Fetching Behavior', () => {
    it('should fetch hooks from API successfully', async () => {
      const mockHooksResponse = {
        hooks: [
          {
            id: 'hook-1',
            name: 'Validation Hook',
            phase: 'pre',
            predicates: [
              { subject: 'user', predicate: 'hasRole', object: 'admin' }
            ],
            enabled: true
          },
          {
            id: 'hook-2',
            name: 'Audit Hook',
            phase: 'post',
            predicates: [],
            enabled: true
          }
        ]
      }

      mockFetch.mockResolvedValueOnce(mockHooksResponse)

      const { hooks, fetchHooks, loading, error } = useKnowledgeHooks()

      // Verify initial state
      expect(hooks.value).toEqual([])
      expect(loading.value).toBe(false)
      expect(error.value).toBe(null)

      // Execute fetch
      await fetchHooks()

      // Verify API call was made with correct endpoint
      expect(mockFetch).toHaveBeenCalledWith('/api/knowledge/hooks', {
        method: 'GET'
      })

      // Verify hooks were populated
      expect(hooks.value).toEqual(mockHooksResponse.hooks)
      expect(loading.value).toBe(false)
      expect(error.value).toBe(null)
    })

    it('should handle API errors gracefully', async () => {
      const apiError = new Error('Failed to fetch hooks')
      mockFetch.mockRejectedValueOnce(apiError)

      const { hooks, fetchHooks, loading, error } = useKnowledgeHooks()

      await fetchHooks()

      // Verify error state
      expect(error.value).toBe(apiError.message)
      expect(hooks.value).toEqual([])
      expect(loading.value).toBe(false)
    })

    it('should handle network timeout gracefully', async () => {
      const timeoutError = new Error('Network timeout')
      mockFetch.mockRejectedValueOnce(timeoutError)

      const { fetchHooks, error } = useKnowledgeHooks()

      await fetchHooks()

      expect(error.value).toBeTruthy()
    })

    it('should set loading state during fetch operation', async () => {
      let resolvePromise
      const fetchPromise = new Promise((resolve) => {
        resolvePromise = resolve
      })

      mockFetch.mockReturnValueOnce(fetchPromise)

      const { fetchHooks, loading } = useKnowledgeHooks()

      const fetchOperation = fetchHooks()

      // Verify loading is true during fetch
      expect(loading.value).toBe(true)

      resolvePromise({ hooks: [] })
      await fetchOperation

      // Verify loading is false after fetch
      expect(loading.value).toBe(false)
    })
  })

  describe('Hook Creation Behavior', () => {
    it('should create hook with Zod validation', async () => {
      const newHook = {
        name: 'New Validation Hook',
        phase: 'pre',
        predicates: [
          { subject: 'user', predicate: 'hasRole', object: 'editor' }
        ],
        enabled: true
      }

      const createdHook = {
        id: 'hook-new',
        ...newHook
      }

      mockFetch.mockResolvedValueOnce({ hook: createdHook })

      const { createHook, hooks, error } = useKnowledgeHooks()

      await createHook(newHook)

      // Verify API call with correct data
      expect(mockFetch).toHaveBeenCalledWith('/api/knowledge/hooks', {
        method: 'POST',
        body: newHook
      })

      // Verify hook was added to local state
      expect(hooks.value).toContainEqual(createdHook)
      expect(error.value).toBe(null)
    })

    it('should reject invalid hook data with Zod validation', async () => {
      const invalidHook = {
        name: '', // Empty name should fail validation
        phase: 'invalid-phase', // Invalid phase
        predicates: 'not-an-array' // Should be array
      }

      const { createHook, error } = useKnowledgeHooks()

      await createHook(invalidHook)

      // Verify validation error
      expect(error.value).toBeTruthy()
      expect(mockFetch).not.toHaveBeenCalled()
    })

    it('should handle API errors during hook creation', async () => {
      const newHook = {
        name: 'Test Hook',
        phase: 'pre',
        predicates: [],
        enabled: true
      }

      const apiError = new Error('Failed to create hook')
      mockFetch.mockRejectedValueOnce(apiError)

      const { createHook, error } = useKnowledgeHooks()

      await createHook(newHook)

      expect(error.value).toBe(apiError.message)
    })

    it('should validate required fields before API call', async () => {
      const incompleteHook = {
        name: 'Incomplete Hook'
        // Missing phase, predicates
      }

      const { createHook, error } = useKnowledgeHooks()

      await createHook(incompleteHook)

      // Verify validation prevented API call
      expect(error.value).toBeTruthy()
      expect(mockFetch).not.toHaveBeenCalled()
    })
  })

  describe('Hook Update Behavior', () => {
    it('should update existing hook successfully', async () => {
      const existingHook = {
        id: 'hook-1',
        name: 'Original Name',
        phase: 'pre',
        predicates: [],
        enabled: true
      }

      const updatedHook = {
        ...existingHook,
        name: 'Updated Name',
        enabled: false
      }

      mockFetch
        .mockResolvedValueOnce({ hooks: [existingHook] })
        .mockResolvedValueOnce({ hook: updatedHook })

      const { hooks, fetchHooks, updateHook } = useKnowledgeHooks()

      await fetchHooks()
      await updateHook('hook-1', { name: 'Updated Name', enabled: false })

      // Verify API call
      expect(mockFetch).toHaveBeenCalledWith('/api/knowledge/hooks/hook-1', {
        method: 'PATCH',
        body: { name: 'Updated Name', enabled: false }
      })

      // Verify local state was updated
      expect(hooks.value.find(h => h.id === 'hook-1').name).toBe('Updated Name')
    })

    it('should handle update errors gracefully', async () => {
      const apiError = new Error('Update failed')
      mockFetch.mockRejectedValueOnce(apiError)

      const { updateHook, error } = useKnowledgeHooks()

      await updateHook('hook-1', { name: 'New Name' })

      expect(error.value).toBe(apiError.message)
    })
  })

  describe('Hook Deletion Behavior', () => {
    it('should delete hook and remove from local state', async () => {
      const existingHooks = [
        { id: 'hook-1', name: 'Hook 1', phase: 'pre', predicates: [], enabled: true },
        { id: 'hook-2', name: 'Hook 2', phase: 'post', predicates: [], enabled: true }
      ]

      mockFetch
        .mockResolvedValueOnce({ hooks: existingHooks })
        .mockResolvedValueOnce({ success: true })

      const { hooks, fetchHooks, deleteHook } = useKnowledgeHooks()

      await fetchHooks()
      await deleteHook('hook-1')

      // Verify API call
      expect(mockFetch).toHaveBeenCalledWith('/api/knowledge/hooks/hook-1', {
        method: 'DELETE'
      })

      // Verify hook was removed from local state
      expect(hooks.value).toHaveLength(1)
      expect(hooks.value.find(h => h.id === 'hook-1')).toBeUndefined()
    })

    it('should handle deletion errors gracefully', async () => {
      const apiError = new Error('Deletion failed')
      mockFetch.mockRejectedValueOnce(apiError)

      const { deleteHook, error } = useKnowledgeHooks()

      await deleteHook('hook-1')

      expect(error.value).toBe(apiError.message)
    })
  })

  describe('Hook Evaluation Behavior', () => {
    it('should evaluate hook with context and return results', async () => {
      const evaluationContext = {
        user: { id: 'user-1', role: 'admin' },
        resource: { id: 'doc-1', type: 'document' }
      }

      const evaluationResult = {
        hookId: 'hook-1',
        matched: true,
        predicates: [
          { matched: true, subject: 'user', predicate: 'hasRole', object: 'admin' }
        ]
      }

      mockFetch.mockResolvedValueOnce({ result: evaluationResult })

      const { evaluateHook } = useKnowledgeHooks()

      const result = await evaluateHook('hook-1', evaluationContext)

      // Verify API call with context
      expect(mockFetch).toHaveBeenCalledWith('/api/knowledge/hooks/hook-1/evaluate', {
        method: 'POST',
        body: { context: evaluationContext }
      })

      // Verify result
      expect(result.matched).toBe(true)
      expect(result.predicates[0].matched).toBe(true)
    })

    it('should handle evaluation errors gracefully', async () => {
      const apiError = new Error('Evaluation failed')
      mockFetch.mockRejectedValueOnce(apiError)

      const { evaluateHook, error } = useKnowledgeHooks()

      await evaluateHook('hook-1', {})

      expect(error.value).toBe(apiError.message)
    })
  })

  describe('Collaboration Patterns', () => {
    it('should coordinate fetch and create operations', async () => {
      const newHook = {
        name: 'Coordination Test',
        phase: 'pre',
        predicates: [],
        enabled: true
      }

      mockFetch
        .mockResolvedValueOnce({ hooks: [] })
        .mockResolvedValueOnce({ hook: { id: 'new-hook', ...newHook } })

      const { hooks, fetchHooks, createHook } = useKnowledgeHooks()

      await fetchHooks()
      expect(hooks.value).toHaveLength(0)

      await createHook(newHook)
      expect(hooks.value).toHaveLength(1)
    })

    it('should maintain state consistency across operations', async () => {
      const initialHooks = [
        { id: 'hook-1', name: 'Hook 1', phase: 'pre', predicates: [], enabled: true }
      ]

      mockFetch
        .mockResolvedValueOnce({ hooks: initialHooks })
        .mockResolvedValueOnce({ hook: { ...initialHooks[0], enabled: false } })
        .mockResolvedValueOnce({ success: true })

      const { hooks, fetchHooks, updateHook, deleteHook } = useKnowledgeHooks()

      await fetchHooks()
      expect(hooks.value).toHaveLength(1)

      await updateHook('hook-1', { enabled: false })
      expect(hooks.value[0].enabled).toBe(false)

      await deleteHook('hook-1')
      expect(hooks.value).toHaveLength(0)
    })
  })

  describe('Error Recovery', () => {
    it('should clear error state after successful operation', async () => {
      mockFetch
        .mockRejectedValueOnce(new Error('First call failed'))
        .mockResolvedValueOnce({ hooks: [] })

      const { fetchHooks, error } = useKnowledgeHooks()

      await fetchHooks()
      expect(error.value).toBeTruthy()

      await fetchHooks()
      expect(error.value).toBe(null)
    })

    it('should allow retry after network failure', async () => {
      const networkError = new Error('Network unavailable')
      mockFetch
        .mockRejectedValueOnce(networkError)
        .mockResolvedValueOnce({ hooks: [] })

      const { fetchHooks, error } = useKnowledgeHooks()

      await fetchHooks()
      expect(error.value).toBeTruthy()

      // Retry should succeed
      await fetchHooks()
      expect(error.value).toBe(null)
    })
  })
})
