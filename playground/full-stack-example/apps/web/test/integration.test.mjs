/**
 * @fileoverview Full-Stack Web Integration Tests
 * @vitest-environment jsdom
 */

import { describe, it, expect, beforeEach, vi } from 'vitest'
import { mount } from '@vue/test-utils'
import { nextTick } from 'vue'
import App from '../src/App.vue'
import { useStore } from '../src/composables/useStore.mjs'
import { apiClient } from '../src/api/client.mjs'

describe('Full-Stack Web Integration', () => {
  describe('Vue App Initialization', () => {
    it('should initialize Vue app', () => {
      const wrapper = mount(App)
      expect(wrapper.exists()).toBe(true)
    })

    it('should mount main component', () => {
      const wrapper = mount(App)
      expect(wrapper.find('#app').exists()).toBe(true)
    })

    it('should initialize with empty state', () => {
      const wrapper = mount(App)
      const vm = wrapper.vm

      expect(vm.quads).toEqual([])
      expect(vm.loading).toBe(false)
    })
  })

  describe('Component Rendering', () => {
    let wrapper

    beforeEach(() => {
      wrapper = mount(App)
    })

    it('should render header', () => {
      expect(wrapper.find('header').exists()).toBe(true)
      expect(wrapper.find('h1').text()).toBe('UNRDF Full-Stack Demo')
    })

    it('should render quad list', () => {
      expect(wrapper.find('.quad-list').exists()).toBe(true)
    })

    it('should render query form', () => {
      expect(wrapper.find('.query-form').exists()).toBe(true)
      expect(wrapper.find('textarea').exists()).toBe(true)
    })

    it('should render action buttons', () => {
      const buttons = wrapper.findAll('button')
      expect(buttons.length).toBeGreaterThan(0)
    })

    it('should show loading state', async () => {
      wrapper.vm.loading = true
      await nextTick()

      expect(wrapper.find('.loading').exists()).toBe(true)
    })

    it('should display error messages', async () => {
      wrapper.vm.error = 'Test error'
      await nextTick()

      expect(wrapper.find('.error').text()).toContain('Test error')
    })
  })

  describe('Store Operations', () => {
    let store

    beforeEach(() => {
      store = useStore()
    })

    it('should add quad to store', async () => {
      const quad = {
        subject: 'http://example.org/alice',
        predicate: 'http://schema.org/name',
        object: 'Alice'
      }

      await store.addQuad(quad)
      await nextTick()

      expect(store.quads.value.length).toBe(1)
    })

    it('should remove quad from store', async () => {
      const quad = {
        subject: 'http://example.org/test',
        predicate: 'http://example.org/prop',
        object: 'value'
      }

      await store.addQuad(quad)
      await nextTick()
      expect(store.quads.value.length).toBe(1)

      await store.removeQuad(quad)
      await nextTick()
      expect(store.quads.value.length).toBe(0)
    })

    it('should query store with SPARQL', async () => {
      const quad = {
        subject: 'http://example.org/alice',
        predicate: 'http://schema.org/name',
        object: 'Alice'
      }

      await store.addQuad(quad)

      const results = await store.query('SELECT * WHERE { ?s ?p ?o }')
      expect(results.length).toBeGreaterThan(0)
    })

    it('should clear all quads', async () => {
      await store.addQuad({
        subject: 'http://example.org/s1',
        predicate: 'http://example.org/p',
        object: 'o1'
      })
      await store.addQuad({
        subject: 'http://example.org/s2',
        predicate: 'http://example.org/p',
        object: 'o2'
      })

      await store.clear()
      await nextTick()

      expect(store.quads.value.length).toBe(0)
    })
  })

  describe('API Client Integration', () => {
    beforeEach(() => {
      vi.clearAllMocks()
    })

    it('should fetch quads from server', async () => {
      const mockFetch = vi.spyOn(global, 'fetch').mockResolvedValue({
        ok: true,
        json: async () => ({ quads: [] })
      })

      const quads = await apiClient.getQuads()

      expect(mockFetch).toHaveBeenCalledWith(
        expect.stringContaining('/api/quads'),
        expect.any(Object)
      )
      expect(Array.isArray(quads)).toBe(true)
    })

    it('should send quad to server', async () => {
      const quad = {
        subject: 'http://example.org/test',
        predicate: 'http://example.org/prop',
        object: 'value'
      }

      const mockFetch = vi.spyOn(global, 'fetch').mockResolvedValue({
        ok: true,
        json: async () => ({ success: true })
      })

      await apiClient.addQuad(quad)

      expect(mockFetch).toHaveBeenCalledWith(
        expect.stringContaining('/api/quads'),
        expect.objectContaining({
          method: 'POST',
          body: expect.stringContaining(quad.subject)
        })
      )
    })

    it('should execute SPARQL query on server', async () => {
      const query = 'SELECT * WHERE { ?s ?p ?o } LIMIT 10'

      const mockFetch = vi.spyOn(global, 'fetch').mockResolvedValue({
        ok: true,
        json: async () => ({ results: [] })
      })

      const results = await apiClient.query(query)

      expect(mockFetch).toHaveBeenCalledWith(
        expect.stringContaining('/api/query'),
        expect.objectContaining({
          method: 'POST',
          body: expect.stringContaining(query)
        })
      )
    })

    it('should handle network errors', async () => {
      vi.spyOn(global, 'fetch').mockRejectedValue(new Error('Network error'))

      await expect(apiClient.getQuads()).rejects.toThrow('Network error')
    })

    it('should handle HTTP errors', async () => {
      vi.spyOn(global, 'fetch').mockResolvedValue({
        ok: false,
        status: 500,
        statusText: 'Internal Server Error'
      })

      await expect(apiClient.getQuads()).rejects.toThrow()
    })
  })

  describe('WebSocket Client', () => {
    it('should connect to WebSocket server', async () => {
      const wrapper = mount(App)

      // Mock WebSocket
      const mockWS = {
        readyState: WebSocket.OPEN,
        close: vi.fn()
      }

      wrapper.vm.ws = mockWS
      await nextTick()

      expect(wrapper.vm.wsConnected).toBe(true)
    })

    it('should handle WebSocket messages', async () => {
      const wrapper = mount(App)

      const message = {
        type: 'quad-added',
        quad: {
          subject: 'http://example.org/test',
          predicate: 'http://example.org/prop',
          object: 'value'
        }
      }

      wrapper.vm.handleWSMessage({ data: JSON.stringify(message) })
      await nextTick()

      expect(wrapper.vm.quads.length).toBeGreaterThan(0)
    })

    it('should reconnect on disconnect', async () => {
      const wrapper = mount(App)
      const reconnect = vi.spyOn(wrapper.vm, 'connectWebSocket')

      wrapper.vm.handleWSClose()
      await nextTick()

      // Should attempt reconnection
      expect(reconnect).toHaveBeenCalled()
    })
  })

  describe('State Management', () => {
    it('should sync state with server', async () => {
      const wrapper = mount(App)

      vi.spyOn(apiClient, 'getQuads').mockResolvedValue([
        {
          subject: 'http://example.org/alice',
          predicate: 'http://schema.org/name',
          object: 'Alice'
        }
      ])

      await wrapper.vm.syncWithServer()
      await nextTick()

      expect(wrapper.vm.quads.length).toBe(1)
    })

    it('should handle concurrent updates', async () => {
      const wrapper = mount(App)

      const updates = []
      for (let i = 0; i < 5; i++) {
        updates.push(
          wrapper.vm.addQuad({
            subject: `http://example.org/s${i}`,
            predicate: 'http://example.org/p',
            object: `value${i}`
          })
        )
      }

      await Promise.all(updates)
      await nextTick()

      expect(wrapper.vm.quads.length).toBe(5)
    })

    it('should maintain consistency', async () => {
      const wrapper = mount(App)
      const quad = {
        subject: 'http://example.org/test',
        predicate: 'http://example.org/prop',
        object: 'value'
      }

      await wrapper.vm.addQuad(quad)
      await nextTick()

      const found = wrapper.vm.quads.find(q =>
        q.subject === quad.subject &&
        q.predicate === quad.predicate &&
        q.object === quad.object
      )

      expect(found).toBeDefined()
    })
  })

  describe('Error Handling', () => {
    it('should display API errors', async () => {
      const wrapper = mount(App)

      vi.spyOn(apiClient, 'getQuads').mockRejectedValue(new Error('API Error'))

      await wrapper.vm.loadQuads()
      await nextTick()

      expect(wrapper.vm.error).toContain('API Error')
    })

    it('should handle validation errors', async () => {
      const wrapper = mount(App)

      const invalid = { subject: 'invalid' }

      await expect(wrapper.vm.addQuad(invalid)).rejects.toThrow()
    })

    it('should recover from errors', async () => {
      const wrapper = mount(App)

      wrapper.vm.error = 'Test error'
      await nextTick()
      expect(wrapper.vm.error).toBeTruthy()

      wrapper.vm.clearError()
      await nextTick()

      expect(wrapper.vm.error).toBe(null)
    })
  })

  describe('UI Interactions', () => {
    let wrapper

    beforeEach(() => {
      wrapper = mount(App)
    })

    it('should handle form submission', async () => {
      const form = wrapper.find('.query-form')
      const textarea = wrapper.find('textarea')

      await textarea.setValue('SELECT * WHERE { ?s ?p ?o }')
      await form.trigger('submit')

      expect(wrapper.vm.queryExecuted).toBe(true)
    })

    it('should handle button clicks', async () => {
      const button = wrapper.find('button.add-quad')

      await button.trigger('click')

      expect(wrapper.vm.showAddForm).toBe(true)
    })

    it('should validate user input', async () => {
      const input = wrapper.find('input[name="subject"]')

      await input.setValue('invalid uri')
      await input.trigger('blur')

      expect(wrapper.find('.validation-error').exists()).toBe(true)
    })

    it('should update on user interaction', async () => {
      const select = wrapper.find('select.predicate')

      await select.setValue('http://schema.org/name')

      expect(wrapper.vm.selectedPredicate).toBe('http://schema.org/name')
    })
  })
})
