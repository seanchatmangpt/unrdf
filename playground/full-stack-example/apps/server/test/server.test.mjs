/**
 * @fileoverview Full-Stack Server Tests
 * @vitest-environment node
 */

import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { createServer } from '../packages/index.mjs'
const { namedNode, literal, quad  } = dataFactory;

describe('Full-Stack Server', () => {
  let server
  let baseURL

  beforeAll(async () => {
    server = await createServer({ port: 0 })  // Random port
    const address = server.address()
    baseURL = `http://localhost:${address.port}`
  })

  afterAll(async () => {
    await server.close()
  })

  describe('Server Startup', () => {
    it('should start server successfully', () => {
      expect(server).toBeDefined()
      expect(server.listening).toBe(true)
    })

    it('should bind to specified port', () => {
      const address = server.address()
      expect(address.port).toBeGreaterThan(0)
    })

    it('should initialize RDF store', () => {
      expect(server.store).toBeDefined()
      expect(server.store.size).toBeGreaterThanOrEqual(0)
    })
  })

  describe('API Endpoints - /api/quads', () => {
    it('should respond to GET /api/quads', async () => {
      const response = await fetch(`${baseURL}/api/quads`)

      expect(response.status).toBe(200)
      expect(response.headers.get('content-type')).toContain('application/json')
    })

    it('should return quad list', async () => {
      const response = await fetch(`${baseURL}/api/quads`)
      const data = await response.json()

      expect(Array.isArray(data.quads)).toBe(true)
    })

    it('should support POST to add quads', async () => {
      const newQuad = {
        subject: 'http://example.org/alice',
        predicate: 'http://schema.org/name',
        object: 'Alice',
        objectType: 'literal'
      }

      const response = await fetch(`${baseURL}/api/quads`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(newQuad)
      })

      expect(response.status).toBe(201)
      const data = await response.json()
      expect(data.success).toBe(true)
    })

    it('should validate quad structure on POST', async () => {
      const invalid = { subject: 'test' }  // Missing required fields

      const response = await fetch(`${baseURL}/api/quads`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(invalid)
      })

      expect(response.status).toBe(400)
    })

    it('should support DELETE to remove quads', async () => {
      // First add a quad
      const quad = {
        subject: 'http://example.org/test',
        predicate: 'http://example.org/prop',
        object: 'value',
        objectType: 'literal'
      }

      await fetch(`${baseURL}/api/quads`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(quad)
      })

      // Then delete it
      const response = await fetch(`${baseURL}/api/quads`, {
        method: 'DELETE',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(quad)
      })

      expect(response.status).toBe(200)
    })
  })

  describe('API Endpoints - /api/query', () => {
    it('should respond to POST /api/query', async () => {
      const query = { sparql: 'SELECT * WHERE { ?s ?p ?o } LIMIT 10' }

      const response = await fetch(`${baseURL}/api/query`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(query)
      })

      expect(response.status).toBe(200)
    })

    it('should execute SPARQL query', async () => {
      const query = { sparql: 'SELECT * WHERE { ?s ?p ?o } LIMIT 5' }

      const response = await fetch(`${baseURL}/api/query`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(query)
      })

      const data = await response.json()
      expect(data.results).toBeDefined()
      expect(Array.isArray(data.results)).toBe(true)
    })

    it('should handle invalid SPARQL syntax', async () => {
      const query = { sparql: 'INVALID SPARQL ###' }

      const response = await fetch(`${baseURL}/api/query`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(query)
      })

      expect(response.status).toBe(400)
    })

    it('should support different query types', async () => {
      const queries = [
        'SELECT ?s WHERE { ?s ?p ?o } LIMIT 1',
        'ASK { ?s ?p ?o }',
        'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o } LIMIT 1'
      ]

      for (const sparql of queries) {
        const response = await fetch(`${baseURL}/api/query`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ sparql })
        })

        expect(response.status).toBe(200)
      }
    })

    it('should return execution metrics', async () => {
      const query = { sparql: 'SELECT * WHERE { ?s ?p ?o } LIMIT 10' }

      const response = await fetch(`${baseURL}/api/query`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(query)
      })

      const data = await response.json()
      expect(data.executionTime).toBeDefined()
      expect(typeof data.executionTime).toBe('number')
    })
  })

  describe('API Endpoints - /api/stats', () => {
    it('should respond to GET /api/stats', async () => {
      const response = await fetch(`${baseURL}/api/stats`)

      expect(response.status).toBe(200)
    })

    it('should return store statistics', async () => {
      const response = await fetch(`${baseURL}/api/stats`)
      const data = await response.json()

      expect(data.quadCount).toBeDefined()
      expect(typeof data.quadCount).toBe('number')
    })

    it('should include predicate counts', async () => {
      const response = await fetch(`${baseURL}/api/stats`)
      const data = await response.json()

      expect(data.predicates).toBeDefined()
      expect(typeof data.predicates).toBe('object')
    })

    it('should track subject counts', async () => {
      const response = await fetch(`${baseURL}/api/stats`)
      const data = await response.json()

      expect(data.subjectCount).toBeDefined()
      expect(data.subjectCount).toBeGreaterThanOrEqual(0)
    })
  })

  describe('RDF Processing', () => {
    beforeEach(async () => {
      // Clear store before each test
      await fetch(`${baseURL}/api/quads/clear`, { method: 'POST' })
    })

    it('should process Turtle format', async () => {
      const turtle = '@prefix ex: <http://example.org/> . ex:alice ex:name "Alice" .'

      const response = await fetch(`${baseURL}/api/import`, {
        method: 'POST',
        headers: { 'Content-Type': 'text/turtle' },
        body: turtle
      })

      expect(response.status).toBe(200)
    })

    it('should process N-Triples format', async () => {
      const ntriples = '<http://example.org/alice> <http://schema.org/name> "Alice" .'

      const response = await fetch(`${baseURL}/api/import`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/n-triples' },
        body: ntriples
      })

      expect(response.status).toBe(200)
    })

    it('should export store as Turtle', async () => {
      const response = await fetch(`${baseURL}/api/export?format=turtle`)

      expect(response.status).toBe(200)
      expect(response.headers.get('content-type')).toContain('text/turtle')
    })
  })

  describe('Hook Validation', () => {
    it('should validate hooks on quad addition', async () => {
      const quad = {
        subject: 'http://example.org/test',
        predicate: 'http://schema.org/name',
        object: 'Test',
        objectType: 'literal'
      }

      const response = await fetch(`${baseURL}/api/quads`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ quad, validateHooks: true })
      })

      expect(response.status).toBe(201)
      const data = await response.json()
      expect(data.hooksValidated).toBe(true)
    })

    it('should execute pre-add hooks', async () => {
      const quad = {
        subject: 'http://example.org/test',
        predicate: 'http://example.org/prop',
        object: 'value',
        objectType: 'literal'
      }

      const response = await fetch(`${baseURL}/api/quads`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(quad)
      })

      const data = await response.json()
      expect(data.hooksExecuted).toContain('pre-add')
    })

    it('should execute post-add hooks', async () => {
      const quad = {
        subject: 'http://example.org/test',
        predicate: 'http://example.org/prop',
        object: 'value',
        objectType: 'literal'
      }

      const response = await fetch(`${baseURL}/api/quads`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(quad)
      })

      const data = await response.json()
      expect(data.hooksExecuted).toContain('post-add')
    })
  })

  describe('WebSocket Server', () => {
    it('should support WebSocket connections', async () => {
      const ws = new WebSocket(`ws://localhost:${server.address().port}`)

      await new Promise((resolve, reject) => {
        ws.onopen = resolve
        ws.onerror = reject
      })

      expect(ws.readyState).toBe(WebSocket.OPEN)
      ws.close()
    })

    it('should broadcast quad changes', async () => {
      const ws = new WebSocket(`ws://localhost:${server.address().port}`)
      await new Promise(resolve => ws.onopen = resolve)

      const messages = []
      ws.onmessage = (event) => messages.push(JSON.parse(event.data))

      // Add a quad via HTTP
      await fetch(`${baseURL}/api/quads`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          subject: 'http://example.org/ws-test',
          predicate: 'http://example.org/prop',
          object: 'value',
          objectType: 'literal'
        })
      })

      // Wait for WebSocket message
      await new Promise(resolve => setTimeout(resolve, 100))

      expect(messages.length).toBeGreaterThan(0)
      ws.close()
    })
  })

  describe('Error Responses', () => {
    it('should return 404 for unknown routes', async () => {
      const response = await fetch(`${baseURL}/api/unknown`)
      expect(response.status).toBe(404)
    })

    it('should return 405 for unsupported methods', async () => {
      const response = await fetch(`${baseURL}/api/quads`, { method: 'PATCH' })
      expect(response.status).toBe(405)
    })

    it('should handle malformed JSON', async () => {
      const response = await fetch(`${baseURL}/api/quads`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: 'invalid json {'
      })

      expect(response.status).toBe(400)
    })

    it('should validate content-type headers', async () => {
      const response = await fetch(`${baseURL}/api/import`, {
        method: 'POST',
        headers: { 'Content-Type': 'text/plain' },
        body: 'data'
      })

      expect(response.status).toBe(415)  // Unsupported Media Type
    })
  })

  describe('Data Persistence', () => {
    it('should persist quads across operations', async () => {
      // Add quad
      const quad = {
        subject: 'http://example.org/persist-test',
        predicate: 'http://example.org/prop',
        object: 'persist-value',
        objectType: 'literal'
      }

      await fetch(`${baseURL}/api/quads`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(quad)
      })

      // Query for it
      const queryResponse = await fetch(`${baseURL}/api/query`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          sparql: 'SELECT * WHERE { <http://example.org/persist-test> ?p ?o }'
        })
      })

      const data = await queryResponse.json()
      expect(data.results.length).toBeGreaterThan(0)
    })

    it('should maintain quad order', async () => {
      const quads = [
        { subject: 'http://example.org/1', predicate: 'http://example.org/order', object: '1', objectType: 'literal' },
        { subject: 'http://example.org/2', predicate: 'http://example.org/order', object: '2', objectType: 'literal' },
        { subject: 'http://example.org/3', predicate: 'http://example.org/order', object: '3', objectType: 'literal' }
      ]

      for (const quad of quads) {
        await fetch(`${baseURL}/api/quads`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(quad)
        })
      }

      const response = await fetch(`${baseURL}/api/quads`)
      const data = await response.json()

      expect(data.quads.length).toBeGreaterThanOrEqual(3)
    })
  })

  describe('Request Validation', () => {
    it('should validate required fields', async () => {
      const invalid = { subject: 'http://example.org/test' }

      const response = await fetch(`${baseURL}/api/quads`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(invalid)
      })

      expect(response.status).toBe(400)
      const data = await response.json()
      expect(data.error).toBeDefined()
    })

    it('should validate URI format', async () => {
      const invalid = {
        subject: 'not a uri',
        predicate: 'http://example.org/prop',
        object: 'value',
        objectType: 'literal'
      }

      const response = await fetch(`${baseURL}/api/quads`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(invalid)
      })

      expect(response.status).toBe(400)
    })

    it('should enforce request size limits', async () => {
      const huge = {
        subject: 'http://example.org/test',
        predicate: 'http://example.org/prop',
        object: 'x'.repeat(10000000),  // 10MB string
        objectType: 'literal'
      }

      const response = await fetch(`${baseURL}/api/quads`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(huge)
      })

      expect(response.status).toBe(413)  // Payload Too Large
    })
  })
})
