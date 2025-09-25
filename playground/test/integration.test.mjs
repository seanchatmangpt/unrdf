#!/usr/bin/env node

/**
 * @fileoverview UNRDF Playground Integration Tests - 80/20 Rule Coverage
 *
 * Tests the most critical functionality that provides 80% of the value:
 * - Hook lifecycle (create, evaluate, manage)
 * - Data source operations
 * - Runtime management
 * - Error handling and edge cases
 */

import { describe, it, beforeEach, expect } from 'vitest'
import { beforeAll, afterAll } from 'vitest'
import { createServer } from 'node:http'
import { initStore } from '../../src/context/index.mjs'
import { useTurtle } from '../../src/composables/use-turtle.mjs'
import { useGraph } from '../../src/composables/use-graph.mjs'

// Test configuration
const BASE_URL = 'http://localhost:3000'
const TEST_TIMEOUT = 10000

// Sample test data
const sampleData = {
  services: `@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:web-service a ex:Service ;
  ex:name "Web Service" ;
  ex:errorRate "0.02"^^xsd:decimal ;
  ex:latency "800"^^xsd:integer ;
  ex:requests "5000"^^xsd:integer .

ex:api-service a ex:Service ;
  ex:name "API Service" ;
  ex:errorRate "0.15"^^xsd:decimal ;
  ex:latency "1200"^^xsd:integer ;
  ex:requests "2000"^^xsd:integer .

ex:db-service a ex:Service ;
  ex:name "Database Service" ;
  ex:errorRate "0.01"^^xsd:decimal ;
  ex:latency "200"^^xsd:integer ;
  ex:requests "1500"^^xsd:integer .`,

  healthCheckHook: {
    id: 'ex:HealthCheckHook',
    name: 'Health Check Hook',
    description: 'Monitors service health metrics',
    select: `SELECT ?service ?errorRate ?latency
WHERE {
  ?service a <http://example.org/Service> ;
           <http://example.org/errorRate> ?errorRate ;
           <http://example.org/latency> ?latency .
}`,
    predicates: [
      {
        kind: 'THRESHOLD',
        spec: {
          var: 'errorRate',
          op: '>',
          value: 0.1
        }
      },
      {
        kind: 'THRESHOLD',
        spec: {
          var: 'latency',
          op: '>',
          value: 1000
        }
      }
    ],
    combine: 'OR'
  }
}

// Utility functions
async function makeRequest(url, options = {}) {
  const { method = 'GET', headers = {}, body } = options

  const fetchOptions = {
    method,
    headers: {
      'Content-Type': 'application/json',
      ...headers
    }
  }

  if (body) {
    fetchOptions.body = JSON.stringify(body)
  }

  const response = await fetch(url, fetchOptions)
  const data = await response.json()

  return {
    status: response.status,
    statusText: response.statusText,
    data,
    ok: response.ok
  }
}

async function waitForServer(timeout = 5000) {
  const startTime = Date.now()

  while (Date.now() - startTime < timeout) {
    try {
      const response = await fetch(`${BASE_URL}/api/runtime/status`)
      if (response.ok) return true
    } catch (error) {
      // Server not ready, wait and retry
      await new Promise(resolve => setTimeout(resolve, 100))
    }
  }

  throw new Error('Server did not become ready within timeout')
}

// Test suite
describe('UNRDF Playground Integration Tests - 80/20 Coverage', () => {
  let server
  let testDataSourceId = 'test-services-' + Date.now()

  beforeAll(async () => {
    console.log('🚀 Starting integration tests...')

    // Ensure server is running
    try {
      await waitForServer()
      console.log('✅ Server is ready')
    } catch (error) {
      throw new Error('Server must be running for integration tests. Run: pnpm server')
    }
  })

  describe('Runtime Management', () => {
    it('should return healthy runtime status', async () => {
      const { data, ok } = await makeRequest(`${BASE_URL}/api/runtime/status`)

      expect(ok).toBe(true)
      expect(data.status).toBe('running')
      expect(data).toHaveProperty('uptime')
      expect(data).toHaveProperty('memory')
      expect(data.hooks).toHaveProperty('total')
      expect(data.data).toHaveProperty('total')
    })

    it('should respond to health check command', async () => {
      const { data, ok } = await makeRequest(`${BASE_URL}/api/runtime/status`, {
        method: 'POST',
        body: { command: 'health-check' }
      })

      expect(ok).toBe(true)
      expect(data.success).toBe(true)
      expect(data.status).toBe('healthy')
    })

    it('should clear all hook results', async () => {
      const { data, ok } = await makeRequest(`${BASE_URL}/api/runtime/status`, {
        method: 'POST',
        body: { command: 'clear-results' }
      })

      expect(ok).toBe(true)
      expect(data.success).toBe(true)
      expect(data.message).toContain('cleared')
    })
  })

  describe('Data Source Management', () => {
    it('should create a new data source', async () => {
      const { data, ok } = await makeRequest(`${BASE_URL}/api/data`, {
        method: 'POST',
        body: {
          id: testDataSourceId,
          name: 'Test Services Data',
          content: sampleData.services,
          format: 'Turtle'
        }
      })

      expect(ok).toBe(true)
      expect(data.success).toBe(true)
      expect(data.dataSource.id).toBe(testDataSourceId)
      expect(data.dataSource.format).toBe('Turtle')
      expect(data.dataSource.size).toBeGreaterThan(0)
    })

    it('should list all data sources', async () => {
      const { data, ok } = await makeRequest(`${BASE_URL}/api/data`)

      expect(ok).toBe(true)
      expect(data).toHaveProperty('dataSources')
      expect(data).toHaveProperty('total')
      expect(Array.isArray(data.dataSources)).toBe(true)

      // Should include our test data source
      const testDataSource = data.dataSources.find(ds => ds.id === testDataSourceId)
      expect(testDataSource).toBeDefined()
      expect(testDataSource.name).toBe('Test Services Data')
    })

    it('should query data source with SELECT', async () => {
      const { data, ok } = await makeRequest(`${BASE_URL}/api/data/${testDataSourceId}/query`, {
        method: 'POST',
        body: {
          query: `SELECT ?service ?errorRate WHERE {
            ?service <http://example.org/errorRate> ?errorRate
          }`
        }
      })

      expect(ok).toBe(true)
      expect(data.success).toBe(true)
      expect(Array.isArray(data.result)).toBe(true)
      expect(data.result.length).toBeGreaterThan(0)

      // Verify structure of first result
      const firstResult = data.result[0]
      expect(firstResult.service).toHaveProperty('value')
      expect(firstResult.errorRate).toHaveProperty('value')
    })

    it('should query data source with ASK', async () => {
      const { data, ok } = await makeRequest(`${BASE_URL}/api/data/${testDataSourceId}/query`, {
        method: 'POST',
        body: {
          query: `ASK WHERE {
            ?service a <http://example.org/Service>
          }`
        }
      })

      expect(ok).toBe(true)
      expect(data.success).toBe(true)
      expect(typeof data.result).toBe('boolean')
      expect(data.result).toBe(true) // Should be true since we have services
    })
  })

  describe('Hook Management', () => {
    let testHookId = 'ex:TestHealthHook-' + Date.now()

    it('should create a new hook', async () => {
      const { data, ok } = await makeRequest(`${BASE_URL}/api/hooks`, {
        method: 'POST',
        body: {
          id: testHookId,
          name: 'Test Health Hook',
          description: 'Integration test hook',
          select: `SELECT ?service ?errorRate WHERE {
            ?service a <http://example.org/Service> ;
                     <http://example.org/errorRate> ?errorRate .
          }`,
          predicates: [
            {
              kind: 'THRESHOLD',
              spec: {
                var: 'errorRate',
                op: '>',
                value: 0.05
              }
            }
          ],
          combine: 'AND'
        }
      })

      expect(ok).toBe(true)
      expect(data.success).toBe(true)
      expect(data.hook.id).toBe(testHookId)
      expect(data.hook.predicates).toBe(1)
    })

    it('should list all hooks', async () => {
      const { data, ok } = await makeRequest(`${BASE_URL}/api/hooks`)

      expect(ok).toBe(true)
      expect(data).toHaveProperty('hooks')
      expect(Array.isArray(data.hooks)).toBe(true)
      expect(data).toHaveProperty('total')

      // Should include our test hook
      const testHook = data.hooks.find(hook => hook.id === testHookId)
      expect(testHook).toBeDefined()
      expect(testHook.name).toBe('Test Health Hook')
    })

    it('should get specific hook details', async () => {
      const { data, ok } = await makeRequest(`${BASE_URL}/api/hooks/${testHookId}`)

      expect(ok).toBe(true)
      expect(data.hook.id).toBe(testHookId)
      expect(data.hook.name).toBe('Test Health Hook')
      expect(data.hook.predicates).toBeDefined()
      expect(data.hook.combine).toBe('AND')
    })

    it('should evaluate hook with custom data', async () => {
      const testData = `@prefix ex: <http://example.org/> .
ex:test-service a ex:Service ;
  ex:errorRate 0.08 .`

      const { data, ok } = await makeRequest(`${BASE_URL}/api/hooks/${testHookId}/evaluate`, {
        method: 'POST',
        body: { data: testData }
      })

      expect(ok).toBe(true)
      expect(data.success).toBe(true)
      expect(data.result.id).toBe(testHookId)
      expect(data.result.fired).toBe(true) // Should fire since errorRate > 0.05
      expect(data.result.predicates).toHaveLength(1)
      expect(data.result.durations.totalMs).toBeGreaterThan(0)
      expect(data.result.provenance).toBeDefined()
    })

    it('should plan hook execution', async () => {
      const { data, ok } = await makeRequest(`${BASE_URL}/api/hooks/${testHookId}/plan`, {
        method: 'POST',
        body: {}
      })

      expect(ok).toBe(true)
      expect(data.plan).toBeDefined()
      expect(data.plan.queryPlan).toBeDefined()
      expect(data.plan.predicatePlan).toHaveLength(1)
    })

    it('should delete a hook', async () => {
      const { data, ok } = await makeRequest(`${BASE_URL}/api/hooks/${testHookId}`, {
        method: 'DELETE'
      })

      expect(ok).toBe(true)
      expect(data.success).toBe(true)
      expect(data.message).toContain('deleted')
    })
  })

  describe('Error Handling', () => {
    it('should handle missing hook gracefully', async () => {
      const { data, status } = await makeRequest(`${BASE_URL}/api/hooks/non-existent-hook/evaluate`, {
        method: 'POST',
        body: {}
      })

      expect(status).toBe(404)
      expect(data.error).toBe('Hook not found')
    })

    it('should handle invalid hook creation', async () => {
      const { data, status } = await makeRequest(`${BASE_URL}/api/hooks`, {
        method: 'POST',
        body: {
          // Missing required fields
          name: 'Invalid Hook'
        }
      })

      expect(status).toBe(400)
      expect(data.error).toBeDefined()
    })

    it('should handle missing data source', async () => {
      const { data, status } = await makeRequest(`${BASE_URL}/api/data/missing-data/query`, {
        method: 'POST',
        body: { query: 'SELECT * WHERE { ?s ?p ?o }' }
      })

      expect(status).toBe(404)
      expect(data.error).toBe('Data source not found')
    })

    it('should handle invalid query', async () => {
      const { data, status } = await makeRequest(`${BASE_URL}/api/data/${testDataSourceId}/query`, {
        method: 'POST',
        body: { query: 'INVALID QUERY' }
      })

      expect(status).toBe(500)
      expect(data.error).toBeDefined()
    })

    it('should handle unsupported query type', async () => {
      const { data, status } = await makeRequest(`${BASE_URL}/api/data/${testDataSourceId}/query`, {
        method: 'POST',
        body: { query: 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }' }
      })

      expect(status).toBe(500)
      expect(data.error).toContain('Only SELECT and ASK queries are supported')
    })
  })

  describe('Performance and Load Testing', () => {
    it('should handle multiple concurrent hook evaluations', async () => {
      const hookId = 'ex:ConcurrentTestHook'
      const concurrentRequests = 5

      // Create hook
      await makeRequest(`${BASE_URL}/api/hooks`, {
        method: 'POST',
        body: {
          id: hookId,
          name: 'Concurrent Test Hook',
          select: 'SELECT ?service WHERE { ?service a <http://example.org/Service> }',
          predicates: [{ kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/Service> }' } }],
          combine: 'AND'
        }
      })

      // Execute multiple concurrent evaluations
      const promises = Array.from({ length: concurrentRequests }, () =>
        makeRequest(`${BASE_URL}/api/hooks/${hookId}/evaluate`, {
          method: 'POST',
          body: {}
        })
      )

      const results = await Promise.all(promises)

      // Verify all succeeded
      results.forEach(result => {
        expect(result.ok).toBe(true)
        expect(result.data.success).toBe(true)
        expect(result.data.result.fired).toBe(true)
      })

      // Cleanup
      await makeRequest(`${BASE_URL}/api/hooks/${hookId}`, { method: 'DELETE' })
    }, TEST_TIMEOUT)

    it('should handle rapid hook creation and deletion', async () => {
      const baseId = 'ex:RapidTestHook-'

      // Create multiple hooks rapidly
      const createPromises = Array.from({ length: 10 }, (_, i) =>
        makeRequest(`${BASE_URL}/api/hooks`, {
          method: 'POST',
          body: {
            id: `${baseId}${i}`,
            name: `Rapid Test Hook ${i}`,
            select: 'SELECT ?s WHERE { ?s a <http://example.org/Service> }',
            predicates: [{ kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/Service> }' } }],
            combine: 'AND'
          }
        })
      )

      const createResults = await Promise.all(createPromises)
      createResults.forEach(result => expect(result.ok).toBe(true))

      // Verify hooks exist
      const { data } = await makeRequest(`${BASE_URL}/api/hooks`)
      const rapidHooks = data.hooks.filter(hook => hook.id.startsWith(baseId))
      expect(rapidHooks.length).toBe(10)

      // Delete hooks rapidly
      const deletePromises = rapidHooks.map(hook =>
        makeRequest(`${BASE_URL}/api/hooks/${hook.id}`, { method: 'DELETE' })
      )

      const deleteResults = await Promise.all(deletePromises)
      deleteResults.forEach(result => expect(result.ok).toBe(true))

      // Verify hooks are deleted
      const { data: finalData } = await makeRequest(`${BASE_URL}/api/hooks`)
      const remainingRapidHooks = finalData.hooks.filter(hook => hook.id.startsWith(baseId))
      expect(remainingRapidHooks.length).toBe(0)
    }, TEST_TIMEOUT)
  })

  describe('End-to-End Workflows', () => {
    it('should support complete data-to-hook workflow', async () => {
      const workflowDataId = 'ex:WorkflowData'
      const workflowHookId = 'ex:WorkflowHook'

      // Step 1: Create data source
      const dataResult = await makeRequest(`${BASE_URL}/api/data`, {
        method: 'POST',
        body: {
          id: workflowDataId,
          name: 'Workflow Test Data',
          content: sampleData.services,
          format: 'Turtle'
        }
      })
      expect(dataResult.ok).toBe(true)

      // Step 2: Create hook that uses the data
      const hookResult = await makeRequest(`${BASE_URL}/api/hooks`, {
        method: 'POST',
        body: {
          id: workflowHookId,
          name: 'Workflow Test Hook',
          select: `SELECT ?service ?errorRate WHERE {
            ?service a <http://example.org/Service> ;
                     <http://example.org/errorRate> ?errorRate .
          }`,
          predicates: [
            {
              kind: 'THRESHOLD',
              spec: {
                var: 'errorRate',
                op: '>',
                value: 0.05
              }
            }
          ],
          combine: 'AND'
        }
      })
      expect(hookResult.ok).toBe(true)

      // Step 3: Query the data to verify it exists
      const queryResult = await makeRequest(`${BASE_URL}/api/data/${workflowDataId}/query`, {
        method: 'POST',
        body: {
          query: 'SELECT ?service WHERE { ?service a <http://example.org/Service> }'
        }
      })
      expect(queryResult.ok).toBe(true)
      expect(queryResult.data.result.length).toBeGreaterThan(0)

      // Step 4: Evaluate hook (should use default data)
      const evalResult = await makeRequest(`${BASE_URL}/api/hooks/${workflowHookId}/evaluate`, {
        method: 'POST',
        body: {}
      })
      expect(evalResult.ok).toBe(true)
      expect(evalResult.data.result.fired).toBe(true)

      // Cleanup
      await makeRequest(`${BASE_URL}/api/hooks/${workflowHookId}`, { method: 'DELETE' })
      // Note: Data source cleanup would be handled by runtime reset
    })

    it('should support complex hook with multiple predicates', async () => {
      const complexHookId = 'ex:ComplexHook'

      const complexHook = {
        id: complexHookId,
        name: 'Complex Multi-Predicate Hook',
        select: `SELECT ?service ?errorRate ?latency WHERE {
          ?service a <http://example.org/Service> ;
                   <http://example.org/errorRate> ?errorRate ;
                   <http://example.org/latency> ?latency .
        }`,
        predicates: [
          {
            kind: 'THRESHOLD',
            spec: {
              var: 'errorRate',
              op: '>',
              value: 0.05
            }
          },
          {
            kind: 'THRESHOLD',
            spec: {
              var: 'latency',
              op: '>',
              value: 500
            }
          }
        ],
        combine: 'OR'
      }

      // Create complex hook
      const createResult = await makeRequest(`${BASE_URL}/api/hooks`, {
        method: 'POST',
        body: complexHook
      })
      expect(createResult.ok).toBe(true)

      // Test with data that should trigger both predicates
      const testData = `@prefix ex: <http://example.org/> .
ex:test a ex:Service ;
  ex:errorRate 0.08 ;
  ex:latency 1200 .`

      const evalResult = await makeRequest(`${BASE_URL}/api/hooks/${complexHookId}/evaluate`, {
        method: 'POST',
        body: { data: testData }
      })
      expect(evalResult.ok).toBe(true)
      expect(evalResult.data.result.fired).toBe(true)
      expect(evalResult.data.result.predicates).toHaveLength(2)

      // Verify both predicates fired
      const thresholdPredicates = evalResult.data.result.predicates.filter(p => p.kind === 'THRESHOLD')
      expect(thresholdPredicates.length).toBe(2)

      // Cleanup
      await makeRequest(`${BASE_URL}/api/hooks/${complexHookId}`, { method: 'DELETE' })
    })
  })

  afterAll(async () => {
    console.log('🧹 Cleaning up test resources...')

    // Clear all results
    await makeRequest(`${BASE_URL}/api/runtime/status`, {
      method: 'POST',
      body: { command: 'clear-results' }
    })

    // Reset runtime to clean state
    await makeRequest(`${BASE_URL}/api/runtime/status`, {
      method: 'POST',
      body: { command: 'reset-runtime' }
    })

    console.log('✅ Integration tests completed')
  })
})
