/**
 * @file Policy Endpoint Integration Tests
 * @description London BDD tests for policy registration API route
 */

import { describe, it, expect } from 'vitest'
import { $fetch, setup } from '@nuxt/test-utils/e2e'

describe('POST /api/policy/register (Integration)', async () => {
  await setup({
    server: true,
    build: true
  })

  it('registers valid SHACL policy', async () => {
    const response = await $fetch('/api/policy/register', {
      method: 'POST',
      body: {
        id: 'test-policy',
        shapes: `
          @prefix sh: <http://www.w3.org/ns/shacl#> .
          @prefix ex: <http://example.org/> .

          ex:PersonShape
            a sh:NodeShape ;
            sh:targetClass ex:Person ;
            sh:property [
              sh:path ex:name ;
              sh:minCount 1 ;
              sh:datatype xsd:string ;
            ] .
        `,
        priority: 10
      }
    })

    expect(response.success).toBe(true)
    expect(response.data).toHaveProperty('policyId')
    expect(response.data.policyId).toBe('test-policy')
  })

  it('registers policy without optional priority', async () => {
    const response = await $fetch('/api/policy/register', {
      method: 'POST',
      body: {
        id: 'test-policy-no-priority',
        shapes: `
          @prefix sh: <http://www.w3.org/ns/shacl#> .
          ex:SimpleShape a sh:NodeShape .
        `
      }
    })

    expect(response.success).toBe(true)
    expect(response.data.policyId).toBe('test-policy-no-priority')
  })

  it('rejects policy without id', async () => {
    try {
      await $fetch('/api/policy/register', {
        method: 'POST',
        body: {
          shapes: '@prefix sh: <http://www.w3.org/ns/shacl#> .'
        }
      })
      expect.fail('Should have thrown validation error')
    } catch (error) {
      expect(error.statusCode).toBe(400)
    }
  })

  it('rejects policy without shapes', async () => {
    try {
      await $fetch('/api/policy/register', {
        method: 'POST',
        body: {
          id: 'test-policy'
        }
      })
      expect.fail('Should have thrown validation error')
    } catch (error) {
      expect(error.statusCode).toBe(400)
    }
  })

  it('registers policy with high priority', async () => {
    const response = await $fetch('/api/policy/register', {
      method: 'POST',
      body: {
        id: 'high-priority-policy',
        shapes: '@prefix sh: <http://www.w3.org/ns/shacl#> .',
        priority: 100
      }
    })

    expect(response.success).toBe(true)
  })

  it('registers policy with complex SHACL shapes', async () => {
    const response = await $fetch('/api/policy/register', {
      method: 'POST',
      body: {
        id: 'complex-policy',
        shapes: `
          @prefix sh: <http://www.w3.org/ns/shacl#> .
          @prefix ex: <http://example.org/> .
          @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

          ex:PersonShape
            a sh:NodeShape ;
            sh:targetClass ex:Person ;
            sh:property [
              sh:path ex:name ;
              sh:minCount 1 ;
              sh:maxCount 1 ;
              sh:datatype xsd:string ;
            ] ;
            sh:property [
              sh:path ex:age ;
              sh:datatype xsd:integer ;
              sh:minInclusive 0 ;
              sh:maxInclusive 150 ;
            ] ;
            sh:property [
              sh:path ex:email ;
              sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
            ] .
        `,
        priority: 50
      }
    })

    expect(response.success).toBe(true)
    expect(response.data.policyId).toBe('complex-policy')
  })
})
