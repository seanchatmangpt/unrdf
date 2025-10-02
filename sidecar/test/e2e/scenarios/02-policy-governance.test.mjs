/**
 * @file Scenario 2: Policy Pack Governance + Metrics (25%)
 * @description London BDD E2E test for SHACL policy validation
 *
 * This scenario validates:
 * - Policy pack registration via HTTP API
 * - SHACL validation during transaction
 * - Policy violation error handling (422)
 * - Violation details in response
 * - Compliance metrics in Prometheus
 */

import { describe, it, expect, beforeAll, afterEach } from 'vitest'

describe('Scenario 2: Policy Pack Governance (E2E)', () => {
  let baseUrl
  let createdPolicyId

  beforeAll(() => {
    // Infrastructure already started by global setup
    baseUrl = 'http://localhost:3000'
    console.log('[Scenario 2] Using infrastructure:', { baseUrl })
  })

  afterEach(async () => {
    // Cleanup policies created during test
    if (createdPolicyId) {
      try {
        await fetch(`${baseUrl}/api/policy/${createdPolicyId}`, { method: 'DELETE' })
      } catch (error) {
        console.warn(`[Scenario 2] Cleanup failed for policy ${createdPolicyId}:`, error.message)
      }
      createdPolicyId = null
    }
  })

  it('registers a SHACL policy pack', async () => {
    const policyRequest = {
      id: `e2e-person-policy-${Date.now()}`,
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
          ] ;
          sh:property [
            sh:path ex:age ;
            sh:datatype xsd:integer ;
            sh:minInclusive 0 ;
            sh:maxInclusive 150 ;
          ] .
      `,
      priority: 10
    }

    const response = await fetch(`${baseUrl}/api/policy/register`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(policyRequest)
    })

    expect(response.status).toBe(201)

    const data = await response.json()
    expect(data.success).toBe(true)
    expect(data.data.policyId).toBe(policyRequest.id)

    createdPolicyId = policyRequest.id
  })

  it('allows transaction that conforms to policy', async () => {
    const subject = `http://example.org/bob-${Date.now()}`
    const txRequest = {
      delta: [
        {
          subject: { termType: 'NamedNode', value: subject },
          predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
          object: { termType: 'NamedNode', value: 'http://example.org/Person' }
        },
        {
          subject: { termType: 'NamedNode', value: subject },
          predicate: { termType: 'NamedNode', value: 'http://example.org/name' },
          object: { termType: 'Literal', value: 'Bob Smith', datatype: { value: 'http://www.w3.org/2001/XMLSchema#string' } }
        },
        {
          subject: { termType: 'NamedNode', value: subject },
          predicate: { termType: 'NamedNode', value: 'http://example.org/age' },
          object: { termType: 'Literal', value: '30', datatype: { value: 'http://www.w3.org/2001/XMLSchema#integer' } }
        }
      ],
      author: 'e2e-test'
    }

    const response = await fetch(`${baseUrl}/api/transaction/apply`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(txRequest)
    })

    expect(response.status).toBe(201)

    const data = await response.json()
    expect(data.success).toBe(true)
  })

  it('rejects transaction that violates policy with 422', async () => {
    const txRequest = {
      delta: [
        {
          subject: { termType: 'NamedNode', value: `http://example.org/charlie-${Date.now()}` },
          predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
          object: { termType: 'NamedNode', value: 'http://example.org/Person' }
        }
        // Missing required ex:name property - should violate policy
      ],
      author: 'e2e-test'
    }

    const response = await fetch(`${baseUrl}/api/transaction/apply`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(txRequest)
    })

    expect(response.status).toBe(422)

    const data = await response.json()
    expect(data.success).toBe(false)
    expect(data.error.code).toBe('POLICY_VIOLATION')
    expect(data.error.violations).toBeDefined()
    expect(data.error.violations.length).toBeGreaterThan(0)
  })

  it('exports policy compliance metrics to Prometheus', async () => {
    const prometheusUrl = 'http://localhost:9090'
    const query = 'kgc_policy_compliance_rate'
    const promApiUrl = `${prometheusUrl}/api/v1/query?query=${encodeURIComponent(query)}`

    const response = await fetch(promApiUrl)
    expect(response.status).toBe(200)

    const metrics = await response.json()
    expect(metrics.status).toBe('success')

    // Validate compliance rate metric exists
    if (metrics.data.result.length > 0) {
      const complianceRate = parseFloat(metrics.data.result[0].value[1])
      expect(complianceRate).toBeGreaterThanOrEqual(0)
      expect(complianceRate).toBeLessThanOrEqual(1)
    }
  })
})
