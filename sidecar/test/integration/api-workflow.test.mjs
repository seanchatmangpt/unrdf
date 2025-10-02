/**
 * @file API Workflow Integration Tests
 * @description Tests for complete API workflows combining multiple endpoints
 */

import { describe, it, expect, beforeAll } from 'vitest'
import { $fetch, setup } from '@nuxt/test-utils/e2e'

describe('API Workflow Integration', async () => {
  await setup({
    server: true,
    build: true
  })

  let testTransactionId
  let testHookId
  let testPolicyId

  describe('Complete transaction lifecycle', () => {
    it('registers hook before transaction', async () => {
      const hookResponse = await $fetch('/api/hooks/register', {
        method: 'POST',
        body: {
          id: 'workflow-hook',
          select: 'SELECT ?s WHERE { ?s a <http://example.org/Person> }',
          predicates: [
            {
              kind: 'COUNT',
              countVariable: 's'
            }
          ],
          combine: 'AND',
          phase: 'pre'
        }
      })

      expect(hookResponse.success).toBe(true)
      testHookId = hookResponse.data.hookId
    })

    it('registers policy before transaction', async () => {
      const policyResponse = await $fetch('/api/policy/register', {
        method: 'POST',
        body: {
          id: 'workflow-policy',
          shapes: `
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org/> .

            ex:PersonShape
              a sh:NodeShape ;
              sh:targetClass ex:Person ;
              sh:property [
                sh:path ex:name ;
                sh:minCount 1 ;
              ] .
          `
        }
      })

      expect(policyResponse.success).toBe(true)
      testPolicyId = policyResponse.data.policyId
    })

    it('applies transaction that triggers hooks', async () => {
      const txResponse = await $fetch('/api/transaction/apply', {
        method: 'POST',
        body: {
          delta: [
            {
              subject: { termType: 'NamedNode', value: 'http://example.org/person/workflow-test' },
              predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
              object: { termType: 'NamedNode', value: 'http://example.org/Person' }
            },
            {
              subject: { termType: 'NamedNode', value: 'http://example.org/person/workflow-test' },
              predicate: { termType: 'NamedNode', value: 'http://example.org/name' },
              object: { termType: 'Literal', value: 'Workflow Test User' }
            }
          ],
          author: 'workflow-test',
          metadata: { test: 'workflow' }
        }
      })

      expect(txResponse.success).toBe(true)
      testTransactionId = txResponse.data.transactionId
    })

    it('queries data after transaction', async () => {
      const queryResponse = await $fetch('/api/query', {
        query: {
          query: 'SELECT ?name WHERE { <http://example.org/person/workflow-test> <http://example.org/name> ?name }'
        }
      })

      expect(queryResponse.success).toBe(true)
      expect(queryResponse.data.results).toBeDefined()
    })
  })

  describe('Hook and effect workflow', () => {
    it('registers effect for hook execution', async () => {
      const effectResponse = await $fetch('/api/effects/register', {
        method: 'POST',
        body: {
          id: 'workflow-effect',
          code: `
            export default function(context) {
              console.log('Effect triggered for:', context.transactionId);
              return { processed: true };
            }
          `,
          timeout: 5000
        }
      })

      expect(effectResponse.success).toBe(true)
    })

    it('registers hook that triggers effect', async () => {
      const hookResponse = await $fetch('/api/hooks/register', {
        method: 'POST',
        body: {
          id: 'effect-trigger-hook',
          select: 'SELECT ?s WHERE { ?s ?p ?o }',
          predicates: [
            {
              kind: 'ASK',
              query: 'ASK { ?s ?p ?o }'
            }
          ],
          combine: 'AND',
          phase: 'post'
        }
      })

      expect(hookResponse.success).toBe(true)
    })

    it('applies transaction that triggers hook and effect', async () => {
      const txResponse = await $fetch('/api/transaction/apply', {
        method: 'POST',
        body: {
          delta: [
            {
              subject: { termType: 'NamedNode', value: 'http://example.org/effect-test' },
              predicate: { termType: 'NamedNode', value: 'http://example.org/test' },
              object: { termType: 'Literal', value: 'effect workflow' }
            }
          ]
        }
      })

      expect(txResponse.success).toBe(true)
    })
  })

  describe('Multi-agent workflow', () => {
    it('registers multiple agents with different priorities', async () => {
      const agent1 = await $fetch('/api/agents/register', {
        method: 'POST',
        body: {
          id: 'workflow-agent-1',
          endpoint: 'http://localhost:3001/agent1',
          priority: 10
        }
      })

      const agent2 = await $fetch('/api/agents/register', {
        method: 'POST',
        body: {
          id: 'workflow-agent-2',
          endpoint: 'http://localhost:3002/agent2',
          priority: 20
        }
      })

      expect(agent1.success).toBe(true)
      expect(agent2.success).toBe(true)
    })
  })

  describe('Policy validation workflow', () => {
    it('registers restrictive policy', async () => {
      const policyResponse = await $fetch('/api/policy/register', {
        method: 'POST',
        body: {
          id: 'restrictive-policy',
          shapes: `
            @prefix sh: <http://www.w3.org/ns/shacl#> .
            @prefix ex: <http://example.org/> .
            @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

            ex:StrictPersonShape
              a sh:NodeShape ;
              sh:targetClass ex:StrictPerson ;
              sh:property [
                sh:path ex:age ;
                sh:minInclusive 18 ;
                sh:maxInclusive 100 ;
                sh:datatype xsd:integer ;
              ] .
          `,
          priority: 100
        }
      })

      expect(policyResponse.success).toBe(true)
    })

    it('applies transaction that may violate policy', async () => {
      // This transaction should either succeed or fail based on policy validation
      const txResponse = await $fetch('/api/transaction/apply', {
        method: 'POST',
        body: {
          delta: [
            {
              subject: { termType: 'NamedNode', value: 'http://example.org/person/strict' },
              predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
              object: { termType: 'NamedNode', value: 'http://example.org/StrictPerson' }
            },
            {
              subject: { termType: 'NamedNode', value: 'http://example.org/person/strict' },
              predicate: { termType: 'NamedNode', value: 'http://example.org/age' },
              object: {
                termType: 'Literal',
                value: '25',
                datatype: { value: 'http://www.w3.org/2001/XMLSchema#integer' }
              }
            }
          ]
        }
      })

      // Should succeed with valid age
      expect(txResponse.success).toBe(true)
    })
  })
})
