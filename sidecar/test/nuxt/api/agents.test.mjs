/**
 * @file Agents Endpoint Integration Tests
 * @description London BDD tests for agent registration API route
 */

import { describe, it, expect } from 'vitest'
import { $fetch, setup } from '@nuxt/test-utils/e2e'

describe('POST /api/agents/register (Integration)', async () => {
  await setup({
    server: true,
    build: true
  })

  it('registers agent with endpoint', async () => {
    const response = await $fetch('/api/agents/register', {
      method: 'POST',
      body: {
        id: 'test-agent',
        endpoint: 'http://localhost:3001/agent',
        priority: 10
      }
    })

    expect(response.success).toBe(true)
    expect(response.data).toHaveProperty('agentId')
    expect(response.data.agentId).toBe('test-agent')
  })

  it('registers agent without optional endpoint', async () => {
    const response = await $fetch('/api/agents/register', {
      method: 'POST',
      body: {
        id: 'test-agent-no-endpoint',
        priority: 5
      }
    })

    expect(response.success).toBe(true)
    expect(response.data.agentId).toBe('test-agent-no-endpoint')
  })

  it('registers agent without optional priority', async () => {
    const response = await $fetch('/api/agents/register', {
      method: 'POST',
      body: {
        id: 'test-agent-no-priority',
        endpoint: 'http://localhost:3002/agent'
      }
    })

    expect(response.success).toBe(true)
  })

  it('rejects agent without id', async () => {
    try {
      await $fetch('/api/agents/register', {
        method: 'POST',
        body: {
          endpoint: 'http://localhost:3003/agent'
        }
      })
      expect.fail('Should have thrown validation error')
    } catch (error) {
      expect(error.statusCode).toBe(400)
    }
  })

  it('registers agent with high priority', async () => {
    const response = await $fetch('/api/agents/register', {
      method: 'POST',
      body: {
        id: 'high-priority-agent',
        endpoint: 'http://localhost:3004/agent',
        priority: 100
      }
    })

    expect(response.success).toBe(true)
  })
})
