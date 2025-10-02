/**
 * @file Effects Endpoint Integration Tests
 * @description London BDD tests for effect registration API route
 */

import { describe, it, expect } from 'vitest'
import { $fetch, setup } from '@nuxt/test-utils/e2e'

describe('POST /api/effects/register (Integration)', async () => {
  await setup({
    server: true,
    build: true
  })

  it('registers valid JavaScript effect', async () => {
    const response = await $fetch('/api/effects/register', {
      method: 'POST',
      body: {
        id: 'test-effect',
        code: `
          export default function(context) {
            console.log('Effect executed');
            return { success: true };
          }
        `,
        timeout: 5000,
        memoryLimit: 1024 * 1024
      }
    })

    expect(response.success).toBe(true)
    expect(response.data).toHaveProperty('effectId')
    expect(response.data.effectId).toBe('test-effect')
  })

  it('registers effect without optional timeout', async () => {
    const response = await $fetch('/api/effects/register', {
      method: 'POST',
      body: {
        id: 'test-effect-no-timeout',
        code: 'export default function() { return true; }'
      }
    })

    expect(response.success).toBe(true)
  })

  it('registers effect without optional memoryLimit', async () => {
    const response = await $fetch('/api/effects/register', {
      method: 'POST',
      body: {
        id: 'test-effect-no-memory',
        code: 'export default function() { return true; }',
        timeout: 3000
      }
    })

    expect(response.success).toBe(true)
  })

  it('rejects effect without id', async () => {
    try {
      await $fetch('/api/effects/register', {
        method: 'POST',
        body: {
          code: 'export default function() { return true; }'
        }
      })
      expect.fail('Should have thrown validation error')
    } catch (error) {
      expect(error.statusCode).toBe(400)
    }
  })

  it('rejects effect without code', async () => {
    try {
      await $fetch('/api/effects/register', {
        method: 'POST',
        body: {
          id: 'test-effect'
        }
      })
      expect.fail('Should have thrown validation error')
    } catch (error) {
      expect(error.statusCode).toBe(400)
    }
  })

  it('rejects effect with negative timeout', async () => {
    try {
      await $fetch('/api/effects/register', {
        method: 'POST',
        body: {
          id: 'test-effect',
          code: 'export default function() { return true; }',
          timeout: -1000
        }
      })
      expect.fail('Should have thrown validation error')
    } catch (error) {
      expect(error.statusCode).toBe(400)
    }
  })

  it('rejects effect with negative memoryLimit', async () => {
    try {
      await $fetch('/api/effects/register', {
        method: 'POST',
        body: {
          id: 'test-effect',
          code: 'export default function() { return true; }',
          memoryLimit: -1024
        }
      })
      expect.fail('Should have thrown validation error')
    } catch (error) {
      expect(error.statusCode).toBe(400)
    }
  })

  it('registers complex effect with async code', async () => {
    const response = await $fetch('/api/effects/register', {
      method: 'POST',
      body: {
        id: 'async-effect',
        code: `
          export default async function(context) {
            await new Promise(resolve => setTimeout(resolve, 100));
            return { processed: context.delta.length };
          }
        `,
        timeout: 10000,
        memoryLimit: 2 * 1024 * 1024
      }
    })

    expect(response.success).toBe(true)
    expect(response.data.effectId).toBe('async-effect')
  })
})
