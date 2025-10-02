/**
 * @file Lockchain Endpoint Integration Tests
 * @description London BDD tests for lockchain initialization API route
 */

import { describe, it, expect } from 'vitest'
import { $fetch, setup } from '@nuxt/test-utils/e2e'

describe('POST /api/lockchain/init (Integration)', async () => {
  await setup({
    server: true,
    build: true
  })

  it('initializes lockchain with valid repo URL', async () => {
    const response = await $fetch('/api/lockchain/init', {
      method: 'POST',
      body: {
        repoUrl: 'http://gitea:3000/kgc/test-lockchain.git',
        branch: 'main'
      }
    })

    expect(response.success).toBe(true)
    expect(response.data).toHaveProperty('repoUrl')
  })

  it('initializes lockchain with credentials', async () => {
    const response = await $fetch('/api/lockchain/init', {
      method: 'POST',
      body: {
        repoUrl: 'http://gitea:3000/kgc/secure-lockchain.git',
        branch: 'main',
        credentials: {
          username: 'test-user',
          password: 'test-pass'
        }
      }
    })

    expect(response.success).toBe(true)
  })

  it('initializes lockchain without optional branch', async () => {
    const response = await $fetch('/api/lockchain/init', {
      method: 'POST',
      body: {
        repoUrl: 'http://gitea:3000/kgc/default-branch.git'
      }
    })

    expect(response.success).toBe(true)
  })

  it('rejects lockchain without repoUrl', async () => {
    try {
      await $fetch('/api/lockchain/init', {
        method: 'POST',
        body: {
          branch: 'main'
        }
      })
      expect.fail('Should have thrown validation error')
    } catch (error) {
      expect(error.statusCode).toBe(400)
    }
  })

  it('rejects lockchain with invalid URL', async () => {
    try {
      await $fetch('/api/lockchain/init', {
        method: 'POST',
        body: {
          repoUrl: 'not-a-valid-url',
          branch: 'main'
        }
      })
      expect.fail('Should have thrown validation error')
    } catch (error) {
      expect(error.statusCode).toBe(400)
    }
  })

  it('initializes lockchain with SSH URL', async () => {
    const response = await $fetch('/api/lockchain/init', {
      method: 'POST',
      body: {
        repoUrl: 'git@github.com:test/lockchain.git',
        branch: 'develop'
      }
    })

    expect(response.success).toBe(true)
  })
})
