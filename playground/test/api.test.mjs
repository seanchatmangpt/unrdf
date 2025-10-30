import { describe, it, expect, beforeAll, afterAll } from 'vitest'
import http from 'node:http'

// Simple HTTP helper
async function request(path, opts = {}) {
  const res = await fetch(`http://localhost:3000${path}`, {
    ...opts,
    headers: { 'content-type': 'application/json', ...(opts.headers || {}) }
  })
  const body = await res.json().catch(() => ({}))
  return { status: res.status, body }
}

describe('Nitro API', () => {
  /**
   * Assumes `pnpm server` runs Nitro on :3000 in another process during tests.
   * 80/20: we only verify happy-path endpoints.
   */

  it('GET /api/runtime/status returns ok', async () => {
    const { status, body } = await request('/api/runtime/status')
    expect(status).toBe(200)
    expect(body.status).toBe('running')
  })

  it('hooks CRUD + evaluate happy path', async () => {
    // Create hook
    const hookSpec = {
      id: 'ex:TestHook',
      name: 'Test Hook',
      select: 'SELECT ?s WHERE { ?s ?p ?o }',
      predicates: []
    }
    const create = await request('/api/hooks', { method: 'POST', body: JSON.stringify(hookSpec) })
    expect(create.status).toBe(200)
    expect(create.body.success).toBe(true)

    // List hooks
    const list = await request('/api/hooks')
    expect(list.status).toBe(200)
    expect(list.body.total).toBeGreaterThan(0)

    // Get hook
    const get = await request('/api/hooks/ex:TestHook')
    expect(get.status).toBe(200)
    expect(get.body.hook.id).toBe('ex:TestHook')

    // Evaluate
    const evalRes = await request('/api/hooks/ex:TestHook/evaluate', { method: 'POST', body: JSON.stringify({}) })
    expect(evalRes.status).toBe(200)
    expect(evalRes.body.success).toBe(true)

    // Delete
    const del = await request('/api/hooks/ex:TestHook', { method: 'DELETE' })
    expect(del.status).toBe(200)
    expect(del.body.success).toBe(true)
  })

  it('data create and query', async () => {
    const ttl = '@prefix ex: <http://example.org/> . ex:a ex:b ex:c .'
    const create = await request('/api/data', { method: 'POST', body: JSON.stringify({ id: 'sample', content: ttl }) })
    expect(create.status).toBe(200)

    const query = await request('/api/data/sample/query', { method: 'POST', body: JSON.stringify({ query: 'ASK { ?s ?p ?o }' }) })
    expect(query.status).toBe(200)
    expect(query.body.success).toBe(true)

    const del = await request('/api/data/sample', { method: 'DELETE' })
    expect(del.status).toBe(200)
  })
})



