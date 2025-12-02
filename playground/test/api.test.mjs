import { describe, it, expect, beforeAll, afterAll } from 'vitest'
import http from 'node:http'

const API_URL = 'http://localhost:3000';
let serverAvailable = false;

// Simple HTTP helper
async function request(path, opts = {}) {
  const res = await fetch(`${API_URL}${path}`, {
    ...opts,
    headers: { 'content-type': 'application/json', ...(opts.headers || {}) }
  })
  const body = await res.json().catch(() => ({}))
  return { status: res.status, body }
}

// Health check before tests
beforeAll(async () => {
  try {
    const response = await fetch(`${API_URL}/api/runtime/status`, {
      signal: AbortSignal.timeout(1000) // 1s timeout
    });
    serverAvailable = response.ok;
  } catch {
    serverAvailable = false;
  }

  if (!serverAvailable) {
    console.log('\n⚠️  API Server not running. Tests will be skipped.');
    console.log('   To run API tests: Start server with `pnpm dev:playground` in another terminal\n');
  }
});

describe('Nitro API Integration Tests', () => {
  /**
   * Assumes Nitro server runs on :3000 in another process during tests.
   * Tests automatically skip if server is not available.
   * 80/20: we only verify happy-path endpoints.
   */

  it.skipIf(!serverAvailable)('GET /api/runtime/status returns ok', async () => {
    const { status, body } = await request('/api/runtime/status')
    expect(status).toBe(200)
    expect(body.status).toBe('running')
  })

  it.skipIf(!serverAvailable)('hooks CRUD + evaluate happy path', async () => {
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

  it.skipIf(!serverAvailable)('data create and query', async () => {
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



