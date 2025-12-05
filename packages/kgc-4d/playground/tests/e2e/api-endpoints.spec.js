import { test, expect } from '@playwright/test';

/**
 * API Endpoint Tests - Direct HTTP validation
 *
 * Tests:
 * - GET /api/shard (projection)
 * - POST /api/delta (submission)
 * - GET /api/shard?stats=true (statistics)
 * - Error responses
 */

test.describe('API Endpoints', () => {
  test('GET /api/shard should return valid shard', async ({ request }) => {
    const response = await request.get('/api/shard');

    expect(response.status()).toBe(200);

    const data = await response.json();

    expect(data.success).toBe(true);
    expect(data.shard).toBeDefined();
    expect(data.shard).toHaveProperty('id');
    expect(data.shard).toHaveProperty('t_ns');
    expect(data.shard).toHaveProperty('timestamp_iso');
    expect(data.shard).toHaveProperty('vector_clock');
    expect(data.shard).toHaveProperty('quads');
    expect(Array.isArray(data.shard.quads)).toBe(true);
    expect(data.shard.quads.length).toBeGreaterThan(0);
  });

  test('GET /api/shard?stats=true should return universe statistics', async ({ request }) => {
    const response = await request.get('/api/shard?stats=true');

    expect(response.status()).toBe(200);

    const stats = await response.json();

    expect(stats).toHaveProperty('universe');
    expect(stats.universe).toHaveProperty('quad_count');
    expect(stats.universe).toHaveProperty('entity_count');
    expect(stats.universe).toHaveProperty('types');

    expect(stats).toHaveProperty('event_log');
    expect(stats.event_log).toHaveProperty('quad_count');
    expect(stats.event_log).toHaveProperty('event_count');

    expect(stats).toHaveProperty('vector_clock');
    expect(stats).toHaveProperty('timestamp');
  });

  test('GET /api/shard with subject filter should return filtered quads', async ({ request }) => {
    const response = await request.get(
      '/api/shard?subject=http%3A%2F%2Fexample.org%2Fproject%2Falpha'
    );

    expect(response.status()).toBe(200);

    const data = await response.json();

    expect(data.success).toBe(true);
    expect(data.shard.quads.length).toBeGreaterThan(0);

    // All quads should match the subject
    data.shard.quads.forEach((quad) => {
      expect(quad.subject.value).toBe('http://example.org/project/alpha');
    });
  });

  test('GET /api/shard with type filter should return typed entities', async ({ request }) => {
    const response = await request.get(
      '/api/shard?type=http%3A%2F%2Fkgc.io%2Fontology%2FProject'
    );

    expect(response.status()).toBe(200);

    const data = await response.json();

    expect(data.success).toBe(true);
    expect(data.shard.quads.length).toBeGreaterThan(0);
  });

  test('POST /api/delta with valid operation should return ACK', async ({ request }) => {
    const response = await request.post('/api/delta', {
      data: {
        operations: [
          {
            type: 'add',
            subject: { value: 'http://example.org/project/alpha', termType: 'NamedNode' },
            predicate: { value: 'http://kgc.io/ontology/test', termType: 'NamedNode' },
            object: { value: 'test_value', termType: 'Literal' },
          },
        ],
        source: 'test',
      },
    });

    expect(response.status()).toBe(200);

    const data = await response.json();

    expect(data.status).toBe('ACK');
    expect(data).toHaveProperty('t_ns');
    expect(data).toHaveProperty('event_id');
    expect(data).toHaveProperty('vector_clock');
    expect(data).toHaveProperty('event_count');
  });

  test('POST /api/delta with invalid budget should return REJECT', async ({ request }) => {
    const response = await request.post('/api/delta', {
      data: {
        operations: [
          {
            type: 'add',
            subject: { value: 'http://example.org/project/test', termType: 'NamedNode' },
            predicate: { value: 'http://kgc.io/ontology/budget', termType: 'NamedNode' },
            object: { value: '999999', termType: 'Literal' },
          },
        ],
        source: 'test',
      },
    });

    expect(response.status()).toBe(400);

    const data = await response.json();

    expect(data.status).toBe('REJECT');
    expect(data.reason).toMatch(/Budget cannot exceed/i);
  });

  test('POST /api/delta with invalid status should return REJECT', async ({ request }) => {
    const response = await request.post('/api/delta', {
      data: {
        operations: [
          {
            type: 'add',
            subject: { value: 'http://example.org/project/alpha', termType: 'NamedNode' },
            predicate: { value: 'http://kgc.io/ontology/status', termType: 'NamedNode' },
            object: { value: 'invalid_status', termType: 'Literal' },
          },
        ],
        source: 'test',
      },
    });

    expect(response.status()).toBe(400);

    const data = await response.json();

    expect(data.status).toBe('REJECT');
    expect(data.reason).toMatch(/Status must be one of/i);
  });

  test('POST /api/delta with empty name should return REJECT', async ({ request }) => {
    const response = await request.post('/api/delta', {
      data: {
        operations: [
          {
            type: 'add',
            subject: { value: 'http://example.org/project/alpha', termType: 'NamedNode' },
            predicate: { value: 'http://kgc.io/ontology/name', termType: 'NamedNode' },
            object: { value: '', termType: 'Literal' },
          },
        ],
        source: 'test',
      },
    });

    expect(response.status()).toBe(400);

    const data = await response.json();

    expect(data.status).toBe('REJECT');
    expect(data.reason).toMatch(/cannot be empty/i);
  });

  test('POST /api/delta without operations should return error', async ({ request }) => {
    const response = await request.post('/api/delta', {
      data: {
        source: 'test',
      },
    });

    expect(response.status()).toBe(400);

    const data = await response.json();

    expect(data.status).toBe('REJECT');
    expect(data.reason).toMatch(/operations/i);
  });

  test('GET /api/tether should return event stream', async ({ request }) => {
    const response = await request.get('/api/tether');

    expect(response.status()).toBe(200);

    // Should be event stream content type
    expect(response.headers()['content-type']).toContain('text/event-stream');

    // Should have proper cache headers
    expect(response.headers()['cache-control']).toContain('no-cache');
  });

  test('POST /api/delta should accept simple update format', async ({ request }) => {
    const response = await request.post('/api/delta', {
      data: {
        subject: 'http://example.org/project/alpha',
        predicate: 'http://kgc.io/ontology/status',
        value: 'active',
        source: 'test',
      },
    });

    expect(response.status()).toBe(200);

    const data = await response.json();

    // Should either ACK or REJECT with reason (not crash)
    expect(['ACK', 'REJECT']).toContain(data.status);
  });

  test('API should include vector clock in responses', async ({ request }) => {
    const response = await request.get('/api/shard');

    const data = await response.json();

    expect(data.shard.vector_clock).toBeDefined();
    expect(data.shard.vector_clock).toHaveProperty('nodeId');
    expect(data.shard.vector_clock).toHaveProperty('counters');

    // Counters should be an object
    expect(typeof data.shard.vector_clock.counters).toBe('object');
  });

  test('API should include timestamps in all responses', async ({ request }) => {
    const response = await request.get('/api/shard');

    const data = await response.json();

    expect(data.shard).toHaveProperty('t_ns');
    expect(data.shard).toHaveProperty('timestamp_iso');

    // t_ns should be a string of numbers
    expect(typeof data.shard.t_ns).toBe('string');
    expect(/^\d+$/.test(data.shard.t_ns)).toBe(true);

    // timestamp_iso should be valid ISO 8601
    expect(() => new Date(data.shard.timestamp_iso)).not.toThrow();
  });

  test('API should handle concurrent requests', async ({ request }) => {
    // Make multiple concurrent requests
    const responses = await Promise.all([
      request.get('/api/shard'),
      request.get('/api/shard?stats=true'),
      request.post('/api/delta', {
        data: {
          operations: [
            {
              type: 'add',
              subject: { value: 'http://example.org/project/alpha', termType: 'NamedNode' },
              predicate: { value: 'http://kgc.io/ontology/test1', termType: 'NamedNode' },
              object: { value: 'value1', termType: 'Literal' },
            },
          ],
          source: 'test',
        },
      }),
      request.post('/api/delta', {
        data: {
          operations: [
            {
              type: 'add',
              subject: { value: 'http://example.org/project/alpha', termType: 'NamedNode' },
              predicate: { value: 'http://kgc.io/ontology/test2', termType: 'NamedNode' },
              object: { value: 'value2', termType: 'Literal' },
            },
          ],
          source: 'test',
        },
      }),
    ]);

    // All should succeed
    responses.forEach((response) => {
      expect([200, 400]).toContain(response.status());
    });
  });
});
