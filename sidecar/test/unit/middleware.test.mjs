import { describe, it, expect, vi, beforeAll } from 'vitest';
import { makeEvent } from '../mocks/nitro-imports.mjs';

// The rate limiter opens an ioredis connection at import time; keep tests offline.
vi.mock('ioredis', () => ({
  default: class FakeRedis {
    on() {}
    quit() {}
  }
}));

let rateLimit;
let authorization;
let getRBACEngine;

beforeAll(async () => {
  rateLimit = (await import('../../server/middleware/03.rate-limit.mjs')).default;
  authorization = (await import('../../server/middleware/02.authorization.mjs')).default;
  ({ getRBACEngine } = await import('../../server/utils/rbac.mjs'));
});

describe('03.rate-limit middleware (h3 event handler)', () => {
  it('is an h3 handler taking a single event', () => {
    expect(typeof rateLimit).toBe('function');
    expect(rateLimit.length).toBe(1);
  });

  it('allows requests under the limit and sets headers', async () => {
    const event = makeEvent({ path: '/api/hooks/list', ip: '203.0.113.1' });
    await rateLimit(event);
    expect(event.responseHeaders['X-RateLimit-Limit']).toBe('100');
    expect(event.responseHeaders['X-RateLimit-Remaining']).toBe('99');
  });

  it('enforces the limit with a 429 once points are exhausted', async () => {
    const ip = '203.0.113.2';
    let blocked;
    for (let i = 0; i < 101; i++) {
      try {
        await rateLimit(makeEvent({ path: '/api/hooks/list', ip }));
      } catch (e) {
        blocked = e;
        break;
      }
    }
    expect(blocked).toBeDefined();
    expect(blocked.statusCode).toBe(429);
    expect(blocked.data.type).toBe('unauthenticated');
  });
});

describe('02.authorization middleware (h3 event handler)', () => {
  it('skips public endpoints', async () => {
    await expect(authorization(makeEvent({ path: '/api/health' }))).resolves.toBeUndefined();
    await expect(authorization(makeEvent({ path: '/' }))).resolves.toBeUndefined();
  });

  it('rejects unauthenticated API access with 401', async () => {
    await expect(authorization(makeEvent({ path: '/api/hooks/list' }))).rejects.toMatchObject({
      statusCode: 401
    });
  });

  it('denies a user without permission with 403 and allows an admin', async () => {
    const rbac = getRBACEngine();
    rbac.assignRole('reader-1', 'reader');
    rbac.assignRole('admin-1', 'admin');

    await expect(
      authorization(
        makeEvent({
          path: '/api/admin/roles',
          method: 'POST',
          auth: { userId: 'reader-1', roles: ['reader'] }
        })
      )
    ).rejects.toMatchObject({ statusCode: 403 });

    const event = makeEvent({
      path: '/api/hooks/list',
      auth: { userId: 'admin-1', roles: ['admin'] }
    });
    await expect(authorization(event)).resolves.toBeUndefined();
    expect(event.context.authDecision.allowed).toBe(true);
  });
});
