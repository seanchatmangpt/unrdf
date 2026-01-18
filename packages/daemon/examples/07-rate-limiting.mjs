/**
 * @file Rate Limiting Examples
 * @description Comprehensive examples demonstrating rate limiting usage
 */

import {
  createRateLimiter,
  createRateLimitMiddleware,
  parseEnvConfig,
} from '../src/middleware/rate-limiter.mjs';

console.log('=== Rate Limiting Examples ===\n');

// Example 1: Basic Rate Limiting
console.log('1. Basic Rate Limiting');
{
  const limiter = createRateLimiter({
    maxRequests: 5,
    windowMs: 10000,
  });

  console.log('Making 7 requests (limit: 5)...');
  for (let i = 1; i <= 7; i++) {
    const result = limiter.check({ ip: '192.168.1.1' });
    console.log(`  Request ${i}: ${result.allowed ? '✅ ALLOWED' : '❌ BLOCKED'} (remaining: ${result.remaining})`);
  }
  console.log();
}

// Example 2: Per-IP vs Per-API-Key
console.log('2. Per-IP vs Per-API-Key Limiting');
{
  const limiter = createRateLimiter({
    maxRequests: 3,
    windowMs: 10000,
  });

  console.log('Same IP, different API keys:');
  const r1 = limiter.check({ ip: '192.168.1.1', apiKey: 'key1' });
  const r2 = limiter.check({ ip: '192.168.1.1', apiKey: 'key2' });
  console.log(`  key1: ${r1.allowed ? '✅' : '❌'} (identifier: ${r1.identifier})`);
  console.log(`  key2: ${r2.allowed ? '✅' : '❌'} (identifier: ${r2.identifier})`);

  console.log('\nDifferent IPs, same API key:');
  const r3 = limiter.check({ ip: '192.168.1.2', apiKey: 'key1' });
  console.log(`  192.168.1.2: ${r3.allowed ? '✅' : '❌'} (uses same key1 bucket)`);
  console.log();
}

// Example 3: Burst Protection
console.log('3. Burst Protection');
{
  const limiter = createRateLimiter({
    maxRequests: 100,
    windowMs: 60000,
    burstSize: 3,
    burstWindowMs: 1000,
  });

  console.log('Rapid requests (burst limit: 3 per second):');
  for (let i = 1; i <= 5; i++) {
    const result = limiter.check({ ip: '192.168.1.10' });
    console.log(`  Request ${i}: ${result.allowed ? '✅ ALLOWED' : '❌ BLOCKED'}`);
    if (!result.allowed) {
      console.log(`    Reason: ${result.reason}`);
      console.log(`    Retry after: ${result.retryAfter}s`);
    }
  }
  console.log();
}

// Example 4: Statistics Tracking
console.log('4. Statistics Tracking');
{
  const limiter = createRateLimiter({
    maxRequests: 5,
  });

  // Generate some traffic
  for (let i = 0; i < 10; i++) {
    limiter.check({ ip: `192.168.1.${i}` });
  }

  for (let i = 0; i < 15; i++) {
    limiter.check({ ip: '192.168.1.100' });
  }

  const stats = limiter.getStats();
  console.log('Rate Limiter Statistics:');
  console.log(`  Total Requests: ${stats.totalRequests}`);
  console.log(`  Allowed: ${stats.allowedRequests}`);
  console.log(`  Blocked: ${stats.blockedRequests}`);
  console.log(`  Unique Identifiers: ${stats.uniqueIdentifiers}`);
  console.log(`  Cache Size: ${stats.cacheSize}`);
  console.log(`  Block Rate: ${(stats.blockRate * 100).toFixed(2)}%`);
  console.log();
}

// Example 5: Middleware Usage
console.log('5. Middleware Usage');
{
  const middleware = createRateLimitMiddleware({
    maxRequests: 3,
    windowMs: 10000,
  });

  async function handleRequest(req) {
    try {
      await middleware({
        ip: req.ip,
        apiKey: req.headers?.['x-api-key'],
      });
      return { status: 200, body: 'Success' };
    } catch (error) {
      if (error.statusCode === 429) {
        return {
          status: 429,
          body: 'Rate limit exceeded',
          headers: {
            'Retry-After': error.retryAfter,
          },
        };
      }
      throw error;
    }
  }

  const mockReq = { ip: '192.168.1.200', headers: {} };

  console.log('Testing middleware with mock requests:');
  for (let i = 1; i <= 5; i++) {
    const response = await handleRequest(mockReq);
    console.log(`  Request ${i}: ${response.status} - ${response.body}`);
    if (response.headers?.['Retry-After']) {
      console.log(`    Retry-After: ${response.headers['Retry-After']}s`);
    }
  }
  console.log();
}

// Example 6: Environment Configuration
console.log('6. Environment Configuration');
{
  // Set environment variables
  process.env.RATE_LIMIT_MAX_REQUESTS = '10';
  process.env.RATE_LIMIT_WINDOW_MS = '5000';
  process.env.RATE_LIMIT_BURST_SIZE = '5';

  const config = parseEnvConfig();
  console.log('Parsed Environment Config:');
  console.log(`  maxRequests: ${config.maxRequests}`);
  console.log(`  windowMs: ${config.windowMs}`);
  console.log(`  burstSize: ${config.burstSize}`);

  const limiter = createRateLimiter(config);
  console.log(`\nCreated limiter with ${config.maxRequests} req/${config.windowMs}ms`);

  // Clean up
  delete process.env.RATE_LIMIT_MAX_REQUESTS;
  delete process.env.RATE_LIMIT_WINDOW_MS;
  delete process.env.RATE_LIMIT_BURST_SIZE;
  console.log();
}

// Example 7: Reset and Clear
console.log('7. Reset and Clear Operations');
{
  const limiter = createRateLimiter({
    maxRequests: 2,
    windowMs: 10000,
  });

  console.log('Making 3 requests:');
  limiter.check({ ip: '192.168.1.1' });
  limiter.check({ ip: '192.168.1.1' });
  const r3 = limiter.check({ ip: '192.168.1.1' });
  console.log(`  Request 3: ${r3.allowed ? '✅' : '❌'}`);

  console.log('\nResetting limits for 192.168.1.1...');
  const identifier = limiter.extractIdentifier({ ip: '192.168.1.1' });
  limiter.reset(identifier);

  const r4 = limiter.check({ ip: '192.168.1.1' });
  console.log(`  Request 4 (after reset): ${r4.allowed ? '✅' : '❌'}`);

  console.log('\nClearing all limits...');
  limiter.clear();
  const stats = limiter.getStats();
  console.log(`  Stats after clear: ${stats.totalRequests} requests, ${stats.cacheSize} cache entries`);
  console.log();
}

// Example 8: LRU Cache Eviction
console.log('8. LRU Cache Eviction');
{
  const limiter = createRateLimiter({
    storageMaxSize: 3,
  });

  console.log('Adding 5 identifiers (max: 3):');
  for (let i = 1; i <= 5; i++) {
    limiter.check({ ip: `192.168.1.${i}` });
    const stats = limiter.getStats();
    console.log(`  Added IP ${i}, cache size: ${stats.cacheSize}`);
  }
  console.log();
}

// Example 9: Integration with Authentication
console.log('9. Integration with Authentication');
{
  const limiter = createRateLimiter({
    maxRequests: 100,
  });

  async function authenticatedRequest(apiKey, ip) {
    // Simulate authentication
    const isAuthenticated = apiKey?.startsWith('valid-');

    if (!isAuthenticated) {
      return { error: 'Unauthorized', status: 401 };
    }

    // Check rate limit
    const rateLimitResult = limiter.check({ ip, apiKey });

    if (!rateLimitResult.allowed) {
      return {
        error: 'Rate limit exceeded',
        status: 429,
        retryAfter: rateLimitResult.retryAfter,
      };
    }

    return {
      success: true,
      status: 200,
      remaining: rateLimitResult.remaining,
    };
  }

  console.log('Testing authenticated requests:');

  const r1 = await authenticatedRequest('invalid-key', '192.168.1.1');
  console.log(`  Invalid key: ${r1.status} - ${r1.error || 'success'}`);

  const r2 = await authenticatedRequest('valid-key-123', '192.168.1.1');
  console.log(`  Valid key: ${r2.status} - Remaining: ${r2.remaining}`);

  const r3 = await authenticatedRequest('valid-key-456', '192.168.1.1');
  console.log(`  Different key, same IP: ${r3.status} - Uses different bucket`);
  console.log();
}

// Example 10: Custom Limits Per Endpoint
console.log('10. Custom Limits Per Endpoint');
{
  const limiters = {
    auth: createRateLimiter({ maxRequests: 5, windowMs: 60000 }),
    api: createRateLimiter({ maxRequests: 100, windowMs: 60000 }),
    websocket: createRateLimiter({ maxRequests: 1000, windowMs: 60000 }),
  };

  function getRateLimiter(path) {
    if (path.startsWith('/auth')) return limiters.auth;
    if (path.startsWith('/ws')) return limiters.websocket;
    return limiters.api;
  }

  const endpoints = ['/auth/login', '/api/data', '/ws/connect'];

  console.log('Different limits per endpoint:');
  endpoints.forEach(endpoint => {
    const limiter = getRateLimiter(endpoint);
    const result = limiter.check({ ip: '192.168.1.1' });
    console.log(`  ${endpoint}: ${result.allowed ? '✅' : '❌'} (limit: ${limiter.maxRequests})`);
  });
  console.log();
}

console.log('=== All Examples Complete ===');
