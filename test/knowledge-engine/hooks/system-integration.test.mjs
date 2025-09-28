/**
 * @file System Integration Tests
 * @module system-integration
 * 
 * @description
 * Tests for system integration scenarios including external service failures,
 * API rate limiting, network partitions, service discovery, and load balancer issues
 * in the knowledge hook system.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { writeFile, unlink, mkdir } from 'fs/promises';
import { defineHook } from '../../../src/knowledge-engine/define-hook.mjs';
import { KnowledgeHookManager } from '../../../src/knowledge-engine/knowledge-hook-manager.mjs';
import { Store } from 'n3';
import { join } from 'path';
import { tmpdir } from 'os';

// Mock condition evaluator at module level
vi.mock('../../../src/knowledge-engine/condition-evaluator.mjs', () => ({
  evaluateCondition: vi.fn().mockResolvedValue(true),
  createConditionEvaluator: vi.fn().mockReturnValue({
    evaluate: vi.fn().mockResolvedValue(true),
    isSatisfied: vi.fn().mockResolvedValue(true)
  }),
  validateCondition: vi.fn().mockReturnValue({ valid: true })
}));

// Mock HTTP client for external service calls
const mockHttpClient = {
  get: vi.fn(),
  post: vi.fn(),
  put: vi.fn(),
  delete: vi.fn(),
  request: vi.fn()
};

describe('System Integration', () => {
  let tempDir;
  let manager;
  let testStore;

  beforeEach(async () => {
    tempDir = join(tmpdir(), `unrdf-integration-test-${Date.now()}`);
    await require('fs/promises').mkdir(tempDir, { recursive: true });
    manager = new KnowledgeHookManager({ basePath: tempDir });
    testStore = new Store();
    
    // Reset mock implementations
    vi.clearAllMocks();
  });

  afterEach(async () => {
    try {
      await require('fs/promises').rm(tempDir, { recursive: true, force: true });
    } catch (error) {
      // Ignore cleanup errors
    }
  });

  describe('External Service Failures', () => {
    it('should handle external API service unavailability', async () => {
      const query = join(tempDir, 'external-api.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Mock external services
      const externalServices = {
        userService: {
          url: 'https://api.users.example.com',
          healthy: false,
          responseTime: Infinity
        },
        dataService: {
          url: 'https://api.data.example.com',
          healthy: true,
          responseTime: 150
        },
        authService: {
          url: 'https://api.auth.example.com',
          healthy: false,
          responseTime: 30000 // Timeout
        }
      };
      
      const hook = defineHook({
        meta: { name: 'external-api-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const serviceResults = [];
          const failedServices = [];
          const healthyServices = [];
          
          // Simulate calls to external services
          for (const [serviceName, service] of Object.entries(externalServices)) {
            const startTime = Date.now();
            
            try {
              if (!service.healthy) {
                throw new Error(`Service ${serviceName} is unavailable`);
              }
              
              if (service.responseTime > 5000) {
                throw new Error(`Service ${serviceName} timeout`);
              }
              
              // Simulate successful response
              await new Promise(resolve => setTimeout(resolve, service.responseTime));
              
              const result = {
                service: serviceName,
                url: service.url,
                status: 'success',
                responseTime: Date.now() - startTime,
                data: { message: `Response from ${serviceName}` }
              };
              
              serviceResults.push(result);
              healthyServices.push(serviceName);
              
            } catch (error) {
              const result = {
                service: serviceName,
                url: service.url,
                status: 'failed',
                responseTime: Date.now() - startTime,
                error: error.message
              };
              
              serviceResults.push(result);
              failedServices.push(serviceName);
            }
          }
          
          // Implement fallback strategies
          const fallbackStrategies = {
            userService: {
              strategy: 'cache',
              fallbackData: { users: ['cached-user-1', 'cached-user-2'] }
            },
            authService: {
              strategy: 'bypass',
              fallbackData: { authenticated: false, reason: 'auth-service-down' }
            }
          };
          
          const fallbackResults = [];
          for (const failedService of failedServices) {
            if (fallbackStrategies[failedService]) {
              fallbackResults.push({
                service: failedService,
                strategy: fallbackStrategies[failedService].strategy,
                data: fallbackStrategies[failedService].fallbackData
              });
            }
          }
          
          return {
            success: healthyServices.length > 0 || fallbackResults.length > 0,
            serviceResults,
            failedServices,
            healthyServices,
            fallbackResults,
            totalServices: Object.keys(externalServices).length,
            serviceAvailability: healthyServices.length / Object.keys(externalServices).length
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should handle external service failures with fallbacks
      expect(results[0].totalServices).toBe(3);
      expect(results[0].failedServices).toContain('userService');
      expect(results[0].failedServices).toContain('authService');
      expect(results[0].healthyServices).toContain('dataService');
      expect(results[0].fallbackResults).toHaveLength(2);
      expect(results[0].serviceAvailability).toBe(1/3);
    });

    it('should handle database connection failures', async () => {
      const query = join(tempDir, 'database-failure.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Mock database connections
      const databases = {
        primary: {
          host: 'db-primary.example.com',
          port: 5432,
          status: 'down',
          lastError: 'Connection refused'
        },
        replica1: {
          host: 'db-replica1.example.com',
          port: 5432,
          status: 'slow',
          responseTime: 8000
        },
        replica2: {
          host: 'db-replica2.example.com',
          port: 5432,
          status: 'healthy',
          responseTime: 200
        }
      };
      
      const hook = defineHook({
        meta: { name: 'database-failure-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const connectionAttempts = [];
          let activeConnection = null;
          
          // Try to connect to databases in priority order
          const connectionPriority = ['primary', 'replica1', 'replica2'];
          
          for (const dbName of connectionPriority) {
            const db = databases[dbName];
            const attempt = {
              database: dbName,
              host: db.host,
              port: db.port,
              timestamp: Date.now()
            };
            
            try {
              if (db.status === 'down') {
                throw new Error(db.lastError || 'Database unavailable');
              }
              
              if (db.status === 'slow' && db.responseTime > 5000) {
                throw new Error('Database response timeout');
              }
              
              // Simulate connection attempt
              await new Promise(resolve => setTimeout(resolve, db.responseTime || 100));
              
              attempt.status = 'success';
              attempt.responseTime = db.responseTime;
              activeConnection = {
                database: dbName,
                host: db.host,
                connectionTime: Date.now()
              };
              
              connectionAttempts.push(attempt);
              break; // Use first successful connection
              
            } catch (error) {
              attempt.status = 'failed';
              attempt.error = error.message;
              connectionAttempts.push(attempt);
            }
          }
          
          // Implement connection pooling and retry logic
          const connectionPool = {
            maxConnections: 10,
            activeConnections: activeConnection ? 1 : 0,
            failedConnections: connectionAttempts.filter(a => a.status === 'failed').length,
            retryAttempts: connectionAttempts.length,
            hasFailover: activeConnection?.database !== 'primary'
          };
          
          return {
            success: activeConnection !== null,
            activeConnection,
            connectionAttempts,
            connectionPool,
            databaseFailover: activeConnection?.database !== 'primary',
            totalAttempts: connectionAttempts.length
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should handle database failover
      expect(results[0].databaseFailover).toBe(true);
      expect(results[0].activeConnection.database).toBe('replica2');
      expect(results[0].totalAttempts).toBe(3);
      expect(results[0].connectionPool.hasFailover).toBe(true);
    });

    it('should handle third-party service integration failures', async () => {
      const query = join(tempDir, 'third-party-integration.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Mock third-party services
      const thirdPartyServices = {
        paymentGateway: {
          name: 'Stripe',
          endpoint: 'https://api.stripe.com',
          status: 'maintenance',
          maintenanceUntil: Date.now() + 3600000
        },
        emailService: {
          name: 'SendGrid',
          endpoint: 'https://api.sendgrid.com',
          status: 'rate-limited',
          rateLimitReset: Date.now() + 900000
        },
        smsService: {
          name: 'Twilio',
          endpoint: 'https://api.twilio.com',
          status: 'operational',
          responseTime: 300
        },
        analyticsService: {
          name: 'Google Analytics',
          endpoint: 'https://analytics.googleapis.com',
          status: 'degraded',
          successRate: 0.7
        }
      };
      
      const hook = defineHook({
        meta: { name: 'third-party-integration-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const integrationResults = [];
          const serviceStatus = {};
          
          for (const [serviceKey, service] of Object.entries(thirdPartyServices)) {
            const integration = {
              service: serviceKey,
              name: service.name,
              endpoint: service.endpoint,
              timestamp: Date.now()
            };
            
            try {
              switch (service.status) {
                case 'maintenance':
                  throw new Error(`${service.name} is under maintenance until ${new Date(service.maintenanceUntil).toISOString()}`);
                  
                case 'rate-limited':
                  throw new Error(`${service.name} rate limit exceeded, reset at ${new Date(service.rateLimitReset).toISOString()}`);
                  
                case 'operational':
                  await new Promise(resolve => setTimeout(resolve, service.responseTime));
                  integration.status = 'success';
                  integration.responseTime = service.responseTime;
                  break;
                  
                case 'degraded':
                  // Simulate intermittent failures
                  if (Math.random() > service.successRate) {
                    throw new Error(`${service.name} service degraded - request failed`);
                  }
                  integration.status = 'success';
                  integration.responseTime = 1000; // Slower than usual
                  integration.degraded = true;
                  break;
                  
                default:
                  throw new Error(`Unknown service status: ${service.status}`);
              }
              
            } catch (error) {
              integration.status = 'failed';
              integration.error = error.message;
            }
            
            integrationResults.push(integration);
            serviceStatus[serviceKey] = integration.status;
          }
          
          // Implement graceful degradation strategies
          const degradationStrategies = {
            paymentGateway: {
              fallback: 'queue-for-retry',
              impact: 'defer-payment-processing'
            },
            emailService: {
              fallback: 'alternative-provider',
              impact: 'use-backup-email-service'
            },
            smsService: {
              fallback: 'none',
              impact: 'sms-notifications-disabled'
            },
            analyticsService: {
              fallback: 'local-logging',
              impact: 'analytics-data-cached-locally'
            }
          };
          
          const activeStrategies = [];
          const successfulIntegrations = integrationResults.filter(r => r.status === 'success');
          const failedIntegrations = integrationResults.filter(r => r.status === 'failed');
          
          for (const failed of failedIntegrations) {
            if (degradationStrategies[failed.service]) {
              activeStrategies.push({
                service: failed.service,
                strategy: degradationStrategies[failed.service]
              });
            }
          }
          
          return {
            success: successfulIntegrations.length > 0,
            integrationResults,
            serviceStatus,
            successfulIntegrations: successfulIntegrations.length,
            failedIntegrations: failedIntegrations.length,
            activeStrategies,
            systemResilience: successfulIntegrations.length / integrationResults.length
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should handle third-party service failures with graceful degradation
      expect(results[0].integrationResults).toHaveLength(4);
      expect(results[0].activeStrategies.length).toBeGreaterThan(0);
      expect(results[0].systemResilience).toBeGreaterThan(0);
      expect(results[0].successfulIntegrations).toBeGreaterThan(0);
    });
  });

  describe('API Rate Limiting', () => {
    it('should handle API rate limit enforcement', async () => {
      const query = join(tempDir, 'rate-limiting.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Mock rate limiter
      const rateLimiter = {
        requests: [],
        limits: {
          perMinute: 60,
          perHour: 1000,
          perDay: 10000
        },
        windows: {
          minute: 60 * 1000,
          hour: 60 * 60 * 1000,
          day: 24 * 60 * 60 * 1000
        }
      };
      
      const checkRateLimit = (clientId = 'default') => {
        const now = Date.now();
        const clientRequests = rateLimiter.requests.filter(r => r.clientId === clientId);
        
        // Clean old requests
        rateLimiter.requests = rateLimiter.requests.filter(r => 
          now - r.timestamp < rateLimiter.windows.day
        );
        
        const recentRequests = {
          minute: clientRequests.filter(r => now - r.timestamp < rateLimiter.windows.minute).length,
          hour: clientRequests.filter(r => now - r.timestamp < rateLimiter.windows.hour).length,
          day: clientRequests.filter(r => now - r.timestamp < rateLimiter.windows.day).length
        };
        
        const isAllowed = {
          minute: recentRequests.minute < rateLimiter.limits.perMinute,
          hour: recentRequests.hour < rateLimiter.limits.perHour,
          day: recentRequests.day < rateLimiter.limits.perDay
        };
        
        const allowed = isAllowed.minute && isAllowed.hour && isAllowed.day;
        
        if (allowed) {
          rateLimiter.requests.push({
            clientId,
            timestamp: now
          });
        }
        
        return {
          allowed,
          recentRequests,
          limits: rateLimiter.limits,
          resetTimes: {
            minute: Math.ceil((now + rateLimiter.windows.minute) / rateLimiter.windows.minute) * rateLimiter.windows.minute,
            hour: Math.ceil((now + rateLimiter.windows.hour) / rateLimiter.windows.hour) * rateLimiter.windows.hour,
            day: Math.ceil((now + rateLimiter.windows.day) / rateLimiter.windows.day) * rateLimiter.windows.day
          }
        };
      };
      
      const hook = defineHook({
        meta: { name: 'rate-limiting-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const requestResults = [];
          const clientIds = ['client-1', 'client-2', 'bulk-client'];
          
          // Simulate different request patterns
          const requestPatterns = {
            'client-1': { count: 10, interval: 1000 }, // Normal usage
            'client-2': { count: 5, interval: 500 },   // Light usage
            'bulk-client': { count: 100, interval: 100 } // Heavy usage - should hit limits
          };
          
          for (const [clientId, pattern] of Object.entries(requestPatterns)) {
            const clientResults = [];
            
            for (let i = 0; i < pattern.count; i++) {
              const rateLimitCheck = checkRateLimit(clientId);
              
              const request = {
                clientId,
                requestNumber: i + 1,
                timestamp: Date.now(),
                allowed: rateLimitCheck.allowed,
                recentRequests: rateLimitCheck.recentRequests,
                resetTimes: rateLimitCheck.resetTimes
              };
              
              if (!rateLimitCheck.allowed) {
                request.rateLimited = true;
                request.reason = 'Rate limit exceeded';
                
                // Determine which limit was hit
                if (rateLimitCheck.recentRequests.minute >= rateLimiter.limits.perMinute) {
                  request.limitType = 'per-minute';
                  request.resetTime = rateLimitCheck.resetTimes.minute;
                } else if (rateLimitCheck.recentRequests.hour >= rateLimiter.limits.perHour) {
                  request.limitType = 'per-hour';
                  request.resetTime = rateLimitCheck.resetTimes.hour;
                } else if (rateLimitCheck.recentRequests.day >= rateLimiter.limits.perDay) {
                  request.limitType = 'per-day';
                  request.resetTime = rateLimitCheck.resetTimes.day;
                }
              }
              
              clientResults.push(request);
              
              // Small delay between requests
              if (i < pattern.count - 1) {
                await new Promise(resolve => setTimeout(resolve, pattern.interval));
              }
            }
            
            requestResults.push({
              clientId,
              pattern,
              requests: clientResults,
              totalRequests: clientResults.length,
              allowedRequests: clientResults.filter(r => r.allowed).length,
              rateLimitedRequests: clientResults.filter(r => r.rateLimited).length,
              rateLimitHit: clientResults.some(r => r.rateLimited)
            });
          }
          
          const totalRequests = requestResults.reduce((sum, r) => sum + r.totalRequests, 0);
          const totalAllowed = requestResults.reduce((sum, r) => sum + r.allowedRequests, 0);
          const totalRateLimited = requestResults.reduce((sum, r) => sum + r.rateLimitedRequests, 0);
          
          return {
            success: true,
            requestResults,
            summary: {
              totalRequests,
              totalAllowed,
              totalRateLimited,
              rateLimitingEffective: totalRateLimited > 0,
              allowanceRate: totalAllowed / totalRequests
            },
            rateLimiter: {
              totalTrackedRequests: rateLimiter.requests.length,
              limits: rateLimiter.limits
            }
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should enforce rate limiting
      expect(results[0].summary.rateLimitingEffective).toBe(true);
      expect(results[0].summary.totalRateLimited).toBeGreaterThan(0);
      expect(results[0].requestResults.some(r => r.rateLimitHit)).toBe(true);
      
      // Bulk client should hit rate limits
      const bulkClientResult = results[0].requestResults.find(r => r.clientId === 'bulk-client');
      expect(bulkClientResult.rateLimitHit).toBe(true);
    });

    it('should implement rate limiting with different strategies', async () => {
      const query = join(tempDir, 'rate-limit-strategies.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Different rate limiting strategies
      const rateLimitStrategies = {
        fixedWindow: {
          name: 'Fixed Window',
          windowSize: 60000, // 1 minute
          maxRequests: 10,
          requests: []
        },
        slidingWindow: {
          name: 'Sliding Window',
          windowSize: 60000, // 1 minute
          maxRequests: 10,
          requests: []
        },
        tokenBucket: {
          name: 'Token Bucket',
          capacity: 10,
          refillRate: 1, // tokens per second
          tokens: 10,
          lastRefill: Date.now()
        },
        leakyBucket: {
          name: 'Leaky Bucket',
          capacity: 10,
          leakRate: 1, // requests per second
          queue: [],
          lastLeak: Date.now()
        }
      };
      
      const checkFixedWindow = (strategy, clientId) => {
        const now = Date.now();
        const windowStart = Math.floor(now / strategy.windowSize) * strategy.windowSize;
        
        const windowRequests = strategy.requests.filter(r => 
          r.clientId === clientId && r.timestamp >= windowStart
        );
        
        const allowed = windowRequests.length < strategy.maxRequests;
        
        if (allowed) {
          strategy.requests.push({ clientId, timestamp: now });
        }
        
        return { allowed, windowRequests: windowRequests.length, windowStart };
      };
      
      const checkSlidingWindow = (strategy, clientId) => {
        const now = Date.now();
        
        // Remove old requests
        strategy.requests = strategy.requests.filter(r => 
          now - r.timestamp < strategy.windowSize
        );
        
        const clientRequests = strategy.requests.filter(r => r.clientId === clientId);
        const allowed = clientRequests.length < strategy.maxRequests;
        
        if (allowed) {
          strategy.requests.push({ clientId, timestamp: now });
        }
        
        return { allowed, recentRequests: clientRequests.length };
      };
      
      const checkTokenBucket = (strategy, clientId) => {
        const now = Date.now();
        const timeSinceRefill = (now - strategy.lastRefill) / 1000;
        
        // Refill tokens
        const tokensToAdd = Math.floor(timeSinceRefill * strategy.refillRate);
        strategy.tokens = Math.min(strategy.capacity, strategy.tokens + tokensToAdd);
        strategy.lastRefill = now;
        
        const allowed = strategy.tokens > 0;
        
        if (allowed) {
          strategy.tokens--;
        }
        
        return { allowed, tokens: strategy.tokens, capacity: strategy.capacity };
      };
      
      const checkLeakyBucket = (strategy, clientId) => {
        const now = Date.now();
        const timeSinceLeak = (now - strategy.lastLeak) / 1000;
        
        // Process leaked requests
        const requestsToLeak = Math.floor(timeSinceLeak * strategy.leakRate);
        for (let i = 0; i < requestsToLeak && strategy.queue.length > 0; i++) {
          strategy.queue.shift();
        }
        strategy.lastLeak = now;
        
        const allowed = strategy.queue.length < strategy.capacity;
        
        if (allowed) {
          strategy.queue.push({ clientId, timestamp: now });
        }
        
        return { allowed, queueSize: strategy.queue.length, capacity: strategy.capacity };
      };
      
      const hook = defineHook({
        meta: { name: 'rate-limit-strategies-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const strategyResults = {};
          const clientId = 'test-client';
          const requestCount = 15; // More than the limit to test rate limiting
          
          // Test each strategy
          for (const [strategyName, strategy] of Object.entries(rateLimitStrategies)) {
            const results = [];
            
            for (let i = 0; i < requestCount; i++) {
              let checkResult;
              
              switch (strategyName) {
                case 'fixedWindow':
                  checkResult = checkFixedWindow(strategy, clientId);
                  break;
                case 'slidingWindow':
                  checkResult = checkSlidingWindow(strategy, clientId);
                  break;
                case 'tokenBucket':
                  checkResult = checkTokenBucket(strategy, clientId);
                  break;
                case 'leakyBucket':
                  checkResult = checkLeakyBucket(strategy, clientId);
                  break;
              }
              
              results.push({
                requestNumber: i + 1,
                timestamp: Date.now(),
                allowed: checkResult.allowed,
                details: checkResult
              });
              
              // Small delay between requests
              await new Promise(resolve => setTimeout(resolve, 100));
            }
            
            strategyResults[strategyName] = {
              strategy: strategy.name,
              results,
              totalRequests: results.length,
              allowedRequests: results.filter(r => r.allowed).length,
              deniedRequests: results.filter(r => !r.allowed).length,
              allowanceRate: results.filter(r => r.allowed).length / results.length
            };
          }
          
          // Compare strategy effectiveness
          const comparison = Object.entries(strategyResults).map(([name, result]) => ({
            strategy: name,
            allowanceRate: result.allowanceRate,
            deniedRequests: result.deniedRequests,
            effectiveness: result.deniedRequests > 0 ? 'effective' : 'ineffective'
          }));
          
          return {
            success: true,
            strategyResults,
            comparison,
            requestCount,
            allStrategiesEffective: comparison.every(c => c.effectiveness === 'effective')
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should test different rate limiting strategies
      expect(results[0].strategyResults).toHaveProperty('fixedWindow');
      expect(results[0].strategyResults).toHaveProperty('slidingWindow');
      expect(results[0].strategyResults).toHaveProperty('tokenBucket');
      expect(results[0].strategyResults).toHaveProperty('leakyBucket');
      
      // All strategies should be effective (deny some requests)
      expect(results[0].allStrategiesEffective).toBe(true);
      expect(results[0].comparison.every(c => c.deniedRequests > 0)).toBe(true);
    });
  });

  describe('Network Partition Scenarios', () => {
    it('should handle network partitions and split-brain scenarios', async () => {
      const query = join(tempDir, 'network-partition.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Simulate network topology
      const networkNodes = {
        'node-1': { region: 'us-east', healthy: true, connections: ['node-2', 'node-3'] },
        'node-2': { region: 'us-west', healthy: true, connections: ['node-1', 'node-4'] },
        'node-3': { region: 'eu-central', healthy: true, connections: ['node-1', 'node-4'] },
        'node-4': { region: 'ap-southeast', healthy: false, connections: ['node-2', 'node-3'] }
      };
      
      // Simulate network partition
      const networkPartition = {
        partition1: ['node-1', 'node-2'], // US nodes
        partition2: ['node-3'], // EU node (isolated)
        unreachable: ['node-4'] // AP node (down)
      };
      
      const hook = defineHook({
        meta: { name: 'network-partition-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const partitionAnalysis = {
            totalNodes: Object.keys(networkNodes).length,
            partitions: [],
            splitBrainRisk: false,
            quorumStatus: {},
            networkHealth: {}
          };
          
          // Analyze each partition
          for (const [partitionName, nodes] of Object.entries(networkPartition)) {
            if (partitionName === 'unreachable') continue;
            
            const partition = {
              name: partitionName,
              nodes: nodes,
              size: nodes.length,
              regions: [...new Set(nodes.map(nodeId => networkNodes[nodeId]?.region))],
              healthyNodes: nodes.filter(nodeId => networkNodes[nodeId]?.healthy),
              canFormQuorum: nodes.length >= Math.ceil(partitionAnalysis.totalNodes / 2)
            };
            
            partitionAnalysis.partitions.push(partition);
          }
          
          // Detect split-brain scenario
          const quorumPartitions = partitionAnalysis.partitions.filter(p => p.canFormQuorum);
          partitionAnalysis.splitBrainRisk = quorumPartitions.length > 1;
          
          // Determine quorum status
          if (quorumPartitions.length === 1) {
            partitionAnalysis.quorumStatus = {
              hasQuorum: true,
              quorumPartition: quorumPartitions[0].name,
              leaderElection: 'possible'
            };
          } else if (quorumPartitions.length > 1) {
            partitionAnalysis.quorumStatus = {
              hasQuorum: false,
              splitBrain: true,
              conflictingPartitions: quorumPartitions.map(p => p.name)
            };
          } else {
            partitionAnalysis.quorumStatus = {
              hasQuorum: false,
              reason: 'no-partition-has-quorum'
            };
          }
          
          // Test network connectivity
          const connectivityTests = [];
          for (const [nodeId, node] of Object.entries(networkNodes)) {
            const connectivity = {
              nodeId,
              region: node.region,
              healthy: node.healthy,
              reachableNodes: []
            };
            
            if (node.healthy) {
              // Test connections to other nodes
              for (const targetNodeId of node.connections) {
                const targetNode = networkNodes[targetNodeId];
                const samePartition = Object.values(networkPartition).some(partition => 
                  partition.includes(nodeId) && partition.includes(targetNodeId)
                );
                
                connectivity.reachableNodes.push({
                  nodeId: targetNodeId,
                  reachable: samePartition && targetNode.healthy,
                  reason: !samePartition ? 'network-partition' : 
                         !targetNode.healthy ? 'node-unhealthy' : 'reachable'
                });
              }
            }
            
            connectivityTests.push(connectivity);
          }
          
          // Calculate network health metrics
          const totalPossibleConnections = Object.values(networkNodes)
            .reduce((sum, node) => sum + node.connections.length, 0);
          
          const activeConnections = connectivityTests
            .reduce((sum, test) => 
              sum + test.reachableNodes.filter(n => n.reachable).length, 0);
          
          partitionAnalysis.networkHealth = {
            totalPossibleConnections,
            activeConnections,
            connectivityRate: activeConnections / totalPossibleConnections,
            partitionedNodes: networkPartition.partition2.length + networkPartition.unreachable.length,
            healthyPartitions: partitionAnalysis.partitions.filter(p => p.healthyNodes.length > 0).length
          };
          
          return {
            success: true,
            partitionAnalysis,
            connectivityTests,
            networkPartitionDetected: partitionAnalysis.partitions.length > 1,
            splitBrainRisk: partitionAnalysis.splitBrainRisk,
            hasQuorum: partitionAnalysis.quorumStatus.hasQuorum
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should detect network partition
      expect(results[0].networkPartitionDetected).toBe(true);
      expect(results[0].partitionAnalysis.partitions).toHaveLength(2);
      expect(results[0].splitBrainRisk).toBe(true); // Both partitions can form quorum
      expect(results[0].partitionAnalysis.networkHealth.connectivityRate).toBeLessThan(1);
    });

    it('should handle network latency and timeout scenarios', async () => {
      const query = join(tempDir, 'network-latency.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Simulate network conditions
      const networkConditions = {
        'low-latency': { latency: 10, jitter: 2, packetLoss: 0.001 },
        'high-latency': { latency: 500, jitter: 100, packetLoss: 0.01 },
        'unstable': { latency: 200, jitter: 150, packetLoss: 0.05 },
        'congested': { latency: 300, jitter: 50, packetLoss: 0.02 }
      };
      
      const simulateNetworkCall = async (condition, timeoutMs = 1000) => {
        const { latency, jitter, packetLoss } = condition;
        
        // Simulate packet loss
        if (Math.random() < packetLoss) {
          throw new Error('Packet loss - request failed');
        }
        
        // Calculate actual latency with jitter
        const actualLatency = latency + (Math.random() - 0.5) * jitter * 2;
        
        // Check for timeout
        if (actualLatency > timeoutMs) {
          throw new Error(`Request timeout - ${actualLatency}ms > ${timeoutMs}ms`);
        }
        
        // Simulate network delay
        await new Promise(resolve => setTimeout(resolve, actualLatency));
        
        return {
          success: true,
          latency: actualLatency,
          timeout: timeoutMs
        };
      };
      
      const hook = defineHook({
        meta: { name: 'network-latency-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const latencyResults = {};
          const timeoutScenarios = [100, 250, 500, 1000, 2000]; // Different timeout values
          
          for (const [conditionName, condition] of Object.entries(networkConditions)) {
            const conditionResults = {
              condition: conditionName,
              parameters: condition,
              timeoutTests: []
            };
            
            for (const timeout of timeoutScenarios) {
              const testResults = [];
              const testCount = 10; // Run multiple tests for each timeout
              
              for (let i = 0; i < testCount; i++) {
                try {
                  const result = await simulateNetworkCall(condition, timeout);
                  testResults.push({
                    attempt: i + 1,
                    success: true,
                    latency: result.latency,
                    timeout
                  });
                } catch (error) {
                  testResults.push({
                    attempt: i + 1,
                    success: false,
                    error: error.message,
                    timeout
                  });
                }
              }
              
              const successfulTests = testResults.filter(t => t.success);
              const failedTests = testResults.filter(t => !t.success);
              const avgLatency = successfulTests.length > 0 ?
                successfulTests.reduce((sum, t) => sum + t.latency, 0) / successfulTests.length : 0;
              
              conditionResults.timeoutTests.push({
                timeout,
                totalTests: testCount,
                successfulTests: successfulTests.length,
                failedTests: failedTests.length,
                successRate: successfulTests.length / testCount,
                averageLatency: avgLatency,
                timeoutRate: failedTests.filter(t => t.error.includes('timeout')).length / testCount,
                packetLossRate: failedTests.filter(t => t.error.includes('Packet loss')).length / testCount
              });
            }
            
            latencyResults[conditionName] = conditionResults;
          }
          
          // Analyze network resilience
          const resilenceAnalysis = {
            bestCondition: null,
            worstCondition: null,
            optimalTimeouts: {},
            networkRecommendations: []
          };
          
          let bestSuccessRate = 0;
          let worstSuccessRate = 1;
          
          for (const [conditionName, result] of Object.entries(latencyResults)) {
            // Find best timeout for each condition (highest success rate)
            const bestTimeout = result.timeoutTests.reduce((best, test) => 
              test.successRate > best.successRate ? test : best
            );
            
            resilenceAnalysis.optimalTimeouts[conditionName] = {
              timeout: bestTimeout.timeout,
              successRate: bestTimeout.successRate
            };
            
            if (bestTimeout.successRate > bestSuccessRate) {
              bestSuccessRate = bestTimeout.successRate;
              resilenceAnalysis.bestCondition = conditionName;
            }
            
            if (bestTimeout.successRate < worstSuccessRate) {
              worstSuccessRate = bestTimeout.successRate;
              resilenceAnalysis.worstCondition = conditionName;
            }
            
            // Generate recommendations
            if (bestTimeout.successRate < 0.9) {
              resilenceAnalysis.networkRecommendations.push({
                condition: conditionName,
                issue: 'low-success-rate',
                recommendation: `Increase timeout beyond ${bestTimeout.timeout}ms or implement retry logic`
              });
            }
            
            if (bestTimeout.packetLossRate > 0.02) {
              resilenceAnalysis.networkRecommendations.push({
                condition: conditionName,
                issue: 'high-packet-loss',
                recommendation: 'Implement packet loss detection and recovery mechanisms'
              });
            }
          }
          
          return {
            success: true,
            latencyResults,
            resilenceAnalysis,
            handlesVariableLatency: Object.values(latencyResults).every(r => 
              r.timeoutTests.some(t => t.successRate > 0.8)
            ),
            adaptiveTimeouts: resilenceAnalysis.networkRecommendations.length > 0
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should handle various network conditions
      expect(results[0].latencyResults).toHaveProperty('low-latency');
      expect(results[0].latencyResults).toHaveProperty('high-latency');
      expect(results[0].latencyResults).toHaveProperty('unstable');
      expect(results[0].latencyResults).toHaveProperty('congested');
      
      // Should provide network resilience analysis
      expect(results[0].resilenceAnalysis.bestCondition).toBe('low-latency');
      expect(results[0].resilenceAnalysis.worstCondition).toBeDefined();
      expect(results[0].handlesVariableLatency).toBe(true);
    });
  });

  describe('Service Discovery Failures', () => {
    it('should handle service discovery and registration failures', async () => {
      const query = join(tempDir, 'service-discovery.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Mock service registry
      const serviceRegistry = {
        services: new Map(),
        healthChecks: new Map(),
        lastUpdate: Date.now()
      };
      
      // Register some services
      const services = [
        { id: 'auth-service-1', name: 'auth-service', host: '192.168.1.10', port: 8080, healthy: true },
        { id: 'auth-service-2', name: 'auth-service', host: '192.168.1.11', port: 8080, healthy: false },
        { id: 'data-service-1', name: 'data-service', host: '192.168.1.20', port: 9090, healthy: true },
        { id: 'cache-service-1', name: 'cache-service', host: '192.168.1.30', port: 6379, healthy: true }
      ];
      
      services.forEach(service => {
        serviceRegistry.services.set(service.id, service);
        serviceRegistry.healthChecks.set(service.id, {
          lastCheck: Date.now(),
          healthy: service.healthy,
          consecutiveFailures: service.healthy ? 0 : 3
        });
      });
      
      const discoverService = (serviceName) => {
        const matchingServices = Array.from(serviceRegistry.services.values())
          .filter(s => s.name === serviceName);
        
        const healthyServices = matchingServices.filter(s => {
          const healthCheck = serviceRegistry.healthChecks.get(s.id);
          return healthCheck?.healthy && healthCheck.consecutiveFailures < 3;
        });
        
        return {
          serviceName,
          totalInstances: matchingServices.length,
          healthyInstances: healthyServices.length,
          instances: healthyServices,
          available: healthyServices.length > 0
        };
      };
      
      const hook = defineHook({
        meta: { name: 'service-discovery-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const discoveryResults = {};
          const requiredServices = ['auth-service', 'data-service', 'cache-service', 'nonexistent-service'];
          
          // Test service discovery for each required service
          for (const serviceName of requiredServices) {
            const discovery = discoverService(serviceName);
            discoveryResults[serviceName] = discovery;
          }
          
          // Simulate service registry failures
          const registryFailureScenarios = [
            {
              name: 'registry-unavailable',
              simulate: () => {
                throw new Error('Service registry is unavailable');
              }
            },
            {
              name: 'stale-data',
              simulate: () => {
                // Return stale service data
                return {
                  serviceName: 'stale-service',
                  totalInstances: 1,
                  healthyInstances: 1,
                  instances: [{ id: 'stale-1', host: '192.168.1.99', port: 8080, lastSeen: Date.now() - 300000 }],
                  available: true,
                  stale: true
                };
              }
            },
            {
              name: 'partial-failure',
              simulate: () => {
                // Some services available, others not
                const partialResults = {};
                const availableServices = ['auth-service'];
                const unavailableServices = ['data-service', 'cache-service'];
                
                availableServices.forEach(service => {
                  partialResults[service] = discoverService(service);
                });
                
                unavailableServices.forEach(service => {
                  partialResults[service] = {
                    serviceName: service,
                    error: 'Service registry partition - service data unavailable'
                  };
                });
                
                return partialResults;
              }
            }
          ];
          
          const failureResults = {};
          
          for (const scenario of registryFailureScenarios) {
            try {
              const result = scenario.simulate();
              failureResults[scenario.name] = {
                scenario: scenario.name,
                success: true,
                result
              };
            } catch (error) {
              failureResults[scenario.name] = {
                scenario: scenario.name,
                success: false,
                error: error.message
              };
            }
          }
          
          // Implement fallback strategies
          const fallbackStrategies = {
            'auth-service': {
              strategy: 'static-config',
              fallbackInstances: [
                { host: '192.168.1.100', port: 8080, source: 'static-config' }
              ]
            },
            'data-service': {
              strategy: 'dns-resolution',
              fallbackInstances: [
                { host: 'data-service.internal', port: 9090, source: 'dns' }
              ]
            },
            'cache-service': {
              strategy: 'local-cache',
              fallbackInstances: [
                { host: 'localhost', port: 6379, source: 'local-fallback' }
              ]
            }
          };
          
          const fallbackResults = {};
          
          // Apply fallbacks for unavailable services
          for (const [serviceName, discovery] of Object.entries(discoveryResults)) {
            if (!discovery.available && fallbackStrategies[serviceName]) {
              fallbackResults[serviceName] = {
                originalDiscovery: discovery,
                fallbackStrategy: fallbackStrategies[serviceName].strategy,
                fallbackInstances: fallbackStrategies[serviceName].fallbackInstances,
                fallbackApplied: true
              };
            }
          }
          
          // Calculate service availability metrics
          const availabilityMetrics = {
            totalServicesRequested: requiredServices.length,
            servicesAvailable: Object.values(discoveryResults).filter(d => d.available).length,
            servicesUnavailable: Object.values(discoveryResults).filter(d => !d.available).length,
            serviceAvailabilityRate: Object.values(discoveryResults).filter(d => d.available).length / requiredServices.length,
            fallbacksActivated: Object.keys(fallbackResults).length,
            totalServiceInstances: Object.values(discoveryResults).reduce((sum, d) => sum + (d.totalInstances || 0), 0),
            healthyServiceInstances: Object.values(discoveryResults).reduce((sum, d) => sum + (d.healthyInstances || 0), 0)
          };
          
          return {
            success: true,
            discoveryResults,
            failureResults,
            fallbackResults,
            availabilityMetrics,
            registryHealthy: serviceRegistry.services.size > 0,
            handlesServiceDiscoveryFailures: Object.keys(fallbackResults).length > 0
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should handle service discovery
      expect(results[0].discoveryResults).toHaveProperty('auth-service');
      expect(results[0].discoveryResults).toHaveProperty('data-service');
      expect(results[0].discoveryResults).toHaveProperty('cache-service');
      expect(results[0].discoveryResults).toHaveProperty('nonexistent-service');
      
      // Should implement fallback strategies
      expect(results[0].handlesServiceDiscoveryFailures).toBe(true);
      expect(results[0].fallbackResults).toBeDefined();
      expect(results[0].availabilityMetrics.serviceAvailabilityRate).toBeGreaterThan(0);
      
      // Nonexistent service should not be available
      expect(results[0].discoveryResults['nonexistent-service'].available).toBe(false);
    });
  });

  describe('Load Balancer Issues', () => {
    it('should handle load balancer failures and failover', async () => {
      const query = join(tempDir, 'load-balancer.sparql');
      await writeFile(query, 'SELECT * WHERE { ?s ?p ?o }');
      
      // Mock load balancer configuration
      const loadBalancers = {
        primary: {
          id: 'lb-primary',
          host: '192.168.1.5',
          healthy: false, // Primary LB is down
          algorithm: 'round-robin',
          backends: [
            { id: 'backend-1', host: '192.168.1.10', port: 8080, healthy: true, weight: 100 },
            { id: 'backend-2', host: '192.168.1.11', port: 8080, healthy: true, weight: 100 },
            { id: 'backend-3', host: '192.168.1.12', port: 8080, healthy: false, weight: 100 }
          ]
        },
        secondary: {
          id: 'lb-secondary',
          host: '192.168.1.6',
          healthy: true,
          algorithm: 'least-connections',
          backends: [
            { id: 'backend-4', host: '192.168.1.13', port: 8080, healthy: true, weight: 100 },
            { id: 'backend-5', host: '192.168.1.14', port: 8080, healthy: true, weight: 50 }
          ]
        }
      };
      
      const selectBackend = (loadBalancer, algorithm = 'round-robin') => {
        const healthyBackends = loadBalancer.backends.filter(b => b.healthy);
        
        if (healthyBackends.length === 0) {
          throw new Error('No healthy backends available');
        }
        
        switch (algorithm) {
          case 'round-robin':
            // Simple round-robin (would need state in real implementation)
            return healthyBackends[Math.floor(Math.random() * healthyBackends.length)];
            
          case 'least-connections':
            // Simulate least connections (randomly for demo)
            const connections = healthyBackends.map(b => ({ ...b, connections: Math.floor(Math.random() * 10) }));
            return connections.reduce((min, backend) => 
              backend.connections < min.connections ? backend : min
            );
            
          case 'weighted':
            // Weighted selection
            const totalWeight = healthyBackends.reduce((sum, b) => sum + b.weight, 0);
            const random = Math.random() * totalWeight;
            let currentWeight = 0;
            
            for (const backend of healthyBackends) {
              currentWeight += backend.weight;
              if (random <= currentWeight) {
                return backend;
              }
            }
            return healthyBackends[0];
            
          default:
            return healthyBackends[0];
        }
      };
      
      const hook = defineHook({
        meta: { name: 'load-balancer-hook' },
        when: {
          kind: 'sparql-ask',
          ref: {
            uri: `file://${query}`,
            sha256: 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3',
            mediaType: 'application/sparql-query'
          }
        },
        run: async (event) => {
          const loadBalancerResults = {};
          const failoverResults = [];
          
          // Test each load balancer
          for (const [lbName, lb] of Object.entries(loadBalancers)) {
            const lbResult = {
              loadBalancer: lbName,
              id: lb.id,
              host: lb.host,
              healthy: lb.healthy,
              algorithm: lb.algorithm,
              totalBackends: lb.backends.length,
              healthyBackends: lb.backends.filter(b => b.healthy).length,
              backendSelections: []
            };
            
            if (lb.healthy) {
              try {
                // Test backend selection multiple times
                for (let i = 0; i < 10; i++) {
                  const selectedBackend = selectBackend(lb, lb.algorithm);
                  lbResult.backendSelections.push({
                    attempt: i + 1,
                    selectedBackend: selectedBackend.id,
                    backendHost: selectedBackend.host,
                    backendHealthy: selectedBackend.healthy
                  });
                }
                
                lbResult.success = true;
                lbResult.averageResponseTime = 150 + Math.random() * 100; // Simulate
                
              } catch (error) {
                lbResult.success = false;
                lbResult.error = error.message;
              }
            } else {
              lbResult.success = false;
              lbResult.error = 'Load balancer is unhealthy';
            }
            
            loadBalancerResults[lbName] = lbResult;
          }
          
          // Implement load balancer failover
          const primaryLB = loadBalancerResults.primary;
          const secondaryLB = loadBalancerResults.secondary;
          
          if (!primaryLB.success && secondaryLB.success) {
            failoverResults.push({
              type: 'lb-failover',
              from: 'primary',
              to: 'secondary',
              reason: primaryLB.error,
              timestamp: Date.now(),
              success: true
            });
            
            // Test traffic routing through secondary LB
            const failoverTraffic = {
              totalRequests: 100,
              routedToSecondary: 100,
              routedToPrimary: 0,
              failoverLatency: 50 // ms to detect and switch
            };
            
            failoverResults.push({
              type: 'traffic-routing',
              trafficDistribution: failoverTraffic,
              success: true
            });
          }
          
          // Test backend health checking
          const healthCheckResults = {};
          
          for (const [lbName, lb] of Object.entries(loadBalancers)) {
            const healthChecks = [];
            
            for (const backend of lb.backends) {
              const healthCheck = {
                backendId: backend.id,
                host: backend.host,
                port: backend.port,
                timestamp: Date.now(),
                healthy: backend.healthy,
                responseTime: backend.healthy ? 50 + Math.random() * 100 : null,
                error: backend.healthy ? null : 'Connection refused'
              };
              
              healthChecks.push(healthCheck);
            }
            
            healthCheckResults[lbName] = {
              loadBalancer: lbName,
              healthChecks,
              totalBackends: healthChecks.length,
              healthyBackends: healthChecks.filter(hc => hc.healthy).length,
              unhealthyBackends: healthChecks.filter(hc => !hc.healthy).length,
              averageResponseTime: healthChecks
                .filter(hc => hc.responseTime)
                .reduce((sum, hc, _, arr) => sum + hc.responseTime / arr.length, 0)
            };
          }
          
          // Calculate overall system health
          const systemHealth = {
            totalLoadBalancers: Object.keys(loadBalancers).length,
            healthyLoadBalancers: Object.values(loadBalancerResults).filter(r => r.success).length,
            totalBackends: Object.values(loadBalancers).reduce((sum, lb) => sum + lb.backends.length, 0),
            healthyBackends: Object.values(loadBalancers).reduce((sum, lb) => 
              sum + lb.backends.filter(b => b.healthy).length, 0),
            hasFailover: failoverResults.length > 0,
            systemAvailable: Object.values(loadBalancerResults).some(r => r.success)
          };
          
          systemHealth.loadBalancerAvailability = systemHealth.healthyLoadBalancers / systemHealth.totalLoadBalancers;
          systemHealth.backendAvailability = systemHealth.healthyBackends / systemHealth.totalBackends;
          
          return {
            success: systemHealth.systemAvailable,
            loadBalancerResults,
            failoverResults,
            healthCheckResults,
            systemHealth,
            handlesLoadBalancerFailure: failoverResults.some(r => r.type === 'lb-failover'),
            maintainsServiceAvailability: systemHealth.systemAvailable
          };
        }
      });

      manager.addKnowledgeHook(hook);

      const event = {
        name: 'test-event',
        payload: {},
        context: { graph: testStore }
      };

      const results = await manager.executeAllKnowledgeHooks(event);
      expect(results).toHaveLength(1);
      expect(results[0].success).toBe(true);
      
      // Should handle load balancer failures
      expect(results[0].loadBalancerResults.primary.success).toBe(false);
      expect(results[0].loadBalancerResults.secondary.success).toBe(true);
      
      // Should implement failover
      expect(results[0].handlesLoadBalancerFailure).toBe(true);
      expect(results[0].maintainsServiceAvailability).toBe(true);
      expect(results[0].failoverResults.length).toBeGreaterThan(0);
      
      // System health metrics should be calculated
      expect(results[0].systemHealth.loadBalancerAvailability).toBe(0.5); // 1 out of 2 LBs healthy
      expect(results[0].systemHealth.backendAvailability).toBeGreaterThan(0.5);
    });
  });
});
