/**
 * @file Rate Limiter Schemas
 * @module @unrdf/daemon/middleware/rate-limiter-schema
 * @description Zod validation schemas for rate limiting configuration and operations
 */

import { z } from 'zod';

/**
 * Rate limit configuration schema
 */
export const RateLimitConfigSchema = z.object({
  windowMs: z.number().int().positive().default(60000), // 1 minute
  maxRequests: z.number().int().positive().default(100),
  burstSize: z.number().int().positive().default(10),
  burstWindowMs: z.number().int().positive().default(1000), // 1 second
  keyPrefix: z.string().default('ratelimit'),
  enablePerIp: z.boolean().default(true),
  enablePerApiKey: z.boolean().default(true),
  storageMaxSize: z.number().int().positive().default(10000),
  logger: z.any().optional(),
});

/**
 * Rate limit bucket state schema
 */
export const BucketStateSchema = z.object({
  tokens: z.number(),
  lastRefill: z.number(),
  requestCount: z.number().int().nonnegative(),
  firstRequest: z.number().optional(),
});

/**
 * Rate limit result schema
 */
export const RateLimitResultSchema = z.object({
  allowed: z.boolean(),
  remaining: z.number().int().nonnegative(),
  resetAt: z.number(),
  retryAfter: z.number().int().nonnegative().optional(),
  identifier: z.string(),
  reason: z.string().optional(),
});

/**
 * Rate limit context schema
 */
export const RateLimitContextSchema = z.object({
  ip: z.string().optional(),
  apiKey: z.string().optional(),
  path: z.string().optional(),
  method: z.string().optional(),
});

/**
 * Rate limit stats schema
 */
export const RateLimitStatsSchema = z.object({
  totalRequests: z.number().int().nonnegative(),
  allowedRequests: z.number().int().nonnegative(),
  blockedRequests: z.number().int().nonnegative(),
  uniqueIdentifiers: z.number().int().nonnegative(),
  cacheSize: z.number().int().nonnegative(),
  blockRate: z.number().min(0).max(1),
});
