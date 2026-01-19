/**
 * @file Security Configuration Schemas
 * @module @unrdf/daemon/middleware/security-schemas
 * @description Zod schemas for security middleware configuration
 */

import { z } from 'zod';

/**
 * Content Security Policy configuration schema
 */
export const CSPConfigSchema = z.object({
  defaultSrc: z.array(z.string()).default(["'self'"]),
  scriptSrc: z.array(z.string()).default(["'self'"]),
  styleSrc: z.array(z.string()).default(["'self'"]),
  imgSrc: z.array(z.string()).default(["'self'", 'data:', 'https:']),
  connectSrc: z.array(z.string()).default(["'self'"]),
  fontSrc: z.array(z.string()).default(["'self'"]),
  objectSrc: z.array(z.string()).default(["'none'"]),
  mediaSrc: z.array(z.string()).default(["'self'"]),
  frameSrc: z.array(z.string()).default(["'none'"]),
  reportUri: z.string().optional(),
  reportOnly: z.boolean().default(false),
});

/**
 * CORS configuration schema
 */
export const CORSConfigSchema = z.object({
  origin: z.union([
    z.string(),
    z.array(z.string()),
    z.function(),
  ]).default('*'),
  methods: z.array(z.string()).default(['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS']),
  allowedHeaders: z.array(z.string()).default(['Content-Type', 'Authorization', 'X-API-Key']),
  exposedHeaders: z.array(z.string()).default([]),
  credentials: z.boolean().default(false),
  maxAge: z.number().default(86400), // 24 hours
  preflightContinue: z.boolean().default(false),
});

/**
 * Request limits configuration schema
 */
export const RequestLimitsSchema = z.object({
  maxBodySize: z.number().default(10 * 1024 * 1024), // 10MB
  maxHeaderSize: z.number().default(8192), // 8KB
  maxUrlLength: z.number().default(2048), // 2KB
  timeout: z.number().default(30000), // 30 seconds
});

/**
 * Rate limiting configuration schema
 */
export const RateLimitConfigSchema = z.object({
  windowMs: z.number().default(60000), // 1 minute
  maxRequests: z.number().default(100),
  keyGenerator: z.function().optional(),
  skipSuccessfulRequests: z.boolean().default(false),
  skipFailedRequests: z.boolean().default(false),
});

/**
 * Security headers configuration schema
 */
export const SecurityHeadersConfigSchema = z.object({
  csp: CSPConfigSchema.optional(),
  cors: CORSConfigSchema.optional(),
  requestLimits: RequestLimitsSchema.optional(),
  rateLimit: RateLimitConfigSchema.optional(),
  enableHSTS: z.boolean().default(true),
  hstsMaxAge: z.number().default(31536000), // 1 year
  enableNoSniff: z.boolean().default(true),
  enableXFrameOptions: z.boolean().default(true),
  xFrameOptions: z.enum(['DENY', 'SAMEORIGIN']).default('DENY'),
  enableXSSProtection: z.boolean().default(true),
  enableReferrerPolicy: z.boolean().default(true),
  referrerPolicy: z.enum([
    'no-referrer',
    'no-referrer-when-downgrade',
    'origin',
    'origin-when-cross-origin',
    'same-origin',
    'strict-origin',
    'strict-origin-when-cross-origin',
    'unsafe-url',
  ]).default('strict-origin-when-cross-origin'),
  enablePermissionsPolicy: z.boolean().default(true),
  customHeaders: z.record(z.string(), z.string()).default({}),
});
