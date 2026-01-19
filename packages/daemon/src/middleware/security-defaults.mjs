/**
 * @file Default Security Configuration
 * @module @unrdf/daemon/middleware/security-defaults
 * @description Production-ready default security configuration
 */

/**
 * Default security configuration (production-ready)
 */
export const DEFAULT_SECURITY_CONFIG = {
  csp: {
    defaultSrc: ["'self'"],
    scriptSrc: ["'self'"],
    styleSrc: ["'self'", "'unsafe-inline'"], // For compatibility
    imgSrc: ["'self'", 'data:', 'https:'],
    connectSrc: ["'self'"],
    fontSrc: ["'self'"],
    objectSrc: ["'none'"],
    mediaSrc: ["'self'"],
    frameSrc: ["'none'"],
    reportOnly: false,
  },
  cors: {
    origin: ['http://localhost:3000', 'http://localhost:8080'],
    methods: ['GET', 'POST', 'PUT', 'DELETE'],
    allowedHeaders: ['Content-Type', 'Authorization', 'X-API-Key'],
    credentials: true,
    maxAge: 86400,
  },
  requestLimits: {
    maxBodySize: 10 * 1024 * 1024, // 10MB
    maxHeaderSize: 8192,
    maxUrlLength: 2048,
    timeout: 30000,
  },
  rateLimit: {
    windowMs: 60000,
    maxRequests: 100,
  },
  enableHSTS: true,
  hstsMaxAge: 31536000,
  enableNoSniff: true,
  enableXFrameOptions: true,
  xFrameOptions: 'DENY',
  enableXSSProtection: true,
  enableReferrerPolicy: true,
  referrerPolicy: 'strict-origin-when-cross-origin',
  enablePermissionsPolicy: true,
};
