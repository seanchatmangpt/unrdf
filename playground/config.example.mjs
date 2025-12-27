/**
 * UNRDF Playground Configuration
 *
 * Copy this file to config.mjs and update the values for your environment
 */

export default {
  // Server Configuration
  env: process.env.NODE_ENV || 'development',
  port: process.env.PORT || 3000,
  host: process.env.HOST || 'localhost',

  // Security
  jwtSecret: process.env.JWT_SECRET || 'your-super-secret-jwt-key-change-this-in-production',
  sessionTimeout: process.env.SESSION_TIMEOUT || '24h',

  // WebSocket Configuration
  enableWebSockets: process.env.ENABLE_WEBSOCKETS !== 'false',

  // Rate Limiting
  enableRateLimiting: process.env.ENABLE_RATE_LIMITING === 'true',
  rateLimitWindowMs: parseInt(process.env.RATE_LIMIT_WINDOW_MS) || 900000, // 15 minutes
  rateLimitMaxRequests: parseInt(process.env.RATE_LIMIT_MAX_REQUESTS) || 1000,

  // CORS Configuration
  corsOrigin: process.env.CORS_ORIGIN || 'http://localhost:3000',

  // Database Configuration
  database: {
    type: process.env.DB_TYPE || 'sqlite',
    path: process.env.DB_PATH || './data/hooks.db',
    // PostgreSQL settings (if using PostgreSQL)
    host: process.env.DB_HOST || 'localhost',
    port: parseInt(process.env.DB_PORT) || 5432,
    name: process.env.DB_NAME || 'unrdf_hooks',
    username: process.env.DB_USER || 'admin',
    password: process.env.DB_PASSWORD || 'secret'
  },

  // Logging
  logLevel: process.env.LOG_LEVEL || 'info',
  logDestination: process.env.LOG_DESTINATION || 'console',

  // Performance Monitoring
  enablePerformanceMonitoring: process.env.ENABLE_PERFORMANCE_MONITORING !== 'false',
  metricsCollectionInterval: parseInt(process.env.METRICS_COLLECTION_INTERVAL) || 5000,

  // Trust Proxy (for reverse proxy deployments)
  trustProxy: process.env.TRUST_PROXY === 'true',

  // SSL/TLS (for HTTPS)
  ssl: {
    enabled: false,
    certPath: process.env.SSL_CERT_PATH || './certs/cert.pem',
    keyPath: process.env.SSL_KEY_PATH || './certs/key.pem'
  },

  // Development Settings
  debug: process.env.DEBUG || (process.env.NODE_ENV === 'development' ? '*' : ''),

  // Feature Flags
  features: {
    authentication: true,
    authorization: true,
    auditTrails: true,
    provenanceTracking: true,
    realTimeUpdates: true,
    performanceMetrics: true,
    dataPersistence: false, // Set to true when database integration is complete
    advancedTemplates: false // Set to true when template system is implemented
  }
}
