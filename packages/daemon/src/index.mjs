/**
 * @file UNRDF Daemon Package
 * @module @unrdf/daemon
 * @description Background daemon for scheduled tasks and event-driven operations
 */

export { Daemon, Daemon as UnrdfDaemon } from './daemon.mjs';
export {
  TriggerEvaluator,
  evaluateTrigger,
  shouldExecuteIdle,
  calculateNextExecutionTime,
  isValidTrigger,
} from './trigger-evaluator.mjs';

// MCP exports
export { createMCPServer, startMCPServer } from './mcp/index.mjs';

export {
  ApiKeySchema,
  ApiKeyHashSchema,
  AuthConfigSchema,
  RetryPolicySchema,
  TriggerSchema,
  ScheduledOperationSchema,
  DaemonConfigSchema,
  OperationReceiptSchema,
  DaemonHealthSchema,
  DaemonMetricsSchema,
} from './schemas.mjs';

export {
  integrateRaftNode,
  distributeWork,
} from './integrations/distributed.mjs';

export { DistributedTaskDistributor } from './integrations/task-distributor.mjs';

export {
  DaemonDeltaGate,
  DeltaContractSchema,
  DeltaOperationSchema,
  DeltaReceiptSchema,
  HealthStatusSchema,
} from './integrations/v6-deltagate.mjs';

// Authentication exports
export {
  ApiKeyAuthenticator,
  createAuthMiddleware,
  createAuthenticator,
} from './auth/api-key-auth.mjs';

export {
  generateSecureApiKey,
  hashApiKey,
  verifyApiKey,
  generateApiKeyPair,
} from './auth/crypto-utils.mjs';

// Rate Limiting exports
export {
  TokenBucketRateLimiter,
  createRateLimitMiddleware,
  createRateLimiter,
  parseEnvConfig,
} from './middleware/rate-limiter.mjs';

export {
  RateLimitConfigSchema,
  BucketStateSchema,
  RateLimitResultSchema,
  RateLimitContextSchema,
  RateLimitStatsSchema,
} from './middleware/rate-limiter.schema.mjs';

// Security Middleware exports
export {
  SecurityHeadersMiddleware,
  createSecurityMiddleware,
  DEFAULT_SECURITY_CONFIG,
  CSPConfigSchema,
  CORSConfigSchema,
  RequestLimitsSchema,
  SecurityHeadersConfigSchema,
} from './middleware/security-headers.mjs';
