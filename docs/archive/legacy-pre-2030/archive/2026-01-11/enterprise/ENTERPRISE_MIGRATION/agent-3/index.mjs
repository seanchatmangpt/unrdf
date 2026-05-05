/**
 * @fileoverview Enterprise Migration Canonical Schemas
 * Central export for all schema modules
 */

// DTO Schemas
export {
  validateMigrationRequest,
  validateDomainMigration,
  validateMigrationResponse,
  createEmptyStats,
  createDomainResult,
} from './dto-schemas.mjs';

// Error Schemas
export {
  ErrorCode,
  ErrorSeverity,
  MigrationError,
  deserializeError,
  createContractDriftError,
  createLensFailureError,
  createReceiptTamperError,
  createShadowMismatchError,
  createRoutingError,
  createValidationError,
  wrapError,
  isRecoverableError,
  isCriticalError,
  aggregateErrors,
} from './error-schemas.mjs';

// Logging Schemas
export {
  LogLevel,
  MigrationPhase,
  createLogEntry,
  debug,
  info,
  warn,
  error,
  fatal,
  performance,
  formatLogEntry,
  formatLogEntryJSON,
  validateLogEntry,
  createLogger,
  aggregateByPhase,
  calculatePhaseStats,
} from './logging-schemas.mjs';

// ID Rules
export {
  ID_PATTERNS,
  generateId,
  generateReceiptId,
  generateCapsuleId,
  generateDomainId,
  generateLensId,
  generateMigrationId,
  validateId,
  parseId,
  extractTimestamp,
  verifyId,
} from './id-rules.mjs';
