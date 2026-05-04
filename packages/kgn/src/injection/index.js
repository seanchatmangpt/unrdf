/**
 * KGEN Injection Operations System
 *
 * Provides atomic, idempotent, and deterministic code injection capabilities
 * for modifying existing files without complete overwriting.
 *
 * Key Features:
 * - Atomic operations with rollback capability
 * - Idempotent with skipIf conditions
 * - Deterministic behavior across runs
 * - Marker-based precise targeting
 * - Multiple injection modes (append, prepend, before, after, replace)
 * - Comprehensive validation and error handling
 */

export { InjectionEngine } from './injection-engine.js';
export { InjectionModes } from './modes/index.js';
export { TargetResolver } from './target-resolver.js';
export { AtomicWriter } from './atomic-writer.js';
export { IdempotencyManager } from './idempotency-manager.js';
export { ValidationEngine } from './validation-engine.js';
export { RollbackManager } from './rollback-manager.js';

// Main injection API
export { inject } from './api.js';

// Types and constants
export { INJECTION_MODES, VALIDATION_RULES, ERROR_CODES } from './constants.js';