/**
 * @file YAWL Cancellation - Public API
 * @module yawl/cancellation
 *
 * @description
 * YAWL cancellation semantics with circuit breaker pattern and timeout enforcement.
 *
 * Refactored structure:
 * - schemas.mjs: Zod schemas and constants
 * - circuit-breaker.mjs: Task circuit breaker
 * - region-manager.mjs: Cancellation region management
 * - receipt-logger.mjs: Audit trail and receipts
 * - manager.mjs: Main cancellation coordinator
 */

// Export schemas and types
export {
  CancellationReasonSchema,
  WorkItemStateSchema,
  WorkItemSchema,
  CancellationRegionSchema,
  CancellationReceiptSchema,
  CircuitBreakerStateSchema,
  VALID_RECEIPT_TYPES,
  createReceipt,
} from './schemas.mjs';

// Export circuit breaker
export { TaskCircuitBreaker } from './circuit-breaker.mjs';

// Export region manager
export { CancellationRegionManager } from './region-manager.mjs';

// Export receipt logger
export { CancellationReceiptLogger } from './receipt-logger.mjs';

// Export main manager and factory
export { YawlCancellationManager, createCancellationManager } from './manager.mjs';

// Re-export default manager factory as default
import { createCancellationManager } from './manager.mjs';
import { CancellationError } from '../../errors.mjs';
export default createCancellationManager;
