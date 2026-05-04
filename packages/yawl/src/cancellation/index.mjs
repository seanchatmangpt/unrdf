/**
 * @file YAWL Cancellation Module Exports
 * @module yawl/cancellation
 */

export {
  // Main manager
  YawlCancellationManager,
  createCancellationManager,
  createCancellationRegion,

  // Circuit breaker
  TaskCircuitBreaker,

  // Region management
  CancellationRegionManager,

  // Receipt logging
  CancellationReceiptLogger,

  // Schemas
  WorkItemSchema,
  CancellationRegionSchema,
  CancellationReceiptSchema,
  CancellationReasonSchema,
  WorkItemStateSchema,
  CircuitBreakerStateSchema,
} from './yawl-cancellation.mjs';
