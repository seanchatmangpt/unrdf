/**
 * @file YAWL Cancellation Semantics - Barrel Export
 * @module yawl/cancellation
 *
 * @description
 * Implements YAWL-style cancellation semantics using circuit breaker pattern
 * and EffectSandbox timeouts for reliable task abortion. Provides:
 *
 * - Cancellation regions: Groups of tasks that cancel together
 * - Work item cancellation with dependent task hooks
 * - Timeout enforcement via EffectSandbox
 * - Circuit breaker integration for cascading failure prevention
 * - Full auditability through receipt logging
 * - Time-travel support via receipt history
 *
 * @see https://www.yamlworkflow.net/
 */

// ============================================================================
// REGION MANAGEMENT
// ============================================================================

export {
  CancellationRegionSchema,
  CancellationRegionManager,
  createRegionManager,
} from './yawl-cancellation-regions.mjs';

// ============================================================================
// CORE INFRASTRUCTURE
// ============================================================================

export {
  CancellationReasonSchema,
  WorkItemStateSchema,
  WorkItemSchema,
  CircuitBreakerStateSchema,
  CancellationReceiptSchema,
  VALID_RECEIPT_TYPES,
  createReceipt,
  TaskCircuitBreaker,
  CancellationReceiptLogger,
} from './yawl-cancellation-core.mjs';

// ============================================================================
// MAIN MANAGER
// ============================================================================

export {
  YawlCancellationManager,
} from './yawl-cancellation-manager.mjs';

// ============================================================================
// FACTORY FUNCTIONS
// ============================================================================

import { YawlCancellationManager } from './yawl-cancellation-manager.mjs';

/**
 * Create a cancellation manager instance
 * @param {Object} [config]
 * @returns {YawlCancellationManager}
 */
export function createCancellationManager(config = {}) {
  return new YawlCancellationManager(config);
}

/**
 * Create a cancellation region
 * @param {YawlCancellationManager} manager
 * @param {Object} options
 * @returns {import('./yawl-cancellation-regions.mjs').CancellationRegion}
 */
export function createCancellationRegion(manager, options) {
  return manager.regionManager.createRegion(options);
}
