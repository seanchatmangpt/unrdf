/**
 * @file Event Automation - Main Entry Point
 * @module @unrdf/event-automation
 * @description Event-driven automation for v6.1.0 with delta processing, receipts, and policy enforcement
 */

// Engine
export {
  EventAutomationEngine,
  createEventAutomationEngine,
} from './event-automation-engine.mjs';

// Delta Processor
export {
  DeltaProcessor,
  createDeltaProcessor,
} from './delta-processor.mjs';

// Receipt Tracker
export {
  ReceiptTracker,
  createReceiptTracker,
} from './receipt-tracker.mjs';

// Policy Enforcer
export {
  PolicyEnforcer,
  createPolicyEnforcer,
} from './policy-enforcer.mjs';

// Schemas
export {
  DeltaOperationSchema,
  DeltaSchema,
  ReceiptSchema,
  PolicySchema,
  EventAutomationConfigSchema,
  ProcessDeltaResultSchema,
  EventMetadataSchema,
  ReplayOptionsSchema,
  StatisticsSchema,
} from './schemas.mjs';

/**
 * Version constant
 */
export const VERSION = '1.0.0';

/**
 * Feature flags
 */
export const FEATURES = {
  deltaProcessing: true,
  receiptGeneration: true,
  policyEnforcement: true,
  eventReplay: true,
  batchProcessing: true,
};
