/**
 * @file YAWL Engine Constants
 * @module @unrdf/yawl/engine-constants
 *
 * @description
 * Shared constants for YAWL engine modules
 */

// =============================================================================
// Constants
// =============================================================================

/** YAWL namespace for RDF */
export const YAWL_NS = 'http://yawl.io/';

/** Named graphs for YAWL data */
export const YAWL_GRAPHS = Object.freeze({
  WORKFLOWS: `${YAWL_NS}workflows`,
  CASES: `${YAWL_NS}cases`,
  WORKITEMS: `${YAWL_NS}workitems`,
  EVENTS: `${YAWL_NS}events`,
});

/** Engine event types for subscription */
export const ENGINE_EVENTS = Object.freeze({
  TASK_ENABLED: 'task:enabled',
  TASK_STARTED: 'task:started',
  TASK_COMPLETED: 'task:completed',
  TASK_CANCELLED: 'task:cancelled',
  TASK_FAILED: 'task:failed',
  TASK_TIMEOUT: 'task:timeout',
  CASE_CREATED: 'case:created',
  CASE_STARTED: 'case:started',
  CASE_COMPLETED: 'case:completed',
  CASE_FAILED: 'case:failed',
  CASE_CANCELLED: 'case:cancelled',
  WORKFLOW_REGISTERED: 'workflow:registered',
  CIRCUIT_BREAKER_OPEN: 'circuit:open',
  CIRCUIT_BREAKER_CLOSE: 'circuit:close',
  CHECKPOINT_CREATED: 'checkpoint:created',
  RESOURCE_ALLOCATED: 'resource:allocated',
  RESOURCE_RELEASED: 'resource:released',
});

/** Engine health status */
export const HealthStatus = Object.freeze({
  HEALTHY: 'healthy',
  DEGRADED: 'degraded',
  UNHEALTHY: 'unhealthy',
});
