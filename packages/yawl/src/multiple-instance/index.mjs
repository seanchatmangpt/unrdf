/**
 * @file Multiple Instance Patterns - YAWL WP13-WP15
 * @module @unrdf/yawl/multiple-instance
 * @description
 * Multiple instance task patterns for parallel execution:
 * - WP14: Runtime A Priori - Count determined before spawning
 * - WP15: Runtime A Posteriori - Dynamic spawning during execution (IMPLEMENTED)
 *
 * Future patterns:
 * - WP13: Static Count - Fixed number of instances
 */

// WP14: Runtime A Priori Knowledge
export {
  spawnInstancesRuntimeApriori,
  createBarrier,
  registerCompletion,
  isBarrierComplete,
  sliceInputData,
  waitForBarrier,
  BarrierSchema,
  WP14ResultSchema,
} from './wp14-runtime-apriori.mjs';

// Expression Evaluator
export {
  evaluateExpression,
  evaluateJSONPath,
  evaluateSPARQL,
  evaluateFunction,
  ExpressionType,
  ExpressionSchema,
  EvaluationResultSchema,
} from './expression-evaluator.mjs';

// WP15: Multiple Instances without A Priori Runtime Knowledge
export {
  DynamicBarrier,
  createDynamicBarrier,
  InstanceStateSchema,
} from './dynamic-barrier.mjs';

export {
  DynamicMIController,
  createDynamicMIController,
  MITaskSchema,
  MIInstanceSchema,
} from './wp15-dynamic.mjs';
