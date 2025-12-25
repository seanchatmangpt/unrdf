/**
 * @file YAWL-Hooks Integration Layer
 * @module yawl/hooks/yawl-hooks
 *
 * @description
 * Integration layer between YAWL (Yet Another Workflow Language) semantics
 * and @unrdf/hooks. Translates YAWL workflow logic into hook execution
 * with SPARQL-based control flow routing.
 *
 * YAWL Patterns Supported:
 * - XOR-split (exclusive choice)
 * - AND-split (parallel activation)
 * - OR-split (conditional parallel)
 * - Cancellation regions
 * - Resource allocation with capacity constraints
 */

import { z } from 'zod';
import { randomUUID } from 'crypto';
import { defineHook } from '@unrdf/hooks';

/* ========================================================================= */
/* Zod Schemas                                                              */
/* ========================================================================= */

/**
 * Schema for YAWL task definition
 */
export const YAWLTaskSchema = z.object({
  id: z.string().min(1),
  kind: z.enum(['AtomicTask', 'CompositeTask', 'MultiInstanceTask', 'EmptyTask']),
  name: z.string().optional(),
  inputConditions: z.array(z.string()).optional(),
  outputConditions: z.array(z.string()).optional(),
  resourcePattern: z.string().optional(),
  cancellationSet: z.array(z.string()).optional(),
  timeout: z.number().int().positive().optional(),
});

/**
 * Schema for control flow edge
 */
export const ControlFlowSchema = z.object({
  source: z.string().min(1),
  target: z.string().min(1),
  predicate: z.string().min(1),
  splitType: z.enum(['XOR', 'AND', 'OR']).default('XOR'),
  priority: z.number().int().min(0).max(100).default(50),
});

/**
 * Schema for resource constraint
 */
export const ResourceConstraintSchema = z.object({
  resourceId: z.string().min(1),
  capacity: z.number().int().positive(),
  eligibility: z.string().optional(), // SPARQL ASK query
});

/**
 * Schema for YAWL workflow specification
 */
export const YAWLWorkflowSchema = z.object({
  id: z.string().uuid().optional(),
  name: z.string().min(1),
  version: z.string().regex(/^\d+\.\d+\.\d+$/).default('1.0.0'),
  tasks: z.array(YAWLTaskSchema).min(1),
  controlFlow: z.array(ControlFlowSchema).default([]),
  resources: z.array(ResourceConstraintSchema).optional(),
  defaultTimeout: z.number().int().positive().default(30000),
  cancellationRegions: z.record(z.array(z.string())).optional(),
});

/**
 * Schema for hook execution receipt
 */
export const HookReceiptSchema = z.object({
  receiptId: z.string().uuid(),
  timestamp: z.string().datetime(),
  hookType: z.enum(['enablement', 'completion', 'allocation', 'cancellation']),
  taskId: z.string(),
  workflowId: z.string(),
  decision: z.enum(['allow', 'deny', 'route']),
  justification: z.object({
    sparqlQuery: z.string().optional(),
    queryResult: z.any().optional(),
    reason: z.string(),
  }),
  enabledTasks: z.array(z.string()).optional(),
  cancelledTasks: z.array(z.string()).optional(),
});

/* ========================================================================= */
/* SPARQL Query Templates                                                   */
/* ========================================================================= */

/**
 * SPARQL prefix declarations for YAWL ontology
 */
const YAWL_PREFIXES = `
PREFIX yawl: <http://www.yawlfoundation.org/yawlschema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX workflow: <http://unrdf.org/workflow#>
`;

/**
 * Generate SPARQL ASK query for task enablement condition
 * @param {string} taskId - Task identifier
 * @param {string[]} inputConditions - Required input conditions
 * @returns {string} SPARQL ASK query
 */
export function generateEnablementQuery(taskId, inputConditions = []) {
  if (inputConditions.length === 0) {
    // No input conditions - task is always enableable
    return `${YAWL_PREFIXES}
ASK {
  ?task rdf:type yawl:Task ;
        yawl:taskId "${taskId}" ;
        yawl:status "enabled" .
}`;
  }

  // All input conditions must be satisfied
  const conditionPatterns = inputConditions
    .map((cond, i) => `  ?cond${i} rdf:type yawl:Condition ;
        yawl:conditionId "${cond}" ;
        yawl:satisfied true .`)
    .join('\n');

  return `${YAWL_PREFIXES}
ASK {
${conditionPatterns}
}`;
}

/**
 * Generate SPARQL ASK query for control flow predicate
 * @param {string} predicate - Predicate expression (e.g., "approved", "!approved")
 * @returns {string} SPARQL ASK query
 */
export function generatePredicateQuery(predicate) {
  const isNegated = predicate.startsWith('!');
  const cleanPredicate = isNegated ? predicate.slice(1) : predicate;

  if (isNegated) {
    return `${YAWL_PREFIXES}
ASK {
  FILTER NOT EXISTS {
    ?var rdf:type yawl:Variable ;
         yawl:name "${cleanPredicate}" ;
         yawl:value true .
  }
}`;
  }

  return `${YAWL_PREFIXES}
ASK {
  ?var rdf:type yawl:Variable ;
       yawl:name "${cleanPredicate}" ;
       yawl:value true .
}`;
}

/**
 * Generate SPARQL ASK query for resource eligibility
 * @param {string} resourceId - Resource identifier
 * @param {number} capacity - Maximum capacity
 * @returns {string} SPARQL ASK query
 */
export function generateResourceCapacityQuery(resourceId, capacity) {
  return `${YAWL_PREFIXES}
ASK {
  SELECT (COUNT(?alloc) AS ?allocCount) WHERE {
    ?alloc rdf:type yawl:Allocation ;
           yawl:resource <${resourceId}> ;
           yawl:status "active" .
  }
  HAVING (?allocCount < ${capacity})
}`;
}

/* ========================================================================= */
/* Hook Definitions                                                          */
/* ========================================================================= */

/**
 * Create task enablement hook
 *
 * Validates task resource eligibility via policy packs and checks
 * input conditions using SPARQL-ASK queries.
 *
 * @param {Object} task - YAWL task definition
 * @param {Object} workflow - Parent workflow specification
 * @param {Object} conditionEvaluator - Condition evaluator instance
 * @returns {Object} Hook definition for task enablement
 */
export function createTaskEnablementHook(task, workflow, conditionEvaluator) {
  const validatedTask = YAWLTaskSchema.parse(task);
  const sparqlQuery = generateEnablementQuery(validatedTask.id, validatedTask.inputConditions);

  return defineHook({
    name: `yawl:enable:${validatedTask.id}`,
    trigger: 'before-add',
    metadata: {
      hookType: 'enablement',
      taskId: validatedTask.id,
      workflowId: workflow.id,
      sparqlQuery,
    },
    validate: quad => {
      // Only process task state quads
      if (!quad.predicate?.value?.includes('taskState')) {
        return true;
      }

      // Extract task ID from quad
      const quadTaskId = extractTaskId(quad);
      if (quadTaskId !== validatedTask.id) {
        return true; // Not our task, pass through
      }

      return true; // Initial validation passes, condition eval happens in async context
    },
  });
}

/**
 * Create task enablement validator (async version with condition evaluation)
 *
 * @param {Object} task - YAWL task definition
 * @param {Object} workflow - Parent workflow specification
 * @param {Object} conditionEvaluator - Condition evaluator instance
 * @returns {Function} Async validator function
 */
export function createTaskEnablementValidator(task, workflow, conditionEvaluator) {
  const validatedTask = YAWLTaskSchema.parse(task);
  const sparqlQuery = generateEnablementQuery(validatedTask.id, validatedTask.inputConditions);

  return async (store, context = {}) => {
    const condition = {
      kind: 'sparql-ask',
      query: sparqlQuery,
    };

    try {
      const satisfied = await conditionEvaluator.evaluate(condition, store, context.env || {});

      return {
        valid: Boolean(satisfied),
        receipt: createReceipt({
          hookType: 'enablement',
          taskId: validatedTask.id,
          workflowId: workflow.id,
          decision: satisfied ? 'allow' : 'deny',
          justification: {
            sparqlQuery,
            queryResult: satisfied,
            reason: satisfied
              ? 'All input conditions satisfied'
              : 'One or more input conditions not satisfied',
          },
        }),
      };
    } catch (error) {
      return {
        valid: false,
        receipt: createReceipt({
          hookType: 'enablement',
          taskId: validatedTask.id,
          workflowId: workflow.id,
          decision: 'deny',
          justification: {
            sparqlQuery,
            reason: `Condition evaluation failed: ${error.message}`,
          },
        }),
      };
    }
  };
}

/**
 * Create task completion hook
 *
 * Evaluates control flow predicates (XOR/AND conditions) and determines
 * which downstream tasks to enable. Creates receipts proving routing decision.
 *
 * @param {Object} task - YAWL task definition
 * @param {Object} workflow - Parent workflow specification
 * @param {Object} conditionEvaluator - Condition evaluator instance
 * @returns {Object} Hook definition for task completion
 */
export function createTaskCompletionHook(task, workflow, conditionEvaluator) {
  const validatedTask = YAWLTaskSchema.parse(task);

  // Find outgoing control flow edges from this task
  const outgoingEdges = workflow.controlFlow.filter(edge => edge.source === validatedTask.id);

  return defineHook({
    name: `yawl:complete:${validatedTask.id}`,
    trigger: 'after-add',
    metadata: {
      hookType: 'completion',
      taskId: validatedTask.id,
      workflowId: workflow.id,
      outgoingEdges: outgoingEdges.length,
    },
    validate: quad => {
      // Only process completion events
      if (!quad.predicate?.value?.includes('taskCompleted')) {
        return true;
      }

      const quadTaskId = extractTaskId(quad);
      return quadTaskId === validatedTask.id;
    },
  });
}

/**
 * Create task completion router (async version with control flow evaluation)
 *
 * @param {Object} task - YAWL task definition
 * @param {Object} workflow - Parent workflow specification
 * @param {Object} conditionEvaluator - Condition evaluator instance
 * @returns {Function} Async router function
 */
export function createTaskCompletionRouter(task, workflow, conditionEvaluator) {
  const validatedTask = YAWLTaskSchema.parse(task);
  const outgoingEdges = workflow.controlFlow.filter(edge => edge.source === validatedTask.id);

  return async (store, context = {}) => {
    const enabledTasks = [];
    const evaluationResults = [];

    // Group edges by split type
    const edgesBySplit = groupBy(outgoingEdges, 'splitType');

    // Handle XOR-split: only first matching edge fires
    if (edgesBySplit.XOR) {
      const sortedXOR = [...edgesBySplit.XOR].sort((a, b) => b.priority - a.priority);

      for (const edge of sortedXOR) {
        const query = generatePredicateQuery(edge.predicate);
        const condition = { kind: 'sparql-ask', query };

        try {
          const satisfied = await conditionEvaluator.evaluate(condition, store, context.env || {});
          evaluationResults.push({ edge, query, satisfied });

          if (satisfied) {
            enabledTasks.push(edge.target);
            break; // XOR: only one path
          }
        } catch (error) {
          evaluationResults.push({ edge, query, error: error.message });
        }
      }
    }

    // Handle AND-split: all edges fire unconditionally
    if (edgesBySplit.AND) {
      for (const edge of edgesBySplit.AND) {
        enabledTasks.push(edge.target);
        evaluationResults.push({
          edge,
          query: 'N/A (AND-split)',
          satisfied: true,
        });
      }
    }

    // Handle OR-split: all matching edges fire
    if (edgesBySplit.OR) {
      for (const edge of edgesBySplit.OR) {
        const query = generatePredicateQuery(edge.predicate);
        const condition = { kind: 'sparql-ask', query };

        try {
          const satisfied = await conditionEvaluator.evaluate(condition, store, context.env || {});
          evaluationResults.push({ edge, query, satisfied });

          if (satisfied) {
            enabledTasks.push(edge.target);
          }
        } catch (error) {
          evaluationResults.push({ edge, query, error: error.message });
        }
      }
    }

    return {
      enabledTasks,
      receipt: createReceipt({
        hookType: 'completion',
        taskId: validatedTask.id,
        workflowId: workflow.id,
        decision: 'route',
        justification: {
          reason: `Evaluated ${evaluationResults.length} control flow edges`,
          queryResult: evaluationResults,
        },
        enabledTasks,
      }),
    };
  };
}

/**
 * Create resource allocation hook
 *
 * Validates resource capacity not exceeded and checks eligibility constraints.
 * Blocks if allocation would violate policy pack rules.
 *
 * @param {Object} resource - Resource constraint definition
 * @param {Object} workflow - Parent workflow specification
 * @param {Object} conditionEvaluator - Condition evaluator instance
 * @returns {Object} Hook definition for resource allocation
 */
export function createResourceAllocationHook(resource, workflow, conditionEvaluator) {
  const validatedResource = ResourceConstraintSchema.parse(resource);

  return defineHook({
    name: `yawl:allocate:${validatedResource.resourceId}`,
    trigger: 'before-add',
    metadata: {
      hookType: 'allocation',
      resourceId: validatedResource.resourceId,
      workflowId: workflow.id,
      capacity: validatedResource.capacity,
    },
    validate: quad => {
      // Only process allocation quads
      if (!quad.predicate?.value?.includes('allocateResource')) {
        return true;
      }

      const quadResourceId = extractResourceId(quad);
      return quadResourceId === validatedResource.resourceId;
    },
  });
}

/**
 * Create resource allocation validator (async version with capacity check)
 *
 * @param {Object} resource - Resource constraint definition
 * @param {Object} workflow - Parent workflow specification
 * @param {Object} conditionEvaluator - Condition evaluator instance
 * @returns {Function} Async validator function
 */
export function createResourceAllocationValidator(resource, workflow, conditionEvaluator) {
  const validatedResource = ResourceConstraintSchema.parse(resource);
  const capacityQuery = generateResourceCapacityQuery(
    validatedResource.resourceId,
    validatedResource.capacity
  );

  return async (store, context = {}) => {
    const capacityCondition = {
      kind: 'sparql-ask',
      query: capacityQuery,
    };

    try {
      // Check capacity
      const hasCapacity = await conditionEvaluator.evaluate(
        capacityCondition,
        store,
        context.env || {}
      );

      if (!hasCapacity) {
        return {
          valid: false,
          receipt: createReceipt({
            hookType: 'allocation',
            taskId: context.taskId || 'unknown',
            workflowId: workflow.id,
            decision: 'deny',
            justification: {
              sparqlQuery: capacityQuery,
              queryResult: false,
              reason: `Resource ${validatedResource.resourceId} at capacity (${validatedResource.capacity})`,
            },
          }),
        };
      }

      // Check eligibility if defined
      if (validatedResource.eligibility) {
        const eligibilityCondition = {
          kind: 'sparql-ask',
          query: validatedResource.eligibility,
        };

        const eligible = await conditionEvaluator.evaluate(
          eligibilityCondition,
          store,
          context.env || {}
        );

        if (!eligible) {
          return {
            valid: false,
            receipt: createReceipt({
              hookType: 'allocation',
              taskId: context.taskId || 'unknown',
              workflowId: workflow.id,
              decision: 'deny',
              justification: {
                sparqlQuery: validatedResource.eligibility,
                queryResult: false,
                reason: 'Resource eligibility constraint not satisfied',
              },
            }),
          };
        }
      }

      return {
        valid: true,
        receipt: createReceipt({
          hookType: 'allocation',
          taskId: context.taskId || 'unknown',
          workflowId: workflow.id,
          decision: 'allow',
          justification: {
            sparqlQuery: capacityQuery,
            queryResult: true,
            reason: 'Resource available and eligible',
          },
        }),
      };
    } catch (error) {
      return {
        valid: false,
        receipt: createReceipt({
          hookType: 'allocation',
          taskId: context.taskId || 'unknown',
          workflowId: workflow.id,
          decision: 'deny',
          justification: {
            reason: `Allocation check failed: ${error.message}`,
          },
        }),
      };
    }
  };
}

/**
 * Create cancellation hook
 *
 * Triggers on task timeout (EffectSandbox timeout enforcement) and
 * propagates cancellation to dependent tasks.
 *
 * @param {Object} task - YAWL task definition
 * @param {Object} workflow - Parent workflow specification
 * @returns {Object} Hook definition for cancellation
 */
export function createCancellationHook(task, workflow) {
  const validatedTask = YAWLTaskSchema.parse(task);

  // Get cancellation set - tasks to cancel when this task fails/times out
  const cancellationSet = validatedTask.cancellationSet || [];

  // Also check workflow-level cancellation regions
  const regionCancellations = [];
  if (workflow.cancellationRegions) {
    for (const [_region, tasks] of Object.entries(workflow.cancellationRegions)) {
      if (tasks.includes(validatedTask.id)) {
        // This task is in a cancellation region
        regionCancellations.push(...tasks.filter(t => t !== validatedTask.id));
      }
    }
  }

  const allCancellations = [...new Set([...cancellationSet, ...regionCancellations])];

  return defineHook({
    name: `yawl:cancel:${validatedTask.id}`,
    trigger: 'on-error',
    metadata: {
      hookType: 'cancellation',
      taskId: validatedTask.id,
      workflowId: workflow.id,
      cancellationSet: allCancellations,
      timeout: validatedTask.timeout || workflow.defaultTimeout,
    },
    validate: quad => {
      // Process error/timeout events for this task
      if (
        !quad.predicate?.value?.includes('taskError') &&
        !quad.predicate?.value?.includes('taskTimeout')
      ) {
        return true;
      }

      const quadTaskId = extractTaskId(quad);
      return quadTaskId === validatedTask.id;
    },
  });
}

/**
 * Create timeout cancellation hook
 *
 * @param {Object} task - YAWL task definition
 * @param {Object} workflow - Parent workflow specification
 * @returns {Object} Hook definition for timeout-triggered cancellation
 */
export function createTimeoutHook(task, workflow) {
  const validatedTask = YAWLTaskSchema.parse(task);
  const timeout = validatedTask.timeout || workflow.defaultTimeout;

  return defineHook({
    name: `yawl:timeout:${validatedTask.id}`,
    trigger: 'on-timeout',
    metadata: {
      hookType: 'cancellation',
      taskId: validatedTask.id,
      workflowId: workflow.id,
      timeout,
    },
    validate: () => true, // Timeout hooks always trigger
  });
}

/**
 * Create cancellation handler (processes cancellation and propagates)
 *
 * @param {Object} task - YAWL task definition
 * @param {Object} workflow - Parent workflow specification
 * @returns {Function} Cancellation handler function
 */
export function createCancellationHandler(task, workflow) {
  const validatedTask = YAWLTaskSchema.parse(task);

  // Get all tasks to cancel
  const cancellationSet = validatedTask.cancellationSet || [];
  const regionCancellations = [];

  if (workflow.cancellationRegions) {
    for (const [_region, tasks] of Object.entries(workflow.cancellationRegions)) {
      if (tasks.includes(validatedTask.id)) {
        regionCancellations.push(...tasks.filter(t => t !== validatedTask.id));
      }
    }
  }

  const allCancellations = [...new Set([...cancellationSet, ...regionCancellations])];

  return (error, context = {}) => {
    const reason =
      error instanceof Error
        ? error.message
        : typeof error === 'string'
          ? error
          : 'Task cancelled';

    const isTimeout = reason.includes('timeout') || reason.includes('Timeout');

    return {
      cancelledTasks: allCancellations,
      receipt: createReceipt({
        hookType: 'cancellation',
        taskId: validatedTask.id,
        workflowId: workflow.id,
        decision: 'deny',
        justification: {
          reason: isTimeout
            ? `Task ${validatedTask.id} timed out after ${validatedTask.timeout || workflow.defaultTimeout}ms`
            : `Task ${validatedTask.id} failed: ${reason}`,
        },
        cancelledTasks: allCancellations,
      }),
    };
  };
}

/* ========================================================================= */
/* Policy Pack Builder                                                       */
/* ========================================================================= */

/**
 * Initialize task-related hooks for a workflow
 * @param {Object} workflow - Validated workflow specification
 * @param {Object} conditionEvaluator - Condition evaluator instance
 * @param {number} priority - Base priority for hooks
 * @returns {Object} Task hooks data structure
 */
function initializeTaskHooks(workflow, conditionEvaluator, priority) {
  const hooks = [];
  const validators = new Map();
  const routers = new Map();
  const cancellationHandlers = new Map();

  for (const task of workflow.tasks) {
    // Task enablement hook
    const enablementHook = createTaskEnablementHook(task, workflow, conditionEvaluator);
    enablementHook.priority = priority;
    hooks.push(enablementHook);

    // Task enablement validator (async)
    if (conditionEvaluator) {
      validators.set(
        task.id,
        createTaskEnablementValidator(task, workflow, conditionEvaluator)
      );
    }

    // Task completion hook (only for tasks with outgoing edges)
    const outgoingEdges = workflow.controlFlow.filter(edge => edge.source === task.id);
    if (outgoingEdges.length > 0) {
      const completionHook = createTaskCompletionHook(task, workflow, conditionEvaluator);
      completionHook.priority = priority;
      hooks.push(completionHook);

      // Task completion router (async)
      if (conditionEvaluator) {
        routers.set(task.id, createTaskCompletionRouter(task, workflow, conditionEvaluator));
      }
    }

    // Cancellation hooks
    const cancellationHook = createCancellationHook(task, workflow);
    cancellationHook.priority = priority + 10; // Higher priority for cancellation
    hooks.push(cancellationHook);

    const timeoutHook = createTimeoutHook(task, workflow);
    timeoutHook.priority = priority + 10;
    hooks.push(timeoutHook);

    // Cancellation handler
    cancellationHandlers.set(task.id, createCancellationHandler(task, workflow));
  }

  return { hooks, validators, routers, cancellationHandlers };
}

/**
 * Initialize resource-related hooks for a workflow
 * @param {Object} workflow - Validated workflow specification
 * @param {Object} conditionEvaluator - Condition evaluator instance
 * @param {number} priority - Base priority for hooks
 * @returns {Object} Resource hooks data structure
 */
function initializeResourceHooks(workflow, conditionEvaluator, priority) {
  const hooks = [];
  const allocators = new Map();

  if (workflow.resources) {
    for (const resource of workflow.resources) {
      const allocationHook = createResourceAllocationHook(resource, workflow, conditionEvaluator);
      allocationHook.priority = priority + 5; // Slightly higher priority
      hooks.push(allocationHook);

      // Resource allocation validator (async)
      if (conditionEvaluator) {
        allocators.set(
          resource.resourceId,
          createResourceAllocationValidator(resource, workflow, conditionEvaluator)
        );
      }
    }
  }

  return { hooks, allocators };
}

/**
 * Build policy pack manifest
 * @param {Object} workflow - Validated workflow specification
 * @param {Array} hooks - All hook definitions
 * @param {number} priority - Default priority
 * @param {boolean} strictMode - Strict mode flag
 * @returns {Object} Policy pack manifest
 */
function buildPolicyManifest(workflow, hooks, priority, strictMode) {
  return {
    id: workflow.id,
    meta: {
      name: `yawl:${workflow.name}`,
      version: workflow.version,
      description: `YAWL Policy Pack for workflow: ${workflow.name}`,
      tags: ['yawl', 'workflow', 'control-flow'],
      ontology: ['http://www.yawlfoundation.org/yawlschema#'],
    },
    config: {
      enabled: true,
      priority,
      strictMode,
      timeout: workflow.defaultTimeout,
    },
    hooks: hooks.map(hook => ({
      name: hook.name,
      enabled: true,
      priority: hook.priority || priority,
    })),
  };
}

/**
 * Create enablement validation method
 * @param {Map} validators - Validators map
 * @returns {Function} validateEnablement method
 */
function createValidateEnablementMethod(validators) {
  return async function validateEnablement(taskId, store, context = {}) {
    const validator = validators.get(taskId);
    if (!validator) {
      throw new Error(`No validator found for task: ${taskId}`);
    }
    return validator(store, context);
  };
}

/**
 * Create completion routing method
 * @param {Map} routers - Routers map
 * @param {Object} workflow - Workflow specification
 * @returns {Function} routeCompletion method
 */
function createRouteCompletionMethod(routers, workflow) {
  return async function routeCompletion(taskId, store, context = {}) {
    const router = routers.get(taskId);
    if (!router) {
      // No outgoing edges - return empty result
      return {
        enabledTasks: [],
        receipt: createReceipt({
          hookType: 'completion',
          taskId,
          workflowId: workflow.id,
          decision: 'route',
          justification: {
            reason: 'No outgoing control flow edges',
          },
          enabledTasks: [],
        }),
      };
    }
    return router(store, context);
  };
}

/**
 * Create allocation validation method
 * @param {Map} allocators - Allocators map
 * @returns {Function} validateAllocation method
 */
function createValidateAllocationMethod(allocators) {
  return async function validateAllocation(resourceId, store, context = {}) {
    const allocator = allocators.get(resourceId);
    if (!allocator) {
      throw new Error(`No allocator found for resource: ${resourceId}`);
    }
    return allocator(store, context);
  };
}

/**
 * Create cancellation handler method
 * @param {Map} cancellationHandlers - Cancellation handlers map
 * @param {Object} workflow - Workflow specification
 * @returns {Function} handleCancellation method
 */
function createHandleCancellationMethod(cancellationHandlers, workflow) {
  return function handleCancellation(taskId, error, context = {}) {
    const handler = cancellationHandlers.get(taskId);
    if (!handler) {
      return {
        cancelledTasks: [],
        receipt: createReceipt({
          hookType: 'cancellation',
          taskId,
          workflowId: workflow.id,
          decision: 'deny',
          justification: {
            reason: 'No cancellation handler defined',
          },
          cancelledTasks: [],
        }),
      };
    }
    return handler(error, context);
  };
}

/**
 * Create workflow statistics getter
 * @param {Object} workflow - Workflow specification
 * @param {Array} allHooks - All hook definitions
 * @param {Map} validators - Validators map
 * @param {Map} routers - Routers map
 * @param {Map} allocators - Allocators map
 * @returns {Function} getStats method
 */
function createGetStatsMethod(workflow, allHooks, validators, routers, allocators) {
  return function getStats() {
    return {
      workflowId: workflow.id,
      workflowName: workflow.name,
      version: workflow.version,
      taskCount: workflow.tasks.length,
      controlFlowEdges: workflow.controlFlow.length,
      resourceCount: workflow.resources?.length || 0,
      hookCount: allHooks.length,
      validatorCount: validators.size,
      routerCount: routers.size,
      allocatorCount: allocators.size,
    };
  };
}

/**
 * Create simple getter methods
 * @param {Array} allHooks - All hook definitions
 * @param {Map} validators - Validators map
 * @param {Map} routers - Routers map
 * @param {Map} allocators - Allocators map
 * @param {Map} cancellationHandlers - Cancellation handlers map
 * @returns {Object} Getter methods
 */
function createGetterMethods(allHooks, validators, routers, allocators, cancellationHandlers) {
  return {
    /**
     * Get all hooks for registration
     * @returns {Array} Array of hook definitions
     */
    getHooks() {
      return allHooks;
    },

    /**
     * Get validator for a task
     * @param {string} taskId - Task identifier
     * @returns {Function|undefined} Async validator function
     */
    getValidator(taskId) {
      return validators.get(taskId);
    },

    /**
     * Get router for a task
     * @param {string} taskId - Task identifier
     * @returns {Function|undefined} Async router function
     */
    getRouter(taskId) {
      return routers.get(taskId);
    },

    /**
     * Get allocator for a resource
     * @param {string} resourceId - Resource identifier
     * @returns {Function|undefined} Async allocator function
     */
    getAllocator(resourceId) {
      return allocators.get(resourceId);
    },

    /**
     * Get cancellation handler for a task
     * @param {string} taskId - Task identifier
     * @returns {Function|undefined} Cancellation handler function
     */
    getCancellationHandler(taskId) {
      return cancellationHandlers.get(taskId);
    },
  };
}

/**
 * Create policy pack API object
 * @param {Object} manifest - Policy pack manifest
 * @param {Array} allHooks - All hook definitions
 * @param {Map} validators - Validators map
 * @param {Map} routers - Routers map
 * @param {Map} allocators - Allocators map
 * @param {Map} cancellationHandlers - Cancellation handlers map
 * @param {Object} workflow - Workflow specification
 * @returns {Object} Policy pack API
 */
function createPolicyPackAPI(
  manifest,
  allHooks,
  validators,
  routers,
  allocators,
  cancellationHandlers,
  workflow
) {
  const getters = createGetterMethods(allHooks, validators, routers, allocators, cancellationHandlers);

  return {
    manifest,
    hooks: allHooks,
    validators,
    routers,
    allocators,
    cancellationHandlers,
    ...getters,
    validateEnablement: createValidateEnablementMethod(validators),
    routeCompletion: createRouteCompletionMethod(routers, workflow),
    validateAllocation: createValidateAllocationMethod(allocators),
    handleCancellation: createHandleCancellationMethod(cancellationHandlers, workflow),
    getStats: createGetStatsMethod(workflow, allHooks, validators, routers, allocators),
  };
}

/**
 * Create YAWL Policy Pack from workflow specification
 *
 * Generates a complete hook pack with:
 * - Task enablement validators
 * - Control flow routers (SPARQL conditions)
 * - Resource allocators
 * - Cancellation handlers
 *
 * @param {Object} workflowSpec - YAWL workflow specification
 * @param {Object} [options] - Policy pack options
 * @param {Object} [options.conditionEvaluator] - Condition evaluator instance
 * @param {number} [options.priority] - Default hook priority (0-100)
 * @param {boolean} [options.strictMode] - Fail fast on errors
 * @returns {Object} PolicyPack ready for HookRegistry
 *
 * @example
 * const workflow = {
 *   name: 'approval-workflow',
 *   tasks: [
 *     { id: 'approve', kind: 'AtomicTask' },
 *     { id: 'reject', kind: 'AtomicTask' },
 *     { id: 'finalize', kind: 'AtomicTask' }
 *   ],
 *   controlFlow: [
 *     { source: 'approve', target: 'finalize', predicate: 'approved' },
 *     { source: 'approve', target: 'reject', predicate: '!approved' }
 *   ]
 * };
 *
 * const policyPack = createYAWLPolicyPack(workflow);
 * hookRegistry.registerPolicyPack(policyPack);
 */
export function createYAWLPolicyPack(workflowSpec, options = {}) {
  const workflow = YAWLWorkflowSchema.parse({
    ...workflowSpec,
    id: workflowSpec.id || randomUUID(),
  });

  const { conditionEvaluator = null, priority = 50, strictMode = false } = options;

  // Initialize task hooks
  const taskHooksData = initializeTaskHooks(workflow, conditionEvaluator, priority);

  // Initialize resource hooks
  const resourceHooksData = initializeResourceHooks(workflow, conditionEvaluator, priority);

  // Combine all hooks
  const allHooks = [...taskHooksData.hooks, ...resourceHooksData.hooks];

  // Build manifest
  const manifest = buildPolicyManifest(workflow, allHooks, priority, strictMode);

  // Create and return policy pack API
  return createPolicyPackAPI(
    manifest,
    allHooks,
    taskHooksData.validators,
    taskHooksData.routers,
    resourceHooksData.allocators,
    taskHooksData.cancellationHandlers,
    workflow
  );
}

/* ========================================================================= */
/* Utility Functions                                                         */
/* ========================================================================= */

/**
 * Create a hook execution receipt
 * @param {Object} params - Receipt parameters
 * @returns {Object} Receipt object
 */
function createReceipt(params) {
  return HookReceiptSchema.parse({
    receiptId: randomUUID(),
    timestamp: new Date().toISOString(),
    ...params,
  });
}

/**
 * Extract task ID from a quad
 * @param {Object} quad - RDF quad
 * @returns {string|null} Task ID or null
 */
function extractTaskId(quad) {
  // Try to extract from subject IRI
  const subjectValue = quad.subject?.value || '';
  const taskMatch = subjectValue.match(/task[:/]([^/]+)$/i);
  if (taskMatch) {
    return taskMatch[1];
  }

  // Try to extract from object literal
  const objectValue = quad.object?.value || '';
  if (quad.predicate?.value?.includes('taskId')) {
    return objectValue;
  }

  return null;
}

/**
 * Extract resource ID from a quad
 * @param {Object} quad - RDF quad
 * @returns {string|null} Resource ID or null
 */
function extractResourceId(quad) {
  const objectValue = quad.object?.value || '';
  if (quad.predicate?.value?.includes('resource')) {
    return objectValue;
  }

  const subjectValue = quad.subject?.value || '';
  const resourceMatch = subjectValue.match(/resource[:/]([^/]+)$/i);
  if (resourceMatch) {
    return resourceMatch[1];
  }

  return null;
}

/**
 * Group array by key
 * @param {Array} array - Array to group
 * @param {string} key - Key to group by
 * @returns {Object} Grouped object
 */
function groupBy(array, key) {
  return array.reduce((result, item) => {
    const groupKey = item[key];
    if (!result[groupKey]) {
      result[groupKey] = [];
    }
    result[groupKey].push(item);
    return result;
  }, {});
}

/* ========================================================================= */
/* Exports                                                                   */
/* ========================================================================= */

export default {
  createYAWLPolicyPack,
  createTaskEnablementHook,
  createTaskEnablementValidator,
  createTaskCompletionHook,
  createTaskCompletionRouter,
  createResourceAllocationHook,
  createResourceAllocationValidator,
  createCancellationHook,
  createTimeoutHook,
  createCancellationHandler,
  generateEnablementQuery,
  generatePredicateQuery,
  generateResourceCapacityQuery,
  YAWLWorkflowSchema,
  YAWLTaskSchema,
  ControlFlowSchema,
  ResourceConstraintSchema,
  HookReceiptSchema,
};
