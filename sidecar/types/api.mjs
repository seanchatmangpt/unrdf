/**
 * @file API Type Definitions (JSDoc)
 * @description Type definitions for KGC Sidecar HTTP API
 */

/**
 * @typedef {'ASK'|'SHACL'|'DELTA'|'THRESHOLD'|'COUNT'|'WINDOW'} PredicateKind
 */

/**
 * @typedef {'>'|'<'|'='|'>='|'<='} ComparisonOperator
 */

/**
 * @typedef {'AND'|'OR'} CombineStrategy
 */

/**
 * @typedef {'pre'|'post'} HookPhase
 */

/**
 * @typedef {Object} HookPredicate
 * @property {PredicateKind} kind - Predicate type
 * @property {string} [query] - SPARQL ASK query
 * @property {any} [shapes] - SHACL shapes
 * @property {string} [variable] - Variable name for THRESHOLD
 * @property {ComparisonOperator} [operator] - Comparison operator
 * @property {number} [value] - Threshold value
 * @property {number} [windowSize] - Window size for WINDOW predicate
 * @property {string} [countVariable] - Variable to count for COUNT predicate
 */

/**
 * @typedef {Object} RegisterHookRequest
 * @property {string} id - Hook identifier
 * @property {string} select - SPARQL SELECT query
 * @property {Array<HookPredicate>} predicates - Predicates to evaluate
 * @property {CombineStrategy} combine - Combination strategy
 * @property {HookPhase} phase - Hook execution phase
 */

/**
 * @typedef {Object} RegisterHookResponse
 * @property {boolean} success - Whether registration succeeded
 * @property {string} hookId - Registered hook ID
 * @property {string} [error] - Error message if failed
 */

/**
 * @typedef {Object} RDFQuad
 * @property {Object} subject - Subject term
 * @property {Object} predicate - Predicate term
 * @property {Object} object - Object term
 * @property {Object} [graph] - Graph term
 */

/**
 * @typedef {Object} ApplyTransactionRequest
 * @property {Array<RDFQuad>} delta - RDF quads to apply
 * @property {string} [author] - Transaction author
 * @property {Object} [metadata] - Additional metadata
 */

/**
 * @typedef {Object} ApplyTransactionResponse
 * @property {boolean} success - Whether transaction succeeded
 * @property {string} transactionId - Transaction ID
 * @property {string} [receiptId] - Lockchain receipt ID
 * @property {Array<string>} [hooksExecuted] - Hooks that fired
 * @property {string} [error] - Error message if failed
 */

/**
 * @typedef {Object} RegisterPolicyRequest
 * @property {string} id - Policy pack identifier
 * @property {string} shapes - SHACL shapes (Turtle format)
 * @property {number} [priority] - Policy priority
 */

/**
 * @typedef {Object} RegisterPolicyResponse
 * @property {boolean} success - Whether registration succeeded
 * @property {string} policyId - Registered policy ID
 * @property {string} [error] - Error message if failed
 */

/**
 * @typedef {Object} RegisterEffectRequest
 * @property {string} id - Effect identifier
 * @property {string} code - JavaScript effect code
 * @property {number} [timeout] - Execution timeout (ms)
 * @property {number} [memoryLimit] - Memory limit (bytes)
 */

/**
 * @typedef {Object} RegisterEffectResponse
 * @property {boolean} success - Whether registration succeeded
 * @property {string} effectId - Registered effect ID
 * @property {string} [error] - Error message if failed
 */

/**
 * @typedef {Object} InitLockchainRequest
 * @property {string} repoUrl - Git repository URL
 * @property {string} [branch] - Git branch name
 * @property {Object} [credentials] - Git credentials
 */

/**
 * @typedef {Object} InitLockchainResponse
 * @property {boolean} success - Whether initialization succeeded
 * @property {string} [repoUrl] - Initialized repository URL
 * @property {string} [error] - Error message if failed
 */

/**
 * @typedef {Object} RegisterAgentRequest
 * @property {string} id - Agent identifier
 * @property {string} [endpoint] - Agent endpoint URL
 * @property {number} [priority] - Agent priority
 */

/**
 * @typedef {Object} RegisterAgentResponse
 * @property {boolean} success - Whether registration succeeded
 * @property {string} agentId - Registered agent ID
 * @property {string} [error] - Error message if failed
 */

/**
 * @typedef {Object} QueryRequest
 * @property {string} query - SPARQL query
 * @property {string} [format] - Output format
 */

/**
 * @typedef {Object} QueryResponse
 * @property {boolean} success - Whether query succeeded
 * @property {Array<Object>|string} results - Query results
 * @property {string} [error] - Error message if failed
 */

/**
 * @typedef {Object} HealthResponse
 * @property {boolean} healthy - Overall health status
 * @property {string} version - Service version
 * @property {Object} checks - Individual health checks
 */

export {}
