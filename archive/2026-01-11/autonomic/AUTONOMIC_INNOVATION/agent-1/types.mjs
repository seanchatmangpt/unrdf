/**
 * @fileoverview JSDoc type definitions for AUTONOMIC_INNOVATION integration
 * @module agent-1/types
 */

/**
 * @typedef {Object} Capsule
 * @property {string} id - Unique capsule identifier
 * @property {Array<Object>} operations - List of RDF operations
 * @property {string} hash - Content-addressed hash
 * @property {Object} metadata - Additional metadata
 */

/**
 * @typedef {Object} Lens
 * @property {string} id - Lens identifier
 * @property {string} source - Source pattern/query
 * @property {string} target - Target pattern/transformation
 * @property {Function} transform - Compiled transformation function
 */

/**
 * @typedef {Object} ImpactSet
 * @property {Set<string>} affected - Set of affected URIs
 * @property {Set<string>} dependencies - Set of dependency URIs
 * @property {number} depth - Dependency depth
 */

/**
 * @typedef {Object} ConflictCertificate
 * @property {boolean} canReorder - Whether operations can be reordered
 * @property {Object|null} witness - Witness to non-commutativity (if any)
 * @property {string} proof - Cryptographic proof
 */

/**
 * @typedef {Object} ConventionsProfile
 * @property {string} id - Profile identifier
 * @property {Array<Object>} rules - Validation rules
 * @property {Object} constraints - Constraint definitions
 */

/**
 * @typedef {Object} DiagnosticReport
 * @property {boolean} valid - Overall validation status
 * @property {Array<Object>} errors - List of errors
 * @property {Array<Object>} warnings - List of warnings
 */

/**
 * @typedef {Object} GeneratedFacade
 * @property {string} code - Generated code
 * @property {number} lineCount - Number of lines
 * @property {Array<string>} exports - List of exported functions
 */

/**
 * @typedef {Object} ApplyReceipt
 * @property {string} receiptHash - Receipt hash
 * @property {number} opsApplied - Number of operations applied
 * @property {string} timestamp - ISO timestamp
 * @property {Array<string>} mutations - List of mutations
 */

/**
 * @typedef {Object} MismatchReport
 * @property {number} count - Number of mismatches
 * @property {Array<Object>} mismatches - List of mismatches
 * @property {string} severity - Severity level
 */

/**
 * @typedef {Object} QualityGateResult
 * @property {number} passed - Number of gates passed
 * @property {number} total - Total number of gates
 * @property {Array<Object>} failures - List of failures
 */

/**
 * @typedef {Object} AgentExports
 * @property {string} status - Agent status (AVAILABLE, STUB, ERROR)
 * @property {Object} exports - Exported functions
 * @property {string|null} error - Error message (if any)
 */

/**
 * @typedef {Object} IntegrationValidation
 * @property {string} agentId - Agent identifier
 * @property {string} status - Integration status
 * @property {Array<string>} missing - List of missing exports
 */

export const TYPES_EXPORTED = true;
