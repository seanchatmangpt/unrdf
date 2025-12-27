/**
 * @file index.mjs
 * @description Public API exports for Agent 3 Lens system
 * @module agent-3
 */

// Core lens functionality
export {
  defineLens,
  compileLens,
  executeLensToGraph,
  executeLensFromGraph
} from './lens.mjs';

// Stable identifier generation
export {
  stableIRI,
  stableSkolem,
  stableHash
} from './stable-ids.mjs';

// Reference implementations
export {
  customerLens,
  customerLensProgram,
  demoCustomerRoundTrip,
  demoMultipleCustomers,
  demoLensSerialization
} from './demo-customer-lens.mjs';

/**
 * @typedef {Object} LensDefinition
 * @property {string} name - Lens name
 * @property {string} domain - Domain namespace
 * @property {string} entity - Entity type
 * @property {Array<LensRule>} rules - Mapping rules
 * @property {string} version - Lens version
 */

/**
 * @typedef {Object} LensRule
 * @property {string} dto_field - DTO field name
 * @property {string} rdf_predicate - RDF predicate IRI
 * @property {string} type - Data type (string, number, boolean, date)
 * @property {string} [transform] - Transform function name
 * @property {Function} [validator] - Validation function
 */

/**
 * @typedef {Object} LensProgram
 * @property {string} name - Program name
 * @property {string} version - Program version
 * @property {Object} stableIds - Stable ID configuration
 * @property {Array<Object>} toGraph - DTO → RDF rules
 * @property {Array<Object>} fromGraph - RDF → DTO rules
 */
