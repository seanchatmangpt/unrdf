/**
 * @fileoverview Public API for @unrdf/lens
 * @module @unrdf/lens
 */

// Lens definition
export { defineLens, schemas } from './lens.mjs';

// Compilation
export { compileLens, verifySerializable } from './compiler.mjs';

// Execution
export {
  executeLensToGraph,
  executeLensFromGraph,
  verifyRoundTrip
} from './execute.mjs';

// IRI/Skolem utilities
export {
  createStableIRI,
  createSkolemID,
  extractFromIRI
} from './skolem.mjs';

// Type exports (JSDoc)
/**
 * @typedef {import('./lens.mjs').Lens} Lens
 * @typedef {import('./lens.mjs').LensProfile} LensProfile
 * @typedef {import('./lens.mjs').EntityMapping} EntityMapping
 * @typedef {import('./lens.mjs').SubjectRule} SubjectRule
 * @typedef {import('./lens.mjs').PredicateMapping} PredicateMapping
 * @typedef {import('./compiler.mjs').CompiledLens} CompiledLens
 * @typedef {import('./compiler.mjs').CompiledMapping} CompiledMapping
 * @typedef {import('./compiler.mjs').CompiledPredicate} CompiledPredicate
 * @typedef {import('./compiler.mjs').CompiledSubject} CompiledSubject
 */
