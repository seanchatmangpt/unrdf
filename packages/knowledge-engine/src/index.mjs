/**
 * @unrdf/knowledge-engine
 *
 * Knowledge Engine - Rule Engine, Inference, and Pattern Matching
 *
 * @module @unrdf/knowledge-engine
 */

// Export rule definition and management
export {
  defineRule,
  compileRule,
  getRule,
  getAllRules,
  clearRules,
} from './knowledge-engine/rules.mjs';

// Export pattern matching
export {
  matchPattern,
  matchPatternWithBindings,
  hasMatch,
  matchMultiplePatterns,
} from './knowledge-engine/pattern-matcher.mjs';

// Export inference engine
export {
  createInferenceEngine,
  addRules,
  runInference,
  getInferredQuads,
  resetEngine,
} from './knowledge-engine/inference-engine.mjs';

// Export built-in rules
export {
  rdfsSubClassRule,
  rdfsSubPropertyRule,
  rdfsDomainRule,
  rdfsRangeRule,
  owlTransitiveRule,
  owlSymmetricRule,
  owlInverseRule,
  builtinRules,
  getBuiltinRules,
  getRDFSRules,
  getOWLRules,
} from './knowledge-engine/builtin-rules.mjs';

// Export pattern DSL
export {
  parsePattern,
  patternToSparql,
  parsePatterns,
  buildPattern,
  isValidPattern,
} from './knowledge-engine/pattern-dsl.mjs';
