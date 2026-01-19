/**
 * @file Chatman Equation Framework
 * @module @unrdf/chatman-equation
 * @description
 * Configuration-driven μ(O ⊔ Δ) → A transformation engine.
 *
 * The Chatman Equation framework provides:
 * - TOML-based configuration for observations, deltas, operators, and artifacts
 * - Zod validation schemas for type safety
 * - Loader utilities for configuration files
 * - Template-driven code generation
 * - 3T Methodology (TOML, Tera, Turtle) for RDF knowledge graphs
 *
 * Core Equation: μ(O ⊔ Δ) → A
 * - O: Observations (current state)
 * - Δ: Delta (proposed changes)
 * - μ: Closure operator (reconciliation function)
 * - ⊔: Join operation
 * - A: Artifacts (results)
 *
 * @example
 * import { loadExamples, validateExample } from '@unrdf/chatman-equation';
 *
 * const examples = loadExamples();
 * const result = validateExample(examples.market_equilibrium);
 *
 * if (result.success) {
 *   console.log('Valid transformation:', result.data);
 * }
 */

// ============================================================================
// 3T METHODOLOGY (Original)
// ============================================================================

export const METHODOLOGY_VERSION = '1.0.0';
export const COMPONENTS = ['TOML', 'Tera', 'Turtle'];

/**
 * Get methodology information
 * @returns {Object} Methodology metadata
 */
export function getMethodologyInfo() {
  return {
    name: '3T Methodology',
    version: METHODOLOGY_VERSION,
    components: COMPONENTS,
    description: 'Configuration-driven RDF knowledge graph generation',
    benefits: [
      'Declarative configuration with TOML',
      'Template-based RDF generation with Tera',
      'Validated knowledge graphs with Turtle',
      'Cryptographic provenance with KGC receipts',
      '80/20 efficiency through focused design',
    ],
  };
}

/**
 * Validate the 3T methodology stack
 * @returns {Promise<boolean>} True if all components are available
 */
export async function validateStack() {
  try {
    await import('@iarna/toml');
    await import('n3');
    await import('zod');
    return true;
  } catch (error) {
    console.error('3T stack validation failed:', error.message);
    return false;
  }
}

// ============================================================================
// CHATMAN EQUATION SCHEMAS (New μ(O ⊔ Δ) → A Framework)
// ============================================================================

export {
  DomainEnum,
  ObservationSchema,
  DeltaSchema,
  DeltaOperationSchema,
  ClosureOperatorSchema,
  ClosureOperatorTypeEnum,
  ConflictResolutionEnum,
  ArtifactSchema,
  UnificationMappingSchema,
  ChatmanExampleSchema,
  validateObservation,
  validateDelta,
  validateClosureOperator,
  validateArtifact,
  validateUnificationMapping,
  validateChatmanExample,
  safeValidate,
} from './schemas.mjs';

export {
  loadToml,
  loadEquationSchema,
  loadExamples,
  extractObservation,
  extractDelta,
  extractClosureOperator,
  extractArtifact,
  validateExample,
  extractUnificationMapping,
  loadAndValidateExamples,
  getAllUnificationMappings,
} from './loader.mjs';

// ============================================================================
// DEFAULT EXPORT
// ============================================================================

export default {
  // 3T Methodology
  METHODOLOGY_VERSION,
  COMPONENTS,
  getMethodologyInfo,
  validateStack,
};
