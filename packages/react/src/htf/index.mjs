/**
 * @fileoverview HTF - Hyper-Thesis Framework
 * Unified μ-architecture blending 7 writing modes into a single thesis
 */

// Core theory
export { default as htfCore } from './htf-core.mjs';
export {
  DeltaFamilies,
  CanonicalLambdaOrder,
  computeLambdaOrder,
  computePiMerge,
  computeGammaGlobalization,
  StandardInvariants,
  evolveTowardMuFixed,
  validateDeltaShard,
} from './htf-core.mjs';

// React hooks
export { default as useLambdaScheduling } from './useLambdaScheduling.mjs';
export { default as usePiProfile } from './usePiProfile.mjs';
export { default as useGammaChecker } from './useGammaChecker.mjs';

// Main hook combining all three
export { useHTFFramework } from './useHTFFramework.mjs';

/**
 * HTF Framework - Quick Start
 *
 * The Hyper-Thesis Framework unifies 7 academic writing modes:
 * 1. IMRaD (Introduction-Method-Results-Discussion)
 * 2. Thesis-by-Papers (Multi-article modularity)
 * 3. Argument (Toulmin reasoning: Claim-Ground-Proof-Objection-Reply)
 * 4. Contribution (Gap-Design-Eval-Impact)
 * 5. Monograph (Context-Canon-Method-Analysis-Conclusion)
 * 6. Design Science Research (Problem-Artifact-Eval-Theory)
 * 7. Narrative (Field-Voice-Pattern-Insight)
 *
 * Three core operations:
 *
 * Λ (Lambda) - Total Ordering
 * ├─ Schedules all shards across a timeline
 * ├─ Computes critical path
 * └─ Ensures dependencies are respected
 *
 * Π (Pi) - Merge
 * ├─ Merges all Δ-shards into unified A
 * ├─ Analyzes cross-family connections
 * └─ Measures overall coherence (0-1)
 *
 * Γ (Gamma) - Globalization
 * ├─ Validates against Q-invariants
 * ├─ Tracks drift toward μ-fixed point
 * └─ Suggests fixes for violations
 *
 * The framework ensures:
 * 1. All 7 modes are integrated
 * 2. Invariants (Q) are preserved
 * 3. Thesis converges to canonical form
 * 4. Authorship remains flexible
 *
 * Example:
 *
 * ```javascript
 * const { scheduling, profile, validation } = useHTFFramework({
 *   shards: yourShards,
 *   deadline: new Date('2025-03-01'),
 *   onConvergence: () => console.log('Thesis ready!')
 * });
 *
 * // Schedule chapters
 * scheduling.adjustTimings(0.1); // 10% buffer for final edits
 *
 * // Analyze cross-family coherence
 * const coherence = profile.analysis.coherence;
 *
 * // Validate and fix violations
 * validation.fixViolations();
 * console.log(validation.state.isConverged);
 * ```
 */

export const HTF_CONCEPT = {
  name: 'Hyper-Thesis Framework',
  version: '1.0',
  description: 'Unified architecture for academic thesis writing',

  families: ['imrad', 'papers', 'argument', 'contribution', 'monograph', 'dsr', 'narrative'],

  operations: {
    lambda: {
      symbol: 'Λ',
      name: 'Total Ordering',
      description: 'Schedule all Δ-shards across timeline',
      produces: 'LambdaOrder',
    },
    pi: {
      symbol: 'Π',
      name: 'Merge',
      description: 'Merge all Δ-shards into unified A',
      produces: 'PiMerge',
    },
    gamma: {
      symbol: 'Γ',
      name: 'Globalization',
      description: 'Validate against Q-invariants',
      produces: 'GammaGlobalization',
    },
  },

  invariants: [
    {
      symbol: 'Q',
      name: 'Query Invariants',
      description: 'Constraints preserved across all layers',
    },
  ],

  convergence: {
    symbol: 'μ',
    name: 'Mu-Fixed Point',
    description: 'Idempotent closure where τ(A) = A',
    threshold: 0.05, // 5% drift = converged
  },
};
