/**
 * Semantic Invariants Validation Layer — Public API
 *
 * Formal μ(O) validation enforcing 7 semantic invariants:
 * 1. Semantic Causality — task_id/workflow_id/case_id stability
 * 2. OTel-OCEL Isomorphism — 1:1 lossless mapping
 * 3. Temporal Consistency — no orphan tasks or broken workflows
 * 4. Replay Determinism — same inputs → same outputs
 * 5. Closed-Loop Completion — BusinessOS→Canopy→OSA→BusinessOS
 * 6. Minimality — no redundant paths or duplicate events
 * 7. Semantic Validity — schema/constraint compliance
 *
 * @example
 * import { validateSemanticInvariants } from '@unrdf/otel/validation';
 *
 * const result = await validateSemanticInvariants(spans, ocelEvents, {
 *   observationWindowMs: 300000,
 *   enableBlocking: true
 * });
 *
 * if (!result.valid) {
 *   console.error('Semantic invariants violated:', result.violations);
 *   process.exit(1);
 * }
 */

export {
  SemanticInvariantsValidator,
  validator,
  validateSemanticInvariants
} from './semantic-invariants-validator.mjs';

export {
  InvariantViolation
} from './semantic-invariants-validator.mjs';
