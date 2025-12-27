/**
 * Synergy Finder - Discover powerful capability combinations
 *
 * Theory:
 *   Synergy(A, B) := Δ where Δ = Value(A ⊕ B) - (Value(A) + Value(B))
 *   Multiplicative(A, B) ⇔ Value(A ⊕ B) ≥ k × Value(A) × Value(B), k > 1
 *
 * Discovery:
 *   1. Measure individual capabilities (baseline)
 *   2. Test all pairwise combinations
 *   3. Calculate synergy delta for each
 *   4. Identify multiplicative effects (k ≥ 2)
 *   5. Flag anti-patterns (negative synergy)
 *
 * @module @unrdf/kgc-claude/capabilities/synergy-finder
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now } from '@unrdf/kgc-4d';

/**
 * Synergy measurement schema
 */
export const SynergyMeasurementSchema = z.object({
  id: z.string(),
  atom_a: z.string(),
  atom_b: z.string(),
  baseline_a: z.number(),
  baseline_b: z.number(),
  composite_value: z.number(),
  synergy_delta: z.number(),
  synergy_type: z.enum(['additive', 'superlinear', 'multiplicative', 'sublinear', 'antipattern']),
  multiplier: z.number(),
  timestamp: z.bigint(),
});

/**
 * @typedef {z.infer<typeof SynergyMeasurementSchema>} SynergyMeasurement
 */

/**
 * Capability pairing schema
 */
export const CapabilityPairingSchema = z.object({
  atom_a: z.string(),
  atom_b: z.string(),
  relationship: z.enum(['complementary', 'orthogonal', 'conflicting', 'redundant']),
  strength: z.number().min(0).max(1),
  evidence: z.array(z.string()),
});

/**
 * @typedef {z.infer<typeof CapabilityPairingSchema>} CapabilityPairing
 */

/**
 * Anti-pattern schema
 */
export const AntiPatternSchema = z.object({
  id: z.string(),
  atoms: z.array(z.string()),
  description: z.string(),
  negative_delta: z.number(),
  reason: z.string(),
  recommendation: z.string(),
});

/**
 * @typedef {z.infer<typeof AntiPatternSchema>} AntiPattern
 */

/**
 * Synergy Finder - Analyze capability combinations for synergistic effects
 */
export class SynergyFinder {
  constructor() {
    /** @type {SynergyMeasurement[]} */
    this.measurements = [];
    /** @type {CapabilityPairing[]} */
    this.pairings = [];
    /** @type {AntiPattern[]} */
    this.antiPatterns = [];
  }

  /**
   * Measure synergy between two capabilities
   * @param {string} atomA
   * @param {string} atomB
   * @param {number} baselineA
   * @param {number} baselineB
   * @param {number} compositeValue
   * @returns {Promise<SynergyMeasurement>}
   */
  async measureSynergy(atomA, atomB, baselineA, baselineB, compositeValue) {
    const expectedAdditive = baselineA + baselineB;
    const synergyDelta = compositeValue - expectedAdditive;

    // Calculate multiplier
    const expectedMultiplicative = baselineA * baselineB;
    const multiplier = expectedMultiplicative > 0
      ? compositeValue / expectedMultiplicative
      : 0;

    // Classify synergy type
    let synergyType;
    if (synergyDelta < 0) {
      synergyType = 'antipattern';
    } else if (multiplier >= 2) {
      synergyType = 'multiplicative';
    } else if (compositeValue > expectedAdditive * 1.5) {
      synergyType = 'superlinear';
    } else if (compositeValue > expectedAdditive * 0.9) {
      synergyType = 'additive';
    } else {
      synergyType = 'sublinear';
    }

    const t_ns = now();

    const measurement = SynergyMeasurementSchema.parse({
      id: `synergy-${atomA}-${atomB}-${t_ns}`,
      atom_a: atomA,
      atom_b: atomB,
      baseline_a: baselineA,
      baseline_b: baselineB,
      composite_value: compositeValue,
      synergy_delta: synergyDelta,
      synergy_type: synergyType,
      multiplier,
      timestamp: t_ns,
    });

    this.measurements.push(measurement);

    // If anti-pattern, record it
    if (synergyType === 'antipattern') {
      await this.recordAntiPattern([atomA, atomB], synergyDelta, measurement);
    }

    return measurement;
  }

  /**
   * Analyze capability pairing
   * @param {string} atomA
   * @param {string} atomB
   * @param {Object} analysis - Analysis with relationship type and evidence
   * @returns {CapabilityPairing}
   */
  analyzePairing(atomA, atomB, analysis) {
    const pairing = CapabilityPairingSchema.parse({
      atom_a: atomA,
      atom_b: atomB,
      relationship: analysis.relationship || 'orthogonal',
      strength: analysis.strength || 0.5,
      evidence: analysis.evidence || [],
    });

    this.pairings.push(pairing);
    return pairing;
  }

  /**
   * Record anti-pattern
   * @param {string[]} atoms
   * @param {number} negativeDelta
   * @param {SynergyMeasurement} measurement
   * @returns {Promise<AntiPattern>}
   */
  async recordAntiPattern(atoms, negativeDelta, measurement) {
    const t_ns = now();

    // Determine reason
    let reason = 'Unknown negative interaction';
    let recommendation = 'Avoid combining these capabilities';

    if (negativeDelta < -50) {
      reason = 'Severe negative interaction - capabilities conflict';
      recommendation = 'Never combine these capabilities together';
    } else if (negativeDelta < -20) {
      reason = 'Moderate negative interaction - overhead exceeds benefit';
      recommendation = 'Use capabilities separately or find alternative composition';
    } else {
      reason = 'Slight negative interaction - minimal synergy loss';
      recommendation = 'Consider if combined value justifies overhead';
    }

    const antiPattern = AntiPatternSchema.parse({
      id: `antipattern-${atoms.join('-')}-${t_ns}`,
      atoms,
      description: `Combining ${atoms.join(' + ')} produces negative synergy`,
      negative_delta: negativeDelta,
      reason,
      recommendation,
    });

    this.antiPatterns.push(antiPattern);
    return antiPattern;
  }

  /**
   * Find multiplicative combinations (k ≥ 2)
   * @returns {SynergyMeasurement[]}
   */
  getMultiplicativeCompositions() {
    return this.measurements.filter(m => m.synergy_type === 'multiplicative');
  }

  /**
   * Find superlinear combinations
   * @returns {SynergyMeasurement[]}
   */
  getSuperlinearCompositions() {
    return this.measurements.filter(m =>
      m.synergy_type === 'superlinear' || m.synergy_type === 'multiplicative'
    );
  }

  /**
   * Get complementary pairs (high synergy)
   * @returns {CapabilityPairing[]}
   */
  getComplementaryPairs() {
    return this.pairings.filter(p => p.relationship === 'complementary');
  }

  /**
   * Get orthogonal pairs (independent value)
   * @returns {CapabilityPairing[]}
   */
  getOrthogonalPairs() {
    return this.pairings.filter(p => p.relationship === 'orthogonal');
  }

  /**
   * Get all anti-patterns
   * @returns {AntiPattern[]}
   */
  getAntiPatterns() {
    return this.antiPatterns;
  }

  /**
   * Find best synergies (top N)
   * @param {number} n
   * @returns {SynergyMeasurement[]}
   */
  getTopSynergies(n = 10) {
    return [...this.measurements]
      .sort((a, b) => b.synergy_delta - a.synergy_delta)
      .slice(0, n);
  }

  /**
   * Recommend compositions for capability
   * @param {string} atomId
   * @param {number} minSynergy
   * @returns {SynergyMeasurement[]}
   */
  recommendCompositions(atomId, minSynergy = 0) {
    return this.measurements.filter(m =>
      (m.atom_a === atomId || m.atom_b === atomId) &&
      m.synergy_delta >= minSynergy &&
      m.synergy_type !== 'antipattern'
    ).sort((a, b) => b.synergy_delta - a.synergy_delta);
  }

  /**
   * Detect capability weakness coverage
   * @param {string} atomA
   * @param {string} atomB
   * @param {Object} weaknesses
   * @returns {Object}
   */
  detectWeaknessCoverage(atomA, atomB, weaknesses) {
    const weaknessA = weaknesses[atomA] || [];
    const weaknessB = weaknesses[atomB] || [];
    const strengthA = weaknesses[`${atomA}_strengths`] || [];
    const strengthB = weaknesses[`${atomB}_strengths`] || [];

    // Check if B's strengths cover A's weaknesses
    const coverageAbyB = weaknessA.filter(w => strengthB.includes(w));
    // Check if A's strengths cover B's weaknesses
    const coverageBbyA = weaknessB.filter(w => strengthA.includes(w));

    return {
      atom_a: atomA,
      atom_b: atomB,
      a_weaknesses_covered_by_b: coverageAbyB,
      b_weaknesses_covered_by_a: coverageBbyA,
      coverage_score: (coverageAbyB.length + coverageBbyA.length) /
                     (weaknessA.length + weaknessB.length || 1),
      is_complementary: coverageAbyB.length > 0 || coverageBbyA.length > 0,
    };
  }

  /**
   * Export synergy analysis
   * @returns {Object}
   */
  export() {
    return {
      measurements: this.measurements,
      pairings: this.pairings,
      anti_patterns: this.antiPatterns,
      stats: {
        total_measurements: this.measurements.length,
        multiplicative: this.getMultiplicativeCompositions().length,
        superlinear: this.getSuperlinearCompositions().length,
        additive: this.measurements.filter(m => m.synergy_type === 'additive').length,
        sublinear: this.measurements.filter(m => m.synergy_type === 'sublinear').length,
        antipatterns: this.antiPatterns.length,
        complementary_pairs: this.getComplementaryPairs().length,
        orthogonal_pairs: this.getOrthogonalPairs().length,
      },
      top_synergies: this.getTopSynergies(5),
    };
  }
}

/**
 * Create synergy finder
 * @returns {SynergyFinder}
 */
export function createSynergyFinder() {
  return new SynergyFinder();
}

export default SynergyFinder;
