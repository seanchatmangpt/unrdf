/**
 * Composition Engine - Capability composition with emergent property detection
 *
 * Theory:
 *   C := {c₁, c₂, ..., cₙ} (capability atoms)
 *   G := (V, E) where V = C, E = productive compositions
 *   Π(c₁, c₂) → c' where value(c') > value(c₁) + value(c₂)
 *
 * Law:
 *   Emergent(c') ⇔ ∃ property p : p ∈ c' ∧ p ∉ c₁ ∧ p ∉ c₂
 *
 * @module @unrdf/kgc-claude/capabilities/composition-engine
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now } from '@unrdf/kgc-4d';

/**
 * Capability atom schema
 */
export const CapabilityAtomSchema = z.object({
  id: z.string(),
  name: z.string(),
  category: z.enum(['control', 'execution', 'policy', 'integration', 'distribution', 'state']),
  properties: z.array(z.string()),
  metrics: z.object({
    latency_ms: z.number().default(0),
    throughput_ops_sec: z.number().default(0),
    operator_steps: z.number().int().default(1),
    policy_strength: z.number().min(0).max(10).default(0),
  }).passthrough(),
});

/**
 * @typedef {z.infer<typeof CapabilityAtomSchema>} CapabilityAtom
 */

/**
 * Composition edge schema
 */
export const CompositionEdgeSchema = z.object({
  from: z.string(),
  to: z.string(),
  relationship: z.string(),
  emergent_properties: z.array(z.string()),
  synergy_delta: z.number(),
  verified: z.boolean().default(false),
  evidence: z.any().optional(),
});

/**
 * @typedef {z.infer<typeof CompositionEdgeSchema>} CompositionEdge
 */

/**
 * Composition result schema
 */
export const CompositionResultSchema = z.object({
  id: z.string(),
  atoms: z.array(z.string()),
  emergent_properties: z.array(z.string()),
  baseline_score: z.number(),
  composite_score: z.number(),
  synergy_delta: z.number(),
  verdict: z.enum(['productive', 'not_productive', 'inconclusive']),
  metrics: z.object({
    operator_steps_delta: z.number(),
    policy_strength_delta: z.number(),
    recovery_time_delta: z.number().optional(),
    parallel_throughput_multiplier: z.number().optional(),
    reproducibility_improvement: z.number().optional(),
  }),
  timestamp: z.bigint(),
  hash: z.string(),
});

/**
 * @typedef {z.infer<typeof CompositionResultSchema>} CompositionResult
 */

/**
 * Capability graph (V, E)
 */
export class CapabilityGraph {
  constructor() {
    /** @type {Map<string, CapabilityAtom>} */
    this.atoms = new Map();
    /** @type {CompositionEdge[]} */
    this.edges = [];
  }

  /**
   * Add capability atom
   * @param {CapabilityAtom} atom
   */
  addAtom(atom) {
    const validated = CapabilityAtomSchema.parse(atom);
    this.atoms.set(validated.id, validated);
  }

  /**
   * Add composition edge
   * @param {CompositionEdge} edge
   */
  addEdge(edge) {
    const validated = CompositionEdgeSchema.parse(edge);
    this.edges.push(validated);
  }

  /**
   * Find edges from atom
   * @param {string} atomId
   * @returns {CompositionEdge[]}
   */
  getEdgesFrom(atomId) {
    return this.edges.filter(e => e.from === atomId);
  }

  /**
   * Find edges to atom
   * @param {string} atomId
   * @returns {CompositionEdge[]}
   */
  getEdgesTo(atomId) {
    return this.edges.filter(e => e.to === atomId);
  }

  /**
   * Find path between atoms
   * @param {string} fromId
   * @param {string} toId
   * @returns {string[] | null}
   */
  findPath(fromId, toId) {
    const visited = new Set();
    const queue = [[fromId]];

    while (queue.length > 0) {
      const path = queue.shift();
      const current = path[path.length - 1];

      if (current === toId) {
        return path;
      }

      if (visited.has(current)) {
        continue;
      }

      visited.add(current);

      const outgoing = this.getEdgesFrom(current);
      for (const edge of outgoing) {
        queue.push([...path, edge.to]);
      }
    }

    return null;
  }

  /**
   * Get all atoms by category
   * @param {string} category
   * @returns {CapabilityAtom[]}
   */
  getByCategory(category) {
    return Array.from(this.atoms.values())
      .filter(a => a.category === category);
  }

  /**
   * Export graph as JSON
   * @returns {Object}
   */
  toJSON() {
    return {
      atoms: Array.from(this.atoms.values()),
      edges: this.edges,
      stats: {
        atom_count: this.atoms.size,
        edge_count: this.edges.length,
        verified_edges: this.edges.filter(e => e.verified).length,
      },
    };
  }
}

/**
 * Composition Engine - Compose capabilities and detect emergent properties
 */
export class CompositionEngine {
  constructor() {
    this.graph = new CapabilityGraph();
    /** @type {CompositionResult[]} */
    this.results = [];
  }

  /**
   * Register capability atom
   * @param {CapabilityAtom} atom
   */
  registerCapability(atom) {
    this.graph.addAtom(atom);
  }

  /**
   * Compose two capabilities
   * Π(c₁, c₂) → c'
   * @param {string} atom1Id
   * @param {string} atom2Id
   * @param {Function} testFn - Function that tests the composition
   * @returns {Promise<CompositionResult>}
   */
  async compose(atom1Id, atom2Id, testFn) {
    const atom1 = this.graph.atoms.get(atom1Id);
    const atom2 = this.graph.atoms.get(atom2Id);

    if (!atom1 || !atom2) {
      throw new Error(`Atoms not found: ${atom1Id}, ${atom2Id}`);
    }

    // Calculate baseline (sum of individual atoms)
    const baselineScore = this.calculateScore(atom1.metrics) +
                         this.calculateScore(atom2.metrics);

    // Execute composition test
    const testResult = await testFn(atom1, atom2);

    // Calculate composite score
    const compositeScore = this.calculateScore(testResult.metrics);

    // Calculate synergy delta
    const synergyDelta = compositeScore - baselineScore;

    // Detect emergent properties
    const emergentProperties = this.detectEmergentProperties(
      atom1.properties,
      atom2.properties,
      testResult.properties || []
    );

    // Calculate metric deltas
    const metrics = {
      operator_steps_delta:
        (atom1.metrics.operator_steps + atom2.metrics.operator_steps) -
        (testResult.metrics.operator_steps || 1),
      policy_strength_delta:
        (testResult.metrics.policy_strength || 0) -
        (atom1.metrics.policy_strength + atom2.metrics.policy_strength),
      recovery_time_delta: testResult.metrics.recovery_time_delta,
      parallel_throughput_multiplier: testResult.metrics.parallel_throughput_multiplier,
      reproducibility_improvement: testResult.metrics.reproducibility_improvement,
    };

    // Determine verdict
    const verdict = this.determineVerdict(synergyDelta, emergentProperties, metrics);

    // Create composition result
    const t_ns = now();
    const resultData = {
      id: `comp-${atom1Id}-${atom2Id}-${t_ns}`,
      atoms: [atom1Id, atom2Id],
      emergent_properties: emergentProperties,
      baseline_score: baselineScore,
      composite_score: compositeScore,
      synergy_delta: synergyDelta,
      verdict,
      metrics,
      timestamp: t_ns,
    };

    const hash = await blake3(JSON.stringify(resultData, (key, value) =>
      typeof value === 'bigint' ? value.toString() : value
    ));

    const result = CompositionResultSchema.parse({
      ...resultData,
      hash,
    });

    this.results.push(result);

    // If productive, add edge to graph
    if (verdict === 'productive') {
      this.graph.addEdge({
        from: atom1Id,
        to: atom2Id,
        relationship: testResult.relationship || 'composes-with',
        emergent_properties: emergentProperties,
        synergy_delta: synergyDelta,
        verified: true,
        evidence: testResult.evidence,
      });
    }

    return result;
  }

  /**
   * Compose three capabilities (triple composition)
   * Π(c₁, c₂, c₃) → c'
   * @param {string} atom1Id
   * @param {string} atom2Id
   * @param {string} atom3Id
   * @param {Function} testFn
   * @returns {Promise<CompositionResult>}
   */
  async composeTriple(atom1Id, atom2Id, atom3Id, testFn) {
    const atom1 = this.graph.atoms.get(atom1Id);
    const atom2 = this.graph.atoms.get(atom2Id);
    const atom3 = this.graph.atoms.get(atom3Id);

    if (!atom1 || !atom2 || !atom3) {
      throw new Error(`Atoms not found: ${atom1Id}, ${atom2Id}, ${atom3Id}`);
    }

    // Calculate baseline (sum of individual atoms)
    const baselineScore = this.calculateScore(atom1.metrics) +
                         this.calculateScore(atom2.metrics) +
                         this.calculateScore(atom3.metrics);

    // Execute composition test
    const testResult = await testFn(atom1, atom2, atom3);

    // Calculate composite score
    const compositeScore = this.calculateScore(testResult.metrics);

    // Calculate synergy delta
    const synergyDelta = compositeScore - baselineScore;

    // Detect emergent properties
    const emergentProperties = this.detectEmergentProperties(
      [...atom1.properties, ...atom2.properties, ...atom3.properties],
      [],
      testResult.properties || []
    );

    // Calculate metric deltas
    const metrics = {
      operator_steps_delta:
        (atom1.metrics.operator_steps + atom2.metrics.operator_steps + atom3.metrics.operator_steps) -
        (testResult.metrics.operator_steps || 1),
      policy_strength_delta:
        (testResult.metrics.policy_strength || 0) -
        (atom1.metrics.policy_strength + atom2.metrics.policy_strength + atom3.metrics.policy_strength),
      recovery_time_delta: testResult.metrics.recovery_time_delta,
      parallel_throughput_multiplier: testResult.metrics.parallel_throughput_multiplier,
      reproducibility_improvement: testResult.metrics.reproducibility_improvement,
    };

    // Determine verdict
    const verdict = this.determineVerdict(synergyDelta, emergentProperties, metrics);

    // Create composition result
    const t_ns = now();
    const resultData = {
      id: `comp-${atom1Id}-${atom2Id}-${atom3Id}-${t_ns}`,
      atoms: [atom1Id, atom2Id, atom3Id],
      emergent_properties: emergentProperties,
      baseline_score: baselineScore,
      composite_score: compositeScore,
      synergy_delta: synergyDelta,
      verdict,
      metrics,
      timestamp: t_ns,
    };

    const hash = await blake3(JSON.stringify(resultData, (key, value) =>
      typeof value === 'bigint' ? value.toString() : value
    ));

    const result = CompositionResultSchema.parse({
      ...resultData,
      hash,
    });

    this.results.push(result);

    return result;
  }

  /**
   * Calculate score from metrics
   * @param {Object} metrics
   * @returns {number}
   */
  calculateScore(metrics) {
    const latencyScore = Math.max(0, 100 - (metrics.latency_ms || 0) / 10);
    const throughputScore = Math.min(100, (metrics.throughput_ops_sec || 0) / 10);
    const stepScore = Math.max(0, 100 - (metrics.operator_steps || 1) * 10);
    const policyScore = (metrics.policy_strength || 0) * 10;

    return (latencyScore + throughputScore + stepScore + policyScore) / 4;
  }

  /**
   * Detect emergent properties
   * Emergent(c') ⇔ ∃ property p : p ∈ c' ∧ p ∉ c₁ ∧ p ∉ c₂
   * @param {string[]} props1
   * @param {string[]} props2
   * @param {string[]} compositeProps
   * @returns {string[]}
   */
  detectEmergentProperties(props1, props2, compositeProps) {
    const baseProps = new Set([...props1, ...props2]);
    return compositeProps.filter(p => !baseProps.has(p));
  }

  /**
   * Determine composition verdict
   * @param {number} synergyDelta
   * @param {string[]} emergentProperties
   * @param {Object} metrics
   * @returns {'productive' | 'not_productive' | 'inconclusive'}
   */
  determineVerdict(synergyDelta, emergentProperties, metrics) {
    // Productive if:
    // 1. Synergy delta > 0
    // 2. At least one emergent property
    // 3. At least one metric shows ≥20% improvement

    if (synergyDelta <= 0) {
      return 'not_productive';
    }

    if (emergentProperties.length === 0) {
      return 'inconclusive';
    }

    const hasSignificantImprovement =
      metrics.operator_steps_delta >= 2 ||
      metrics.policy_strength_delta >= 1 ||
      (metrics.recovery_time_delta && metrics.recovery_time_delta >= 0.5) ||
      (metrics.parallel_throughput_multiplier && metrics.parallel_throughput_multiplier >= 1.5) ||
      (metrics.reproducibility_improvement && metrics.reproducibility_improvement >= 0.1);

    if (hasSignificantImprovement) {
      return 'productive';
    }

    return 'inconclusive';
  }

  /**
   * Get productive compositions
   * @returns {CompositionResult[]}
   */
  getProductiveCompositions() {
    return this.results.filter(r => r.verdict === 'productive');
  }

  /**
   * Export engine state
   * @returns {Object}
   */
  export() {
    return {
      graph: this.graph.toJSON(),
      results: this.results,
      productive_count: this.getProductiveCompositions().length,
      stats: {
        total_compositions_tested: this.results.length,
        productive: this.results.filter(r => r.verdict === 'productive').length,
        not_productive: this.results.filter(r => r.verdict === 'not_productive').length,
        inconclusive: this.results.filter(r => r.verdict === 'inconclusive').length,
      },
    };
  }
}

/**
 * Create composition engine
 * @returns {CompositionEngine}
 */
export function createCompositionEngine() {
  return new CompositionEngine();
}

export default CompositionEngine;
