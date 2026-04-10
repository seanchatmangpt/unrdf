/**
 * @file Causality Chain Module
 * @module manufacturing/causality
 * @description A=μ(O) lineage tracking — records the ontology→operators→artifact transformation chain
 */

/**
 * Tracks the causal chain of operators applied to an ontology to produce an artifact.
 * Implements A = μ(O): artifact is a projection of ontology via transformation sequence.
 */
export class CausalityChain {
  /**
   * @param {string} ontologySource - Identifier of the source ontology (e.g., 'seed-dna.nt')
   */
  constructor(ontologySource) {
    this.ontologySource = ontologySource;
    this.steps = [];
    this.createdAt = new Date().toISOString();
  }

  /**
   * Record an operator step in the causal chain.
   * @param {string} operatorName - Name of the operator applied
   * @param {object} input - Input to the operator
   * @param {object} output - Output from the operator
   * @returns {CausalityChain} this (chainable)
   */
  addStep(operatorName, input, output) {
    this.steps.push({
      operatorName,
      input,
      output,
      timestamp: new Date().toISOString(),
    });
    return this;
  }

  /**
   * Number of operators applied so far.
   * @returns {number}
   */
  get depth() {
    return this.steps.length;
  }

  /**
   * Serialize the chain for audit and receipt generation.
   * @returns {{ ontologySource: string, createdAt: string, steps: Array, depth: number }}
   */
  toJSON() {
    return {
      ontologySource: this.ontologySource,
      createdAt: this.createdAt,
      steps: this.steps,
      depth: this.depth,
    };
  }
}
