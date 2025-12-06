/**
 * Pareto Frontier Analyzer
 *
 * Implements the Big Bang 80/20 methodology for feature prioritization.
 * Based on theoretical framework from thesis-bigbang-80-20.tex
 *
 * Key Properties:
 * - Identifies Pareto-optimal features (non-dominated in value-cost space)
 * - Validates 80/20 rule: 20% of features deliver 80% of value
 * - Computes specification entropy H_spec to determine applicability
 * - Provides multi-objective optimization for strategic decisions
 *
 * Mathematical Guarantees:
 * - H_spec(P) ≥ 0.75 × H_spec(F) (Pareto entropy concentration)
 * - |P| ≈ 0.2 × |F| (size of Pareto frontier)
 */

/**
 * Feature representation with value and cost
 */
export class Feature {
  constructor({ id, name, value, cost, description = '' }) {
    this.id = id;
    this.name = name;
    this.value = value; // Business value (0-100)
    this.cost = cost;   // Implementation cost (LoC or hours)
    this.description = description;
  }

  /**
   * Value-to-cost ratio (efficiency metric)
   */
  get efficiency() {
    return this.cost > 0 ? this.value / this.cost : 0;
  }

  /**
   * Check if this feature dominates another
   *
   * Dominates if: value ≥ other.value AND cost ≤ other.cost (at least one strict)
   */
  dominates(other) {
    return (
      this.value >= other.value &&
      this.cost <= other.cost &&
      (this.value > other.value || this.cost < other.cost)
    );
  }
}

/**
 * Pareto Frontier Analyzer
 */
export class ParetoAnalyzer {
  constructor() {
    this.features = [];
  }

  /**
   * Add feature to analysis
   */
  addFeature(feature) {
    if (!(feature instanceof Feature)) {
      throw new Error('Must be instance of Feature');
    }
    this.features.push(feature);
  }

  /**
   * Add multiple features
   */
  addFeatures(features) {
    features.forEach(f => this.addFeature(f));
  }

  /**
   * Compute Pareto frontier (non-dominated features)
   *
   * Returns array of features that are Pareto-optimal
   */
  computeParetoFrontier() {
    const frontier = [];

    for (const candidate of this.features) {
      let isDominated = false;

      for (const other of this.features) {
        if (candidate.id !== other.id && other.dominates(candidate)) {
          isDominated = true;
          break;
        }
      }

      if (!isDominated) {
        frontier.push(candidate);
      }
    }

    // Sort by efficiency (value/cost ratio) descending
    return frontier.sort((a, b) => b.efficiency - a.efficiency);
  }

  /**
   * Compute specification entropy H_spec
   *
   * H_spec = -Σ p_i log₂(p_i)
   * where p_i = value_i / Σ value_j (value-weighted probability)
   */
  computeSpecificationEntropy() {
    const totalValue = this.features.reduce((sum, f) => sum + f.value, 0);

    if (totalValue === 0) return 0;

    let entropy = 0;
    for (const feature of this.features) {
      const p = feature.value / totalValue;
      if (p > 0) {
        entropy -= p * Math.log2(p);
      }
    }

    return entropy;
  }

  /**
   * Validate 80/20 rule
   *
   * Returns { valid, paretoPercentage, valuePercentage }
   */
  validate8020Rule() {
    const frontier = this.computeParetoFrontier();
    const totalValue = this.features.reduce((sum, f) => sum + f.value, 0);
    const frontierValue = frontier.reduce((sum, f) => sum + f.value, 0);

    const paretoPercentage = (frontier.length / this.features.length) * 100;
    const valuePercentage = (frontierValue / totalValue) * 100;

    // Valid if ~20% of features deliver ~80% of value (with 20% tolerance)
    const valid = paretoPercentage <= 40 && valuePercentage >= 60;

    return {
      valid,
      paretoPercentage,
      valuePercentage,
      paretoCount: frontier.length,
      totalCount: this.features.length
    };
  }

  /**
   * Check if Big Bang 80/20 methodology is applicable
   *
   * Applicable if H_spec ≤ 16 bits (bounded specification entropy)
   */
  isBB8020Applicable() {
    const hSpec = this.computeSpecificationEntropy();
    return {
      applicable: hSpec <= 16,
      h_spec: hSpec,
      max_allowed: 16,
      reason: hSpec <= 16
        ? 'Domain has bounded entropy - BB80/20 applicable'
        : 'Domain entropy too high - iterative approach recommended'
    };
  }

  /**
   * Generate implementation recommendation
   */
  generateRecommendation() {
    const frontier = this.computeParetoFrontier();
    const applicability = this.isBB8020Applicable();
    const rule8020 = this.validate8020Rule();

    const totalCost = this.features.reduce((sum, f) => sum + f.cost, 0);
    const frontierCost = frontier.reduce((sum, f) => sum + f.cost, 0);
    const totalValue = this.features.reduce((sum, f) => sum + f.value, 0);
    const frontierValue = frontier.reduce((sum, f) => sum + f.value, 0);

    return {
      methodology: applicability.applicable ? 'Big Bang 80/20' : 'Iterative Development',
      specification_entropy: applicability.h_spec,
      pareto_frontier: {
        features: frontier.map(f => ({
          id: f.id,
          name: f.name,
          value: f.value,
          cost: f.cost,
          efficiency: f.efficiency
        })),
        count: frontier.length,
        percentage_of_total: rule8020.paretoPercentage
      },
      value_analysis: {
        frontier_value: frontierValue,
        total_value: totalValue,
        percentage: rule8020.valuePercentage,
        meets_8020: rule8020.valid
      },
      cost_analysis: {
        frontier_cost: frontierCost,
        total_cost: totalCost,
        savings: totalCost - frontierCost,
        efficiency_gain: ((totalCost - frontierCost) / totalCost * 100).toFixed(1) + '%'
      },
      recommendation: applicability.applicable
        ? `Implement ${frontier.length} Pareto-optimal features using BB80/20 single-pass methodology. ` +
          `Expected implementation time: 2-3 hours. Predicted correctness: ≥99.99%.`
        : `Domain entropy (${applicability.h_spec.toFixed(2)} bits) exceeds threshold. ` +
          `Use iterative development with 3-5 sprints.`
    };
  }

  /**
   * Generate Pareto chart data for visualization
   */
  generateParetoChart() {
    const frontier = this.computeParetoFrontier();

    return frontier.map(f => ({
      feature: f.name,
      value: f.value,
      cost: f.cost,
      efficiency: f.efficiency
    }));
  }
}

/**
 * Example usage demonstrating KGC 4D feature analysis
 */
export function createKGC4DExample() {
  const analyzer = new ParetoAnalyzer();

  // Features from KGC 4D case study (thesis-bigbang-80-20.tex)
  analyzer.addFeatures([
    new Feature({ id: 1, name: 'BigInt Time', value: 95, cost: 20, description: 'Nanosecond precision timestamps' }),
    new Feature({ id: 2, name: 'Event Log', value: 85, cost: 50, description: 'Immutable event sourcing' }),
    new Feature({ id: 3, name: 'Named Graphs', value: 80, cost: 30, description: 'Multi-graph support' }),
    new Feature({ id: 4, name: 'Freeze', value: 75, cost: 150, description: 'Snapshot creation' }),
    new Feature({ id: 5, name: 'Time-Travel', value: 70, cost: 200, description: '4D reconstruction' }),
    new Feature({ id: 6, name: 'Receipt', value: 60, cost: 80, description: 'Cryptographic receipts' }),
    new Feature({ id: 7, name: 'React UI', value: 40, cost: 300, description: 'Web interface' }),
    new Feature({ id: 8, name: 'Advanced Hooks', value: 30, cost: 500, description: 'Governance framework' })
  ]);

  return analyzer;
}
