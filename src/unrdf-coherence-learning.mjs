/**
 * @file Coherence Learning System
 * @module unrdf-coherence-learning
 * @description Machine learning component that learns from repair outcomes and predicts repair effectiveness
 */

export class CoherenceLearningSystem {
  constructor() {
    // Pattern database: maps contradiction type + repair type -> outcomes
    this.repairOutcomes = new Map();

    // Feature extraction: contradictions with similar patterns
    this.patternClusters = new Map();

    // Model parameters: weights for repair type effectiveness
    this.repairEffectiveness = new Map();

    // Historical data for analysis
    this.repairHistory = [];
    this.contradictionHistory = [];

    // Feedback cache: stores verification results
    this.feedbackCache = [];

    // Learning state
    this.learningState = {
      samplesProcessed: 0,
      patternsDiscovered: 0,
      modelAccuracy: 0,
      lastUpdate: Date.now(),
    };

    this.maxHistory = 1000;
  }

  recordRepairAttempt(contradiction, repair, outcome, verification) {
    const record = {
      timestamp: Date.now(),
      contradiction: {
        type: contradiction.type,
        package: contradiction.package,
        dependency: contradiction.dependency,
        severity: contradiction.severity,
      },
      repair: {
        type: repair.type,
        action: repair.action,
        difficulty: repair.difficulty,
        risk: repair.risk,
      },
      outcome: outcome,
      verification: verification,
      effectiveness: this._calculateEffectiveness(outcome, verification),
    };

    this.repairHistory.push(record);
    if (this.repairHistory.length > this.maxHistory) {
      this.repairHistory = this.repairHistory.slice(-this.maxHistory);
    }

    // Update learning
    this._updateRepairModel(record);
    return record;
  }

  _calculateEffectiveness(outcome, verification) {
    let score = 0;

    // Did the repair apply successfully?
    if (outcome.success) score += 40;

    // Did it resolve the contradiction?
    if (verification.resolved) score += 50;

    // Was it low risk?
    const riskScores = { low: 10, medium: 5, high: 0 };
    score += riskScores[outcome.risk] || 0;

    return Math.min(100, Math.max(0, score));
  }

  _updateRepairModel(record) {
    this.learningState.samplesProcessed++;

    const key = `${record.contradiction.type}::${record.repair.type}`;

    if (!this.repairOutcomes.has(key)) {
      this.repairOutcomes.set(key, {
        attempts: 0,
        successes: 0,
        resolutions: 0,
        avgEffectiveness: 0,
        outcomes: [],
      });
    }

    const model = this.repairOutcomes.get(key);
    model.attempts++;

    if (record.outcome.success) model.successes++;
    if (record.verification.resolved) model.resolutions++;

    model.outcomes.push(record.effectiveness);
    model.avgEffectiveness = model.outcomes.reduce((a, b) => a + b, 0) / model.outcomes.length;

    // Update effectiveness weights
    if (!this.repairEffectiveness.has(record.repair.type)) {
      this.repairEffectiveness.set(record.repair.type, {
        weight: 0.5,
        sampleSize: 0,
        avgOutcome: 0,
      });
    }

    const repairModel = this.repairEffectiveness.get(record.repair.type);
    repairModel.sampleSize++;
    repairModel.avgOutcome = (repairModel.avgOutcome * (repairModel.sampleSize - 1) + record.effectiveness) / repairModel.sampleSize;

    // Adjust weight based on outcomes
    repairModel.weight = Math.max(0.1, Math.min(1.0, repairModel.avgOutcome / 100));

    this.learningState.lastUpdate = Date.now();
  }

  recordContradiction(contradiction) {
    const record = {
      timestamp: Date.now(),
      type: contradiction.type,
      package: contradiction.package,
      dependency: contradiction.dependency,
      severity: contradiction.severity,
    };

    this.contradictionHistory.push(record);
    if (this.contradictionHistory.length > this.maxHistory) {
      this.contradictionHistory = this.contradictionHistory.slice(-this.maxHistory);
    }

    // Extract features for clustering
    this._extractFeatures(contradiction);
  }

  _extractFeatures(contradiction) {
    const featureVector = {
      type: contradiction.type,
      severity: contradiction.severity,
      packageNameLength: contradiction.package.length,
      packageNamePattern: this._analyzeNamePattern(contradiction.package),
      depNamePattern: this._analyzeNamePattern(contradiction.dependency),
    };

    const clusterKey = JSON.stringify(featureVector);

    if (!this.patternClusters.has(clusterKey)) {
      this.patternClusters.set(clusterKey, {
        count: 0,
        members: [],
        centroid: featureVector,
      });
      this.learningState.patternsDiscovered++;
    }

    const cluster = this.patternClusters.get(clusterKey);
    cluster.count++;
    cluster.members.push(contradiction);
  }

  _analyzeNamePattern(name) {
    const parts = name.split('-');
    return {
      partCount: parts.length,
      hasCore: name.includes('core'),
      hasUtils: name.includes('utils'),
      hasKgc: name.includes('kgc'),
      hasV6: name.includes('v6'),
    };
  }

  predictRepairOutcome(contradiction, repairOption) {
    const key = `${contradiction.type}::${repairOption.type}`;
    const outcomes = this.repairOutcomes.get(key);

    if (!outcomes) {
      // No historical data - make conservative prediction
      return {
        predicted: false,
        confidence: 0.3,
        reason: 'No historical data',
        reasoning: [
          `Repair type '${repairOption.type}' has not been tried for '${contradiction.type}' contradictions`,
          'Conservative prediction: low confidence',
          'Recommendation: proceed with caution',
        ],
      };
    }

    const successRate = outcomes.successes / outcomes.attempts;
    const resolutionRate = outcomes.resolutions / outcomes.attempts;
    const effectiveness = outcomes.avgEffectiveness / 100;

    const repairWeight = this.repairEffectiveness.get(repairOption.type)?.weight || 0.5;
    const confidence = Math.min(1.0, (outcomes.attempts / 10) * 0.5 + effectiveness * 0.5);

    return {
      predicted: effectiveness > 0.5,
      confidence,
      successRate: (successRate * 100).toFixed(1),
      resolutionRate: (resolutionRate * 100).toFixed(1),
      effectiveness: (effectiveness * 100).toFixed(1),
      repairWeight,
      reasoning: [
        `Repair type '${repairOption.type}' has ${outcomes.attempts} historical attempts`,
        `Success rate: ${(successRate * 100).toFixed(1)}%`,
        `Contradiction resolution rate: ${(resolutionRate * 100).toFixed(1)}%`,
        `Average effectiveness: ${(effectiveness * 100).toFixed(1)}/100`,
        `Confidence in prediction: ${(confidence * 100).toFixed(1)}%`,
      ],
    };
  }

  rankRepairOptions(contradiction, repairOptions) {
    const scored = repairOptions.map((option) => ({
      option,
      prediction: this.predictRepairOutcome(contradiction, option),
    }));

    // Score by: predicted outcome * confidence * (1 - risk)
    const riskScores = { low: 0.1, medium: 0.5, high: 1.0 };

    scored.forEach((item) => {
      const riskPenalty = riskScores[item.option.risk] || 0.5;
      const prediction = item.prediction.predicted ? 1.0 : 0.0;
      const confidence = item.prediction.confidence;

      item.score = prediction * confidence * (1.0 - riskPenalty * 0.3);
    });

    scored.sort((a, b) => b.score - a.score);

    return scored.map((item) => ({
      option: item.option,
      score: item.score,
      prediction: item.prediction,
    }));
  }

  getSimilarContradictions(contradiction) {
    // Find contradictions with similar patterns
    const similar = [];

    for (const history of this.contradictionHistory) {
      if (history.type === contradiction.type && history.severity === contradiction.severity) {
        similar.push(history);
      }
    }

    return {
      count: similar.length,
      matches: similar.slice(-5), // Return last 5
      pattern: {
        type: contradiction.type,
        severity: contradiction.severity,
      },
    };
  }

  getRepairMetamodel() {
    const model = {
      timestamp: Date.now(),
      learning_state: this.learningState,
      repair_types: {},
      contradiction_types: {},
      accuracy_metrics: {},
    };

    // Analyze each repair type
    for (const [type, data] of this.repairEffectiveness) {
      model.repair_types[type] = {
        sampleSize: data.sampleSize,
        weight: data.weight.toFixed(2),
        avgOutcome: data.avgOutcome.toFixed(1),
      };
    }

    // Analyze outcomes by contradiction type
    const contradictionTypes = new Set();
    for (const key of this.repairOutcomes.keys()) {
      const [type] = key.split('::');
      contradictionTypes.add(type);
    }

    for (const type of contradictionTypes) {
      const typeOutcomes = Array.from(this.repairOutcomes.entries())
        .filter(([key]) => key.startsWith(type + '::'))
        .map(([, outcome]) => outcome);

      const totalAttempts = typeOutcomes.reduce((sum, o) => sum + o.attempts, 0);
      const totalSuccesses = typeOutcomes.reduce((sum, o) => sum + o.successes, 0);
      const totalResolutions = typeOutcomes.reduce((sum, o) => sum + o.resolutions, 0);

      model.contradiction_types[type] = {
        attempts: totalAttempts,
        successRate: ((totalSuccesses / totalAttempts) * 100).toFixed(1),
        resolutionRate: ((totalResolutions / totalAttempts) * 100).toFixed(1),
      };
    }

    // Overall accuracy
    const totalAttempts = this.repairHistory.length;
    if (totalAttempts > 0) {
      const totalSuccess = this.repairHistory.filter((r) => r.outcome.success).length;
      const totalResolved = this.repairHistory.filter((r) => r.verification.resolved).length;

      model.accuracy_metrics = {
        totalRepairs: totalAttempts,
        successRate: ((totalSuccess / totalAttempts) * 100).toFixed(1),
        resolutionRate: ((totalResolved / totalAttempts) * 100).toFixed(1),
        avgEffectiveness: (this.repairHistory.reduce((sum, r) => sum + r.effectiveness, 0) / totalAttempts).toFixed(1),
      };
    }

    return model;
  }

  hasEnoughData(contradictionType, repairType) {
    const key = `${contradictionType}::${repairType}`;
    const outcomes = this.repairOutcomes.get(key);
    return outcomes && outcomes.attempts >= 3;
  }

  getPatternInsights() {
    const insights = {
      totalPatterns: this.patternClusters.size,
      patterns: [],
    };

    for (const [, cluster] of this.patternClusters) {
      insights.patterns.push({
        frequency: cluster.count,
        example: cluster.members[0] || {},
        variants: cluster.members.length,
      });
    }

    return insights;
  }

  export() {
    return {
      timestamp: Date.now(),
      learningState: this.learningState,
      metamodel: this.getRepairMetamodel(),
      patternInsights: this.getPatternInsights(),
      historySize: {
        repairs: this.repairHistory.length,
        contradictions: this.contradictionHistory.length,
      },
    };
  }
}

export default CoherenceLearningSystem;
