/**
 * @file Chatman Equation Integration Tests
 * @module knowledge-engine/test/chatman-integration
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  ChatmanOperator,
  createChatmanOperator,
  ArtifactGenerator,
  createArtifactGenerator,
  DarkFieldDetector,
  createDarkFieldDetector,
  FormationTheorems,
  createFormationTheorems,
  ChatmanEngine,
  createChatmanEngine,
} from '../src/index.mjs';

describe('ChatmanOperator', () => {
  let operator;

  beforeEach(() => {
    operator = createChatmanOperator({ observableRatio: 0.05 });
  });

  it('should create operator with default options', () => {
    expect(operator).toBeInstanceOf(ChatmanOperator);
    expect(operator.observableRatio).toBe(0.05);
    expect(operator.closureThreshold).toBe(0.95);
  });

  it('should apply μ operator to compute closure', async () => {
    const observable = {
      type: 'market',
      patterns: ['customer_feedback', 'sales_trends', 'competitor_analysis'],
      visibility: 0.05,
    };

    const result = await operator.apply(observable);

    expect(result).toBeDefined();
    expect(result.observable).toBeDefined();
    expect(result.darkField).toBeDefined();
    expect(result.darkField.patterns.length).toBeGreaterThan(
      observable.patterns.length
    );
    expect(result.completeness).toBeGreaterThan(0.9);
  });

  it('should generate dark field patterns for market dynamics', async () => {
    const observable = {
      type: 'market',
      patterns: ['product_demand'],
      visibility: 0.05,
    };

    const result = await operator.apply(observable);

    expect(result.darkField.patterns).toContain(
      'hidden_customer_needs:derived_from:product_demand'
    );
    expect(result.darkField.visibility).toBe(0.95);
  });

  it('should generate dark field patterns for organizational dynamics', async () => {
    const observable = {
      type: 'organizational',
      patterns: ['team_structure'],
      visibility: 0.05,
    };

    const result = await operator.apply(observable);

    expect(result.darkField.patterns.some(p => p.includes('informal_power_structures'))).toBe(
      true
    );
  });

  it('should track operator metrics', async () => {
    const observable = {
      type: 'strategic',
      patterns: ['market_position'],
      visibility: 0.05,
    };

    await operator.apply(observable);

    const metrics = operator.getMetrics();
    expect(metrics.closureOperations).toBe(1);
    expect(metrics.averageCompleteness).toBeGreaterThan(0);
    expect(metrics.totalDarkFieldRevealed).toBeGreaterThan(0);
  });
});

describe('ArtifactGenerator', () => {
  let generator;

  beforeEach(() => {
    generator = createArtifactGenerator({ observableRatio: 0.05 });
  });

  it('should create generator with default options', () => {
    expect(generator).toBeInstanceOf(ArtifactGenerator);
  });

  it('should generate artifact from observable: A = μ(O)', async () => {
    const observable = {
      type: 'market',
      patterns: ['customer_feedback', 'sales_data'],
      visibility: 0.05,
    };

    const artifact = await generator.generate(observable);

    expect(artifact).toBeDefined();
    expect(artifact.id).toBeDefined();
    expect(artifact.type).toBe('market');
    expect(artifact.observable.patterns).toEqual(observable.patterns);
    expect(artifact.darkField).toBeDefined();
    expect(artifact.totalPatterns).toBeGreaterThan(observable.patterns.length);
    expect(artifact.completeness).toBeGreaterThan(0.9);
  });

  it('should include receipt when requested', async () => {
    const observable = {
      type: 'organizational',
      patterns: ['workflow_patterns'],
      visibility: 0.05,
    };

    const artifact = await generator.generate(observable, { includeReceipt: true });

    expect(artifact.receipt).toBeDefined();
    expect(artifact.receipt.operation).toBe('artifact_generation');
  });

  it('should generate batch artifacts', async () => {
    const observables = [
      { type: 'market', patterns: ['trend1'], visibility: 0.05 },
      { type: 'strategic', patterns: ['strategy1'], visibility: 0.05 },
    ];

    const artifacts = await generator.generateBatch(observables);

    expect(artifacts).toHaveLength(2);
    expect(artifacts[0].type).toBe('market');
    expect(artifacts[1].type).toBe('strategic');
  });

  it('should track generator metrics', async () => {
    const observable = {
      type: 'disruption',
      patterns: ['innovation_trend'],
      visibility: 0.05,
    };

    await generator.generate(observable);

    const metrics = generator.getMetrics();
    expect(metrics.artifactsGenerated).toBe(1);
    expect(metrics.averageCompleteness).toBeGreaterThan(0);
    expect(metrics.averageDarkFieldSize).toBeGreaterThan(0);
  });
});

describe('DarkFieldDetector', () => {
  let detector;

  beforeEach(() => {
    detector = createDarkFieldDetector({ targetRatio: 0.95 });
  });

  it('should create detector with default options', () => {
    expect(detector).toBeInstanceOf(DarkFieldDetector);
    expect(detector.targetRatio).toBe(0.95);
  });

  it('should detect 95% dark field patterns', async () => {
    const observable = {
      type: 'market',
      patterns: ['visible_trend'],
      visibility: 0.05,
    };

    const result = await detector.detect(observable);

    expect(result).toBeDefined();
    expect(result.darkFieldRatio).toBeCloseTo(0.95, 1);
    expect(result.darkFieldSize).toBeGreaterThan(result.observableSize * 15);
  });

  it('should generate confidence scores for detected patterns', async () => {
    const observable = {
      type: 'strategic',
      patterns: ['strategy1', 'strategy2'],
      visibility: 0.05,
    };

    const result = await detector.detect(observable, { minConfidence: 0.7 });

    expect(result.patterns).toBeDefined();
    expect(result.patterns.length).toBeGreaterThan(0);
    result.patterns.forEach(pattern => {
      expect(pattern.confidence).toBeGreaterThanOrEqual(0.7);
      expect(pattern.dimension).toBeDefined();
      expect(pattern.derivedFrom).toBeDefined();
    });
  });

  it('should respect maxPatterns option', async () => {
    const observable = {
      type: 'organizational',
      patterns: ['pattern1'],
      visibility: 0.05,
    };

    const result = await detector.detect(observable, { maxPatterns: 5 });

    expect(result.patterns.length).toBeLessThanOrEqual(5);
  });

  it('should track detector metrics', async () => {
    const observable = {
      type: 'disruption',
      patterns: ['tech_shift'],
      visibility: 0.05,
    };

    await detector.detect(observable);

    const metrics = detector.getMetrics();
    expect(metrics.detectionsPerformed).toBe(1);
    expect(metrics.averageDarkFieldRatio).toBeGreaterThan(0);
    expect(metrics.totalPatternsDetected).toBeGreaterThan(0);
  });
});

describe('FormationTheorems', () => {
  let theorems;
  let artifact;

  beforeEach(async () => {
    theorems = createFormationTheorems();

    // Create sample artifact
    const generator = createArtifactGenerator({ observableRatio: 0.05 });
    artifact = await generator.generate({
      type: 'market',
      patterns: ['customer_need_1', 'market_trend_1'],
      visibility: 0.05,
    });
  });

  it('should create formation theorems calculator', () => {
    expect(theorems).toBeInstanceOf(FormationTheorems);
  });

  it('should apply emergence theorem', async () => {
    const formation = await theorems.derive(artifact, { theorem: 'emergence' });

    expect(formation).toBeDefined();
    expect(formation.theorem).toBe('emergence');
    expect(formation.output.formation).toBeDefined();
    expect(formation.output.valueProposition).toBeDefined();
    expect(formation.output.strategicMoves).toBeDefined();
    expect(formation.confidence).toBeGreaterThan(0.7);
  });

  it('should apply value innovation theorem', async () => {
    const formation = await theorems.derive(artifact, { theorem: 'value_innovation' });

    expect(formation.theorem).toBe('value_innovation');
    expect(formation.output.formation.some(f => f.includes('value_innovation'))).toBe(
      true
    );
  });

  it('should apply strategic canvas theorem', async () => {
    const formation = await theorems.derive(artifact, { theorem: 'strategic_canvas' });

    expect(formation.theorem).toBe('strategic_canvas');
    expect(formation.output.strategicMoves.some(m => m.startsWith('eliminate:'))).toBe(
      true
    );
  });

  it('should apply four actions theorem', async () => {
    const formation = await theorems.derive(artifact, { theorem: 'four_actions' });

    expect(formation.theorem).toBe('four_actions');
    expect(formation.output.formation.some(f => f.startsWith('eliminate:'))).toBe(true);
    expect(formation.output.formation.some(f => f.startsWith('create:'))).toBe(true);
  });

  it('should track theorem metrics', async () => {
    await theorems.derive(artifact, { theorem: 'emergence' });

    const metrics = theorems.getMetrics();
    expect(metrics.theoremsApplied).toBe(1);
    expect(metrics.formationsGenerated).toBeGreaterThan(0);
    expect(metrics.averageConfidence).toBeGreaterThan(0);
  });
});

describe('ChatmanEngine', () => {
  let engine;

  beforeEach(() => {
    engine = createChatmanEngine({
      observableRatio: 0.05,
      enableReceipts: true,
    });
  });

  it('should create engine with default options', () => {
    expect(engine).toBeInstanceOf(ChatmanEngine);
    expect(engine.enableReceipts).toBe(true);
  });

  it('should execute closure operation', async () => {
    const observable = {
      type: 'market',
      patterns: ['trend1', 'trend2'],
      visibility: 0.05,
    };

    const result = await engine.executeClosure(observable);

    expect(result).toBeDefined();
    expect(result.operation).toBe('closure');
    expect(result.output.closure).toBeDefined();
    expect(result.receipt).toBeDefined();
  });

  it('should execute artifact generation', async () => {
    const observable = {
      type: 'organizational',
      patterns: ['process1'],
      visibility: 0.05,
    };

    const result = await engine.executeArtifact(observable);

    expect(result.operation).toBe('artifact');
    expect(result.output.artifact).toBeDefined();
    expect(result.output.artifact.id).toBeDefined();
  });

  it('should execute dark field detection', async () => {
    const observable = {
      type: 'strategic',
      patterns: ['strategy1'],
      visibility: 0.05,
    };

    const result = await engine.executeDetection(observable);

    expect(result.operation).toBe('detection');
    expect(result.output.detection).toBeDefined();
    expect(result.output.detection.darkFieldRatio).toBeCloseTo(0.95, 1);
  });

  it('should execute formation derivation', async () => {
    const artifact = {
      type: 'market',
      observable: {
        patterns: ['obs1', 'obs2'],
        visibility: 0.05,
      },
      darkField: {
        patterns: ['dark1', 'dark2'],
        visibility: 0.95,
        coverage: 0.5,
      },
      totalPatterns: 4,
      completeness: 0.95,
      generatedAt: Date.now(),
    };

    const result = await engine.executeFormation(artifact, { theorem: 'emergence' });

    expect(result.operation).toBe('formation');
    expect(result.output.formation).toBeDefined();
    expect(result.output.formation.theorem).toBe('emergence');
  });

  it('should execute full pipeline', async () => {
    const observable = {
      type: 'disruption',
      patterns: ['tech_trend'],
      visibility: 0.05,
    };

    const result = await engine.executePipeline(observable);

    expect(result.operation).toBe('full_pipeline');
    expect(result.output.artifact).toBeDefined();
    expect(result.output.detection).toBeDefined();
    expect(result.output.formation).toBeDefined();
    expect(result.receipt).toBeDefined();
  });

  it('should track engine metrics', async () => {
    const observable = {
      type: 'market',
      patterns: ['trend'],
      visibility: 0.05,
    };

    await engine.executeClosure(observable);

    const metrics = engine.getMetrics();
    expect(metrics.operationsExecuted).toBe(1);
    expect(metrics.receiptsGenerated).toBeGreaterThanOrEqual(0);
    expect(metrics.components).toBeDefined();
  });

  it('should reset all metrics', async () => {
    const observable = {
      type: 'market',
      patterns: ['trend'],
      visibility: 0.05,
    };

    await engine.executeClosure(observable);
    engine.resetMetrics();

    const metrics = engine.getMetrics();
    expect(metrics.operationsExecuted).toBe(0);
    expect(metrics.receiptsGenerated).toBe(0);
  });
});

describe('Integration Tests', () => {
  it('should execute complete Chatman workflow', async () => {
    // Create engine
    const engine = createChatmanEngine({
      observableRatio: 0.05,
      enableReceipts: true,
    });

    // Observable pattern
    const observable = {
      type: 'market',
      patterns: [
        'customer_dissatisfaction_with_pricing',
        'competitor_feature_gap',
        'market_share_decline',
      ],
      visibility: 0.05,
    };

    // Execute full pipeline
    const result = await engine.executePipeline(observable, { theorem: 'emergence' });

    // Verify complete workflow
    expect(result.output.artifact).toBeDefined();
    expect(result.output.artifact.darkField.patterns.length).toBeGreaterThan(10);

    expect(result.output.detection).toBeDefined();
    expect(result.output.detection.darkFieldRatio).toBeCloseTo(0.95, 1);

    expect(result.output.formation).toBeDefined();
    expect(result.output.formation.theorem).toBe('emergence');
    expect(result.output.formation.output.formation.length).toBeGreaterThan(0);

    expect(result.receipt).toBeDefined();
    expect(result.receipt.operation).toBe('chatman_full_pipeline');
  });

  it('should handle different pattern types', async () => {
    const engine = createChatmanEngine();

    const types = ['market', 'organizational', 'strategic', 'disruption'];

    for (const type of types) {
      const observable = {
        type,
        patterns: [`${type}_pattern`],
        visibility: 0.05,
      };

      const result = await engine.executeArtifact(observable);

      expect(result.output.artifact.type).toBe(type);
      expect(result.output.artifact.darkField.patterns.length).toBeGreaterThan(0);
    }
  });
});
