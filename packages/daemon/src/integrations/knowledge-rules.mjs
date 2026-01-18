/**
 * @file Daemon Knowledge Engine Rule Integration
 * @module @unrdf/daemon/integrations/knowledge-rules
 * @description Knowledge engine rule system for daemon intelligent triggers.
 * Provides SPARQL-based pattern matching, inference chains, confidence scoring,
 * and rule versioning for autonomous daemon operation orchestration.
 */

import { EventEmitter } from 'events';
import {
  calculateConfidence,
  generateExplanation,
  checkRuleConflict,
} from './knowledge-rules-utils.mjs';
import {
  ConfidenceLevels,
  InferenceRuleSchema,
  RuleExecutionResultSchema,
  DaemonRuleEngineConfigSchema,
} from './knowledge-rules-schemas.mjs';
import {
  evaluateCondition,
} from './knowledge-rules-evaluators.mjs';
import {
  buildInferenceChains,
  getInferenceChain,
} from './knowledge-rules-inference.mjs';

// Re-export schemas for external use
export {
  ConfidenceLevels,
  InferenceRuleSchema,
  RuleExecutionResultSchema,
  DaemonRuleEngineConfigSchema,
};

/**
 * Daemon Rule Engine extending knowledge-engine with intelligent rule execution
 * Provides SPARQL pattern matching, inference chains, confidence scoring,
 * and A/B testing for autonomous operation triggering.
 *
 * @extends EventEmitter
 * @example
 * ```javascript
 * const engine = new DaemonRuleEngine(daemon, config);
 * await engine.registerRule(rule);
 * const result = await engine.evaluateRules(operationMetadata);
 * if (result.matchedRules.length > 0) {
 *   await daemon.execute(result.matchedRules[0].action);
 * }
 * ```
 */
export class DaemonRuleEngine extends EventEmitter {
  /**
   * Creates a new Daemon Rule Engine instance
   * @param {Object} daemon - Daemon instance for operation execution
   * @param {Object} [config] - Engine configuration
   * @throws {Error} If configuration is invalid
   */
  constructor(daemon, config = {}) {
    super();

    const validated = DaemonRuleEngineConfigSchema.parse(config);

    this.daemon = daemon;
    this.config = validated;
    this.logger = config.logger || console;

    // Rule registry by ID and by name for fast lookups
    this.rulesById = new Map();
    this.rulesByName = new Map();

    // Rule versions for A/B testing
    this.ruleVersions = new Map();

    // Execution history and metrics
    this.executionHistory = [];
    this.metrics = {
      totalEvaluations: 0,
      matchedRules: 0,
      failedRules: 0,
      averageConfidence: 0,
    };

    // Inference cache for cycle detection
    this.inferenceCache = new Map();
  }

  /**
   * Register a rule in the engine
   * @param {Object} rule - Rule definition
   * @returns {void}
   * @throws {Error} If rule is invalid or duplicate
   * @example
   * await engine.registerRule({
   *   id: 'rule-1',
   *   name: 'High Load Trigger',
   *   condition: { type: 'sparql', query: '...' },
   *   action: { type: 'scale', payload: { replicas: 5 } }
   * });
   */
  registerRule(rule) {
    const validated = InferenceRuleSchema.parse(rule);

    if (this.rulesById.has(validated.id)) {
      throw new Error(`Rule with ID ${validated.id} already exists`);
    }

    if (this.rulesByName.has(validated.name)) {
      throw new Error(`Rule with name ${validated.name} already exists`);
    }

    this.rulesById.set(validated.id, validated);
    this.rulesByName.set(validated.name, validated);

    // Track versions for A/B testing
    if (validated.metadata.abTest?.enabled) {
      const variants = this.ruleVersions.get(validated.name) || new Map();
      variants.set(validated.metadata.abTest.variant, validated);
      this.ruleVersions.set(validated.name, variants);
    }

    this.logger.debug(`[RuleEngine] Rule registered: ${validated.name} v${validated.version}`);
    this.emit('rule:registered', { ruleId: validated.id, ruleName: validated.name });
  }

  /**
   * Unregister a rule from the engine
   * @param {string} ruleId - Rule identifier
   * @returns {boolean} Whether rule was found and removed
   */
  unregisterRule(ruleId) {
    const rule = this.rulesById.get(ruleId);
    if (!rule) {
      return false;
    }

    this.rulesById.delete(ruleId);
    this.rulesByName.delete(rule.name);

    if (this.ruleVersions.has(rule.name)) {
      this.ruleVersions.delete(rule.name);
    }

    this.logger.debug(`[RuleEngine] Rule unregistered: ${rule.name}`);
    return true;
  }

  /**
   * Get all registered rules
   * @returns {Array} Array of rule definitions
   */
  getRules() {
    return Array.from(this.rulesById.values());
  }

  /**
   * Get rule by ID
   * @param {string} ruleId - Rule identifier
   * @returns {Object|null} Rule definition or null if not found
   */
  getRule(ruleId) {
    return this.rulesById.get(ruleId) || null;
  }

  /**
   * Evaluate all rules against operation metadata
   * @param {Object} operationMetadata - Operation metadata for pattern matching
   * @param {Object} [context] - Additional context for evaluation
   * @returns {Promise<Object>} Evaluation results with matched rules and inference chains
   * @example
   * const results = await engine.evaluateRules(
   *   { operationType: 'write', entityType: 'Document', size: 1000 },
   *   { rdfStore: store }
   * );
   */
  async evaluateRules(operationMetadata, context = {}) {
    const startTime = Date.now();
    const evaluationId = `eval-${Date.now()}`;

    this.metrics.totalEvaluations += 1;

    const matchedRules = [];
    const failedRules = [];
    const explanations = [];

    try {
      // Evaluate each rule
      for (const rule of this.getRules()) {
        const ruleStartTime = Date.now();

        try {
          const result = await this._evaluateRule(
            rule,
            operationMetadata,
            context,
            evaluationId
          );

          if (result.matched) {
            matchedRules.push(result);
            this.metrics.matchedRules += 1;
          }

          explanations.push(result.explanation);

          // Record execution
          this.executionHistory.push({
            evaluationId,
            ...result,
            duration: Date.now() - ruleStartTime,
          });
        } catch (error) {
          this.logger.warn(`[RuleEngine] Rule evaluation failed: ${rule.name}`, error);
          failedRules.push({
            ruleId: rule.id,
            ruleName: rule.name,
            error: error.message,
          });
          this.metrics.failedRules += 1;

          // Still record in history
          this.executionHistory.push({
            evaluationId,
            ruleId: rule.id,
            ruleName: rule.name,
            matched: false,
            confidence: 0,
            explanation: {
              reason: `Rule evaluation failed: ${error.message}`,
              matchedPatterns: [],
              failedConditions: [error.message],
            },
            duration: Date.now() - ruleStartTime,
          });
        }
      }

      // Build inference chains between matched rules
      const inferenceChains = this.config.enableInference
        ? buildInferenceChains(matchedRules, this, this.config.maxRuleChainDepth)
        : [];

      // Update average confidence
      if (matchedRules.length > 0) {
        const avgConfidence = matchedRules.reduce((sum, r) => sum + r.confidence, 0) / matchedRules.length;
        this.metrics.averageConfidence = avgConfidence;
      }

      const duration = Date.now() - startTime;

      const result = {
        evaluationId,
        timestamp: new Date(),
        duration,
        operationMetadata,
        matchedRules: matchedRules.sort((a, b) => b.confidence - a.confidence),
        failedRules,
        inferenceChains,
        metrics: {
          totalRules: this.rulesById.size,
          matchedCount: matchedRules.length,
          failedCount: failedRules.length,
          averageConfidence: this.metrics.averageConfidence,
        },
      };

      this.emit('evaluation:complete', result);
      return result;
    } catch (error) {
      this.logger.error(`[RuleEngine] Evaluation failed`, error);
      throw error;
    }
  }

  /**
   * Evaluate a single rule against operation metadata
   * @param {Object} rule - Rule definition
   * @param {Object} operationMetadata - Operation metadata
   * @param {Object} context - Evaluation context
   * @param {string} evaluationId - Evaluation identifier
   * @returns {Promise<Object>} Rule execution result
   * @private
   */
  async _evaluateRule(rule, operationMetadata, context, evaluationId) {
    const startTime = Date.now();

    // Handle A/B testing - select variant
    const selectedRule = this._selectRuleVariant(rule);

    // Evaluate condition
    const conditionResult = await evaluateCondition(
      selectedRule.condition,
      operationMetadata,
      context
    );

    // Calculate confidence based on condition match quality
    const confidence = calculateConfidence(
      conditionResult,
      selectedRule,
      operationMetadata
    );

    // Check if meets minimum confidence threshold
    const matched = confidence >= selectedRule.minConfidence && conditionResult.matched;

    // Generate explanation
    const explanation = this.config.enableExplanations
      ? generateExplanation(rule, conditionResult, confidence)
      : { reason: 'Explanations disabled', matchedPatterns: [] };

    const result = RuleExecutionResultSchema.parse({
      ruleId: selectedRule.id,
      ruleName: selectedRule.name,
      matched,
      confidence,
      explanation: {
        ...explanation,
        inferenceChain: getInferenceChain(selectedRule.id, evaluationId, this.executionHistory),
      },
      action: matched ? selectedRule.action : undefined,
      duration: Date.now() - startTime,
    });

    return result;
  }

  /**
   * Select rule variant for A/B testing
   * @param {Object} rule - Rule definition
   * @returns {Object} Selected rule variant or original rule
   * @private
   */
  _selectRuleVariant(rule) {
    if (!this.config.enableABTesting || !rule.metadata.abTest?.enabled) {
      return rule;
    }

    // Deterministic variant selection based on hash
    const random = Math.random() * 100;
    const selectedVariant = random < rule.metadata.abTest.splitPercentage
      ? 'treatment'
      : 'control';

    const variants = this.ruleVersions.get(rule.name);
    if (variants && variants.has(selectedVariant)) {
      return variants.get(selectedVariant);
    }

    return rule;
  }

  /**
   * Detect rule conflicts (rules with contradicting actions)
   * @returns {Array} Array of conflict objects
   */
  detectConflicts() {
    const conflicts = [];
    const rules = this.getRules();

    for (let i = 0; i < rules.length; i++) {
      for (let j = i + 1; j < rules.length; j++) {
        const conflict = checkRuleConflict(rules[i], rules[j]);
        if (conflict) conflicts.push(conflict);
      }
    }

    return conflicts;
  }

  /**
   * Get engine metrics and statistics
   * @returns {Object} Metrics object
   */
  getMetrics() {
    return {
      engineId: this.config.engineId,
      totalRules: this.rulesById.size,
      totalEvaluations: this.metrics.totalEvaluations,
      matchedRules: this.metrics.matchedRules,
      failedRules: this.metrics.failedRules,
      averageConfidence: this.metrics.averageConfidence,
      successRate: this.metrics.totalEvaluations > 0
        ? ((this.metrics.totalEvaluations - this.metrics.failedRules) / this.metrics.totalEvaluations) * 100
        : 0,
      executionHistorySize: this.executionHistory.length,
    };
  }

  /**
   * Clear execution history
   * @param {number} [keepLast=100] - Number of recent entries to keep
   */
  clearExecutionHistory(keepLast = 100) {
    if (this.executionHistory.length > keepLast) {
      this.executionHistory = this.executionHistory.slice(-keepLast);
    }
  }

  /**
   * Reset engine state (for testing)
   */
  reset() {
    this.rulesById.clear();
    this.rulesByName.clear();
    this.ruleVersions.clear();
    this.executionHistory = [];
    this.metrics = {
      totalEvaluations: 0,
      matchedRules: 0,
      failedRules: 0,
      averageConfidence: 0,
    };
    this.inferenceCache.clear();
  }
}
