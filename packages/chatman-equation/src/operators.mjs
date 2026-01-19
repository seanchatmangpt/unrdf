/**
 * @file Closure operator implementations for Chatman Equation
 * @module @unrdf/chatman-equation/operators
 * @description Implements μ operators for different market dynamics domains
 */

/**
 * Evaluates a condition expression against observation data
 * @param {string} condition - Condition expression to evaluate
 * @param {Object} context - Observation context data
 * @returns {boolean} Whether condition is met
 */
export function evaluateCondition(condition, context) {
  // Simple expression evaluator for configuration-driven conditions
  // Supports basic comparisons: >, <, >=, <=, ==, !=, AND, OR

  try {
    // Replace variable names with context values
    let expression = condition;

    // Handle common patterns
    for (const [key, value] of Object.entries(context)) {
      const regex = new RegExp(`\\b${key}\\b`, 'g');
      if (typeof value === 'number') {
        expression = expression.replace(regex, String(value));
      } else if (typeof value === 'boolean') {
        expression = expression.replace(regex, String(value));
      } else if (typeof value === 'string') {
        expression = expression.replace(regex, `"${value}"`);
      }
    }

    // Convert logical operators
    expression = expression
      .replace(/\bAND\b/g, '&&')
      .replace(/\bOR\b/g, '||')
      .replace(/\bNOT\b/g, '!');

    // Evaluate in isolated scope (safe for config-driven expressions)
    // In production, use a proper expression parser
    return new Function(`return ${expression}`)();
  } catch (error) {
    console.warn(`Failed to evaluate condition: ${condition}`, error);
    return false;
  }
}

/**
 * Applies a single rule to observation data
 * @param {Object} rule - Rule definition
 * @param {Object} observations - Observation data
 * @param {Object} context - Execution context
 * @returns {Object|null} Rule result or null if condition not met
 */
export function applyRule(rule, observations, context = {}) {
  const evalContext = {
    ...observations,
    ...context,
  };

  const conditionMet = evaluateCondition(rule.condition, evalContext);

  if (!conditionMet) {
    return null;
  }

  return {
    rule: rule.name,
    action: rule.action,
    parameters: rule.parameters || {},
    triggered: true,
    context: evalContext,
  };
}

/**
 * Applies closure operator to observations
 * μ(O) = A
 *
 * @param {Object} closureOperator - Closure operator definition
 * @param {Object} observations - Observation data (O)
 * @returns {Object} Artifacts (A = μ(O))
 */
export function applyClosureOperator(closureOperator, observations) {
  const results = {
    operator: closureOperator.name,
    type: closureOperator.type,
    idempotent: closureOperator.idempotent,
    deterministic: closureOperator.deterministic,
    rulesApplied: [],
    artifacts: {},
  };

  // Apply each rule in sequence
  for (const rule of closureOperator.rules) {
    const ruleResult = applyRule(rule, observations, results.artifacts);

    if (ruleResult) {
      results.rulesApplied.push(ruleResult);

      // Accumulate artifacts from rule execution
      results.artifacts[rule.action] = ruleResult;
    }
  }

  return results;
}

/**
 * Tests idempotence: μ(μ(O)) = μ(O)
 * @param {Object} closureOperator - Closure operator definition
 * @param {Object} observations - Observation data
 * @returns {Object} Idempotence test results
 */
export function testIdempotence(closureOperator, observations) {
  const firstRun = applyClosureOperator(closureOperator, observations);
  const secondRun = applyClosureOperator(closureOperator, firstRun.artifacts);

  // For true idempotence, second application should yield same result
  const firstHash = hashObject(firstRun);
  const secondHash = hashObject(secondRun);

  return {
    firstRun,
    secondRun,
    firstHash,
    secondHash,
    idempotent: firstHash === secondHash,
  };
}

/**
 * Tests determinism: same input → same output
 * @param {Object} closureOperator - Closure operator definition
 * @param {Object} observations - Observation data
 * @param {number} iterations - Number of test iterations
 * @returns {Object} Determinism test results
 */
export function testDeterminism(closureOperator, observations, iterations = 100) {
  const results = [];
  const hashes = new Set();

  for (let i = 0; i < iterations; i++) {
    const result = applyClosureOperator(closureOperator, observations);
    const hash = hashObject(result);

    results.push(result);
    hashes.add(hash);
  }

  return {
    iterations,
    uniqueOutputs: hashes.size,
    deterministic: hashes.size === 1,
    determinismScore: 1.0 - (hashes.size - 1) / iterations,
    hashes: Array.from(hashes),
  };
}

/**
 * Simple deterministic hash for objects
 * @param {Object} obj - Object to hash
 * @returns {string} Hash string
 */
function hashObject(obj) {
  // Deterministic JSON stringification
  const str = JSON.stringify(obj, Object.keys(obj).sort());

  // Simple hash function (FNV-1a)
  let hash = 2166136261;
  for (let i = 0; i < str.length; i++) {
    hash ^= str.charCodeAt(i);
    hash += (hash << 1) + (hash << 4) + (hash << 7) + (hash << 8) + (hash << 24);
  }

  return (hash >>> 0).toString(16);
}

/**
 * Creates market-specific operator instances
 */
export const MarketOperators = {
  /**
   * Order book divergence operator
   * @param {Object} observations - Order book snapshots
   * @returns {Object} Arbitrage opportunities
   */
  orderBookDivergence(observations) {
    const snapshots = observations.snapshots || [];

    const opportunities = [];

    // Find cross-exchange arbitrage
    for (let i = 0; i < snapshots.length; i++) {
      for (let j = i + 1; j < snapshots.length; j++) {
        const snap1 = snapshots[i];
        const snap2 = snapshots[j];

        // Check if we can buy on one and sell on another
        if (snap1.ask_price < snap2.bid_price) {
          opportunities.push({
            buyExchange: snap1.exchange,
            buyPrice: snap1.ask_price,
            sellExchange: snap2.exchange,
            sellPrice: snap2.bid_price,
            profitBps: ((snap2.bid_price - snap1.ask_price) / snap1.ask_price) * 10000,
          });
        }

        if (snap2.ask_price < snap1.bid_price) {
          opportunities.push({
            buyExchange: snap2.exchange,
            buyPrice: snap2.ask_price,
            sellExchange: snap1.exchange,
            sellPrice: snap1.bid_price,
            profitBps: ((snap1.bid_price - snap2.ask_price) / snap2.ask_price) * 10000,
          });
        }
      }
    }

    return { opportunities };
  },

  /**
   * Network effects operator (Metcalfe's Law)
   * @param {Object} observations - User activity graph
   * @returns {Object} Network value
   */
  networkEffects(observations) {
    const { total_users = 0, graph_metrics = {} } = observations.user_metrics || observations;
    const valuePerConnection = 0.15;

    // Metcalfe's Law: V = k * n^2
    const networkValue = valuePerConnection * Math.pow(total_users, 2);
    const valuePerUser = networkValue / total_users;

    return {
      totalNetworkValue: networkValue,
      valuePerUser,
      criticalMassAchieved: total_users > 5000,
      growthPhase: total_users > 10000 ? 'exponential' : 'linear',
    };
  },

  /**
   * Timing windows operator
   * @param {Object} observations - Price candles and indicators
   * @returns {Object} Trading signals
   */
  timingWindows(observations) {
    const { indicators = {}, candles = [] } = observations;
    const signals = [];

    // Mean reversion signal
    if (indicators.rsi_14 < 30) {
      signals.push({
        type: 'mean_reversion_long',
        confidence: (30 - indicators.rsi_14) / 30,
        reasoning: 'RSI oversold',
      });
    }

    if (indicators.rsi_14 > 70) {
      signals.push({
        type: 'mean_reversion_short',
        confidence: (indicators.rsi_14 - 70) / 30,
        reasoning: 'RSI overbought',
      });
    }

    // MACD signal
    if (indicators.macd_histogram > 0.05) {
      signals.push({
        type: 'trend_continuation',
        confidence: Math.min(indicators.macd_histogram / 0.2, 1.0),
        reasoning: 'MACD bullish',
      });
    }

    return { signals, windowClosed: true };
  },

  /**
   * Moat formation operator
   * @param {Object} observations - Market position data
   * @returns {Object} Competitive advantage metrics
   */
  moatFormation(observations) {
    const { moat_indicators = {}, market_position = {} } = observations;

    // Calculate individual moat components
    const networkMoat = moat_indicators.network_effect_strength * 10;
    const scaleMoat = (market_position.gross_margin || 0) * 10;
    const switchingMoat = (moat_indicators.switching_cost_score || 0);
    const brandMoat = (moat_indicators.brand_recognition_score || 0);

    // Weighted geometric mean for combined score
    const weights = [0.35, 0.25, 0.30, 0.10];
    const scores = [networkMoat, scaleMoat, switchingMoat, brandMoat];

    const geometricMean = Math.pow(
      scores.reduce((acc, score, i) => acc * Math.pow(score, weights[i]), 1),
      1
    );

    return {
      overallMoatStrength: geometricMean,
      moatRating: geometricMean > 8 ? 'wide' : geometricMean > 5 ? 'narrow' : 'none',
      components: {
        networkEffects: networkMoat,
        scaleEconomies: scaleMoat,
        switchingCosts: switchingMoat,
        intangibleAssets: brandMoat,
      },
    };
  },
};
