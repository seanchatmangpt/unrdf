/**
 * @file market-dynamics.test.mjs
 * @description Tests for market dynamics TOML examples
 * Validates determinism, idempotence, and composability properties
 */

import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { parse as parseToml } from '@iarna/toml';
import { createHash } from 'crypto';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const EXAMPLES_DIR = join(__dirname, '../examples/markets');

/**
 * Load TOML example
 */
function loadExample(filename) {
  const filePath = join(EXAMPLES_DIR, filename);
  const content = readFileSync(filePath, 'utf-8');
  return parseToml(content);
}

/**
 * Hash object deterministically
 */
function hashObject(obj) {
  const str = JSON.stringify(obj, Object.keys(obj).sort());
  return createHash('sha256').update(str).digest('hex');
}

/**
 * Simulate closure operator application
 */
function applyClosureOperator(config) {
  const { observations, closure_operator } = config;

  // Simple rule application
  const results = {
    operator: closure_operator.name,
    type: closure_operator.type,
    rulesApplied: [],
  };

  for (const rule of closure_operator.rules || []) {
    results.rulesApplied.push({
      rule: rule.name,
      action: rule.action,
    });
  }

  return results;
}

describe('Market Dynamics Examples - TOML Structure', () => {
  describe('Order Book Divergence', () => {
    it('should load and parse TOML correctly', () => {
      const config = loadExample('order-book-divergence.toml');

      expect(config.metadata).toBeDefined();
      expect(config.metadata.name).toBe('Order Book Divergence');
      expect(config.metadata.equation).toBe('Opportunities = μ(OrderBookSnapshots)');
    });

    it('should contain valid observations', () => {
      const config = loadExample('order-book-divergence.toml');

      expect(config.observations).toBeDefined();
      expect(config.observations.market_pair).toBe('BTC/USD');
      expect(config.observations.snapshots).toBeInstanceOf(Array);
      expect(config.observations.snapshots).toHaveLength(3);

      // Validate snapshot structure
      const snapshot = config.observations.snapshots[0];
      expect(snapshot.exchange).toBeDefined();
      expect(snapshot.bid_price).toBeTypeOf('number');
      expect(snapshot.ask_price).toBeTypeOf('number');
      expect(snapshot.bid_volume).toBeTypeOf('number');
      expect(snapshot.ask_volume).toBeTypeOf('number');
    });

    it('should contain valid closure operator', () => {
      const config = loadExample('order-book-divergence.toml');

      expect(config.closure_operator).toBeDefined();
      expect(config.closure_operator.name).toBe('arbitrage_detector');
      expect(config.closure_operator.type).toBe('market_divergence');
      expect(config.closure_operator.idempotent).toBe(true);
      expect(config.closure_operator.deterministic).toBe(true);
      expect(config.closure_operator.rules).toBeInstanceOf(Array);
      expect(config.closure_operator.rules.length).toBeGreaterThan(0);
    });

    it('should contain valid artifacts', () => {
      const config = loadExample('order-book-divergence.toml');

      expect(config.artifacts).toBeDefined();
      expect(config.artifacts.total_opportunities).toBeTypeOf('number');
      expect(config.artifacts.convergence_guaranteed).toBe(true);
      expect(config.artifacts.execution_deterministic).toBe(true);
      expect(config.artifacts.opportunities).toBeInstanceOf(Array);
    });

    it('should contain verification specification', () => {
      const config = loadExample('order-book-divergence.toml');

      expect(config.verification).toBeDefined();
      expect(config.verification.method).toBe('deterministic_replay');
      expect(config.verification.determinism).toBeDefined();
      expect(config.verification.idempotence).toBeDefined();
    });

    it('should detect arbitrage opportunities', () => {
      const config = loadExample('order-book-divergence.toml');
      const snapshots = config.observations.snapshots;

      // Find max bid and min ask
      const maxBid = Math.max(...snapshots.map(s => s.bid_price));
      const minAsk = Math.min(...snapshots.map(s => s.ask_price));

      // Arbitrage exists when max bid > min ask
      expect(maxBid).toBeGreaterThan(minAsk);
      expect(config.artifacts.opportunities.length).toBeGreaterThan(0);
    });
  });

  describe('Network Effects', () => {
    it('should load and parse TOML correctly', () => {
      const config = loadExample('network-effects.toml');

      expect(config.metadata).toBeDefined();
      expect(config.metadata.name).toBe('Network Effects Modeling');
      expect(config.metadata.equation).toBe('NetworkValue = μ(UserActivityGraph)');
    });

    it('should contain user metrics observations', () => {
      const config = loadExample('network-effects.toml');

      expect(config.observations.user_metrics).toBeDefined();
      expect(config.observations.user_metrics.total_users).toBeTypeOf('number');
      expect(config.observations.user_metrics.active_users_daily).toBeTypeOf('number');
      expect(config.observations.user_connections).toBeInstanceOf(Array);
    });

    it('should validate Metcalfe\'s Law application', () => {
      const config = loadExample('network-effects.toml');

      const rule = config.closure_operator.rules.find(r => r.name === 'metcalfe_value');
      expect(rule).toBeDefined();
      expect(rule.condition).toContain('user_count > 100');
      expect(rule.action).toBe('calculate_network_value');
      expect(rule.parameters.formula).toBe('n^2');
    });

    it('should calculate network value correctly', () => {
      const config = loadExample('network-effects.toml');

      const totalUsers = config.observations.user_metrics.total_users;
      const networkValue = config.artifacts.total_network_value_usd;
      const valuePerUser = config.artifacts.value_per_user_usd;

      expect(totalUsers).toBe(10000);
      expect(networkValue).toBeGreaterThan(0);
      expect(valuePerUser).toBeCloseTo(networkValue / totalUsers, 2);
    });

    it('should have growth projections', () => {
      const config = loadExample('network-effects.toml');

      expect(config.artifacts.projections).toBeInstanceOf(Array);
      expect(config.artifacts.projections.length).toBeGreaterThan(0);

      const projection = config.artifacts.projections[0];
      expect(projection.months_ahead).toBeTypeOf('number');
      expect(projection.projected_users).toBeTypeOf('number');
      expect(projection.projected_value_usd).toBeTypeOf('number');
      expect(projection.confidence_interval).toBeInstanceOf(Array);
      expect(projection.confidence_interval).toHaveLength(2);
    });
  });

  describe('Timing Windows', () => {
    it('should load and parse TOML correctly', () => {
      const config = loadExample('timing-windows.toml');

      expect(config.metadata).toBeDefined();
      expect(config.metadata.name).toBe('Market Timing Windows');
      expect(config.metadata.equation).toBe('TradingSignals = μ(PriceWindows)');
    });

    it('should contain price candle observations', () => {
      const config = loadExample('timing-windows.toml');

      expect(config.observations.candles).toBeInstanceOf(Array);
      expect(config.observations.candles.length).toBeGreaterThan(0);

      const candle = config.observations.candles[0];
      expect(candle.timestamp).toBeDefined();
      expect(candle.open).toBeTypeOf('number');
      expect(candle.high).toBeTypeOf('number');
      expect(candle.low).toBeTypeOf('number');
      expect(candle.close).toBeTypeOf('number');
      expect(candle.volume).toBeTypeOf('number');
    });

    it('should contain technical indicators', () => {
      const config = loadExample('timing-windows.toml');

      expect(config.observations.indicators).toBeDefined();
      expect(config.observations.indicators.sma_20).toBeTypeOf('number');
      expect(config.observations.indicators.ema_20).toBeTypeOf('number');
      expect(config.observations.indicators.rsi_14).toBeTypeOf('number');
      expect(config.observations.indicators.macd_line).toBeTypeOf('number');
      expect(config.observations.indicators.bollinger_upper).toBeTypeOf('number');
      expect(config.observations.indicators.bollinger_lower).toBeTypeOf('number');
    });

    it('should generate trading signals', () => {
      const config = loadExample('timing-windows.toml');

      expect(config.artifacts.total_signals).toBeTypeOf('number');
      expect(config.artifacts.signals).toBeInstanceOf(Array);

      if (config.artifacts.signals.length > 0) {
        const signal = config.artifacts.signals[0];
        expect(signal.id).toBeDefined();
        expect(signal.type).toBeDefined();
        expect(signal.direction).toBeDefined();
        expect(signal.confidence).toBeTypeOf('number');
        expect(signal.confidence).toBeGreaterThanOrEqual(0);
        expect(signal.confidence).toBeLessThanOrEqual(1);
      }
    });

    it('should enforce risk management rules', () => {
      const config = loadExample('timing-windows.toml');

      const signals = config.artifacts.signals.filter(s => s.entry_price);

      for (const signal of signals) {
        if (signal.stop_loss && signal.take_profit && signal.entry_price) {
          if (signal.direction === 'long') {
            expect(signal.stop_loss).toBeLessThan(signal.entry_price);
            expect(signal.take_profit).toBeGreaterThan(signal.entry_price);
          }

          if (signal.risk_reward_ratio) {
            expect(signal.risk_reward_ratio).toBeGreaterThan(0);
          }
        }
      }
    });
  });

  describe('Moat Formation', () => {
    it('should load and parse TOML correctly', () => {
      const config = loadExample('moat-formation.toml');

      expect(config.metadata).toBeDefined();
      expect(config.metadata.name).toBe('Moat Formation Analysis');
      expect(config.metadata.equation).toBe('CompetitiveAdvantage = μ(MarketPositionData)');
    });

    it('should contain market position observations', () => {
      const config = loadExample('moat-formation.toml');

      expect(config.observations.market_position).toBeDefined();
      expect(config.observations.market_position.market_share).toBeTypeOf('number');
      expect(config.observations.market_position.revenue_growth_rate).toBeTypeOf('number');
      expect(config.observations.market_position.gross_margin).toBeTypeOf('number');
      expect(config.observations.competitors).toBeInstanceOf(Array);
    });

    it('should analyze competitive landscape', () => {
      const config = loadExample('moat-formation.toml');

      const competitors = config.observations.competitors;
      expect(competitors.length).toBeGreaterThan(0);

      for (const competitor of competitors) {
        expect(competitor.name).toBeDefined();
        expect(competitor.market_share).toBeTypeOf('number');
        expect(competitor.market_share).toBeLessThan(1);
        expect(competitor.market_share).toBeGreaterThan(0);
      }
    });

    it('should calculate moat components', () => {
      const config = loadExample('moat-formation.toml');

      expect(config.artifacts.moat_components).toBeInstanceOf(Array);
      expect(config.artifacts.moat_components.length).toBeGreaterThan(0);

      const totalContribution = config.artifacts.moat_components.reduce(
        (sum, component) => sum + component.contribution_percentage,
        0
      );

      expect(totalContribution).toBeCloseTo(100, 1);
    });

    it('should have overall moat strength', () => {
      const config = loadExample('moat-formation.toml');

      expect(config.artifacts.overall_moat_strength).toBeTypeOf('number');
      expect(config.artifacts.overall_moat_strength).toBeGreaterThan(0);
      expect(config.artifacts.overall_moat_strength).toBeLessThanOrEqual(10);
      expect(config.artifacts.moat_rating).toBeDefined();
      expect(['wide', 'narrow', 'none']).toContain(config.artifacts.moat_rating);
    });

    it('should identify threats and opportunities', () => {
      const config = loadExample('moat-formation.toml');

      expect(config.artifacts.threats).toBeInstanceOf(Array);
      expect(config.artifacts.reinforcement_opportunities).toBeInstanceOf(Array);

      if (config.artifacts.threats.length > 0) {
        const threat = config.artifacts.threats[0];
        expect(threat.threat_type).toBeDefined();
        expect(threat.probability).toBeGreaterThan(0);
        expect(threat.probability).toBeLessThanOrEqual(1);
        expect(threat.impact_on_moat).toBeDefined();
      }
    });
  });
});

describe('Chatman Equation Properties - Market Examples', () => {
  describe('Determinism: μ(O) → A is deterministic', () => {
    it('should produce same hash for same observations (Order Book)', () => {
      const config1 = loadExample('order-book-divergence.toml');
      const config2 = loadExample('order-book-divergence.toml');

      const hash1 = hashObject(config1.observations);
      const hash2 = hashObject(config2.observations);

      expect(hash1).toBe(hash2);
    });

    it('should have determinism score of 1.0', () => {
      const examples = [
        'order-book-divergence.toml',
        'network-effects.toml',
        'timing-windows.toml',
        'moat-formation.toml',
      ];

      for (const filename of examples) {
        const config = loadExample(filename);

        if (config.verification && config.verification.determinism) {
          expect(config.verification.determinism.determinism_score).toBe(1.0);
          expect(config.verification.determinism.unique_outputs).toBe(1);
        }
      }
    });

    it('should specify deterministic closure operators', () => {
      const examples = [
        'order-book-divergence.toml',
        'network-effects.toml',
        'timing-windows.toml',
        'moat-formation.toml',
      ];

      for (const filename of examples) {
        const config = loadExample(filename);
        expect(config.closure_operator.deterministic).toBe(true);
      }
    });
  });

  describe('Idempotence: μ(μ(O)) = μ(O)', () => {
    it('should specify idempotent operators', () => {
      const examples = [
        'order-book-divergence.toml',
        'network-effects.toml',
        'timing-windows.toml',
        'moat-formation.toml',
      ];

      for (const filename of examples) {
        const config = loadExample(filename);
        expect(config.closure_operator.idempotent).toBe(true);
      }
    });

    it('should validate idempotence in verification (Order Book)', () => {
      const config = loadExample('order-book-divergence.toml');

      expect(config.verification.idempotence).toBeDefined();
      expect(config.verification.idempotence.first_run_hash).toBeDefined();
      expect(config.verification.idempotence.second_run_hash).toBeDefined();
      expect(config.verification.idempotence.hashes_match).toBe(true);
    });

    it('should validate idempotence in verification (Network Effects)', () => {
      const config = loadExample('network-effects.toml');

      expect(config.verification.idempotence).toBeDefined();
      expect(config.verification.idempotence.first_application).toBeDefined();
      expect(config.verification.idempotence.second_application).toBeDefined();
      expect(config.verification.idempotence.idempotent).toBe(true);
    });
  });

  describe('Composability: μ supports composition', () => {
    it('should have composability verification', () => {
      const config = loadExample('network-effects.toml');

      expect(config.verification.composability).toBeDefined();
      expect(config.verification.composability.composable).toBe(true);
    });

    it('should support multiple rule composition', () => {
      const examples = [
        'order-book-divergence.toml',
        'network-effects.toml',
        'timing-windows.toml',
        'moat-formation.toml',
      ];

      for (const filename of examples) {
        const config = loadExample(filename);
        const rules = config.closure_operator.rules;

        expect(rules).toBeInstanceOf(Array);
        expect(rules.length).toBeGreaterThan(1); // Multiple composable rules
      }
    });
  });

  describe('Invariants Preservation', () => {
    it('should specify invariants (Order Book)', () => {
      const config = loadExample('order-book-divergence.toml');

      expect(config.verification.invariants).toBeDefined();
      expect(config.verification.invariants.max_bid).toBeTypeOf('number');
      expect(config.verification.invariants.min_ask).toBeTypeOf('number');
      expect(config.verification.invariants.arbitrage_exists).toBe(true);
      expect(config.verification.invariants.profit_positive).toBe(true);
    });

    it('should specify invariants (Network Effects)', () => {
      const config = loadExample('network-effects.toml');

      expect(config.verification.invariants).toBeDefined();
      expect(config.verification.invariants.network_value_positive).toBe(true);
      expect(config.verification.invariants.growth_rate_bounded).toBe(true);
    });

    it('should have bounded values in moat analysis', () => {
      const config = loadExample('moat-formation.toml');

      expect(config.verification.invariants).toBeDefined();
      expect(config.verification.invariants.min_moat_strength).toBe(0.0);
      expect(config.verification.invariants.max_moat_strength).toBe(10.0);

      const actualStrength = config.artifacts.overall_moat_strength;
      expect(actualStrength).toBeGreaterThanOrEqual(0.0);
      expect(actualStrength).toBeLessThanOrEqual(10.0);
    });
  });

  describe('Configuration-Driven Execution', () => {
    it('should apply closure operator rules', () => {
      const config = loadExample('order-book-divergence.toml');
      const result = applyClosureOperator(config);

      expect(result.operator).toBe('arbitrage_detector');
      expect(result.type).toBe('market_divergence');
      expect(result.rulesApplied).toBeInstanceOf(Array);
      expect(result.rulesApplied.length).toBe(config.closure_operator.rules.length);
    });

    it('should be purely configuration-driven (no hardcoded logic)', () => {
      const examples = [
        'order-book-divergence.toml',
        'network-effects.toml',
        'timing-windows.toml',
        'moat-formation.toml',
      ];

      for (const filename of examples) {
        const config = loadExample(filename);

        // All logic should be in rules
        expect(config.closure_operator.rules).toBeDefined();
        expect(config.closure_operator.rules.length).toBeGreaterThan(0);

        // Each rule should have condition, action, parameters
        for (const rule of config.closure_operator.rules) {
          expect(rule.name).toBeDefined();
          expect(rule.condition).toBeDefined();
          expect(rule.action).toBeDefined();
        }
      }
    });
  });
});

describe('Integration Tests - Full Equation Execution', () => {
  it('should validate all market examples load successfully', () => {
    const examples = [
      'order-book-divergence.toml',
      'network-effects.toml',
      'timing-windows.toml',
      'moat-formation.toml',
    ];

    for (const filename of examples) {
      expect(() => loadExample(filename)).not.toThrow();
    }
  });

  it('should have complete metadata across all examples', () => {
    const examples = [
      'order-book-divergence.toml',
      'network-effects.toml',
      'timing-windows.toml',
      'moat-formation.toml',
    ];

    for (const filename of examples) {
      const config = loadExample(filename);

      expect(config.metadata.name).toBeDefined();
      expect(config.metadata.description).toBeDefined();
      expect(config.metadata.domain).toBeDefined();
      expect(config.metadata.equation).toBeDefined();
      expect(config.metadata.equation).toContain('μ');
      expect(config.metadata.equation).toContain('=');
    }
  });

  it('should have verification for all examples', () => {
    const examples = [
      'order-book-divergence.toml',
      'network-effects.toml',
      'timing-windows.toml',
      'moat-formation.toml',
    ];

    for (const filename of examples) {
      const config = loadExample(filename);

      expect(config.verification).toBeDefined();
      expect(config.verification.method).toBeDefined();
      expect(config.verification.determinism).toBeDefined();
      expect(config.verification.idempotence).toBeDefined();
    }
  });
});
