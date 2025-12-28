# KGC-SWARM ML Module

**ML-enhanced decision making and pattern recognition for KGC-SWARM**

Native JavaScript implementation with no external ML libraries (no TensorFlow.js) - using statistical methods, typed arrays, and interpretable algorithms.

## Features

- **Pattern Recognition**: Detect recurring patterns in observables O_τ, learn compression strategies, predict convergence time
- **Q-Learning**: Reinforcement learning for agent action selection with epsilon-greedy exploration
- **Anomaly Detection**: Statistical outlier detection (Z-score, IQR), drift pattern anomalies, guard violation prediction
- **Native JavaScript**: No heavy ML dependencies - pure JavaScript with typed arrays for performance
- **Interpretability**: Focus on interpretable algorithms over black-box accuracy

## Quick Start

```javascript
import {
  createPatternRecognizer,
  createQLearningAgent,
  createAnomalyDetector,
  calculateReward,
} from './src/ml/index.mjs';

// Pattern recognition
const recognizer = createPatternRecognizer({ windowSize: 5 });
recognizer.observe({
  artifacts: ['a1', 'a2'],
  driftValue: 10,
  normalized: 0.5,
  epoch: 1,
  timestamp: Date.now(),
});
const prediction = recognizer.predictConvergence();

// Q-learning agent
const agent = createQLearningAgent({
  actions: [
    { id: 'aggressive', name: 'Aggressive Compression' },
    { id: 'balanced', name: 'Balanced' },
  ],
  epsilon: 0.1,
  learningRate: 0.1,
});
const action = agent.selectAction({
  driftLevel: 'high',
  epochCount: 'early',
  budgetUsed: 'low'
});

// Anomaly detection
const detector = createAnomalyDetector({ zScoreThreshold: 2.5 });
detector.observe({ epoch: 1, drift: 10, normalized: 0.5, timestamp: Date.now() });
const anomalies = detector.detect();
```

## Demo

Run the interactive demo:

```bash
node src/ml/demo.mjs
```

## Architecture

### 1. Pattern Recognition (`pattern-recognition.mjs`)

Learns patterns from observable sequences O_τ to predict convergence and suggest compression strategies.

**Key Methods**:
- `observe(observation)`: Record new epoch state
- `getPatterns()`: Get recognized patterns with statistics
- `predictConvergence()`: Predict convergence time based on learned patterns
- `suggestCompressionStrategy()`: Recommend compression strategy based on artifact frequency

**Algorithms**:
- Frequency analysis for pattern extraction
- Linear regression for trend analysis
- Shannon entropy for compression strategy selection
- Pattern hashing (FNV-1a) for deterministic identification

**Performance**:
- O(n) observation recording
- O(w) pattern extraction (w = window size)
- Typed arrays (Float64Array) for time series

### 2. Q-Learning (`q-learning.mjs`)

Reinforcement learning for adaptive agent control.

**State Space**:
- `driftLevel`: high, medium, low, converged
- `epochCount`: early, middle, late
- `budgetUsed`: low, medium, high

**Actions**:
- User-defined (e.g., aggressive, balanced, conservative compression)

**Algorithms**:
- Epsilon-greedy exploration
- Q-learning update rule: `Q(s,a) ← Q(s,a) + α[r + γ max_a' Q(s',a') - Q(s,a)]`
- UCB (Upper Confidence Bound) for exploration bonus
- Experience replay for stable learning

**Reward Function**:
```javascript
reward = (10 / convergenceTime) + (compressionRatio * 5) - (budgetUsed * 2)
```

**Performance**:
- O(1) action selection (Q-table lookup)
- O(1) Q-value updates
- Typed arrays (Float64Array, Uint32Array) for Q-table

### 3. Anomaly Detection (`anomaly-detection.mjs`)

Statistical outlier detection for drift patterns.

**Detection Methods**:
1. **Z-score**: Detects values > 2.5 standard deviations from mean
2. **IQR (Interquartile Range)**: Robust to outliers, uses 1.5 × IQR
3. **Pattern-based**: Drift spikes (sudden increases), plateaus (no convergence)

**Guard Violation Prediction**:
- Linear regression on recent drift trend
- Predicts when drift will cross threshold
- Risk score and confidence metrics

**Performance**:
- O(n log n) for quartile calculation
- O(1) for Z-score (with cached statistics)
- Typed arrays (Float64Array) for drift time series

## API Reference

### PatternRecognizer

```javascript
const recognizer = new PatternRecognizer({
  windowSize: 5,           // Pattern window size
  minSupport: 3,           // Minimum pattern occurrences
  similarityThreshold: 0.8 // Pattern similarity threshold
});

// Observe epoch
recognizer.observe({
  artifacts: ['a1', 'a2'],  // Artifact identifiers
  driftValue: 10,           // Absolute drift
  normalized: 0.5,          // Normalized drift (0-1)
  epoch: 1,                 // Epoch number τ
  timestamp: Date.now()     // Optional timestamp
});

// Get patterns
const patterns = recognizer.getPatterns({ onlyFrequent: true });
// Returns: [{ patternId, frequency, avgDrift, avgConvergenceTime, confidence, examples }]

// Predict convergence
const prediction = recognizer.predictConvergence();
// Returns: { estimatedEpochs, confidence, basedOnPattern }

// Suggest compression
const strategy = recognizer.suggestCompressionStrategy();
// Returns: { strategy, reason, expectedRatio }

// Get summary
const summary = recognizer.getSummary();
// Returns: { totalObservations, uniquePatterns, uniqueArtifacts, averageDrift }
```

### QLearningAgent

```javascript
const agent = new QLearningAgent({
  actions: [
    { id: 'action1', name: 'Action 1' },
    { id: 'action2', name: 'Action 2' }
  ],
  epsilon: 0.1,           // Exploration rate (0-1)
  learningRate: 0.1,      // Learning rate α (0-1)
  discountFactor: 0.9,    // Discount factor γ (0-1)
  epsilonDecay: 0.995,    // Epsilon decay per episode
  minEpsilon: 0.01        // Minimum epsilon
});

// Select action (epsilon-greedy)
const action = agent.selectAction(
  { driftLevel: 'high', epochCount: 'early', budgetUsed: 'low' },
  { explore: true }
);

// Learn from experience
agent.learn(
  state,        // Current state
  'action1',    // Action taken
  10.5,         // Reward received
  nextState,    // Next state
  false         // Terminal flag
);

// End episode (decays epsilon)
agent.endEpisode();

// Get learned policy
const policy = agent.getPolicy();
// Returns: Map<stateKey, actionId>

// Export/import Q-table
const exported = agent.exportQTable();
agent.importQTable(exported);
```

### AnomalyDetector

```javascript
const detector = new AnomalyDetector({
  zScoreThreshold: 2.5,      // Z-score threshold
  iqrMultiplier: 1.5,        // IQR multiplier
  windowSize: 10,            // Moving window size
  guardRiskThreshold: 0.7    // Guard violation risk threshold
});

// Observe drift
detector.observe({
  epoch: 1,
  drift: 10,               // Absolute drift
  normalized: 0.5,         // Normalized drift
  timestamp: Date.now(),
  added: 5,                // Optional: added artifacts
  removed: 2,              // Optional: removed artifacts
  modified: 1              // Optional: modified artifacts
});

// Detect anomalies
const anomalies = detector.detect({
  useZScore: true,
  useIQR: true,
  detectPatterns: true
});
// Returns: [{ type, severity, description, epoch, value, threshold, suggestions }]

// Predict guard violation
const prediction = detector.predictGuardViolation({
  name: 'max-drift',
  driftThreshold: 1.0
});
// Returns: { risk, willViolate, estimatedEpochs, confidence }

// Get summary
const summary = detector.getSummary();
// Returns: { totalObservations, detectedAnomalies, statistics, recentDrift, anomalyTypes }
```

### Utility Functions

```javascript
// Calculate reward for Q-learning
const reward = calculateReward({
  convergenceTime: 5,      // Epochs to convergence
  compressionRatio: 0.8,   // Compression ratio (0-1)
  budgetUsed: 0.3          // Budget used (0-1, optional)
});
// Returns: reward value (higher is better)
```

## Integration with KGC-SWARM

### Pattern Recognition for Convergence Prediction

```javascript
import { createPatternRecognizer } from './src/ml/index.mjs';
import { ConvergenceDetector } from './packages/kgc-swarm/src/convergence.mjs';

const recognizer = createPatternRecognizer();
const convergence = new ConvergenceDetector({ driftThreshold: 0.01 });

// During each epoch
function onEpoch(artifactState) {
  const drift = convergence.recordEpoch(artifactState);

  if (drift) {
    recognizer.observe({
      artifacts: Array.from(artifactState.artifacts),
      driftValue: drift.drift,
      normalized: drift.normalized,
      epoch: convergence.history.length,
      timestamp: Date.now(),
    });
  }

  // Get convergence prediction
  const prediction = recognizer.predictConvergence();
  console.log(`Predicted convergence in ${prediction.estimatedEpochs} epochs (confidence: ${prediction.confidence})`);

  // Get compression strategy
  const strategy = recognizer.suggestCompressionStrategy();
  console.log(`Recommended: ${strategy.strategy} (expected ratio: ${strategy.expectedRatio})`);
}
```

### Q-Learning for Agent Action Selection

```javascript
import { createQLearningAgent, calculateReward } from './src/ml/index.mjs';

const agent = createQLearningAgent({
  actions: [
    { id: 'aggressive', name: 'Aggressive Compression' },
    { id: 'balanced', name: 'Balanced' },
    { id: 'conservative', name: 'Conservative' },
  ],
  epsilon: 0.1,
  learningRate: 0.1,
});

// Agent control loop
function selectStrategy(metrics) {
  const state = {
    driftLevel: metrics.drift > 0.5 ? 'high' : metrics.drift > 0.1 ? 'medium' : 'low',
    epochCount: metrics.epoch < 10 ? 'early' : metrics.epoch < 50 ? 'middle' : 'late',
    budgetUsed: metrics.budgetRatio < 0.3 ? 'low' : metrics.budgetRatio < 0.7 ? 'medium' : 'high',
  };

  const action = agent.selectAction(state);
  return action;
}

// After convergence
function onConvergence(metrics, action, startState) {
  const reward = calculateReward({
    convergenceTime: metrics.epochs,
    compressionRatio: metrics.compressionRatio,
    budgetUsed: metrics.budgetUsed,
  });

  const endState = { driftLevel: 'converged', epochCount: 'late', budgetUsed: 'high' };
  agent.learn(startState, action, reward, endState, true);
  agent.endEpisode();
}
```

### Anomaly Detection for Guard Violations

```javascript
import { createAnomalyDetector } from './src/ml/index.mjs';

const detector = createAnomalyDetector({ zScoreThreshold: 2.5 });

// Monitor drift patterns
function monitorDrift(driftMetrics) {
  detector.observe({
    epoch: driftMetrics.epoch,
    drift: driftMetrics.drift,
    normalized: driftMetrics.normalized,
    timestamp: Date.now(),
  });

  // Check for anomalies
  const anomalies = detector.detect();
  if (anomalies.length > 0) {
    anomalies.forEach(a => {
      console.warn(`ANOMALY: ${a.description} (severity: ${a.severity})`);
      console.warn(`Suggestions: ${a.suggestions.join('; ')}`);
    });
  }

  // Predict guard violations
  const prediction = detector.predictGuardViolation({
    name: 'max-drift-guard',
    driftThreshold: 0.8,
  });

  if (prediction.willViolate) {
    console.error(`GUARD VIOLATION PREDICTED in ${prediction.estimatedEpochs} epochs (risk: ${prediction.risk})`);
  }
}
```

## Performance Characteristics

| Operation | Time Complexity | Space Complexity |
|-----------|----------------|------------------|
| Pattern observation | O(1) | O(1) |
| Pattern extraction | O(w) | O(w) |
| Convergence prediction | O(1) | O(1) |
| Q-learning action selection | O(1) | O(1) |
| Q-learning update | O(1) | O(1) |
| Anomaly detection (Z-score) | O(1) amortized | O(n) |
| Anomaly detection (IQR) | O(n log n) | O(n) |
| Guard violation prediction | O(n) | O(1) |

Where:
- `n` = number of observations
- `w` = pattern window size (default: 5)

## Design Principles

1. **No Heavy Dependencies**: Native JavaScript only - no TensorFlow.js, no scikit-learn equivalents
2. **Typed Arrays**: Use Float64Array, Uint32Array for performance
3. **Interpretability**: Simple, understandable algorithms (linear regression, frequency analysis, Z-score)
4. **Zod Validation**: Schema validation for all inputs
5. **Stateless Where Possible**: Clear state management with reset() methods

## Limitations

- **Not Deep Learning**: No neural networks (by design) - uses statistical ML
- **Limited State Space**: Q-learning uses discretized state space (72 states)
- **Fixed Window**: Pattern recognition uses fixed window size
- **Linear Assumptions**: Drift prediction assumes linear trends

These limitations are intentional - prioritizing interpretability and lightweight implementation over maximum accuracy.

## Testing

Run the demo to validate all modules:

```bash
node src/ml/demo.mjs
```

Test file location: `test/ml/ml.test.mjs`

## File Structure

```
src/ml/
├── index.mjs                 # Main exports
├── pattern-recognition.mjs   # Pattern recognition from O_τ
├── q-learning.mjs            # Q-learning agent
├── anomaly-detection.mjs     # Anomaly detection
├── demo.mjs                  # Interactive demo
└── README.md                 # This file

test/ml/
└── ml.test.mjs               # Comprehensive tests
```

## License

MIT

## References

- Sutton & Barto, "Reinforcement Learning: An Introduction" (2nd ed.)
- Aggarwal, "Outlier Analysis" (2nd ed.)
- Shannon, "A Mathematical Theory of Communication" (1948)
