/**
 * @file YAWL AI - ML-powered workflow optimization
 * @module @unrdf/yawl-ai
 *
 * @description
 * AI-powered workflow optimization using TensorFlow.js and existing YAWL patterns.
 * Provides path prediction, performance optimization, and anomaly detection.
 *
 * Features:
 * - Workflow path prediction using neural networks
 * - Performance bottleneck analysis with ML
 * - Pattern anomaly detection
 * - Real-time workflow monitoring
 * - Integration with YAWL workflow engine
 *
 * @example
 * ```javascript
 * import { createPredictor, createOptimizer, createDetector, createAdapter } from '@unrdf/yawl-ai';
 * import { WorkflowEngine } from '@unrdf/yawl/engine';
 *
 * const engine = new WorkflowEngine();
 * const predictor = createPredictor();
 * const optimizer = createOptimizer();
 * const detector = createDetector();
 *
 * const adapter = createAdapter(engine, { predictor, optimizer, detector });
 * await adapter.trainModels();
 *
 * // Get predictions
 * const prediction = await predictor.predict(['task1', 'task2']);
 *
 * // Get optimization report
 * const report = await optimizer.generateReport();
 *
 * // Detect anomalies
 * const anomalies = await detector.detect(execution);
 * ```
 */

// ML Components
export {
  WorkflowPathPredictor,
  createPredictor,
} from './ml/workflow-predictor.mjs';

export {
  PerformanceOptimizer,
  createOptimizer,
} from './ml/performance-optimizer.mjs';

export {
  AnomalyDetector,
  createDetector,
} from './ml/anomaly-detector.mjs';

// Integration
export { YAWLMLAdapter, createAdapter } from './integration/yawl-adapter.mjs';

// Default export
import { createPredictor } from './ml/workflow-predictor.mjs';
import { createOptimizer } from './ml/performance-optimizer.mjs';
import { createDetector } from './ml/anomaly-detector.mjs';
import { createAdapter } from './integration/yawl-adapter.mjs';

export default {
  createPredictor,
  createOptimizer,
  createDetector,
  createAdapter,
};
