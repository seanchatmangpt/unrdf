/**
 * @file YAWL Adapter - Integration layer for ML components with YAWL engine
 * @module @unrdf/yawl-ai/adapter
 *
 * @description
 * Provides seamless integration between YAWL workflow engine and AI/ML
 * components. Converts YAWL event logs into ML training data, and provides
 * hooks for real-time prediction and optimization.
 *
 * Integration Points:
 * - Event log extraction and transformation
 * - Real-time workflow monitoring
 * - Prediction-based task scheduling
 * - Performance metrics collection
 *
 * YAWL Integration:
 * - Uses WorkflowEngine event system for data collection
 * - Leverages event-sourcing for historical data
 * - Integrates with YAWL patterns for optimization suggestions
 */

import { z } from 'zod';

// =============================================================================
// Schemas
// =============================================================================

/**
 * @typedef {Object} YAWLEventLog
 * @property {string} caseId - Case identifier
 * @property {string} workflowId - Workflow identifier
 * @property {string} eventType - Event type from YAWL_EVENT_TYPES
 * @property {Object} payload - Event payload
 * @property {number} timestamp - Event timestamp
 */
const YAWLEventLogSchema = z.object({
  caseId: z.string(),
  workflowId: z.string(),
  eventType: z.string(),
  payload: z.record(z.unknown()),
  timestamp: z.number(),
});

// =============================================================================
// YAWL ML Adapter
// =============================================================================

/**
 * Adapter for integrating ML components with YAWL workflow engine
 *
 * @class YAWLMLAdapter
 * @description
 * Bridges YAWL workflow engine with ML models for prediction, optimization,
 * and anomaly detection. Extracts training data from YAWL events and provides
 * ML-powered insights.
 */
export class YAWLMLAdapter {
  /**
   * @param {Object} engine - YAWL WorkflowEngine instance
   * @param {Object} mlComponents - ML component instances
   * @param {Object} mlComponents.predictor - WorkflowPathPredictor
   * @param {Object} mlComponents.optimizer - PerformanceOptimizer
   * @param {Object} mlComponents.detector - AnomalyDetector
   */
  constructor(engine, mlComponents = {}) {
    this.engine = engine;
    this.predictor = mlComponents.predictor || null;
    this.optimizer = mlComponents.optimizer || null;
    this.detector = mlComponents.detector || null;

    this.eventBuffer = [];
    this.caseExecutions = new Map(); // caseId -> execution data
    this.monitoring = false;
  }

  /**
   * Extract workflow sequences from YAWL event log
   *
   * @param {Object[]} events - YAWL event log entries
   * @returns {Object[]} Workflow sequences for ML training
   */
  extractWorkflowSequences(events) {
    const caseMap = new Map();

    // Group events by case
    for (const event of events) {
      if (!caseMap.has(event.caseId)) {
        caseMap.set(event.caseId, {
          caseId: event.caseId,
          workflowId: event.workflowId,
          taskIds: [],
          durations: [],
          events: [],
        });
      }

      const caseData = caseMap.get(event.caseId);

      // Track TASK_COMPLETED events for sequence learning
      if (event.eventType === 'YAWL_TASK_COMPLETED') {
        const taskId = event.payload.taskId;
        const duration =
          (event.payload.completedAt || event.timestamp) -
          (event.payload.startedAt || event.timestamp);

        caseData.taskIds.push(taskId);
        caseData.durations.push(duration);
      }

      caseData.events.push(event);
    }

    return Array.from(caseMap.values());
  }

  /**
   * Extract execution metrics for performance optimization
   *
   * @param {Object[]} events - YAWL event log entries
   * @returns {Object[]} Execution data with task metrics
   */
  extractExecutionMetrics(events) {
    const executions = [];
    const caseMap = new Map();

    for (const event of events) {
      if (!caseMap.has(event.caseId)) {
        caseMap.set(event.caseId, {
          workflowId: event.workflowId,
          tasks: [],
          dependencies: new Map(),
        });
      }

      const caseData = caseMap.get(event.caseId);

      // Extract task execution data
      if (event.eventType === 'YAWL_TASK_COMPLETED') {
        const task = {
          taskId: event.payload.taskId,
          duration:
            (event.payload.completedAt || event.timestamp) -
            (event.payload.startedAt || event.timestamp),
          startTime: event.payload.startedAt || event.timestamp,
          dependencies: [], // Would need workflow definition for accurate deps
        };

        caseData.tasks.push(task);
      }

      // Track control flow for dependencies
      if (event.eventType === 'YAWL_CONTROL_FLOW_EVALUATED') {
        const sourceTask = event.payload.sourceTaskId;
        const targetTask = event.payload.targetTaskId;

        if (sourceTask && targetTask) {
          if (!caseData.dependencies.has(targetTask)) {
            caseData.dependencies.set(targetTask, []);
          }
          caseData.dependencies.get(targetTask).push(sourceTask);
        }
      }
    }

    // Convert to execution format
    for (const [caseId, caseData] of caseMap) {
      // Apply dependencies to tasks
      for (const task of caseData.tasks) {
        task.dependencies = caseData.dependencies.get(task.taskId) || [];
      }

      executions.push({
        workflowId: caseData.workflowId,
        tasks: caseData.tasks,
      });
    }

    return executions;
  }

  /**
   * Extract workflow executions for anomaly detection
   *
   * @param {Object[]} events - YAWL event log entries
   * @returns {Object[]} Workflow executions with timing data
   */
  extractWorkflowExecutions(events) {
    const caseMap = new Map();

    for (const event of events) {
      if (!caseMap.has(event.caseId)) {
        caseMap.set(event.caseId, {
          caseId: event.caseId,
          workflowId: event.workflowId,
          events: [],
          startTime: null,
          endTime: null,
        });
      }

      const caseData = caseMap.get(event.caseId);

      if (event.eventType === 'YAWL_CASE_CREATED') {
        caseData.startTime = event.timestamp;
      }

      if (event.eventType === 'YAWL_TASK_COMPLETED') {
        caseData.events.push({
          taskId: event.payload.taskId,
          duration:
            (event.payload.completedAt || event.timestamp) -
            (event.payload.startedAt || event.timestamp),
        });
        caseData.endTime = event.payload.completedAt || event.timestamp;
      }
    }

    return Array.from(caseMap.values())
      .filter((c) => c.events.length > 0)
      .map((c) => ({
        caseId: c.caseId,
        workflowId: c.workflowId,
        events: c.events,
        totalDuration: c.endTime && c.startTime ? c.endTime - c.startTime : 0,
      }));
  }

  /**
   * Train all ML models using YAWL event history
   *
   * @param {Object} [options] - Training options
   * @param {number} [options.lookbackDays=30] - Days of history to use
   * @returns {Promise<Object>} Training results
   */
  async trainModels(options = {}) {
    const { lookbackDays = 30 } = options;

    console.log('Fetching YAWL event history...');

    // Get all events from engine (would normally filter by date)
    const allEvents = this.engine.getAllEvents ? this.engine.getAllEvents() : [];

    // Validate events
    const validEvents = allEvents.map((e) => YAWLEventLogSchema.parse(e));

    console.log(`Processing ${validEvents.length} events...`);

    const results = {};

    // Train predictor
    if (this.predictor) {
      console.log('Training workflow path predictor...');
      const sequences = this.extractWorkflowSequences(validEvents);

      if (sequences.length > 0) {
        results.predictor = await this.predictor.train(sequences, {
          epochs: 30,
          batchSize: 16,
        });
        console.log(
          `Predictor trained on ${sequences.length} workflow sequences`,
        );
      } else {
        console.log('No sequences found for predictor training');
      }
    }

    // Train optimizer
    if (this.optimizer) {
      console.log('Training performance optimizer...');
      const executions = this.extractExecutionMetrics(validEvents);

      if (executions.length > 0) {
        await this.optimizer.analyze(executions);
        console.log(`Optimizer analyzed ${executions.length} executions`);
        results.optimizer = { analyzed: true };
      } else {
        console.log('No executions found for optimizer training');
      }
    }

    // Train detector
    if (this.detector) {
      console.log('Training anomaly detector...');
      const workflowExecs = this.extractWorkflowExecutions(validEvents);

      if (workflowExecs.length > 0) {
        results.detector = await this.detector.train(workflowExecs, {
          epochs: 30,
          batchSize: 8,
        });
        console.log(
          `Detector trained on ${workflowExecs.length} workflow executions`,
        );
      } else {
        console.log('No workflow executions found for detector training');
      }
    }

    return results;
  }

  /**
   * Start real-time monitoring of workflow executions
   *
   * @param {Object} [options] - Monitoring options
   * @param {Function} [options.onPrediction] - Callback for predictions
   * @param {Function} [options.onAnomaly] - Callback for anomalies
   * @returns {void}
   */
  startMonitoring(options = {}) {
    const { onPrediction, onAnomaly } = options;

    if (this.monitoring) {
      console.warn('Monitoring already active');
      return;
    }

    this.monitoring = true;

    // Subscribe to YAWL events
    if (this.engine.on) {
      // Task completed - trigger prediction
      this.engine.on('TASK_COMPLETED', async (event) => {
        if (!this.caseExecutions.has(event.caseId)) {
          this.caseExecutions.set(event.caseId, {
            caseId: event.caseId,
            workflowId: event.workflowId,
            taskSequence: [],
            events: [],
          });
        }

        const execution = this.caseExecutions.get(event.caseId);
        execution.taskSequence.push(event.payload.taskId);
        execution.events.push(event);

        // Predict next task
        if (this.predictor && this.predictor.trained && onPrediction) {
          try {
            const prediction = await this.predictor.predict(
              execution.taskSequence,
            );
            onPrediction({
              caseId: event.caseId,
              prediction,
              currentSequence: execution.taskSequence,
            });
          } catch (error) {
            console.error('Prediction error:', error.message);
          }
        }

        // Detect anomalies
        if (this.detector && this.detector.trained && onAnomaly) {
          const workflowExec = {
            caseId: event.caseId,
            workflowId: event.workflowId,
            events: execution.events.map((e) => ({
              taskId: e.payload.taskId,
              duration:
                (e.payload.completedAt || e.timestamp) -
                (e.payload.startedAt || e.timestamp),
            })),
            totalDuration: Date.now() - (execution.events[0]?.timestamp || 0),
          };

          try {
            const anomalies = await this.detector.detect(workflowExec);
            if (anomalies.length > 0) {
              onAnomaly({
                caseId: event.caseId,
                anomalies,
              });
            }
          } catch (error) {
            console.error('Anomaly detection error:', error.message);
          }
        }
      });

      console.log('Real-time monitoring started');
    } else {
      console.warn('Engine does not support event subscriptions');
    }
  }

  /**
   * Stop real-time monitoring
   */
  stopMonitoring() {
    if (this.engine.off) {
      this.engine.off('TASK_COMPLETED');
    }
    this.monitoring = false;
    console.log('Monitoring stopped');
  }

  /**
   * Get optimization recommendations for a workflow
   *
   * @param {string} workflowId - Workflow to optimize
   * @returns {Promise<Object>} Optimization report
   */
  async getOptimizationReport(workflowId) {
    if (!this.optimizer || !this.optimizer.analyzed) {
      throw new Error('Optimizer not ready. Train models first.');
    }

    const report = await this.optimizer.generateReport();

    return {
      workflowId,
      ...report,
      generatedAt: new Date().toISOString(),
    };
  }

  /**
   * Predict optimal path for a case
   *
   * @param {string} caseId - Case identifier
   * @param {number} [steps=5] - Number of steps to predict
   * @returns {Promise<Object>} Path prediction
   */
  async predictPath(caseId, steps = 5) {
    if (!this.predictor || !this.predictor.trained) {
      throw new Error('Predictor not ready. Train models first.');
    }

    const execution = this.caseExecutions.get(caseId);
    if (!execution) {
      throw new Error(`No execution data for case: ${caseId}`);
    }

    const suggestions = await this.predictor.suggestResources(
      execution.taskSequence,
      steps,
    );

    return {
      caseId,
      currentPath: execution.taskSequence,
      predictedPath: suggestions.criticalPath,
      estimatedDuration: suggestions.estimatedTotalDuration,
      parallelizationSuggestions: suggestions.recommendedParallelization,
    };
  }

  /**
   * Get comprehensive ML insights
   *
   * @returns {Object} ML model summaries
   */
  getMLSummary() {
    return {
      predictor: this.predictor ? this.predictor.getSummary() : null,
      optimizer: this.optimizer ? this.optimizer.getSummary() : null,
      detector: this.detector ? this.detector.getSummary() : null,
      monitoring: this.monitoring,
      activeCases: this.caseExecutions.size,
    };
  }

  /**
   * Dispose of all ML resources
   */
  dispose() {
    this.stopMonitoring();

    if (this.predictor) this.predictor.dispose();
    if (this.detector) this.detector.dispose();

    this.caseExecutions.clear();
    this.eventBuffer = [];
  }
}

/**
 * Create a YAWL ML adapter
 *
 * @param {Object} engine - YAWL WorkflowEngine instance
 * @param {Object} mlComponents - ML components
 * @returns {YAWLMLAdapter} New adapter instance
 */
export function createAdapter(engine, mlComponents = {}) {
  return new YAWLMLAdapter(engine, mlComponents);
}

export default { YAWLMLAdapter, createAdapter };
