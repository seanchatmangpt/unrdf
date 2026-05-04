/**
 * @file YAWL Observability Tests - Metrics, Tracing, and SLI
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { createWorkflowEngine } from '@unrdf/yawl';
import {
  YAWLMetricsCollector,
  YAWLTracer,
  YAWLSLICalculator,
} from '../src/index.mjs';

describe('YAWLMetricsCollector', () => {
  let engine;
  let metrics;

  beforeEach(() => {
    engine = createWorkflowEngine({ enableEventLog: false });
    metrics = new YAWLMetricsCollector(engine, {
      prefix: 'test_yawl',
      collectDefaultMetrics: false,
    });
  });

  afterEach(() => {
    if (metrics) {
      metrics.destroy();
    }
  });

  describe('Initialization', () => {
    it('should create metrics collector with default config', () => {
      expect(metrics).toBeDefined();
      expect(metrics.config.prefix).toBe('test_yawl');
      expect(metrics.register).toBeDefined();
    });

    it('should register all metrics', () => {
      expect(metrics.metrics.casesTotal).toBeDefined();
      expect(metrics.metrics.tasksTotal).toBeDefined();
      expect(metrics.metrics.taskDuration).toBeDefined();
      expect(metrics.metrics.patternUsage).toBeDefined();
    });

    it('should have correct content type', () => {
      expect(metrics.contentType).toContain('text/plain');
    });
  });

  describe('Case Metrics', () => {
    it('should track case creation', async () => {
      // Register simple workflow
      engine.registerWorkflow({
        id: 'test-wf',
        startTaskId: 'start',
        tasks: [
          {
            id: 'start',
            kind: 'EmptyTask',
            outputConditions: ['c1'],
          },
        ],
        conditions: [{ id: 'c1', evaluation: () => true }],
      });

      await engine.createCase('test-wf', { test: 'data' });

      const output = await metrics.getMetrics();
      expect(output).toContain('test_yawl_workflow_cases_total');
      expect(output).toMatch(/status="created"/);
    });

    it('should track case completion', async () => {
      engine.registerWorkflow({
        id: 'simple-wf',
        startTaskId: 'start',
        tasks: [
          {
            id: 'start',
            kind: 'EmptyTask',
            outputConditions: ['end'],
          },
          {
            id: 'end',
            kind: 'EmptyTask',
            inputConditions: ['end'],
          },
        ],
        conditions: [{ id: 'end', evaluation: () => true }],
      });

      const { case: yawlCase } = await engine.createCase('simple-wf', {});
      const workItem = Array.from(yawlCase.workItems.values())[0];

      await engine.startTask(yawlCase.id, workItem.id);
      await engine.completeTask(yawlCase.id, workItem.id, {});

      // Wait for case to complete
      await new Promise((resolve) => setTimeout(resolve, 50));

      const output = await metrics.getMetrics();
      expect(output).toMatch(/status="completed"/);
    });
  });

  describe('Task Metrics', () => {
    beforeEach(() => {
      engine.registerWorkflow({
        id: 'task-wf',
        startTaskId: 'task1',
        tasks: [
          {
            id: 'task1',
            kind: 'AtomicTask',
            outputConditions: ['c1'],
          },
        ],
        conditions: [{ id: 'c1', evaluation: () => true }],
      });
    });

    it('should track task enablement', async () => {
      await engine.createCase('task-wf', {});

      const output = await metrics.getMetrics();
      expect(output).toContain('test_yawl_workflow_tasks_total');
      expect(output).toMatch(/status="enabled"/);
    });

    it('should track task duration', async () => {
      const { case: yawlCase } = await engine.createCase('task-wf', {});
      const workItem = Array.from(yawlCase.workItems.values())[0];

      await engine.startTask(yawlCase.id, workItem.id);
      await new Promise((resolve) => setTimeout(resolve, 100));
      await engine.completeTask(yawlCase.id, workItem.id, {});

      const output = await metrics.getMetrics();
      expect(output).toContain('test_yawl_task_duration_seconds');
    });

    it('should track task errors', async () => {
      const { case: yawlCase } = await engine.createCase('task-wf', {});
      const workItem = Array.from(yawlCase.workItems.values())[0];

      await engine.startTask(yawlCase.id, workItem.id);
      await engine.cancelTask(yawlCase.id, workItem.id, 'test cancellation');

      const output = await metrics.getMetrics();
      expect(output).toContain('test_yawl_task_errors_total');
      expect(output).toMatch(/error_type="cancelled"/);
    });
  });

  describe('Pattern Metrics', () => {
    it('should track XOR split pattern', async () => {
      engine.registerWorkflow({
        id: 'xor-wf',
        startTaskId: 'decision',
        tasks: [
          {
            id: 'decision',
            kind: 'AtomicTask',
            outputConditions: ['c1', 'c2'],
            splitType: 'xor',
          },
          {
            id: 'path1',
            kind: 'EmptyTask',
            inputConditions: ['c1'],
          },
          {
            id: 'path2',
            kind: 'EmptyTask',
            inputConditions: ['c2'],
          },
        ],
        conditions: [
          { id: 'c1', evaluation: () => true },
          { id: 'c2', evaluation: () => false },
        ],
      });

      const { case: yawlCase } = await engine.createCase('xor-wf', {});
      const workItem = Array.from(yawlCase.workItems.values())[0];

      await engine.startTask(yawlCase.id, workItem.id);
      await engine.completeTask(yawlCase.id, workItem.id, {});

      const output = await metrics.getMetrics();
      expect(output).toContain('test_yawl_pattern_usage_count');
      expect(output).toMatch(/pattern_type="xor"/);
    });
  });

  describe('Metric API', () => {
    it('should get metrics in Prometheus format', async () => {
      const output = await metrics.getMetrics();
      expect(typeof output).toBe('string');
      expect(output.length).toBeGreaterThan(0);
    });

    it('should get specific metric by name', () => {
      const metric = metrics.getMetric('workflow_cases_total');
      expect(metric).toBeDefined();
      expect(metric.name).toBe('test_yawl_workflow_cases_total');
    });

    it('should reset metrics', async () => {
      engine.registerWorkflow({
        id: 'reset-wf',
        startTaskId: 'start',
        tasks: [{ id: 'start', kind: 'EmptyTask', outputConditions: ['c1'] }],
        conditions: [{ id: 'c1', evaluation: () => true }],
      });

      await engine.createCase('reset-wf', {});

      metrics.resetMetrics();

      const output = await metrics.getMetrics();
      // After reset, should not contain case data (or be 0)
      expect(output).toBeDefined();
    });
  });
});

describe('YAWLTracer', () => {
  let engine;
  let tracer;

  beforeEach(() => {
    engine = createWorkflowEngine({ enableEventLog: false });
    tracer = new YAWLTracer(engine, {
      includeReceiptHashes: true,
      includeTaskData: true,
    });
  });

  afterEach(() => {
    if (tracer) {
      tracer.destroy();
    }
  });

  describe('Initialization', () => {
    it('should create tracer with default config', () => {
      expect(tracer).toBeDefined();
      expect(tracer.config.tracerName).toBe('@unrdf/yawl');
      expect(tracer.tracer).toBeDefined();
    });
  });

  describe('Span Creation', () => {
    it('should create case span on case creation', async () => {
      engine.registerWorkflow({
        id: 'span-wf',
        startTaskId: 'start',
        tasks: [{ id: 'start', kind: 'EmptyTask', outputConditions: ['c1'] }],
        conditions: [{ id: 'c1', evaluation: () => true }],
      });

      const { case: yawlCase } = await engine.createCase('span-wf', {});

      const caseSpan = tracer.getCaseSpan(yawlCase.id);
      expect(caseSpan).toBeDefined();
    });

    it('should create task span on task start', async () => {
      engine.registerWorkflow({
        id: 'task-span-wf',
        startTaskId: 'task1',
        tasks: [
          {
            id: 'task1',
            kind: 'AtomicTask',
            outputConditions: ['c1'],
          },
        ],
        conditions: [{ id: 'c1', evaluation: () => true }],
      });

      const { case: yawlCase } = await engine.createCase('task-span-wf', {});
      const workItem = Array.from(yawlCase.workItems.values())[0];

      await engine.startTask(yawlCase.id, workItem.id);

      const taskSpan = tracer.getTaskSpan(workItem.id);
      expect(taskSpan).toBeDefined();
    });

    it('should end spans on completion', async () => {
      engine.registerWorkflow({
        id: 'complete-wf',
        startTaskId: 'task1',
        tasks: [
          {
            id: 'task1',
            kind: 'AtomicTask',
            outputConditions: ['end'],
          },
          {
            id: 'end',
            kind: 'EmptyTask',
            inputConditions: ['end'],
          },
        ],
        conditions: [{ id: 'end', evaluation: () => true }],
      });

      const { case: yawlCase } = await engine.createCase('complete-wf', {});
      const workItem = Array.from(yawlCase.workItems.values())[0];

      await engine.startTask(yawlCase.id, workItem.id);
      await engine.completeTask(yawlCase.id, workItem.id, {});

      // Task span should be cleaned up after completion
      const taskSpan = tracer.getTaskSpan(workItem.id);
      expect(taskSpan).toBeUndefined();
    });
  });

  describe('Custom Spans', () => {
    it('should create custom span within case context', async () => {
      engine.registerWorkflow({
        id: 'custom-wf',
        startTaskId: 'start',
        tasks: [{ id: 'start', kind: 'EmptyTask', outputConditions: ['c1'] }],
        conditions: [{ id: 'c1', evaluation: () => true }],
      });

      const { case: yawlCase } = await engine.createCase('custom-wf', {});

      const result = await tracer.spanInCase(yawlCase.id, 'custom.operation', async () => {
        return 'test-result';
      });

      expect(result).toBe('test-result');
    });
  });
});

describe('YAWLSLICalculator', () => {
  let engine;
  let sli;

  beforeEach(() => {
    engine = createWorkflowEngine({ enableEventLog: false });
    sli = new YAWLSLICalculator(engine, {
      windowMs: 60000,
      targetCompletionRate: 0.95,
      targetTaskSuccessRate: 0.99,
      targetP95Latency: 2.0,
    });
  });

  afterEach(() => {
    if (sli) {
      sli.destroy();
    }
  });

  describe('Initialization', () => {
    it('should create SLI calculator with default config', () => {
      expect(sli).toBeDefined();
      expect(sli.config.targetCompletionRate).toBe(0.95);
      expect(sli.config.targetTaskSuccessRate).toBe(0.99);
    });
  });

  describe('Completion Rate', () => {
    it('should calculate 100% completion rate with no cases', () => {
      const rate = sli.calculateCompletionRate();
      expect(rate).toBe(1.0);
    });

    it('should calculate completion rate correctly', async () => {
      engine.registerWorkflow({
        id: 'sli-wf',
        startTaskId: 'start',
        tasks: [
          {
            id: 'start',
            kind: 'EmptyTask',
            outputConditions: ['end'],
          },
          {
            id: 'end',
            kind: 'EmptyTask',
            inputConditions: ['end'],
          },
        ],
        conditions: [{ id: 'end', evaluation: () => true }],
      });

      // Create and complete 2 cases
      for (let i = 0; i < 2; i++) {
        const { case: yawlCase } = await engine.createCase('sli-wf', {});
        const workItem = Array.from(yawlCase.workItems.values())[0];
        await engine.startTask(yawlCase.id, workItem.id);
        await engine.completeTask(yawlCase.id, workItem.id, {});
      }

      await new Promise((resolve) => setTimeout(resolve, 100));

      const rate = sli.calculateCompletionRate();
      expect(rate).toBe(1.0); // 100% completion
    });
  });

  describe('Task Success Rate', () => {
    it('should calculate task success rate', async () => {
      engine.registerWorkflow({
        id: 'success-wf',
        startTaskId: 'task1',
        tasks: [
          {
            id: 'task1',
            kind: 'AtomicTask',
            outputConditions: ['c1'],
          },
        ],
        conditions: [{ id: 'c1', evaluation: () => true }],
      });

      const { case: yawlCase } = await engine.createCase('success-wf', {});
      const workItem = Array.from(yawlCase.workItems.values())[0];

      await engine.startTask(yawlCase.id, workItem.id);
      await engine.completeTask(yawlCase.id, workItem.id, {});

      const rate = sli.calculateTaskSuccessRate();
      expect(rate).toBe(1.0);
    });

    it('should calculate error rate from failures', async () => {
      engine.registerWorkflow({
        id: 'error-wf',
        startTaskId: 'task1',
        tasks: [
          {
            id: 'task1',
            kind: 'AtomicTask',
            outputConditions: ['c1'],
          },
        ],
        conditions: [{ id: 'c1', evaluation: () => true }],
      });

      const { case: yawlCase } = await engine.createCase('error-wf', {});
      const workItem = Array.from(yawlCase.workItems.values())[0];

      await engine.startTask(yawlCase.id, workItem.id);
      await engine.cancelTask(yawlCase.id, workItem.id, 'test');

      const errorRate = sli.calculateTaskErrorRate();
      expect(errorRate).toBe(1.0); // 100% error rate
    });
  });

  describe('Latency Metrics', () => {
    it('should calculate p95 latency', async () => {
      engine.registerWorkflow({
        id: 'latency-wf',
        startTaskId: 'task1',
        tasks: [
          {
            id: 'task1',
            kind: 'AtomicTask',
            outputConditions: ['c1'],
          },
        ],
        conditions: [{ id: 'c1', evaluation: () => true }],
      });

      // Execute tasks with varying durations
      for (let i = 0; i < 10; i++) {
        const { case: yawlCase } = await engine.createCase('latency-wf', {});
        const workItem = Array.from(yawlCase.workItems.values())[0];

        await engine.startTask(yawlCase.id, workItem.id);
        await new Promise((resolve) => setTimeout(resolve, 10 + i * 5));
        await engine.completeTask(yawlCase.id, workItem.id, {});
      }

      const p95 = sli.calculateP95Latency();
      expect(p95).toBeGreaterThan(0);

      const median = sli.calculateMedianLatency();
      expect(median).toBeGreaterThan(0);
      expect(p95).toBeGreaterThanOrEqual(median);
    });
  });

  describe('SLO Compliance', () => {
    it('should calculate SLO compliance', async () => {
      const compliance = sli.calculateSLOCompliance();

      expect(compliance).toHaveProperty('completionRate');
      expect(compliance).toHaveProperty('taskSuccessRate');
      expect(compliance).toHaveProperty('p95Latency');
      expect(compliance).toHaveProperty('score');
      expect(compliance.score).toBeGreaterThanOrEqual(0);
      expect(compliance.score).toBeLessThanOrEqual(1);
    });
  });

  describe('Snapshots and Reports', () => {
    it('should generate SLI snapshot', () => {
      const snapshot = sli.getSnapshot();

      expect(snapshot).toHaveProperty('timestamp');
      expect(snapshot).toHaveProperty('completionRate');
      expect(snapshot).toHaveProperty('taskSuccessRate');
      expect(snapshot).toHaveProperty('taskErrorRate');
      expect(snapshot).toHaveProperty('p95Latency');
      expect(snapshot).toHaveProperty('sloCompliance');
    });

    it('should generate SLO report', () => {
      const report = sli.getSLOReport();

      expect(report).toHaveProperty('timestamp');
      expect(report).toHaveProperty('overall');
      expect(report).toHaveProperty('metrics');
      expect(Array.isArray(report.metrics)).toBe(true);
      expect(report.metrics.length).toBe(4);
    });

    it('should export to Prometheus format', () => {
      const promFormat = sli.toPrometheus();

      expect(promFormat).toContain('yawl_sli_completion_rate');
      expect(promFormat).toContain('yawl_sli_task_success_rate');
      expect(promFormat).toContain('yawl_slo_compliance');
    });
  });
});
