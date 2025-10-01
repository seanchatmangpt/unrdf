/**
 * @file Scenario-Based Test Framework for CLI + Sidecar Integration
 * @module scenario-framework
 *
 * @description
 * 80/20 scenario execution framework with OTEL trace validation.
 * Focuses on the 20% of scenarios that validate 80% of real-world usage.
 */

import { exec } from 'child_process';
import { promisify } from 'util';
import { randomUUID } from 'crypto';

const execAsync = promisify(exec);

/**
 * Scenario execution context with OTEL tracing
 */
export class ScenarioContext {
  constructor(stack, jaegerClient) {
    this.stack = stack;
    this.jaegerClient = jaegerClient;
    this.traceId = null;
    this.spanId = null;
    this.results = [];
    this.startTime = null;
    this.endTime = null;
  }

  /**
   * Get traces for this scenario
   */
  async getTraces() {
    if (!this.traceId) {
      return [];
    }
    return await this.jaegerClient.getTracesByTraceId(this.traceId);
  }

  /**
   * Get all spans for this scenario
   */
  async getSpans() {
    const traces = await this.getTraces();
    return traces.flatMap(trace => trace.spans || []);
  }

  /**
   * Add step result
   */
  addResult(step, result) {
    this.results.push({ step, result, timestamp: Date.now() });
  }

  /**
   * Get scenario duration
   */
  getDuration() {
    if (!this.startTime || !this.endTime) {
      return 0;
    }
    return this.endTime - this.startTime;
  }
}

/**
 * Scenario runner that executes CLI commands and validates OTEL traces
 */
export class ScenarioRunner {
  constructor(stack) {
    this.stack = stack;
    this.cli = null;
    this.traceCollector = null;
  }

  /**
   * Initialize scenario runner with CLI path and Jaeger client
   */
  async initialize(cliPath, jaegerClient) {
    this.cliPath = cliPath;
    this.jaegerClient = jaegerClient;
  }

  /**
   * Run a complete scenario with all steps
   */
  async runScenario(scenario) {
    console.log(`\n🎬 Running scenario: ${scenario.name}`);
    console.log(`📝 Description: ${scenario.description}`);

    const traceId = this.generateTraceId();
    const context = new ScenarioContext(this.stack, this.jaegerClient);
    context.traceId = traceId;
    context.startTime = Date.now();

    try {
      // Execute all steps in sequence
      for (let i = 0; i < scenario.steps.length; i++) {
        const step = scenario.steps[i];
        console.log(`\n  Step ${i + 1}/${scenario.steps.length}: ${step.name}`);

        const result = await this.executeStep(step, traceId, context);
        context.addResult(step.name, result);

        // Validate step if it has validation function
        if (step.validation) {
          console.log(`    ✓ Running validation...`);
          await step.validation(context);
        }
      }

      // Wait for traces to propagate to Jaeger
      console.log(`\n  ⏳ Waiting for OTEL traces to propagate...`);
      await this.sleep(2000);

      // Validate OTEL traces if expected spans are defined
      if (scenario.expectedSpans && scenario.expectedSpans.length > 0) {
        console.log(`\n  🔍 Validating OTEL traces...`);
        await this.validateTraces(traceId, scenario.expectedSpans, context);
      }

      // Run scenario-level assertions
      if (scenario.assertions && scenario.assertions.length > 0) {
        console.log(`\n  ✅ Running scenario assertions...`);
        for (const assertion of scenario.assertions) {
          await assertion(context);
        }
      }

      context.endTime = Date.now();
      const duration = context.getDuration();

      console.log(`\n✅ Scenario completed successfully in ${duration}ms`);
      return { success: true, context, duration };

    } catch (error) {
      context.endTime = Date.now();
      console.error(`\n❌ Scenario failed: ${error.message}`);
      throw error;
    }
  }

  /**
   * Execute a single step with trace context
   */
  async executeStep(step, traceId, context) {
    const spanId = this.generateSpanId();
    context.spanId = spanId;

    // Build environment with trace context
    const env = {
      ...process.env,
      OTEL_TRACE_ID: traceId,
      OTEL_SPAN_ID: spanId,
      OTEL_SERVICE_NAME: 'unrdf-cli',
      OTEL_EXPORTER_OTLP_ENDPOINT: this.stack.getOtelEndpoint(),
    };

    try {
      if (step.command) {
        // Execute CLI command
        const { stdout, stderr } = await execAsync(step.command, { env });

        const result = {
          stdout: stdout.trim(),
          stderr: stderr.trim(),
          exitCode: 0,
        };

        // Validate exit code
        if (step.expectedExitCode !== undefined) {
          if (result.exitCode !== step.expectedExitCode) {
            throw new Error(
              `Expected exit code ${step.expectedExitCode}, got ${result.exitCode}`
            );
          }
        }

        // Validate output
        if (step.expectedOutput) {
          const regex = step.expectedOutput instanceof RegExp
            ? step.expectedOutput
            : new RegExp(step.expectedOutput);

          if (!regex.test(result.stdout)) {
            throw new Error(
              `Output does not match expected pattern: ${step.expectedOutput}`
            );
          }
        }

        console.log(`    ✓ Command executed: ${step.command}`);
        if (result.stdout) {
          console.log(`    📤 Output: ${result.stdout.substring(0, 100)}...`);
        }

        return result;
      }

      return { success: true };

    } catch (error) {
      // Command execution failed
      const exitCode = error.code || 1;

      // Check if this is an expected failure
      if (step.expectedExitCode !== undefined && exitCode === step.expectedExitCode) {
        console.log(`    ✓ Command failed as expected (exit code: ${exitCode})`);
        return { exitCode, error: error.message };
      }

      throw new Error(`Step failed: ${error.message}`);
    }
  }

  /**
   * Validate OTEL traces against expected spans
   */
  async validateTraces(traceId, expectedSpans, context) {
    const traces = await this.jaegerClient.getTracesByTraceId(traceId);

    if (!traces || traces.length === 0) {
      console.warn(`    ⚠️  No traces found for trace ID: ${traceId}`);
      return;
    }

    const allSpans = traces.flatMap(trace => trace.spans || []);
    console.log(`    🔍 Found ${allSpans.length} spans`);

    // Validate each expected span exists
    for (const expectedSpan of expectedSpans) {
      const span = allSpans.find(s =>
        s.operationName === expectedSpan ||
        s.name === expectedSpan
      );

      if (!span) {
        console.warn(`    ⚠️  Missing expected span: ${expectedSpan}`);
      } else {
        console.log(`    ✓ Found span: ${expectedSpan} (${span.duration || 0}µs)`);
      }
    }

    // Validate span structure
    const rootSpans = allSpans.filter(s => !s.references || s.references.length === 0);
    if (rootSpans.length === 0) {
      console.warn(`    ⚠️  No root span found`);
    } else {
      console.log(`    ✓ Root span: ${rootSpans[0].operationName || rootSpans[0].name}`);
    }

    return allSpans;
  }

  /**
   * Generate unique trace ID
   */
  generateTraceId() {
    return randomUUID().replace(/-/g, '');
  }

  /**
   * Generate unique span ID
   */
  generateSpanId() {
    return randomUUID().replace(/-/g, '').substring(0, 16);
  }

  /**
   * Sleep utility
   */
  sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

/**
 * Scenario builder utility
 */
export class ScenarioBuilder {
  constructor() {
    this.scenario = {
      name: '',
      description: '',
      steps: [],
      expectedSpans: [],
      assertions: [],
      priority: 'P0',
      tags: [],
    };
  }

  /**
   * Set scenario name
   */
  name(name) {
    this.scenario.name = name;
    return this;
  }

  /**
   * Set scenario description
   */
  description(description) {
    this.scenario.description = description;
    return this;
  }

  /**
   * Add a step
   */
  step(step) {
    this.scenario.steps.push(step);
    return this;
  }

  /**
   * Add expected span
   */
  expectSpan(spanName) {
    this.scenario.expectedSpans.push(spanName);
    return this;
  }

  /**
   * Add assertion
   */
  assert(assertion) {
    this.scenario.assertions.push(assertion);
    return this;
  }

  /**
   * Set priority
   */
  priority(priority) {
    this.scenario.priority = priority;
    return this;
  }

  /**
   * Add tag
   */
  tag(tag) {
    this.scenario.tags.push(tag);
    return this;
  }

  /**
   * Build scenario
   */
  build() {
    return this.scenario;
  }
}

/**
 * Performance metrics calculator
 */
export class PerformanceMetrics {
  static calculateP99(values) {
    if (!values || values.length === 0) return 0;

    const sorted = [...values].sort((a, b) => a - b);
    const index = Math.ceil(sorted.length * 0.99) - 1;
    return sorted[index] || 0;
  }

  static calculateP95(values) {
    if (!values || values.length === 0) return 0;

    const sorted = [...values].sort((a, b) => a - b);
    const index = Math.ceil(sorted.length * 0.95) - 1;
    return sorted[index] || 0;
  }

  static calculateP50(values) {
    if (!values || values.length === 0) return 0;

    const sorted = [...values].sort((a, b) => a - b);
    const index = Math.ceil(sorted.length * 0.50) - 1;
    return sorted[index] || 0;
  }

  static calculateAverage(values) {
    if (!values || values.length === 0) return 0;

    const sum = values.reduce((acc, val) => acc + val, 0);
    return sum / values.length;
  }

  static calculateThroughput(count, durationMs) {
    if (durationMs === 0) return 0;
    return (count / durationMs) * 1000; // operations per second
  }
}

export default ScenarioRunner;
