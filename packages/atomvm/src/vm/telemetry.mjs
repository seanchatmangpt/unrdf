/**
 * Deterministic Telemetry Exporter
 */
import { metrics } from '@opentelemetry/api';

export class VmTelemetry {
  constructor() {
    const meter = metrics.getMeter('atomvm-runtime');
    
    this.executionDuration = meter.createHistogram('vm.execution.duration', {
      description: 'Time spent executing a microtask block',
      unit: 'ms'
    });

    this.opcodeThroughput = meter.createCounter('vm.opcode.throughput', {
      description: 'Number of opcodes processed per second',
      unit: '1'
    });
  }

  recordExecutionTime(startTime) {
    const duration = performance.now() - startTime;
    this.executionDuration.record(duration);
  }

  recordOpcodesProcessed(count) {
    this.opcodeThroughput.add(count);
  }
}
