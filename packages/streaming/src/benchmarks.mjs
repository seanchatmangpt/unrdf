/**
 * @file Streaming Performance Benchmarks
 * @module streaming/benchmarks
 *
 * @description
 * Comprehensive performance benchmarking suite for streaming operations
 * including throughput, latency, memory efficiency, and backpressure handling.
 */

import { writeFileSync } from 'fs';
import { Readable, pipeline } from 'stream';
import { promisify } from 'util';
import { trace } from '@opentelemetry/api';
import { createRDFStreamParser } from './rdf-stream-parser.mjs';
import { createPerformanceMonitor } from './performance-monitor.mjs';
import { createChangeFeed } from './streaming/change-feed.mjs';
import { dataFactory } from '@unrdf/oxigraph';
const { namedNode, literal } = dataFactory;

const pipelineAsync = promisify(pipeline);
const tracer = trace.getTracer('@unrdf/streaming');

/**
 * Benchmark configuration
 */
const DEFAULT_BENCHMARK_CONFIG = {
  iterations: 3,
  warmupIterations: 1,
  datasetSizes: [1000, 10000, 100000],
  formats: ['turtle', 'n-triples'],
  chunkSizes: [100, 1000, 10000],
};

/**
 * Generate synthetic RDF data
 *
 * @param {number} quadCount - Number of quads to generate
 * @param {string} [format='turtle'] - RDF format
 * @returns {string} RDF data
 */
export function generateSyntheticRDF(quadCount, format = 'turtle') {
  const quads = [];

  for (let i = 0; i < quadCount; i++) {
    const subject = `<http://example.org/entity/${i}>`;
    const predicate = `<http://example.org/prop/${i % 10}>`;
    const object = `"Value ${i}"`;

    if (format === 'turtle') {
      quads.push(`${subject} ${predicate} ${object} .`);
    } else if (format === 'n-triples') {
      quads.push(`${subject} ${predicate} ${object} .`);
    }
  }

  return quads.join('\n');
}

/**
 * Create a readable stream from string
 *
 * @param {string} data - Data to stream
 * @param {Object} [options] - Stream options
 * @returns {Readable} Readable stream
 */
export function createReadableStreamFromString(data, options = {}) {
  const chunkSize = options.chunkSize || 1024;
  let offset = 0;

  return new Readable({
    read() {
      if (offset >= data.length) {
        this.push(null);
        return;
      }

      const chunk = data.slice(offset, offset + chunkSize);
      offset += chunkSize;
      this.push(chunk);
    },
  });
}

/**
 * Benchmark RDF stream parsing throughput
 *
 * @param {Object} [config] - Benchmark configuration
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkParsingThroughput(config = {}) {
  return tracer.startActiveSpan('benchmark.parsing-throughput', async (span) => {
    const results = {
      testName: 'RDF Stream Parsing Throughput',
      timestamp: Date.now(),
      datasets: [],
    };

    const datasetSizes = config.datasetSizes || DEFAULT_BENCHMARK_CONFIG.datasetSizes;
    const formats = config.formats || DEFAULT_BENCHMARK_CONFIG.formats;
    const iterations = config.iterations || DEFAULT_BENCHMARK_CONFIG.iterations;

    for (const size of datasetSizes) {
      for (const format of formats) {
        const datasetResults = {
          size,
          format,
          iterations: [],
        };

        // Generate data
        const rdfData = generateSyntheticRDF(size, format);

        for (let i = 0; i < iterations; i++) {
          const monitor = createPerformanceMonitor();
          const parser = createRDFStreamParser({ format, chunkSize: 1000 });

          const stream = createReadableStreamFromString(rdfData);
          const startTime = Date.now();

          monitor.start();

          let quadCount = 0;
          parser.on('data', (chunk) => {
            if (chunk.type === 'quads') {
              quadCount += chunk.count;
              monitor.recordQuad();
              monitor.recordBytes(chunk.count * 50); // Estimate 50 bytes per quad
            }
          });

          await pipelineAsync(stream, parser);

          monitor.stop();
          const duration = Date.now() - startTime;
          const report = monitor.getReport();

          datasetResults.iterations.push({
            iteration: i,
            quadsParsed: quadCount,
            duration,
            throughput: (quadCount / duration) * 1000, // quads/sec
            metrics: report,
          });
        }

        // Calculate averages
        const avgThroughput = datasetResults.iterations.reduce((sum, it) => sum + it.throughput, 0) / iterations;
        const avgDuration = datasetResults.iterations.reduce((sum, it) => sum + it.duration, 0) / iterations;

        datasetResults.average = {
          throughput: avgThroughput,
          duration: avgDuration,
        };

        results.datasets.push(datasetResults);
      }
    }

    span.setAttributes({
      'benchmark.datasets': results.datasets.length,
      'benchmark.iterations': iterations,
    });
    span.end();

    return results;
  });
}

/**
 * Benchmark change feed latency
 *
 * @param {Object} [config] - Benchmark configuration
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkChangeFeedLatency(config = {}) {
  return tracer.startActiveSpan('benchmark.change-feed-latency', async (span) => {
    const results = {
      testName: 'Change Feed Latency',
      timestamp: Date.now(),
      scenarios: [],
    };

    const eventCounts = config.eventCounts || [100, 1000, 10000];
    const iterations = config.iterations || DEFAULT_BENCHMARK_CONFIG.iterations;

    for (const eventCount of eventCounts) {
      const scenarioResults = {
        eventCount,
        iterations: [],
      };

      for (let i = 0; i < iterations; i++) {
        const feed = createChangeFeed();
        const latencies = [];

        feed.subscribe((change) => {
          const latency = Date.now() - change.timestamp;
          latencies.push(latency);
        });

        const startTime = Date.now();

        // Emit events
        for (let j = 0; j < eventCount; j++) {
          feed.emitChange({
            type: 'add',
            quad: {
              subject: namedNode(`http://example.org/s${j}`),
              predicate: namedNode('http://example.org/p'),
              object: literal(`value${j}`),
            },
            timestamp: Date.now(),
          });
        }

        const duration = Date.now() - startTime;

        // Calculate latency statistics
        latencies.sort((a, b) => a - b);
        const avgLatency = latencies.reduce((sum, l) => sum + l, 0) / latencies.length;

        scenarioResults.iterations.push({
          iteration: i,
          duration,
          latency: {
            mean: avgLatency,
            p50: latencies[Math.floor(latencies.length * 0.5)],
            p95: latencies[Math.floor(latencies.length * 0.95)],
            p99: latencies[Math.floor(latencies.length * 0.99)],
            max: latencies[latencies.length - 1],
          },
        });
      }

      results.scenarios.push(scenarioResults);
    }

    span.setAttributes({
      'benchmark.scenarios': results.scenarios.length,
    });
    span.end();

    return results;
  });
}

/**
 * Benchmark backpressure handling
 *
 * @param {Object} [config] - Benchmark configuration
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkBackpressure(config = {}) {
  return tracer.startActiveSpan('benchmark.backpressure', async (span) => {
    const results = {
      testName: 'Backpressure Handling',
      timestamp: Date.now(),
      tests: [],
    };

    const dataSize = config.dataSize || 100000;
    const chunkSizes = config.chunkSizes || DEFAULT_BENCHMARK_CONFIG.chunkSizes;

    for (const chunkSize of chunkSizes) {
      const rdfData = generateSyntheticRDF(dataSize, 'n-triples');
      const monitor = createPerformanceMonitor({
        sampleInterval: 100, // Sample every 100ms
      });

      const parser = createRDFStreamParser({
        format: 'n-triples',
        chunkSize,
        enableBackpressure: true,
      });

      const stream = createReadableStreamFromString(rdfData, { chunkSize: 1024 });

      monitor.start();

      let _backpressureEvents = 0;
      let processedChunks = 0;

      parser.on('data', (chunk) => {
        if (chunk.type === 'quads') {
          processedChunks++;
          monitor.recordChunk();

          // Simulate slow processing
          if (processedChunks % 10 === 0) {
            const pauseDuration = Math.random() * 10;
            const start = Date.now();
            while (Date.now() - start < pauseDuration) {
              // Busy wait
            }
          }
        }
      });

      const startTime = Date.now();
      await pipelineAsync(stream, parser);
      const duration = Date.now() - startTime;

      monitor.stop();
      const report = monitor.getReport();

      results.tests.push({
        chunkSize,
        duration,
        processedChunks,
        backpressureEvents: report.backpressure.events,
        backpressureRate: report.backpressure.rate,
        throughput: report.summary.averageThroughput,
      });
    }

    span.setAttributes({
      'benchmark.tests': results.tests.length,
    });
    span.end();

    return results;
  });
}

/**
 * Benchmark memory efficiency
 *
 * @param {Object} [config] - Benchmark configuration
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkMemoryEfficiency(config = {}) {
  return tracer.startActiveSpan('benchmark.memory-efficiency', async (span) => {
    const results = {
      testName: 'Memory Efficiency',
      timestamp: Date.now(),
      tests: [],
    };

    const datasetSizes = config.datasetSizes || [10000, 50000, 100000];

    for (const size of datasetSizes) {
      const rdfData = generateSyntheticRDF(size, 'turtle');
      const monitor = createPerformanceMonitor({
        enableMemoryTracking: true,
        sampleInterval: 500,
      });

      const parser = createRDFStreamParser({
        format: 'turtle',
        chunkSize: 1000,
      });

      const stream = createReadableStreamFromString(rdfData);

      // Force GC if available
      if (global.gc) {
        global.gc();
      }

      const initialMemory = process.memoryUsage();
      monitor.start();

      await pipelineAsync(stream, parser);

      monitor.stop();
      const finalMemory = process.memoryUsage();
      const report = monitor.getReport();

      results.tests.push({
        datasetSize: size,
        memory: {
          initial: initialMemory,
          final: finalMemory,
          delta: {
            rss: finalMemory.rss - initialMemory.rss,
            heapUsed: finalMemory.heapUsed - initialMemory.heapUsed,
            heapTotal: finalMemory.heapTotal - initialMemory.heapTotal,
          },
          stats: report.memory,
        },
        perQuadMemory: (finalMemory.heapUsed - initialMemory.heapUsed) / size,
      });
    }

    span.setAttributes({
      'benchmark.tests': results.tests.length,
    });
    span.end();

    return results;
  });
}

/**
 * Run comprehensive streaming benchmark suite
 *
 * @param {Object} [config] - Benchmark configuration
 * @returns {Promise<Object>} Complete benchmark results
 */
export async function runComprehensiveBenchmarks(config = {}) {
  return tracer.startActiveSpan('benchmark.comprehensive', async (span) => {
    console.log('🚀 Starting Comprehensive Streaming Benchmarks...\n');

    const results = {
      timestamp: Date.now(),
      config,
      benchmarks: {},
    };

    // 1. Parsing Throughput
    console.log('📊 Benchmarking parsing throughput...');
    results.benchmarks.parsingThroughput = await benchmarkParsingThroughput(config);
    console.log('✅ Parsing throughput benchmark complete\n');

    // 2. Change Feed Latency
    console.log('⏱️  Benchmarking change feed latency...');
    results.benchmarks.changeFeedLatency = await benchmarkChangeFeedLatency(config);
    console.log('✅ Change feed latency benchmark complete\n');

    // 3. Backpressure Handling
    console.log('🔄 Benchmarking backpressure handling...');
    results.benchmarks.backpressure = await benchmarkBackpressure(config);
    console.log('✅ Backpressure benchmark complete\n');

    // 4. Memory Efficiency
    console.log('💾 Benchmarking memory efficiency...');
    results.benchmarks.memoryEfficiency = await benchmarkMemoryEfficiency(config);
    console.log('✅ Memory efficiency benchmark complete\n');

    results.duration = Date.now() - results.timestamp;
    results.summary = generateBenchmarkSummary(results);

    span.setAttributes({
      'benchmark.duration': results.duration,
      'benchmark.tests': Object.keys(results.benchmarks).length,
    });
    span.end();

    return results;
  });
}

/**
 * Generate benchmark summary
 *
 * @param {Object} results - Benchmark results
 * @returns {Object} Summary
 */
function generateBenchmarkSummary(results) {
  const summary = {
    totalDuration: results.duration,
    tests: {
      parsingThroughput: {
        avgThroughput: 0,
        maxThroughput: 0,
      },
      changeFeedLatency: {
        avgLatency: 0,
        p99Latency: 0,
      },
      backpressure: {
        avgBackpressureRate: 0,
      },
      memoryEfficiency: {
        avgMemoryPerQuad: 0,
      },
    },
  };

  // Parsing throughput
  if (results.benchmarks.parsingThroughput) {
    const throughputs = results.benchmarks.parsingThroughput.datasets.flatMap(d =>
      d.iterations.map(it => it.throughput)
    );
    summary.tests.parsingThroughput.avgThroughput = throughputs.reduce((sum, t) => sum + t, 0) / throughputs.length;
    summary.tests.parsingThroughput.maxThroughput = Math.max(...throughputs);
  }

  // Change feed latency
  if (results.benchmarks.changeFeedLatency) {
    const latencies = results.benchmarks.changeFeedLatency.scenarios.flatMap(s =>
      s.iterations.map(it => it.latency.mean)
    );
    const p99s = results.benchmarks.changeFeedLatency.scenarios.flatMap(s =>
      s.iterations.map(it => it.latency.p99)
    );
    summary.tests.changeFeedLatency.avgLatency = latencies.reduce((sum, l) => sum + l, 0) / latencies.length;
    summary.tests.changeFeedLatency.p99Latency = Math.max(...p99s);
  }

  // Backpressure
  if (results.benchmarks.backpressure) {
    const rates = results.benchmarks.backpressure.tests.map(t => t.backpressureRate);
    summary.tests.backpressure.avgBackpressureRate = rates.reduce((sum, r) => sum + r, 0) / rates.length;
  }

  // Memory
  if (results.benchmarks.memoryEfficiency) {
    const memPerQuad = results.benchmarks.memoryEfficiency.tests.map(t => t.perQuadMemory);
    summary.tests.memoryEfficiency.avgMemoryPerQuad = memPerQuad.reduce((sum, m) => sum + m, 0) / memPerQuad.length;
  }

  return summary;
}

/**
 * Save benchmark results to file
 *
 * @param {Object} results - Benchmark results
 * @param {string} filename - Output filename
 */
export function saveBenchmarkResults(results, filename) {
  writeFileSync(filename, JSON.stringify(results, null, 2), 'utf8');
  console.log(`📝 Benchmark results saved to ${filename}`);
}
