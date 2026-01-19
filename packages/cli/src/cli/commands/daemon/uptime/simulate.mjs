/**
 * @file Simulate Command - Run Uptime Simulations
 * @module cli/commands/daemon/uptime/simulate
 * @description CLI command for running uptime simulations with chaos injection
 *
 * @example
 * # Run a 30-second simulation with medium chaos
 * unrdf daemon uptime simulate --duration 30000 --chaos-level medium
 *
 * # Run simulation with 5% failure rate and JSON output
 * unrdf daemon uptime simulate --failure-rate 0.05 --json
 *
 * # Run reproducible simulation with seed
 * unrdf daemon uptime simulate --seed 12345 --verbose
 */

import { defineCommand } from 'citty';
import { z } from 'zod';
import { SimulateArgsSchema } from './schemas.mjs';
import {
  generateId,
  createSeededRandom,
  formatDuration,
  formatPercentage,
  simulateHeartbeat,
  calculateMTBF,
  calculateMTTR,
  writeOutput,
  printHeader,
  printSeparator,
  CHAOS_PARAMS,
} from './helpers.mjs';

/**
 * Run the uptime simulation
 * @param {Object} options - Simulation options
 * @returns {Promise<Object>} Simulation result
 */
async function runSimulation(options) {
  const {
    duration,
    chaosLevel,
    failureRate,
    seed,
    verbose,
    onEvent,
  } = options;

  const simulationId = generateId('sim');
  const random = seed !== undefined ? createSeededRandom(seed) : Math.random;
  const startTime = new Date();
  const events = [];
  const heartbeatInterval = 100; // 100ms between heartbeats

  let totalHeartbeats = 0;
  let successfulHeartbeats = 0;
  let failedHeartbeats = 0;
  let currentlyDown = false;
  let downSince = null;
  let totalDowntime = 0;
  let recoveryCount = 0;

  // Start event
  events.push({
    timestamp: startTime.toISOString(),
    type: 'start',
    details: { simulationId, duration, chaosLevel, failureRate },
  });

  if (verbose && onEvent) {
    onEvent('start', { simulationId, duration, chaosLevel });
  }

  const iterations = Math.floor(duration / heartbeatInterval);

  for (let i = 0; i < iterations; i++) {
    const heartbeat = simulateHeartbeat({
      random: typeof random === 'function' ? random : Math.random,
      baseFailureRate: failureRate,
      chaosLevel,
    });

    totalHeartbeats++;

    if (heartbeat.success) {
      successfulHeartbeats++;

      if (currentlyDown) {
        // Recovery
        const downDuration = Date.now() - downSince;
        totalDowntime += downDuration;
        recoveryCount++;
        currentlyDown = false;

        events.push({
          timestamp: new Date().toISOString(),
          type: 'recovery',
          details: { downDuration, recoveryCount },
        });

        if (verbose && onEvent) {
          onEvent('recovery', { downDuration });
        }
      }
    } else {
      failedHeartbeats++;

      if (!currentlyDown) {
        // Failure
        currentlyDown = true;
        downSince = Date.now();

        events.push({
          timestamp: new Date().toISOString(),
          type: 'failure',
          details: { heartbeatNumber: totalHeartbeats },
        });

        if (verbose && onEvent) {
          onEvent('failure', { heartbeatNumber: totalHeartbeats });
        }
      }

      // Apply chaos recovery delay
      const chaos = CHAOS_PARAMS[chaosLevel];
      if (chaos.recoveryDelay > 0) {
        await new Promise(resolve => setTimeout(resolve, chaos.recoveryDelay / 10));
      }
    }

    // Periodic heartbeat event (every 10%)
    if (i > 0 && i % Math.floor(iterations / 10) === 0) {
      events.push({
        timestamp: new Date().toISOString(),
        type: 'heartbeat',
        details: {
          progress: Math.round((i / iterations) * 100),
          successful: successfulHeartbeats,
          failed: failedHeartbeats,
        },
      });
    }

    // Small delay to simulate real timing
    await new Promise(resolve => setTimeout(resolve, 1));
  }

  // Handle case where simulation ends while down
  if (currentlyDown) {
    totalDowntime += Date.now() - downSince;
  }

  const endTime = new Date();
  const actualDuration = endTime - startTime;
  const uptime = ((successfulHeartbeats / totalHeartbeats) * 100);
  const mtbf = calculateMTBF(actualDuration - totalDowntime, failedHeartbeats);
  const mttr = calculateMTTR(totalDowntime, recoveryCount);

  events.push({
    timestamp: endTime.toISOString(),
    type: 'end',
    details: { actualDuration, uptime },
  });

  return {
    simulationId,
    startTime: startTime.toISOString(),
    endTime: endTime.toISOString(),
    duration: actualDuration,
    chaosLevel,
    failureRate,
    events,
    metrics: {
      totalHeartbeats,
      successfulHeartbeats,
      failedHeartbeats,
      uptime,
      mtbf,
      mttr,
    },
  };
}

/**
 * Format simulation result for text output
 * @param {Object} result - Simulation result
 * @returns {string} Formatted text
 */
function formatTextOutput(result) {
  const lines = [];

  lines.push('\nUptime Simulation Results');
  lines.push('='.repeat(60));
  lines.push(`Simulation ID: ${result.simulationId}`);
  lines.push(`Duration: ${formatDuration(result.duration)}`);
  lines.push(`Chaos Level: ${result.chaosLevel}`);
  lines.push(`Base Failure Rate: ${(result.failureRate * 100).toFixed(2)}%`);
  lines.push('');
  lines.push('Metrics');
  lines.push('-'.repeat(60));
  lines.push(`Total Heartbeats: ${result.metrics.totalHeartbeats}`);
  lines.push(`Successful: ${result.metrics.successfulHeartbeats}`);
  lines.push(`Failed: ${result.metrics.failedHeartbeats}`);
  lines.push(`Uptime: ${formatPercentage(result.metrics.uptime)}%`);
  lines.push(`MTBF: ${formatDuration(result.metrics.mtbf)}`);
  lines.push(`MTTR: ${formatDuration(result.metrics.mttr)}`);
  lines.push('='.repeat(60));

  // Event summary
  const eventCounts = result.events.reduce((acc, evt) => {
    acc[evt.type] = (acc[evt.type] || 0) + 1;
    return acc;
  }, {});

  lines.push('\nEvent Summary');
  lines.push('-'.repeat(60));
  Object.entries(eventCounts).forEach(([type, count]) => {
    lines.push(`  ${type}: ${count}`);
  });

  return lines.join('\n');
}

export const simulateCommand = defineCommand({
  meta: {
    name: 'simulate',
    description: 'Run uptime simulation with chaos injection',
  },
  args: {
    duration: {
      type: 'string',
      description: 'Simulation duration in milliseconds (1000-3600000)',
      default: '60000',
    },
    'chaos-level': {
      type: 'string',
      description: 'Chaos level: none, low, medium, high, extreme',
      default: 'none',
    },
    'failure-rate': {
      type: 'string',
      description: 'Base failure rate (0-1)',
      default: '0.01',
    },
    output: {
      type: 'string',
      description: 'Output file path (default: stdout)',
    },
    json: {
      type: 'boolean',
      description: 'Output as JSON',
      default: false,
    },
    seed: {
      type: 'string',
      description: 'Random seed for reproducible simulations',
    },
    verbose: {
      type: 'boolean',
      description: 'Show verbose output during simulation',
      default: false,
    },
  },
  async run({ args }) {
    try {
      // Parse numeric args from strings
      const parsedArgs = {
        duration: parseInt(args.duration, 10),
        'chaos-level': args['chaos-level'],
        'failure-rate': parseFloat(args['failure-rate']),
        output: args.output,
        json: args.json,
        seed: args.seed ? parseInt(args.seed, 10) : undefined,
        verbose: args.verbose,
      };

      const validated = SimulateArgsSchema.parse(parsedArgs);

      if (validated.verbose && !validated.json) {
        printHeader('Starting Uptime Simulation');
        console.log(`Duration: ${formatDuration(validated.duration)}`);
        console.log(`Chaos Level: ${validated['chaos-level']}`);
        console.log(`Failure Rate: ${(validated['failure-rate'] * 100).toFixed(2)}%`);
        if (validated.seed !== undefined) {
          console.log(`Seed: ${validated.seed}`);
        }
        printSeparator('-');
        console.log('Simulation in progress...\n');
      }

      const result = await runSimulation({
        duration: validated.duration,
        chaosLevel: validated['chaos-level'],
        failureRate: validated['failure-rate'],
        seed: validated.seed,
        verbose: validated.verbose,
        onEvent: validated.verbose && !validated.json ? (type, details) => {
          const timestamp = new Date().toISOString().slice(11, 23);
          console.log(`[${timestamp}] ${type.toUpperCase()}: ${JSON.stringify(details)}`);
        } : null,
      });

      if (validated.json) {
        const output = JSON.stringify(result, null, 2);
        await writeOutput(output, validated.output);
      } else {
        const output = formatTextOutput(result);
        if (validated.output) {
          await writeOutput(output, validated.output);
        } else {
          console.log(output);
        }
      }
    } catch (error) {
      if (error instanceof z.ZodError) {
        console.error(`Validation error: ${error.errors.map(e => e.message).join(', ')}`);
        console.error('\nUsage examples:');
        console.error('  unrdf daemon uptime simulate --duration 30000');
        console.error('  unrdf daemon uptime simulate --chaos-level medium --failure-rate 0.05');
        console.error('  unrdf daemon uptime simulate --seed 12345 --json --output result.json');
      } else {
        console.error(`Error: ${error.message}`);
      }
      process.exit(1);
    }
  },
});
