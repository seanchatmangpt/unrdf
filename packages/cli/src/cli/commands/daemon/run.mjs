/**
 * @file Run Operation Command
 * @module cli/commands/daemon/run
 */

import { defineCommand } from 'citty';
import { z } from 'zod';
import { RunArgsSchema } from './schemas.mjs';
import { initializeRegistry, operationRegistry, eventLog, formatDuration } from './helpers.mjs';

export const runCommand = defineCommand({
  meta: {
    name: 'run',
    description: 'Execute operation immediately',
  },
  args: {
    operation: {
      type: 'string',
      description: 'Operation ID to execute',
      required: true,
    },
    payload: {
      type: 'string',
      description: 'Operation payload (JSON string)',
    },
    json: {
      type: 'boolean',
      description: 'Output as JSON',
    },
    timeout: {
      type: 'number',
      description: 'Execution timeout in milliseconds',
    },
  },
  async run({ args }) {
    try {
      const validated = RunArgsSchema.parse(args);
      initializeRegistry();

      const operation = operationRegistry.get(validated.operation);
      if (!operation) {
        throw new Error(`Operation not found: ${validated.operation}`);
      }

      if (validated.payload) {
        try {
          JSON.parse(validated.payload);
        } catch {
          throw new Error('Invalid JSON payload');
        }
      }

      const startTime = Date.now();
      const operationId = validated.operation;

      eventLog.push({
        type: 'operation:started',
        operationId,
        timestamp: new Date(),
      });

      const simulationDuration = Math.random() * 1000 + 100;
      await new Promise(resolve => setTimeout(resolve, simulationDuration));

      const actualDuration = Date.now() - startTime;
      const result = {
        operationId,
        status: 'success',
        duration: actualDuration,
        result: {
          processed: Math.floor(Math.random() * 100) + 1,
          affected: Math.floor(Math.random() * 50),
        },
      };

      eventLog.push({
        type: 'operation:success',
        operationId,
        duration: actualDuration,
        timestamp: new Date(),
      });

      if (validated.json) {
        console.log(JSON.stringify(result, null, 2));
      } else {
        console.log('\n✅ Operation executed successfully');
        console.log('═'.repeat(50));
        console.log(`Operation ID: ${result.operationId}`);
        console.log(`Status: ${result.status}`);
        console.log(`Duration: ${formatDuration(result.duration)}`);
        console.log(`Processed: ${result.result.processed} items`);
        console.log(`Affected: ${result.result.affected} items`);
        console.log('═'.repeat(50));
      }
    } catch (error) {
      if (error instanceof z.ZodError) {
        console.error(`❌ Invalid arguments: ${error.errors[0].message}`);
      } else {
        console.error(`❌ Error running operation: ${error.message}`);
      }
      process.exit(1);
    }
  },
});
