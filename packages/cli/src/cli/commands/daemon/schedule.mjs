/**
 * @file Schedule Operation Command
 * @module cli/commands/daemon/schedule
 */

import { defineCommand } from 'citty';
import { z } from 'zod';
import { ScheduleArgsSchema } from './schemas.mjs';
import { initializeRegistry, operationRegistry, eventLog } from './helpers.mjs';

export const scheduleCommand = defineCommand({
  meta: {
    name: 'schedule',
    description: 'Add scheduled trigger to operation',
  },
  args: {
    operation: {
      type: 'string',
      description: 'Operation ID to schedule',
      required: true,
    },
    trigger: {
      type: 'string',
      description: 'Trigger type (cron, interval, reactive, event)',
      required: true,
    },
    payload: {
      type: 'string',
      description: 'Trigger payload (JSON string)',
    },
    json: {
      type: 'boolean',
      description: 'Output as JSON',
    },
  },
  async run({ args }) {
    try {
      const validated = ScheduleArgsSchema.parse(args);
      initializeRegistry();

      const operation = operationRegistry.get(validated.operation);
      if (!operation) {
        throw new Error(`Operation not found: ${validated.operation}`);
      }

      let triggerDescription = '';
      switch (validated.trigger) {
        case 'cron':
          triggerDescription = '0 2 * * *';
          break;
        case 'interval':
          triggerDescription = '3600000';
          break;
        case 'reactive':
          triggerDescription = 'create,update';
          break;
        case 'event':
          triggerDescription = 'data-changed';
          break;
        default:
          throw new Error(`Unknown trigger type: ${validated.trigger}`);
      }

      const result = {
        operationId: validated.operation,
        triggerId: `trigger-${Date.now()}`,
        triggerType: validated.trigger,
        triggerDescription,
        status: 'scheduled',
        enabled: true,
      };

      eventLog.push({
        type: 'trigger:scheduled',
        operationId: validated.operation,
        triggerId: result.triggerId,
        triggerType: validated.trigger,
        timestamp: new Date(),
      });

      if (validated.json) {
        console.log(JSON.stringify(result, null, 2));
      } else {
        console.log('\n📅 Trigger scheduled successfully');
        console.log('═'.repeat(50));
        console.log(`Operation ID: ${result.operationId}`);
        console.log(`Trigger ID: ${result.triggerId}`);
        console.log(`Trigger Type: ${result.triggerType}`);
        console.log(`Description: ${triggerDescription}`);
        console.log(`Status: ${result.status}`);
        console.log(`Enabled: ${result.enabled}`);
        console.log('═'.repeat(50));
      }
    } catch (error) {
      if (error instanceof z.ZodError) {
        console.error(`❌ Invalid arguments: ${error.errors[0].message}`);
      } else {
        console.error(`❌ Error scheduling trigger: ${error.message}`);
      }
      process.exit(1);
    }
  },
});
