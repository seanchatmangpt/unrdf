/**
 * @file Logs Command
 * @module cli/commands/daemon/logs
 */

import { defineCommand } from 'citty';
import { z } from 'zod';
import { LogsArgsSchema } from './schemas.mjs';
import { eventLog, formatDuration } from './helpers.mjs';

export const logsCommand = defineCommand({
  meta: {
    name: 'logs',
    description: 'View operation logs with filtering',
  },
  args: {
    follow: {
      type: 'boolean',
      description: 'Follow log output (stream mode)',
    },
    filter: {
      type: 'string',
      description: 'Filter logs by pattern',
    },
    'max-lines': {
      type: 'number',
      description: 'Maximum lines to display',
    },
    json: {
      type: 'boolean',
      description: 'Output as JSON',
    },
  },
  async run({ args }) {
    try {
      const validated = LogsArgsSchema.parse(args);

      let filteredLogs = [...eventLog];

      if (validated.filter) {
        const filterRegex = new RegExp(validated.filter, 'i');
        filteredLogs = filteredLogs.filter(
          log => filterRegex.test(log.type) || filterRegex.test(log.operationId || '')
        );
      }

      filteredLogs = filteredLogs.slice(-validated['max-lines']);

      if (validated.json) {
        console.log(JSON.stringify({ logs: filteredLogs }, null, 2));
      } else {
        console.log('\n📝 Operation Logs');
        console.log('═'.repeat(90));
        console.log(
          `${'Timestamp'.padEnd(25)} ${'Type'.padEnd(20)} ${'Operation ID'.padEnd(20)} ${'Duration'.padEnd(15)}`
        );
        console.log('─'.repeat(90));

        filteredLogs.forEach(log => {
          const timestamp = log.timestamp.toISOString().padEnd(25);
          const type = (log.type || '-').padEnd(20);
          const opId = (log.operationId || '-').padEnd(20);
          const duration = log.duration ? formatDuration(log.duration).padEnd(15) : '-'.padEnd(15);
          console.log(`${timestamp}${type}${opId}${duration}`);
        });

        console.log('─'.repeat(90));
        console.log(`Total Entries: ${filteredLogs.length}`);
        console.log('═'.repeat(90));

        if (validated.follow) {
          console.log('\n📡 Following logs (Ctrl+C to stop)...');
        }
      }
    } catch (error) {
      if (error instanceof z.ZodError) {
        console.error(`❌ Invalid arguments: ${error.errors[0].message}`);
      } else {
        console.error(`❌ Error reading logs: ${error.message}`);
      }
      process.exit(1);
    }
  },
});
