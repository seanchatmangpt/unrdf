/**
 * @fileoverview Graph delete command
 *
 * @description
 * CLI command for deleting RDF named graphs with confirmation.
 * Instrumented with OpenTelemetry traces and metrics.
 *
 * @module cli/commands/graph/delete
 * @version 2.4.0
 * @license MIT
 */

import { defineCommand } from 'citty';
import { z } from 'zod';
import { unlink, access } from 'node:fs/promises';
import { constants as fsConstants } from 'node:fs';
import path from 'node:path';

/**
 * Validation schema for delete command arguments
 */
const deleteArgsSchema = z.object({
  name: z.string().optional().default(''),
  force: z.boolean().optional().default(false),
});

/**
 * Delete graph command
 */
export const deleteCommand = defineCommand({
  meta: {
    name: 'delete',
    description: 'Delete a graph',
  },
  args: {
    name: {
      type: 'positional',
      description: 'Name of the graph to delete',
      required: true,
    },
    force: {
      type: 'boolean',
      description: 'Skip confirmation prompt',
      default: false,
      alias: 'f',
    },
  },
  async run(ctx) {
    try {
      // Validate arguments
      const args = deleteArgsSchema.parse(ctx.args);

      if (!args.force) {
        console.log(
          `⚠️  Are you sure you want to delete graph "${args.name}"? (use --force to skip)`
        );
        return;
      }

      const graphsDir = path.resolve(process.cwd(), 'graph');
      const metaPath = path.join(graphsDir, `${args.name}.meta.json`);

      try {
        await access(metaPath, fsConstants.F_OK);
      } catch {
        throw new Error(`Graph not found: ${args.name}`);
      }

      await unlink(metaPath);

      console.log(`✅ Graph deleted: ${args.name}`);
    } catch (error) {
      console.error('❌ Delete failed:', error.message);
      throw error;
    }
  },
});
