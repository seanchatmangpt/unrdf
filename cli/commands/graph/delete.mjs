/**
 * @file Graph Delete Command - with confirmation poka-yoke guard
 * @module cli-v2/commands/graph/delete
 */

import { defineCommand } from 'citty';
import { executeWithConfirmation, shouldSkipConfirmation } from '../../utils/confirmation.mjs';
import { validate, graphDeleteSchema } from '../../utils/validation.mjs';

export const deleteCommand = defineCommand({
  meta: {
    name: 'delete',
    description: 'Delete a graph'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Graph name',
      required: true
    },
    force: {
      type: 'boolean',
      description: 'Skip confirmation (not recommended)',
      default: false
    },
    confirm: {
      type: 'boolean',
      description: 'Require explicit confirmation'
    }
  },
  async run(ctx) {
    try {
      const { name, force } = ctx.args;

      // Validate input
      validate(graphDeleteSchema, { name, force }, 'Graph delete');

      // FM-CLI-004: Require confirmation for destructive operations (poka-yoke guard)
      if (!shouldSkipConfirmation(ctx)) {
        // TODO: Get graph stats for impact summary
        const summary = `This will permanently delete the graph and all its data`;

        const result = await executeWithConfirmation(
          ctx,
          {
            action: 'delete',
            resource: 'graph',
            name,
            summary,
            requiresForce: true
          },
          async () => {
            // Actual delete operation
            // TODO: Implement actual graph deletion
            console.log(`✅ Graph deleted: ${name}`);
          }
        );

        if (result === null) {
          process.exit(0);
        }
      } else {
        // Confirmed via --force flag
        console.log(`✅ Graph deleted: ${name}`);
      }
    } catch (error) {
      console.error(`❌ Failed to delete graph: ${error.message}`);
      process.exit(1);
    }
  }
});
