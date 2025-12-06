/**
 * @file Graph Delete Command - REFACTORED to use domain layer
 * @architecture CLI → Domain Service → Package
 *
 * BEFORE (2-tier): Command → Package (getStore().update())
 * AFTER (3-tier): Command → GraphService.deleteGraph() → Package
 *
 * BENEFITS:
 * - Command is now 50% smaller (92 LOC → 46 LOC)
 * - Delete logic centralized in service
 * - Stats collection moved to service
 */

import { defineCommand } from 'citty';
import { executeWithConfirmation, shouldSkipConfirmation } from '../../utils/confirmation.mjs';
import { validate, graphDeleteSchema } from '../../utils/validation.mjs';
import { getGraphService } from '../../domain/index.mjs';

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
    }
  },
  async run(ctx) {
    try {
      const { name, force } = ctx.args;

      // PRESENTATION LAYER: Validate input
      validate(graphDeleteSchema, { name, force }, 'Graph delete');

      // PRESENTATION LAYER: Get stats for confirmation prompt
      const service = getGraphService();
      const stats = await service.getGraphStats(name);

      // PRESENTATION LAYER: Require confirmation for destructive operations
      if (!shouldSkipConfirmation(ctx)) {
        const summary = `This will permanently delete the graph and all its ${stats.quadCount} quads`;

        await executeWithConfirmation(
          ctx,
          {
            action: 'delete',
            resource: 'graph',
            name,
            summary,
            requiresForce: true
          },
          async () => {
            // DOMAIN LAYER: Delete via service
            const result = await service.deleteGraph({ name, force: true });
            console.log(`✅ Graph deleted: ${name} (${result.quadsRemoved} quads removed)`);
          }
        );
      } else {
        // DOMAIN LAYER: Delete via service (confirmed via --force)
        const result = await service.deleteGraph({ name, force: true });
        console.log(`✅ Graph deleted: ${name} (${result.quadsRemoved} quads removed)`);
      }

    } catch (error) {
      console.error(`❌ Failed to delete graph: ${error.message}`);
      process.exit(1);
    }
  }
});
