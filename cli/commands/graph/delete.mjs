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
        // Get graph stats for impact summary
        const { getStore, getDataFactory } = await import('../../utils/store-instance.mjs');
        const store = getStore();
        const df = getDataFactory();

        const graphNode = name === 'default' ? df.defaultGraph() : df.namedNode(name);
        const quads = store.match(null, null, null, graphNode);
        const quadCount = quads.length;

        const summary = `This will permanently delete the graph and all its ${quadCount} quads`;

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
            // Actual delete operation - use SPARQL UPDATE
            const deleteQuery = name === 'default'
              ? `CLEAR DEFAULT`
              : `CLEAR GRAPH <${name}>`;

            store.update(deleteQuery);
            console.log(`✅ Graph deleted: ${name} (${quadCount} quads removed)`);
          }
        );

        if (result === null) {
          process.exit(0);
        }
      } else {
        // Confirmed via --force flag
        const { getStore } = await import('../../utils/store-instance.mjs');
        const store = getStore();

        const deleteQuery = name === 'default'
          ? `CLEAR DEFAULT`
          : `CLEAR GRAPH <${name}>`;

        store.update(deleteQuery);
        console.log(`✅ Graph deleted: ${name}`);
      }
    } catch (error) {
      console.error(`❌ Failed to delete graph: ${error.message}`);
      process.exit(1);
    }
  }
});
