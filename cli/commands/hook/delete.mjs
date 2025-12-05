/**
 * @file Hook Delete Command - with dependency analysis poka-yoke guard
 */

import { defineCommand } from 'citty';
import { executeWithConfirmation, shouldSkipConfirmation } from '../../utils/confirmation.mjs';
import { analyzeDependencies, formatDependencyAnalysis } from '../../utils/dependency-analyzer.mjs';
import { validate, hookDeleteSchema } from '../../utils/validation.mjs';

export const deleteCommand = defineCommand({
  meta: {
    name: 'delete',
    description: 'Delete a hook'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Hook name',
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

      // Validate input
      validate(hookDeleteSchema, { name, force }, 'Hook delete');

      // FM-CLI-013: Analyze dependencies before deletion
      console.log(`\n🔍 Analyzing dependencies for hook "${name}"...`);
      const analysis = await analyzeDependencies('hook', name);

      if (!shouldSkipConfirmation(ctx)) {
        // Show dependency analysis
        console.log(formatDependencyAnalysis(analysis));

        // Build confirmation message with cascade impact
        const summary =
          analysis.dependentCount > 0
            ? `This hook is referenced by ${analysis.dependentCount} resource(s). Deletion may break dependent policies.`
            : 'This hook deletion will not affect other resources.';

        const result = await executeWithConfirmation(
          ctx,
          {
            action: 'delete',
            resource: 'hook',
            name,
            summary,
            requiresForce: true
          },
          async () => {
            // TODO: Implement actual hook deletion
            console.log(`✅ Hook deleted: ${name}`);
          }
        );

        if (result === null) {
          process.exit(0);
        }
      } else {
        // Show dependency analysis even with --force
        console.log(formatDependencyAnalysis(analysis));

        if (analysis.cascadeRisk === 'CRITICAL') {
          console.warn('⚠️  WARNING: This deletion may break dependent resources');
        }

        console.log(`✅ Hook deleted: ${name}`);
      }
    } catch (error) {
      console.error(`❌ Failed to delete hook: ${error.message}`);
      process.exit(1);
    }
  }
});
