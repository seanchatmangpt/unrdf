/**
 * @file Project structure diff - convenience wrapper over diff.mjs
 * @module project-engine/project-diff
 */

import { diffOntologyFromStores } from '../diff.mjs';
import { ProjectStructureLens } from './lens/project-structure.mjs';
import { z } from 'zod';

const ProjectDiffOptionsSchema = z.object({
  actualStore: z.object({}).passthrough(),
  goldenStore: z.object({}).passthrough(),
});

/**
 * Compute project structure diff using low-level diff.mjs
 *
 * @param {Object} options
 * @param {Store} options.actualStore - Current project graph
 * @param {Store} options.goldenStore - Expected golden structure
 * @returns {OntologyDiff}
 */
export function diffProjectStructure(options) {
  const validated = ProjectDiffOptionsSchema.parse(options);
  const { actualStore, goldenStore } = validated;

  return diffOntologyFromStores(goldenStore, actualStore, ProjectStructureLens);
}
