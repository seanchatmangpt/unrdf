/**
 * @file Project structure diff - convenience wrapper over the core ontology diff
 * @module project-engine/project-diff
 */

import { z } from 'zod';
import { diffGraphFromStores, diffOntologyFromGraphDiff } from '@unrdf/core';
import { ProjectStructureLens } from './lens/project-structure.mjs';

const ProjectDiffOptionsSchema = z.object({
  actualStore: z.object({}).passthrough(),
  goldenStore: z.object({}).passthrough(),
});

/**
 * Compute project structure diff (golden -> actual).
 *
 * @param {Object} options
 * @param {Object} options.actualStore - Current project graph
 * @param {Object} options.goldenStore - Expected golden structure
 * @returns {Object} OntologyDiff
 */
export function diffProjectStructure(options) {
  // Validate only: parse() would return plain copies that lose the stores' prototype methods
  ProjectDiffOptionsSchema.parse(options);
  const { actualStore, goldenStore } = options;
  const graphDiff = diffGraphFromStores(goldenStore, actualStore);
  return diffOntologyFromGraphDiff(graphDiff, ProjectStructureLens);
}
