/**
 * @fileoverview Projection Module - Documentation as Projection
 *
 * **Core Concept**: M_t = Pi_doc(mu(O_t))
 *
 * Documentation is a deterministic projection from the monorepo universe:
 * - Same inputs always produce the same outputs
 * - Every projection is verifiable via content hash
 * - Every projection emits a receipt for audit trail
 *
 * @module projection
 *
 * @example
 * import { ProjectionPipeline } from '@unrdf/projection';
 *
 * const pipeline = new ProjectionPipeline({
 *   projectName: 'MyProject',
 *   version: '1.0.0'
 * });
 *
 * const result = await pipeline.run({
 *   sources: [...],
 *   universe: universe,
 *   receipts: []
 * });
 *
 * console.log(result.outputHash); // Deterministic hash
 */

// Core projectors
export { JsDocProjector } from './jsdoc-projector.mjs';
export { ArchitectureProjector } from './architecture-projector.mjs';
export { ExampleProjector } from './example-projector.mjs';
export { GuideProjector } from './guide-projector.mjs';
export { ChangelogProjector } from './changelog-projector.mjs';
export { DiataxisRenderer } from './diataxis-renderer.mjs';

// Pipeline
export { ProjectionPipeline } from './projection-pipeline.mjs';

// Default export
export { ProjectionPipeline as default } from './projection-pipeline.mjs';

/**
 * Create a standard projection pipeline with default configuration
 *
 * @param {string} projectName - Project name
 * @param {string} [version] - Project version
 * @returns {ProjectionPipeline} Configured pipeline
 *
 * @example
 * const pipeline = createPipeline('MyProject', '1.0.0');
 * const result = await pipeline.run({ sources: [...] });
 */
export function createPipeline(projectName, version) {
  const { ProjectionPipeline } = await import('./projection-pipeline.mjs');
  return new ProjectionPipeline({
    projectName,
    version,
    audiences: ['user', 'contributor', 'operator', 'auditor'],
    projectors: {
      jsdoc: true,
      architecture: true,
      examples: true,
      guides: true,
      changelog: true,
    },
  });
}

/**
 * Quick project a single source file to API documentation
 *
 * @param {string} sourceContent - Source file content
 * @param {string} moduleName - Module name
 * @returns {Promise<{markdown: string, hash: string}>} API documentation
 *
 * @example
 * const { markdown, hash } = await projectApiDoc(source, 'my-module');
 */
export async function projectApiDoc(sourceContent, moduleName) {
  const { JsDocProjector } = await import('./jsdoc-projector.mjs');
  const projector = new JsDocProjector();
  return projector.project(sourceContent, moduleName);
}

/**
 * Quick project test file to examples documentation
 *
 * @param {string} testContent - Test file content
 * @param {string} fileName - File name
 * @returns {Promise<{markdown: string, hash: string}>} Example documentation
 *
 * @example
 * const { markdown, hash } = await projectExamples(testContent, 'test.mjs');
 */
export async function projectExamples(testContent, fileName) {
  const { ExampleProjector } = await import('./example-projector.mjs');
  const projector = new ExampleProjector();
  return projector.project(testContent, fileName);
}

/**
 * Quick project receipts to changelog
 *
 * @param {Array<Object>} receipts - Receipt chain
 * @param {Object} [metadata] - Changelog metadata
 * @returns {Promise<{markdown: string, hash: string}>} Changelog documentation
 *
 * @example
 * const { markdown, hash } = await projectChangelog(receipts, { projectName: 'My Project' });
 */
export async function projectChangelog(receipts, metadata = {}) {
  const { ChangelogProjector } = await import('./changelog-projector.mjs');
  const projector = new ChangelogProjector();
  return projector.project(receipts, metadata);
}
