/**
 * @unrdf/project-engine
 *
 * Project Engine - Self-hosting Tools and Infrastructure
 *
 * @module @unrdf/project-engine
 */

// Documentation Generation
export {
  generateApiDocs,
  generatePackageGuide,
  generateChangelog,
} from './project-engine/doc-generator.mjs';

// Code Analysis
export { analyzePackage, findExports, countCoverage } from './project-engine/code-analyzer.mjs';

// Build Utilities
export { buildPackage, verifyPackage, listPackages } from './project-engine/build-utils.mjs';

// Metrics Collection
export { collectMetrics, reportMetrics } from './project-engine/metrics.mjs';

// Infrastructure
export {
  createProjectConfig,
  setupDevEnvironment,
  createDeploymentConfig,
} from './project-engine/infrastructure.mjs';

// Project Management API
import { analyzePackage } from './project-engine/code-analyzer.mjs';
import { listPackages } from './project-engine/build-utils.mjs';

/**
 *
 */
export function createProject(options = {}) {
  return {
    name: options.name || 'unrdf-project',
    version: options.version || '1.0.0',
    packages: [],
    artifacts: [],
    metadata: {},
  };
}

/**
 *
 */
export async function loadProject(configPath = '.') {
  return createProject({ name: configPath });
}

/**
 *
 */
export async function saveProject(project, outputPath = '.') {
  return { saved: true, path: outputPath };
}

/**
 *
 */
export function describeProject(project) {
  return {
    name: project?.name || 'unknown',
    packages: project?.packages || [],
    artifacts: project?.artifacts || [],
  };
}

/**
 *
 */
export async function analyzeProject(project) {
  const analysis = analyzePackage ? await analyzePackage(project?.name) : {};
  return { name: project?.name, analysis };
}

/**
 *
 */
export function listArtifacts(project) {
  return project?.artifacts || (listPackages ? listPackages() : []);
}
