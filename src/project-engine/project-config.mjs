/**
 * @file Project engine configuration - central config for all capabilities
 * @module project-engine/project-config
 */

import { z } from 'zod'

/**
 * Full project engine configuration schema
 */
export const ProjectEngineConfigSchema = z.object({
  // Filesystem scanning
  fs: z.object({
    ignorePatterns: z.array(z.string()).optional(),
    baseIri: z.string().default('http://example.org/unrdf/fs#'),
    scanHiddenFiles: z.boolean().default(false),
  }).optional(),

  // Project modeling
  project: z.object({
    conventions: z.object({
      sourcePaths: z.array(z.string()).default(['src']),
      featurePaths: z.array(z.string()).default(['features', 'modules']),
      testPaths: z.array(z.string()).default(['__tests__', 'test', 'tests']),
    }).optional(),
    baseIri: z.string().default('http://example.org/unrdf/project#'),
  }).optional(),

  // Golden structure
  golden: z.object({
    profile: z.enum([
      'react-feature-v1',
      'next-app-router-v1',
      'next-pages-v1',
      'nest-api-v1',
      'express-api-v1',
    ]).optional(),
    loadFromPath: z.string().optional(),
  }).optional(),

  // Diff/comparison
  diff: z.object({
    structureLens: z.literal('project-structure').default('project-structure'),
    transactionLens: z.string().optional(),
  }).optional(),

  // Materialization
  materialize: z.object({
    templateConfig: z.record(z.string(), z.any()).optional(),
    outputRoot: z.string().default('.'),
    dryRun: z.boolean().default(false),
  }).optional(),

  // Observability
  observability: z.object({
    enableTracing: z.boolean().default(true),
    enableMetrics: z.boolean().default(true),
  }).optional(),
}).strict()

/**
 * Get project engine configuration from environment + defaults
 *
 * @param {Object} [overrides] - Config overrides
 * @returns {Object} Validated configuration
 */
export function getProjectEngineConfig(overrides = {}) {
  const defaults = {
    fs: {
      ignorePatterns: [
        'node_modules',
        '.git',
        'dist',
        'build',
        '.next',
        '.turbo',
        'coverage',
      ],
      baseIri: 'http://example.org/unrdf/fs#',
    },
    project: {
      conventions: {
        sourcePaths: ['src'],
        featurePaths: ['features', 'modules'],
        testPaths: ['__tests__', 'test', 'tests', 'spec'],
      },
      baseIri: 'http://example.org/unrdf/project#',
    },
    golden: {
      profile: 'react-feature-v1',
    },
    diff: {
      structureLens: 'project-structure',
    },
    materialize: {
      outputRoot: '.',
      dryRun: false,
    },
    observability: {
      enableTracing: true,
      enableMetrics: true,
    },
  }

  // Merge overrides
  const merged = deepMerge(defaults, overrides)

  // Validate
  return ProjectEngineConfigSchema.parse(merged)
}

/**
 * Deep merge objects for configuration
 *
 * @private
 */
function deepMerge(target, source) {
  const result = { ...target }

  for (const key in source) {
    if (Array.isArray(source[key]) && Array.isArray(result[key])) {
      // Merge arrays by combining them
      result[key] = [...result[key], ...source[key]]
    } else if (source[key] && typeof source[key] === 'object' && !Array.isArray(source[key])) {
      result[key] = deepMerge(result[key] || {}, source[key])
    } else {
      result[key] = source[key]
    }
  }

  return result
}
