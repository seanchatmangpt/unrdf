/**
 * @file Metrics Collection - Gather and report project metrics
 * @module @unrdf/project-engine/metrics
 */

import { z } from 'zod';
import { analyzePackage } from './code-analyzer.mjs';
import { listPackages } from './build-utils.mjs';

/**
 * Project metrics schema
 */
const ProjectMetricsSchema = z.object({
  timestamp: z.string(),
  totalPackages: z.number(),
  totalLinesOfCode: z.number(),
  totalExports: z.number(),
  averageTestCoverage: z.number().min(0).max(100),
  packages: z.array(
    z.object({
      name: z.string(),
      linesOfCode: z.number(),
      exportCount: z.number(),
      testCoverage: z.number(),
      complexity: z.enum(['low', 'medium', 'high']),
    })
  ),
});

/**
 * Collect metrics for all packages in monorepo
 * @param {string} [monorepoPath='.'] - Path to monorepo root
 * @returns {Promise<Object>} Comprehensive metrics data
 *
 * @throws {TypeError} If monorepoPath is not a string
 * @throws {Error} If metrics collection fails
 *
 * @example
 * const metrics = await collectMetrics('.');
 * console.log('Total packages:', metrics.totalPackages);
 * console.log('Average coverage:', metrics.averageTestCoverage);
 */
export async function collectMetrics(monorepoPath = '.') {
  if (typeof monorepoPath !== 'string') {
    throw new TypeError('collectMetrics: monorepoPath must be a string');
  }

  try {
    const packages = await listPackages(monorepoPath);

    let totalLinesOfCode = 0;
    let totalExports = 0;
    let totalCoverage = 0;
    const packageMetrics = [];

    for (const pkg of packages) {
      try {
        const analysis = await analyzePackage(pkg.path);

        totalLinesOfCode += analysis.linesOfCode;
        totalExports += analysis.exportCount;
        totalCoverage += analysis.testCoverage;

        packageMetrics.push({
          name: analysis.name,
          linesOfCode: analysis.linesOfCode,
          exportCount: analysis.exportCount,
          testCoverage: analysis.testCoverage,
          complexity: analysis.complexity,
        });
      } catch (error) {
        // Skip packages that fail analysis
        console.error(`Failed to analyze ${pkg.name}: ${error.message}`);
      }
    }

    const averageTestCoverage =
      packageMetrics.length > 0 ? Math.round(totalCoverage / packageMetrics.length) : 0;

    const metrics = {
      timestamp: new Date().toISOString(),
      totalPackages: packageMetrics.length,
      totalLinesOfCode,
      totalExports,
      averageTestCoverage,
      packages: packageMetrics,
    };

    return ProjectMetricsSchema.parse(metrics);
  } catch (error) {
    throw new Error(`collectMetrics failed: ${error.message}`);
  }
}

/**
 * Format metrics as human-readable report
 * @param {Object} metrics - Metrics data from collectMetrics
 * @returns {string} Formatted text report
 *
 * @throws {TypeError} If metrics is not an object
 *
 * @example
 * const metrics = await collectMetrics();
 * const report = reportMetrics(metrics);
 * console.log(report);
 */
export function reportMetrics(metrics) {
  if (typeof metrics !== 'object' || metrics === null) {
    throw new TypeError('reportMetrics: metrics must be an object');
  }

  const lines = [];

  lines.push('='.repeat(60));
  lines.push('PROJECT METRICS REPORT');
  lines.push('='.repeat(60));
  lines.push('');
  lines.push(`Generated: ${new Date(metrics.timestamp).toLocaleString()}`);
  lines.push('');
  lines.push('OVERVIEW');
  lines.push('-'.repeat(60));
  lines.push(`Total Packages:        ${metrics.totalPackages}`);
  lines.push(`Total Lines of Code:   ${metrics.totalLinesOfCode.toLocaleString()}`);
  lines.push(`Total Exports:         ${metrics.totalExports}`);
  lines.push(`Average Test Coverage: ${metrics.averageTestCoverage}%`);
  lines.push('');
  lines.push('PACKAGE BREAKDOWN');
  lines.push('-'.repeat(60));
  lines.push('');

  // Sort packages by lines of code descending
  const sorted = [...metrics.packages].sort((a, b) => b.linesOfCode - a.linesOfCode);

  for (const pkg of sorted) {
    lines.push(`Package: ${pkg.name}`);
    lines.push(`  Lines of Code:   ${pkg.linesOfCode.toLocaleString()}`);
    lines.push(`  Exports:         ${pkg.exportCount}`);
    lines.push(`  Test Coverage:   ${pkg.testCoverage}%`);
    lines.push(`  Complexity:      ${pkg.complexity}`);
    lines.push('');
  }

  lines.push('='.repeat(60));

  return lines.join('\n');
}
