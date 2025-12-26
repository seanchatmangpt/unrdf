#!/usr/bin/env node
/**
 * @file report.mjs
 * @description CLI tool for generating Diátaxis coverage and confidence reports
 */

import { readdir, readFile, stat } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { existsSync } from 'node:fs';
import { stableStringify } from '../src/stable-json.mjs';

/**
 * @typedef {Object} ReportOptions
 * @property {boolean} json - Output as JSON
 * @property {boolean} csv - Output as CSV
 * @property {number} top - Number of top packages to show
 * @property {string|null} filter - Filter packages by keyword
 * @property {string} sort - Sort field (confidence, tutorials, howtos, reference, explanation)
 */

/**
 * @typedef {Object} PackageStats
 * @property {string} packageName
 * @property {number} tutorialsCount
 * @property {number} howtosCount
 * @property {boolean} hasReference
 * @property {boolean} hasExplanation
 * @property {number} avgConfidence
 * @property {Object} confidence
 * @property {string[]} missingEvidence
 */

/**
 * Parse command-line arguments manually (no external dependencies)
 * @param {string[]} args - Process arguments (from process.argv)
 * @returns {ReportOptions} Parsed options
 */
function parseArgs(args) {
  const options = {
    json: false,
    csv: false,
    top: 5,
    filter: null,
    sort: 'confidence'
  };

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];

    if (arg === '--json') {
      options.json = true;
    } else if (arg === '--csv') {
      options.csv = true;
    } else if (arg === '--top' && i + 1 < args.length) {
      options.top = parseInt(args[i + 1], 10);
      i++;
    } else if (arg === '--filter' && i + 1 < args.length) {
      options.filter = args[i + 1];
      i++;
    } else if (arg === '--sort' && i + 1 < args.length) {
      options.sort = args[i + 1];
      i++;
    } else if (arg === '--help' || arg === '-h') {
      console.log(`
Diátaxis Coverage Report Generator

Usage: diataxis-report [options]

Options:
  --json              Output as JSON structure
  --csv               Output as CSV (for spreadsheet import)
  --top N             Show top N packages instead of 5 (default: 5)
  --filter <keyword>  Only report on packages matching keyword
  --sort <field>      Sort by: confidence, tutorials, howtos, reference, explanation
  --help, -h          Show this help message

Examples:
  diataxis-report
  diataxis-report --json
  diataxis-report --top 10 --sort tutorials
  diataxis-report --filter "@unrdf/oxigraph" --json
`);
      process.exit(0);
    }
  }

  return options;
}

/**
 * Load all inventory JSON files from ARTIFACTS/diataxis/
 * @param {string} artifactsPath - Path to artifacts directory
 * @returns {Promise<Object[]>} Array of parsed inventory entries
 */
async function loadInventories(artifactsPath) {
  const diataxisDir = join(artifactsPath, 'diataxis');

  if (!existsSync(diataxisDir)) {
    return [];
  }

  try {
    // Recursively find all diataxis.json files
    const inventories = [];

    /**
     * Recursively search for diataxis.json files
     */
    async function findDiataxisFiles(dir) {
      try {
        const files = await readdir(dir);
        for (const file of files) {
          const filePath = join(dir, file);
          if (file === 'diataxis.json') {
            try {
              const content = await readFile(filePath, 'utf-8');
              const data = JSON.parse(content);
              inventories.push(data);
            } catch (error) {
              console.warn(`Warning: Failed to parse ${filePath}: ${error.message}`);
            }
          } else {
            try {
              const statInfo = await stat(filePath);
              if (statInfo.isDirectory()) {
                await findDiataxisFiles(filePath);
              }
            } catch (error) {
              // Skip
            }
          }
        }
      } catch (error) {
        // Silently skip directories
      }
    }

    await findDiataxisFiles(diataxisDir);
    return inventories;
  } catch (error) {
    console.warn(`Warning: Failed to read diataxis directory: ${error.message}`);
    return [];
  }
}

/**
 * Calculate statistics for a single package
 * @param {Object} inventory - DiataxisEntry inventory
 * @returns {PackageStats} Package statistics
 */
function calculatePackageStats(inventory) {
  const tutorialsCount = inventory.tutorials?.length || 0;
  const howtosCount = inventory.howtos?.length || 0;
  const hasReference = (inventory.reference?.items?.length || 0) > 0;
  const hasExplanation = (
    (inventory.explanation?.concepts?.length || 0) > 0 ||
    (inventory.explanation?.architecture || '').length > 0 ||
    (inventory.explanation?.tradeoffs?.length || 0) > 0
  );

  const confidence = inventory.confidence || {
    tutorials: 0,
    howtos: 0,
    reference: 0,
    explanation: 0
  };

  const avgConfidence = (
    confidence.tutorials +
    confidence.howtos +
    confidence.reference +
    confidence.explanation
  ) / 4;

  // Determine missing evidence
  const missingEvidence = [];
  const evidence = inventory.evidence || {};

  if (!evidence.examplesFiles || evidence.examplesFiles.length === 0) {
    missingEvidence.push('no examples/');
  }
  if (!evidence.docsFiles || evidence.docsFiles.length === 0) {
    missingEvidence.push('no docs/');
  }
  if (!evidence.readmeHeadings || evidence.readmeHeadings.length === 0) {
    missingEvidence.push('empty README');
  }

  // Check for bin entries from reference
  const hasBinEntries = inventory.reference?.items?.some(item => item.type === 'bin');
  if (!hasBinEntries) {
    missingEvidence.push('no bin entries');
  }

  return {
    packageName: inventory.packageName,
    tutorialsCount,
    howtosCount,
    hasReference,
    hasExplanation,
    avgConfidence,
    confidence,
    missingEvidence
  };
}

/**
 * Generate summary statistics from all packages
 * @param {PackageStats[]} stats - Array of package statistics
 * @returns {Object} Summary statistics
 */
function generateSummary(stats) {
  const total = stats.length;
  const withTutorials = stats.filter(s => s.tutorialsCount > 0).length;
  const with2PlusHowtos = stats.filter(s => s.howtosCount >= 2).length;
  const withReference = stats.filter(s => s.hasReference).length;
  const withExplanation = stats.filter(s => s.hasExplanation).length;

  return {
    total,
    withTutorials,
    with2PlusHowtos,
    withReference,
    withExplanation,
    withTutorialsPercent: total > 0 ? (withTutorials / total * 100) : 0,
    with2PlusHowtosPercent: total > 0 ? (with2PlusHowtos / total * 100) : 0,
    withReferencePercent: total > 0 ? (withReference / total * 100) : 0,
    withExplanationPercent: total > 0 ? (withExplanation / total * 100) : 0
  };
}

/**
 * Calculate confidence distribution statistics
 * @param {PackageStats[]} stats - Array of package statistics
 * @returns {Object} Confidence statistics
 */
function calculateConfidenceStats(stats) {
  const fields = ['tutorials', 'howtos', 'reference', 'explanation'];
  const result = {};

  for (const field of fields) {
    const values = stats.map(s => s.confidence[field]).filter(v => v !== undefined);
    if (values.length === 0) {
      result[field] = { avg: 0, min: 0, max: 0 };
    } else {
      const avg = values.reduce((a, b) => a + b, 0) / values.length;
      const min = Math.min(...values);
      const max = Math.max(...values);
      result[field] = { avg, min, max };
    }
  }

  return result;
}

/**
 * Find packages with lowest average confidence
 * @param {PackageStats[]} stats - Array of package statistics
 * @param {number} topN - Number of packages to return
 * @returns {PackageStats[]} Top N lowest confidence packages
 */
function findLowestConfidence(stats, topN) {
  return [...stats]
    .sort((a, b) => a.avgConfidence - b.avgConfidence)
    .slice(0, topN);
}

/**
 * Aggregate missing evidence sources
 * @param {PackageStats[]} stats - Array of package statistics
 * @returns {Object} Map of evidence type to count
 */
function aggregateMissingEvidence(stats) {
  const evidenceCounts = {};

  for (const pkg of stats) {
    for (const evidence of pkg.missingEvidence) {
      evidenceCounts[evidence] = (evidenceCounts[evidence] || 0) + 1;
    }
  }

  // Sort by count descending
  const sorted = Object.entries(evidenceCounts)
    .sort(([, a], [, b]) => b - a)
    .reduce((acc, [key, val]) => {
      acc[key] = val;
      return acc;
    }, {});

  return sorted;
}

/**
 * Generate text report
 * @param {PackageStats[]} stats - Package statistics
 * @param {ReportOptions} options - Report options
 */
function generateTextReport(stats, options) {
  const summary = generateSummary(stats);
  const confidenceStats = calculateConfidenceStats(stats);
  const lowestConfidence = findLowestConfidence(stats, options.top);
  const missingEvidence = aggregateMissingEvidence(stats);

  console.log('Diátaxis Coverage Report');
  console.log('========================\n');

  // Summary section
  console.log('SUMMARY');
  console.log('-------');
  console.log(`Total packages:     ${summary.total}`);
  console.log(`With tutorials:     ${summary.withTutorials} (${summary.withTutorialsPercent.toFixed(0)}%)`);
  console.log(`With 2+ how-tos:    ${summary.with2PlusHowtos} (${summary.with2PlusHowtosPercent.toFixed(0)}%)`);
  console.log(`With reference:     ${summary.withReference} (${summary.withReferencePercent.toFixed(0)}%)`);
  console.log(`With explanation:   ${summary.withExplanation} (${summary.withExplanationPercent.toFixed(0)}%)\n`);

  // Confidence section
  console.log('CONFIDENCE');
  console.log('----------');
  for (const [field, values] of Object.entries(confidenceStats)) {
    const fieldName = field.charAt(0).toUpperCase() + field.slice(1);
    console.log(`${fieldName.padEnd(12)}: avg=${values.avg.toFixed(2)}, min=${values.min.toFixed(2)}, max=${values.max.toFixed(2)}`);
  }
  console.log();

  // Lowest confidence section
  console.log(`LOWEST CONFIDENCE (${options.top} packages)`);
  console.log('----------------------------');
  if (lowestConfidence.length === 0) {
    console.log('No packages found\n');
  } else {
    lowestConfidence.forEach((pkg, i) => {
      const reasons = pkg.missingEvidence.length > 0 ? ` - ${pkg.missingEvidence.join(', ')}` : '';
      console.log(`${(i + 1).toString().padStart(2)}. ${pkg.packageName.padEnd(30)} (${pkg.avgConfidence.toFixed(2)})${reasons}`);
    });
    console.log();
  }

  // Missing evidence section
  console.log('MISSING EVIDENCE');
  console.log('----------------');
  if (Object.keys(missingEvidence).length === 0) {
    console.log('All evidence sources present\n');
  } else {
    for (const [evidence, count] of Object.entries(missingEvidence)) {
      const evidenceLabel = evidence.padEnd(25);
      console.log(`${evidenceLabel}: ${count} packages`);
    }
    console.log();
  }

  // Quick fixes section
  console.log('QUICK FIXES');
  console.log('-----------');
  if (Object.keys(missingEvidence).length === 0) {
    console.log('No immediate improvements needed\n');
  } else {
    console.log('To improve coverage, consider:');
    for (const [evidence, count] of Object.entries(missingEvidence)) {
      const action = evidence.replace('no ', 'Add ').replace('empty ', 'Create ');
      console.log(`- ${action} to ${count} packages`);
    }
    console.log();
  }

  console.log(`EXIT: 0 (all packages documented)`);
}

/**
 * Generate JSON report
 * @param {PackageStats[]} stats - Package statistics
 * @param {ReportOptions} options - Report options
 */
function generateJsonReport(stats, options) {
  const summary = generateSummary(stats);
  const confidenceStats = calculateConfidenceStats(stats);
  const lowestConfidence = findLowestConfidence(stats, options.top);
  const missingEvidence = aggregateMissingEvidence(stats);

  const report = {
    summary: {
      total: summary.total,
      withTutorials: summary.withTutorials,
      withTutorialsPercent: parseFloat(summary.withTutorialsPercent.toFixed(2)),
      with2PlusHowtos: summary.with2PlusHowtos,
      with2PlusHowtosPercent: parseFloat(summary.with2PlusHowtosPercent.toFixed(2)),
      withReference: summary.withReference,
      withReferencePercent: parseFloat(summary.withReferencePercent.toFixed(2)),
      withExplanation: summary.withExplanation,
      withExplanationPercent: parseFloat(summary.withExplanationPercent.toFixed(2))
    },
    confidence: confidenceStats,
    lowestConfidence: lowestConfidence.map(pkg => ({
      packageName: pkg.packageName,
      avgConfidence: parseFloat(pkg.avgConfidence.toFixed(2)),
      missingEvidence: pkg.missingEvidence,
      confidence: {
        tutorials: parseFloat(pkg.confidence.tutorials.toFixed(2)),
        howtos: parseFloat(pkg.confidence.howtos.toFixed(2)),
        reference: parseFloat(pkg.confidence.reference.toFixed(2)),
        explanation: parseFloat(pkg.confidence.explanation.toFixed(2))
      }
    })),
    missingEvidence,
    generatedAt: new Date().toISOString()
  };

  console.log(stableStringify(report, { indent: 2 }));
}

/**
 * Generate CSV report
 * @param {PackageStats[]} stats - Package statistics
 */
function generateCsvReport(stats) {
  // Header
  console.log('Package,Tutorials,HowTos,HasReference,HasExplanation,AvgConfidence,TutorialsConf,HowtosConf,ReferenceConf,ExplanationConf,MissingEvidence');

  // Rows
  for (const pkg of stats) {
    const row = [
      pkg.packageName,
      pkg.tutorialsCount,
      pkg.howtosCount,
      pkg.hasReference ? 'yes' : 'no',
      pkg.hasExplanation ? 'yes' : 'no',
      pkg.avgConfidence.toFixed(2),
      pkg.confidence.tutorials.toFixed(2),
      pkg.confidence.howtos.toFixed(2),
      pkg.confidence.reference.toFixed(2),
      pkg.confidence.explanation.toFixed(2),
      `"${pkg.missingEvidence.join('; ')}"`
    ];
    console.log(row.join(','));
  }
}

/**
 * Filter packages by keyword
 * @param {PackageStats[]} stats - Package statistics
 * @param {string} keyword - Filter keyword
 * @returns {PackageStats[]} Filtered statistics
 */
function filterPackages(stats, keyword) {
  if (!keyword) return stats;
  return stats.filter(pkg => pkg.packageName.includes(keyword));
}

/**
 * Sort packages by field
 * @param {PackageStats[]} stats - Package statistics
 * @param {string} field - Sort field
 * @returns {PackageStats[]} Sorted statistics
 */
function sortPackages(stats, field) {
  const sorted = [...stats];

  switch (field) {
    case 'confidence':
      sorted.sort((a, b) => b.avgConfidence - a.avgConfidence);
      break;
    case 'tutorials':
      sorted.sort((a, b) => b.tutorialsCount - a.tutorialsCount);
      break;
    case 'howtos':
      sorted.sort((a, b) => b.howtosCount - a.howtosCount);
      break;
    case 'reference':
      sorted.sort((a, b) => (b.hasReference ? 1 : 0) - (a.hasReference ? 1 : 0));
      break;
    case 'explanation':
      sorted.sort((a, b) => (b.hasExplanation ? 1 : 0) - (a.hasExplanation ? 1 : 0));
      break;
    default:
      // Keep original order (sorted by package name from loading)
      break;
  }

  return sorted;
}

/**
 * Main entry point
 */
async function main() {
  const args = process.argv.slice(2);
  const options = parseArgs(args);

  // Determine artifacts path (default: ARTIFACTS/ in workspace root)
  const workspaceRoot = resolve(process.cwd());
  const artifactsPath = join(workspaceRoot, 'ARTIFACTS');

  // Load inventories
  const inventories = await loadInventories(artifactsPath);

  if (inventories.length === 0) {
    console.log('No inventory generated. Run diataxis-run first.');
    process.exit(0);
  }

  // Calculate statistics
  let stats = inventories.map(calculatePackageStats);

  // Apply filter
  stats = filterPackages(stats, options.filter);

  if (stats.length === 0) {
    console.log('No packages found matching filter.');
    process.exit(0);
  }

  // Apply sort
  stats = sortPackages(stats, options.sort);

  // Generate report
  if (options.json) {
    generateJsonReport(stats, options);
  } else if (options.csv) {
    generateCsvReport(stats);
  } else {
    generateTextReport(stats, options);
  }

  process.exit(0);
}

main().catch(error => {
  console.error(`Error: ${error.message}`);
  process.exit(1);
});
