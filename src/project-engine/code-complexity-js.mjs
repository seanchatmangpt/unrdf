/**
 * @file JavaScript Code Complexity Analysis Capability
 * @module project-engine/code-complexity-js
 * @description Analyzes JavaScript/TypeScript code complexity using typhonjs-escomplex,
 * emits RDF triples to project store with unmetric: vocabulary
 */

import { z } from 'zod';
import { Store, DataFactory } from 'n3';
import { readdir, readFile } from 'fs/promises';
import { join, extname } from 'path';
import { statSync } from 'fs';
import ProjectReport from 'typhonjs-escomplex';
import * as unmetric from '../ontologies/unmetric-ontology.mjs';

const { namedNode, literal, quad } = DataFactory;

/**
 * @typedef {Object} JsComplexityInput
 * @property {string} projectRoot - Root directory to analyze
 * @property {Store} [baseStore] - Existing RDF store to merge into
 * @property {string[]} [excludePatterns] - Patterns to exclude from analysis
 * @property {string} [mode] - Analysis mode: off, observe, enforce
 */

/**
 * @typedef {Object} FunctionMetrics
 * @property {string} name - Function name
 * @property {number} startLine - Start line number
 * @property {number} cyclomatic - Cyclomatic complexity
 * @property {number} halsteadVolume - Halstead volume
 * @property {number} maintainabilityIndex - Maintainability index (0-100)
 */

/**
 * @typedef {Object} FileMetrics
 * @property {string} filePath - Relative file path
 * @property {number} cyclomatic - Average cyclomatic complexity
 * @property {number} halsteadVolume - File halstead volume
 * @property {number} maintainabilityIndex - File maintainability index
 * @property {number} linesOfCode - Physical lines of code
 * @property {FunctionMetrics[]} functions - Per-function metrics
 */

/**
 * @typedef {Object} JsComplexitySummary
 * @property {number} filesAnalyzed - Number of files analyzed
 * @property {number} averageCyclomatic - Project-wide average CC
 * @property {FileMetrics[]} topRisks - Worst 5 files by CC and MI
 * @property {string} mode - Analysis mode
 * @property {string} timestamp - ISO 8601 timestamp
 */

const JsComplexityInputSchema = z.object({
  projectRoot: z.string(),
  baseStore: z.instanceof(Store).optional(),
  excludePatterns: z.array(z.string()).optional(),
  mode: z.enum(['off', 'observe', 'enforce']).default('observe'),
});

const DEFAULT_EXCLUDE_PATTERNS = [
  '**/node_modules/**',
  '**/dist/**',
  '**/build/**',
  '**/coverage/**',
  '**/.next/**',
  '**/test/**',
  '**/__tests__/**',
  '**/spec/**',
  '**/*.test.mjs',
  '**/*.test.js',
  '**/*.spec.mjs',
  '**/*.spec.js',
];

/**
 * Analyze JavaScript/TypeScript code complexity and emit RDF triples
 *
 * @param {JsComplexityInput} input - Analysis input
 * @returns {Promise<{ store: Store, summary: JsComplexitySummary }>}
 */
export async function analyzeJsComplexity(input) {
  const validated = JsComplexityInputSchema.parse(input);

  if (validated.mode === 'off') {
    // Return empty store if analysis is disabled
    const emptyStore = new Store();
    return {
      store: emptyStore,
      summary: {
        filesAnalyzed: 0,
        averageCyclomatic: 0,
        topRisks: [],
        mode: 'off',
        timestamp: new Date().toISOString(),
      },
    };
  }

  const store = validated.baseStore || new Store();
  const excludePatterns = [...(validated.excludePatterns || []), ...DEFAULT_EXCLUDE_PATTERNS];

  // Find all JS/TS files
  const jsFiles = await findJavaScriptFiles(validated.projectRoot, excludePatterns);

  if (jsFiles.length === 0) {
    return {
      store,
      summary: {
        filesAnalyzed: 0,
        averageCyclomatic: 0,
        topRisks: [],
        mode: validated.mode,
        timestamp: new Date().toISOString(),
      },
    };
  }

  // Analyze each file
  const fileMetrics = [];
  const allFunctionMetrics = [];

  for (const filePath of jsFiles) {
    try {
      const content = await readFile(filePath, 'utf-8');
      const metrics = analyzeFileComplexity(content, filePath, validated.projectRoot);

      if (metrics) {
        fileMetrics.push(metrics);
        allFunctionMetrics.push(...metrics.functions.map(f => ({ ...f, file: filePath })));

        // Emit RDF triples for this file
        emitFileComplexityTriples(store, metrics, validated.projectRoot);
      }
    } catch (err) {
      // Silently skip files that can't be analyzed
      // (e.g., syntax errors, binary files)
    }
  }

  // Calculate summary
  const summary = calculateSummary(fileMetrics, validated.mode);

  // Emit root report node
  const reportNode = namedNode(`http://example.org/unrdf/metrics#report-${Date.now()}`);
  store.addQuad(
    quad(
      reportNode,
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      unmetric.ComplexityReport
    )
  );
  store.addQuad(quad(reportNode, unmetric.filesAnalyzed, literal(fileMetrics.length)));
  store.addQuad(quad(reportNode, unmetric.analysisMode, literal(validated.mode)));
  store.addQuad(
    quad(
      reportNode,
      unmetric.analysisTimestamp,
      literal(summary.timestamp, namedNode('http://www.w3.org/2001/XMLSchema#dateTime'))
    )
  );

  return { store, summary };
}

/**
 * Find all JavaScript/TypeScript files in project
 *
 * @private
 * @param {string} root - Root directory
 * @param {string[]} excludePatterns - Patterns to exclude
 * @returns {Promise<string[]>}
 */
async function findJavaScriptFiles(root, excludePatterns) {
  const files = [];
  const jsExtensions = ['.js', '.mjs', '.ts', '.tsx', '.jsx'];

  async function scan(dir) {
    try {
      const entries = await readdir(dir, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = join(dir, entry.name);
        const relativePath = fullPath.substring(root.length + 1);

        // Check if path matches exclude patterns
        const shouldExclude = excludePatterns.some(pattern => {
          if (pattern.startsWith('**/')) {
            const patternEnd = pattern.substring(3);
            return (
              relativePath.includes(patternEnd.replace('/**', '')) ||
              fullPath.endsWith(patternEnd.replace('**', '').replace(/\*/g, ''))
            );
          }
          return fullPath.endsWith(pattern.replace(/\*/g, ''));
        });

        if (shouldExclude) continue;

        if (entry.isDirectory()) {
          await scan(fullPath);
        } else if (jsExtensions.includes(extname(entry.name))) {
          files.push(fullPath);
        }
      }
    } catch {
      // Skip directories that can't be read
    }
  }

  await scan(root);
  return files.sort();
}

/**
 * Analyze a single file's complexity using escomplex
 *
 * @private
 * @param {string} content - File content
 * @param {string} filePath - Full file path
 * @param {string} projectRoot - Project root for relative path
 * @returns {FileMetrics|null}
 */
function analyzeFileComplexity(content, filePath, projectRoot) {
  try {
    // Use typhonjs-escomplex to analyze
    const report = ProjectReport.analyze(content);

    if (!report || !report.modules || report.modules.length === 0) {
      return null;
    }

    const module = report.modules[0];
    const relativePath = filePath.substring(projectRoot.length + 1);

    // Extract function metrics
    const functions = (module.functions || []).map(fn => ({
      name: fn.name || '(anonymous)',
      startLine: fn.line || 0,
      cyclomatic: fn.cyclomatic || 1,
      halsteadVolume: fn.halstead?.volume || 0,
      maintainabilityIndex: fn.maintainability || 100,
    }));

    return {
      filePath: relativePath,
      cyclomatic: module.cyclomatic || 1,
      halsteadVolume: module.halstead?.volume || 0,
      maintainabilityIndex: module.maintainability || 100,
      linesOfCode: module.loc?.physical || 0,
      functions,
    };
  } catch {
    // Return null for files that can't be analyzed
    return null;
  }
}

/**
 * Emit RDF triples for file complexity metrics
 *
 * @private
 * @param {Store} store - RDF store to emit to
 * @param {FileMetrics} metrics - File metrics
 * @param {string} projectRoot - Project root
 */
function emitFileComplexityTriples(store, metrics, projectRoot) {
  const fileNode = namedNode(
    `http://example.org/unrdf/metrics#file-${Date.now()}-${Math.random()}`
  );

  // File type and path
  store.addQuad(
    quad(
      fileNode,
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      unmetric.FileComplexity
    )
  );
  store.addQuad(quad(fileNode, unmetric.location, literal(metrics.filePath)));

  // File-level metrics
  store.addQuad(quad(fileNode, unmetric.cyclomatic, literal(metrics.cyclomatic)));
  store.addQuad(quad(fileNode, unmetric.halsteadVolume, literal(metrics.halsteadVolume)));
  store.addQuad(
    quad(fileNode, unmetric.maintainabilityIndex, literal(metrics.maintainabilityIndex))
  );
  store.addQuad(quad(fileNode, unmetric.linesOfCode, literal(metrics.linesOfCode)));

  // Severity flags
  if (metrics.cyclomatic > 10) {
    store.addQuad(quad(fileNode, unmetric.highComplexity, literal(true)));
  }
  if (metrics.maintainabilityIndex < 50) {
    store.addQuad(quad(fileNode, unmetric.lowMaintainability, literal(true)));
  }

  // Emit function-level metrics
  for (const fn of metrics.functions) {
    const fnNode = namedNode(
      `http://example.org/unrdf/metrics#func-${Date.now()}-${Math.random()}`
    );

    store.addQuad(
      quad(
        fnNode,
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        unmetric.FunctionComplexity
      )
    );
    store.addQuad(quad(fnNode, unmetric.location, literal(`${metrics.filePath}:${fn.startLine}`)));
    store.addQuad(
      quad(fnNode, namedNode('http://example.org/unrdf/metrics#name'), literal(fn.name))
    );

    store.addQuad(quad(fnNode, unmetric.cyclomatic, literal(fn.cyclomatic)));
    store.addQuad(quad(fnNode, unmetric.halsteadVolume, literal(fn.halsteadVolume)));
    store.addQuad(quad(fnNode, unmetric.maintainabilityIndex, literal(fn.maintainabilityIndex)));

    // Link function to file
    store.addQuad(quad(fileNode, unmetric.hasFunctionComplexity, fnNode));
  }
}

/**
 * Calculate summary metrics from analyzed files
 *
 * @private
 * @param {FileMetrics[]} fileMetrics - All file metrics
 * @param {string} mode - Analysis mode
 * @returns {JsComplexitySummary}
 */
function calculateSummary(fileMetrics, mode) {
  if (fileMetrics.length === 0) {
    return {
      filesAnalyzed: 0,
      averageCyclomatic: 0,
      topRisks: [],
      mode,
      timestamp: new Date().toISOString(),
    };
  }

  // Calculate average cyclomatic
  const totalCyclomatic = fileMetrics.reduce((sum, f) => sum + f.cyclomatic, 0);
  const averageCyclomatic = totalCyclomatic / fileMetrics.length;

  // Find top risks: lowest MI and highest CC
  const risks = [...fileMetrics]
    .sort((a, b) => {
      // Primary sort: MI (ascending, lower is worse)
      if (a.maintainabilityIndex !== b.maintainabilityIndex) {
        return a.maintainabilityIndex - b.maintainabilityIndex;
      }
      // Secondary sort: CC (descending, higher is worse)
      return b.cyclomatic - a.cyclomatic;
    })
    .slice(0, 5);

  return {
    filesAnalyzed: fileMetrics.length,
    averageCyclomatic: Math.round(averageCyclomatic * 100) / 100,
    topRisks: risks,
    mode,
    timestamp: new Date().toISOString(),
  };
}
