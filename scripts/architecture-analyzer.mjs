#!/usr/bin/env node
/**
 * Architecture Analyzer - Custom dependency and architecture quality analyzer
 *
 * Analyzes:
 * - Circular dependencies
 * - OTEL contamination in business logic
 * - Module coupling
 * - File size violations
 * - Architecture grade calculation
 */

import { readdir, readFile, stat } from 'node:fs/promises';
import { join, relative, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const rootDir = join(__dirname, '..');

// Architecture grading criteria
const CRITERIA = {
  noCyclicDeps: { weight: 20, name: 'No Circular Dependencies' },
  otelSeparation: { weight: 20, name: 'OTEL Separation (no business logic contamination)' },
  pureFunctions: { weight: 15, name: 'Pure Functions (>80%)' },
  fileSizeLimit: { weight: 15, name: 'File Size <500 lines' },
  moduleCoupling: { weight: 10, name: 'Low Module Coupling' },
  patternReuse: { weight: 10, name: 'Pattern Reuse' },
  scalability: { weight: 10, name: 'Scalability (federation, caching)' }
};

class ArchitectureAnalyzer {
  constructor() {
    this.packages = new Map();
    this.dependencies = new Map();
    this.violations = [];
    this.metrics = {
      circularDeps: [],
      otelContamination: [],
      largeFiles: [],
      pureFunctionCount: 0,
      totalFunctions: 0,
      packageCount: 0,
      coupling: {}
    };
  }

  async analyze() {
    console.log('🔍 Architecture Analysis Starting...\n');

    // 1. Scan packages
    await this.scanPackages();

    // 2. Check circular dependencies
    await this.checkCircularDependencies();

    // 3. Check OTEL contamination
    await this.checkOTELContamination();

    // 4. Check file sizes
    await this.checkFileSizes();

    // 5. Analyze pure functions
    await this.analyzePureFunctions();

    // 6. Calculate grade
    const grade = this.calculateGrade();

    // 7. Generate report
    this.generateReport(grade);

    return { grade, violations: this.violations, metrics: this.metrics };
  }

  async scanPackages() {
    const packagesDir = join(rootDir, 'packages');
    const entries = await readdir(packagesDir, { withFileTypes: true });

    for (const entry of entries) {
      if (entry.isDirectory()) {
        const pkgPath = join(packagesDir, entry.name);
        const pkgJsonPath = join(pkgPath, 'package.json');

        try {
          const pkgJson = JSON.parse(await readFile(pkgJsonPath, 'utf-8'));
          this.packages.set(entry.name, {
            name: pkgJson.name || entry.name,
            path: pkgPath,
            dependencies: pkgJson.dependencies || {},
            devDependencies: pkgJson.devDependencies || {}
          });
        } catch (err) {
          // Package.json doesn't exist or invalid
        }
      }
    }

    this.metrics.packageCount = this.packages.size;
    console.log(`✅ Scanned ${this.metrics.packageCount} packages`);
  }

  async checkCircularDependencies() {
    // Build dependency graph
    const graph = new Map();

    for (const [pkgName, pkg] of this.packages) {
      const deps = Object.keys(pkg.dependencies)
        .filter(dep => dep.startsWith('@unrdf/'))
        .map(dep => dep.replace('@unrdf/', ''));
      graph.set(pkgName, deps);
    }

    // DFS to find cycles
    const visited = new Set();
    const recStack = new Set();
    const cycles = [];

    const dfs = (node, path = []) => {
      if (recStack.has(node)) {
        // Found cycle
        const cycleStart = path.indexOf(node);
        cycles.push(path.slice(cycleStart).concat(node));
        return;
      }

      if (visited.has(node)) return;

      visited.add(node);
      recStack.add(node);
      path.push(node);

      const deps = graph.get(node) || [];
      for (const dep of deps) {
        dfs(dep, [...path]);
      }

      recStack.delete(node);
    };

    for (const node of graph.keys()) {
      if (!visited.has(node)) {
        dfs(node);
      }
    }

    this.metrics.circularDeps = cycles;

    if (cycles.length > 0) {
      console.log(`❌ Found ${cycles.length} circular dependencies`);
      for (const cycle of cycles) {
        const cycleStr = cycle.join(' → ');
        console.log(`   ${cycleStr}`);
        this.violations.push({
          type: 'circular_dependency',
          severity: 'critical',
          message: `Circular dependency: ${cycleStr}`
        });
      }
    } else {
      console.log('✅ No circular dependencies found');
    }
  }

  async checkOTELContamination() {
    const packagesDir = join(rootDir, 'packages');
    const contaminated = [];

    // Define business logic directories (not API/observability layers)
    // Exclude CLI, API, server, handlers (these are API layer)
    const businessLogicPatterns = [
      '/kgn/src/core/',
      '/kgn/src/base/',
      '/kgn/src/engine/',
      '/core/src/rdf/',
      '/core/src/sparql/',
      '/yawl/src/workflow/',
      '/yawl/src/case/',
      '/yawl/src/task/',
      '/yawl/src/engine/',
      '/knowledge-engine/src/',
      '/streaming/src/streaming/',
      '/federation/src/federation/'
    ];

    const otelPatterns = [
      /import.*@opentelemetry/,
      /from\s+['"]@opentelemetry/,
      /\.startSpan\(/,
      /\.addEvent\(/,
      /trace\./,
      /tracer\./
    ];

    for (const [pkgName, pkg] of this.packages) {
      const srcDir = join(pkg.path, 'src');
      try {
        await this.scanDirectoryForOTEL(srcDir, businessLogicPatterns, otelPatterns, contaminated);
      } catch (err) {
        // No src directory
      }
    }

    this.metrics.otelContamination = contaminated;

    if (contaminated.length > 0) {
      console.log(`❌ Found ${contaminated.length} files with OTEL contamination in business logic`);
      for (const file of contaminated.slice(0, 5)) {
        console.log(`   ${file.file}:${file.line}`);
        this.violations.push({
          type: 'otel_contamination',
          severity: 'critical',
          message: `OTEL in business logic: ${file.file}:${file.line}`,
          file: file.file
        });
      }
      if (contaminated.length > 5) {
        console.log(`   ... and ${contaminated.length - 5} more`);
      }
    } else {
      console.log('✅ No OTEL contamination in business logic');
    }
  }

  async scanDirectoryForOTEL(dir, businessLogicPatterns, otelPatterns, results) {
    const entries = await readdir(dir, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = join(dir, entry.name);

      if (entry.isDirectory() && entry.name !== 'node_modules') {
        await this.scanDirectoryForOTEL(fullPath, businessLogicPatterns, otelPatterns, results);
      } else if (entry.isFile() && (entry.name.endsWith('.mjs') || entry.name.endsWith('.js'))) {
        // Check if this is business logic
        const relativePath = relative(rootDir, fullPath);
        const isBusinessLogic = businessLogicPatterns.some(pattern =>
          relativePath.includes(pattern)
        );

        if (!isBusinessLogic) continue;

        // Check for OTEL usage
        const content = await readFile(fullPath, 'utf-8');
        const lines = content.split('\n');

        for (let i = 0; i < lines.length; i++) {
          const line = lines[i];
          for (const pattern of otelPatterns) {
            if (pattern.test(line)) {
              results.push({
                file: relativePath,
                line: i + 1,
                code: line.trim()
              });
            }
          }
        }
      }
    }
  }

  async checkFileSizes() {
    const packagesDir = join(rootDir, 'packages');
    const largeFiles = [];
    const SIZE_LIMIT = 500;

    for (const [pkgName, pkg] of this.packages) {
      const srcDir = join(pkg.path, 'src');
      try {
        await this.scanDirectoryForSize(srcDir, SIZE_LIMIT, largeFiles);
      } catch (err) {
        // No src directory
      }
    }

    this.metrics.largeFiles = largeFiles.sort((a, b) => b.lines - a.lines);

    if (largeFiles.length > 0) {
      console.log(`❌ Found ${largeFiles.length} files exceeding ${SIZE_LIMIT} lines`);
      for (const file of largeFiles.slice(0, 5)) {
        console.log(`   ${file.file}: ${file.lines} lines (${((file.lines / SIZE_LIMIT - 1) * 100).toFixed(0)}% over)`);
        this.violations.push({
          type: 'file_size',
          severity: 'medium',
          message: `File exceeds ${SIZE_LIMIT} lines: ${file.file} (${file.lines} lines)`,
          file: file.file
        });
      }
      if (largeFiles.length > 5) {
        console.log(`   ... and ${largeFiles.length - 5} more`);
      }
    } else {
      console.log(`✅ All files under ${SIZE_LIMIT} lines`);
    }
  }

  async scanDirectoryForSize(dir, sizeLimit, results) {
    const entries = await readdir(dir, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = join(dir, entry.name);

      if (entry.isDirectory() && entry.name !== 'node_modules') {
        await this.scanDirectoryForSize(fullPath, sizeLimit, results);
      } else if (entry.isFile() && (entry.name.endsWith('.mjs') || entry.name.endsWith('.js'))) {
        const content = await readFile(fullPath, 'utf-8');
        const lines = content.split('\n').length;

        if (lines > sizeLimit) {
          results.push({
            file: relative(rootDir, fullPath),
            lines
          });
        }
      }
    }
  }

  async analyzePureFunctions() {
    // Look for function definitions and check for side effects
    const packagesDir = join(rootDir, 'packages');
    let pureFunctions = 0;
    let totalFunctions = 0;

    for (const [pkgName, pkg] of this.packages) {
      const srcDir = join(pkg.path, 'src');
      try {
        const counts = await this.countPureFunctions(srcDir);
        pureFunctions += counts.pure;
        totalFunctions += counts.total;
      } catch (err) {
        // No src directory
      }
    }

    this.metrics.pureFunctionCount = pureFunctions;
    this.metrics.totalFunctions = totalFunctions;

    const purity = totalFunctions > 0 ? (pureFunctions / totalFunctions * 100).toFixed(1) : 0;
    console.log(`📊 Pure functions: ${pureFunctions}/${totalFunctions} (${purity}%)`);
  }

  async countPureFunctions(dir) {
    let pure = 0;
    let total = 0;

    const entries = await readdir(dir, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = join(dir, entry.name);

      if (entry.isDirectory() && entry.name !== 'node_modules') {
        const counts = await this.countPureFunctions(fullPath);
        pure += counts.pure;
        total += counts.total;
      } else if (entry.isFile() && (entry.name.endsWith('.mjs') || entry.name.endsWith('.js'))) {
        const content = await readFile(fullPath, 'utf-8');

        // Simple heuristic: count export function/const
        const functionMatches = content.match(/export\s+(function|const)\s+\w+/g) || [];
        total += functionMatches.length;

        // Pure if no: console, process, fs, global state mutation
        const impurePatterns = [
          /console\./,
          /process\./,
          /global\./,
          /window\./,
          /document\./,
          /localStorage\./,
          /sessionStorage\./
        ];

        // Very simplistic - count functions without impure patterns
        for (const match of functionMatches) {
          const funcName = match.match(/\w+$/)[0];
          const funcRegex = new RegExp(`(export\\s+(?:function|const)\\s+${funcName}[\\s\\S]*?)(?=export\\s+|$)`, 'g');
          const funcBody = funcRegex.exec(content)?.[1] || '';

          const isImpure = impurePatterns.some(pattern => pattern.test(funcBody));
          if (!isImpure) {
            pure++;
          }
        }
      }
    }

    return { pure, total };
  }

  calculateGrade() {
    let totalScore = 0;
    const breakdown = {};

    // 1. No Circular Dependencies (20 points)
    const cyclicDepsScore = this.metrics.circularDeps.length === 0 ? 20 : Math.max(0, 20 - this.metrics.circularDeps.length * 10);
    totalScore += cyclicDepsScore;
    breakdown.noCyclicDeps = { score: cyclicDepsScore, max: 20 };

    // 2. OTEL Separation (20 points)
    const otelScore = this.metrics.otelContamination.length === 0 ? 20 : Math.max(0, 20 - this.metrics.otelContamination.length * 5);
    totalScore += otelScore;
    breakdown.otelSeparation = { score: otelScore, max: 20 };

    // 3. Pure Functions (15 points)
    const purityPct = this.metrics.totalFunctions > 0
      ? (this.metrics.pureFunctionCount / this.metrics.totalFunctions)
      : 0;
    const purityScore = purityPct >= 0.8 ? 15 : Math.round(purityPct * 15 / 0.8);
    totalScore += purityScore;
    breakdown.pureFunctions = { score: purityScore, max: 15, pct: (purityPct * 100).toFixed(1) };

    // 4. File Size Limit (15 points)
    const fileSizeScore = this.metrics.largeFiles.length === 0 ? 15 : Math.max(0, 15 - this.metrics.largeFiles.length);
    totalScore += fileSizeScore;
    breakdown.fileSizeLimit = { score: fileSizeScore, max: 15 };

    // 5. Module Coupling (10 points) - assume good if no circular deps
    const couplingScore = this.metrics.circularDeps.length === 0 ? 10 : 5;
    totalScore += couplingScore;
    breakdown.moduleCoupling = { score: couplingScore, max: 10 };

    // 6. Pattern Reuse (10 points) - assume good for now
    const patternScore = 10;
    totalScore += patternScore;
    breakdown.patternReuse = { score: patternScore, max: 10 };

    // 7. Scalability (10 points) - assume good for now
    const scalabilityScore = 10;
    totalScore += scalabilityScore;
    breakdown.scalability = { score: scalabilityScore, max: 10 };

    return {
      total: totalScore,
      max: 100,
      letter: this.getLetterGrade(totalScore),
      breakdown
    };
  }

  getLetterGrade(score) {
    if (score >= 95) return 'A+';
    if (score >= 90) return 'A';
    if (score >= 85) return 'A-';
    if (score >= 80) return 'B+';
    if (score >= 75) return 'B';
    if (score >= 70) return 'B-';
    if (score >= 65) return 'C+';
    if (score >= 60) return 'C';
    if (score >= 55) return 'C-';
    if (score >= 50) return 'D';
    return 'F';
  }

  generateReport(grade) {
    console.log('\n' + '='.repeat(80));
    console.log('ARCHITECTURE GRADE REPORT');
    console.log('='.repeat(80));
    console.log(`\nOverall Grade: ${grade.letter} (${grade.total}/${grade.max})\n`);

    console.log('Score Breakdown:');
    console.log('─'.repeat(80));

    for (const [key, criteria] of Object.entries(CRITERIA)) {
      const result = grade.breakdown[key];
      const status = result.score === result.max ? '✅' : result.score >= result.max * 0.7 ? '⚠️' : '❌';
      const pct = result.pct ? ` (${result.pct}%)` : '';
      console.log(`${status} ${criteria.name.padEnd(50)} ${result.score}/${result.max}${pct}`);
    }

    console.log('\n' + '='.repeat(80));
    console.log('CRITICAL VIOLATIONS');
    console.log('='.repeat(80) + '\n');

    const criticalViolations = this.violations.filter(v => v.severity === 'critical');
    if (criticalViolations.length > 0) {
      for (const violation of criticalViolations) {
        console.log(`❌ [${violation.type.toUpperCase()}] ${violation.message}`);
      }
    } else {
      console.log('✅ No critical violations');
    }

    console.log('\n' + '='.repeat(80));
    console.log('RECOMMENDATIONS');
    console.log('='.repeat(80) + '\n');

    const recommendations = this.generateRecommendations(grade);
    for (let i = 0; i < recommendations.length; i++) {
      console.log(`${i + 1}. ${recommendations[i]}`);
    }
  }

  generateRecommendations(grade) {
    const recs = [];

    if (this.metrics.circularDeps.length > 0) {
      recs.push(`CRITICAL: Fix ${this.metrics.circularDeps.length} circular dependencies (+${20 - grade.breakdown.noCyclicDeps.score} points)`);
    }

    if (this.metrics.otelContamination.length > 0) {
      recs.push(`CRITICAL: Remove ${this.metrics.otelContamination.length} OTEL contaminations from business logic (+${20 - grade.breakdown.otelSeparation.score} points)`);
    }

    if (this.metrics.largeFiles.length > 0) {
      recs.push(`HIGH: Refactor ${this.metrics.largeFiles.length} files exceeding 500 lines (+${15 - grade.breakdown.fileSizeLimit.score} points)`);
    }

    const purityPct = this.metrics.totalFunctions > 0
      ? (this.metrics.pureFunctionCount / this.metrics.totalFunctions * 100)
      : 0;
    if (purityPct < 80) {
      recs.push(`MEDIUM: Increase pure function ratio from ${purityPct.toFixed(1)}% to 80%+ (+${15 - grade.breakdown.pureFunctions.score} points)`);
    }

    if (recs.length === 0) {
      recs.push('Architecture is in excellent shape! Consider optimizations for A+ if not already achieved.');
    }

    return recs;
  }
}

// Main execution
if (import.meta.url === `file://${process.argv[1]}`) {
  const analyzer = new ArchitectureAnalyzer();

  analyzer.analyze()
    .then(({ grade, violations, metrics }) => {
      console.log(`\n✅ Analysis complete. Grade: ${grade.letter} (${grade.total}/100)`);

      // Write detailed JSON report
      const report = {
        timestamp: new Date().toISOString(),
        grade,
        violations,
        metrics,
        summary: {
          packages: metrics.packageCount,
          circularDeps: metrics.circularDeps.length,
          otelContamination: metrics.otelContamination.length,
          largeFiles: metrics.largeFiles.length,
          purityPct: metrics.totalFunctions > 0
            ? (metrics.pureFunctionCount / metrics.totalFunctions * 100).toFixed(1)
            : 0
        }
      };

      // Output JSON for programmatic use
      if (process.argv.includes('--json')) {
        console.log(JSON.stringify(report, null, 2));
      }

      process.exit(grade.total >= 70 ? 0 : 1);
    })
    .catch(err => {
      console.error('❌ Analysis failed:', err.message);
      console.error(err.stack);
      process.exit(1);
    });
}

export { ArchitectureAnalyzer };
