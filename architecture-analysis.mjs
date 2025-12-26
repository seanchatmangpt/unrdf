#!/usr/bin/env node
/**
 * Architecture Analysis Tool
 * Parses microframeworks and generates dependency graphs
 */

import { readFileSync, readdirSync } from 'fs';
import { join } from 'path';

const ROOT = '/home/user/unrdf';

class ArchitectureAnalyzer {
  constructor() {
    this.frameworks = [];
    this.dependencyGraph = new Map();
  }

  /**
   * Extract import statements from file
   */
  parseImports(filePath) {
    const content = readFileSync(filePath, 'utf-8');
    const lines = content.split('\n');

    const imports = new Set();
    const externalPackages = new Set();
    const unrdfPackages = new Set();

    for (const line of lines) {
      const trimmed = line.trim();

      // Match: import ... from 'package'
      const importMatch = trimmed.match(/import\s+.*\s+from\s+['"]([^'"]+)['"]/);
      if (importMatch) {
        const pkg = importMatch[1];
        imports.add(pkg);

        if (pkg.startsWith('@unrdf/')) {
          unrdfPackages.add(pkg);
        } else if (!pkg.startsWith('.') && !pkg.startsWith('/')) {
          externalPackages.add(pkg);
        }
      }
    }

    return {
      allImports: Array.from(imports),
      unrdfPackages: Array.from(unrdfPackages),
      externalPackages: Array.from(externalPackages),
      totalUnique: imports.size,
      unrdfCount: unrdfPackages.size,
      externalCount: externalPackages.size,
    };
  }

  /**
   * Analyze file for architectural patterns
   */
  analyzeFile(filePath, fileName) {
    const content = readFileSync(filePath, 'utf-8');
    const lines = content.split('\n');

    const imports = this.parseImports(filePath);

    // Count classes, functions, exports
    const classes = (content.match(/class\s+\w+/g) || []).length;
    const functions = (content.match(/(?:async\s+)?function\s+\w+/g) || []).length;
    const exports = (content.match(/export\s+(?:default\s+)?(?:class|function|const|let|var|\{)/g) || []).length;

    // Check for mock implementations
    const hasMocks = content.includes('Mock') || content.includes('mock');

    // Check for OTEL/observability
    const hasOTEL = content.includes('otel') || content.includes('observability') || content.includes('trace');

    // Line count
    const lineCount = lines.length;
    const codeLines = lines.filter(l => l.trim() && !l.trim().startsWith('//')).length;

    return {
      fileName,
      filePath,
      lineCount,
      codeLines,
      classes,
      functions,
      exports,
      hasMocks,
      hasOTEL,
      ...imports,
    };
  }

  /**
   * Find all framework files
   */
  findFrameworks() {
    const files = readdirSync(ROOT);
    const frameworkFiles = files.filter(f =>
      (f.includes('framework') || f.includes('microfw')) && f.endsWith('.mjs')
    );

    return frameworkFiles.map(f => join(ROOT, f));
  }

  /**
   * Run full analysis
   */
  analyze() {
    const frameworkPaths = this.findFrameworks();

    console.log('╔════════════════════════════════════════════════════════════╗');
    console.log('║ UNRDF Architecture Analysis - Maximum-Combination Frameworks║');
    console.log('╚════════════════════════════════════════════════════════════╝\n');

    console.log(`Found ${frameworkPaths.length} framework files:\n`);

    for (const path of frameworkPaths) {
      const fileName = path.split('/').pop();
      const analysis = this.analyzeFile(path, fileName);
      this.frameworks.push(analysis);

      console.log(`━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━`);
      console.log(`FILE: ${fileName}`);
      console.log(`━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━`);
      console.log(`Lines of Code: ${analysis.codeLines} / ${analysis.lineCount} total`);
      console.log(`Package Integrations: ${analysis.totalUnique}`);
      console.log(`  - @unrdf/* packages: ${analysis.unrdfCount}`);
      console.log(`  - External packages: ${analysis.externalCount}`);
      console.log(`Architecture:`);
      console.log(`  - Classes: ${analysis.classes}`);
      console.log(`  - Functions: ${analysis.functions}`);
      console.log(`  - Exports: ${analysis.exports}`);
      console.log(`Flags:`);
      console.log(`  - Has Mocks: ${analysis.hasMocks ? 'YES ⚠️' : 'NO'}`);
      console.log(`  - Has OTEL: ${analysis.hasOTEL ? 'YES ⚠️' : 'NO'}`);
      console.log(`\n@unrdf Packages (${analysis.unrdfCount}):`);
      analysis.unrdfPackages.forEach((pkg, i) => {
        console.log(`  ${i + 1}. ${pkg}`);
      });
      if (analysis.externalPackages.length > 0) {
        console.log(`\nExternal Packages (${analysis.externalCount}):`);
        analysis.externalPackages.forEach((pkg, i) => {
          console.log(`  ${i + 1}. ${pkg}`);
        });
      }
      console.log('');
    }

    return this.frameworks;
  }

  /**
   * Generate dependency matrix
   */
  generateDependencyMatrix() {
    console.log('\n╔════════════════════════════════════════════════════════════╗');
    console.log('║ DEPENDENCY MATRIX                                          ║');
    console.log('╚════════════════════════════════════════════════════════════╝\n');

    // Collect all unique packages
    const allPackages = new Set();
    for (const fw of this.frameworks) {
      fw.unrdfPackages.forEach(p => allPackages.add(p));
      fw.externalPackages.forEach(p => allPackages.add(p));
    }

    const packages = Array.from(allPackages).sort();

    // Build matrix
    console.log('Framework → Package Dependencies:\n');
    for (const fw of this.frameworks) {
      console.log(`${fw.fileName}:`);
      const deps = [...fw.unrdfPackages, ...fw.externalPackages];
      deps.forEach(d => console.log(`  ✓ ${d}`));
      console.log('');
    }

    return { frameworks: this.frameworks, packages };
  }

  /**
   * Adversarial validation
   */
  adversarialCheck() {
    console.log('\n╔════════════════════════════════════════════════════════════╗');
    console.log('║ ADVERSARIAL VALIDATION CHECKS                              ║');
    console.log('╚════════════════════════════════════════════════════════════╝\n');

    const issues = [];

    // Check 1: Claimed vs actual framework count
    console.log('CHECK 1: Framework Count Verification');
    console.log(`  Claimed (commit message): 10 frameworks`);
    console.log(`  Actual files found: ${this.frameworks.length}`);
    if (this.frameworks.length < 10) {
      const issue = `❌ FAIL: Only ${this.frameworks.length}/10 frameworks exist`;
      console.log(`  ${issue}\n`);
      issues.push({ check: 'Framework Count', severity: 'CRITICAL', issue });
    } else {
      console.log(`  ✅ PASS\n`);
    }

    // Check 2: Integration count verification (3-12 claim)
    console.log('CHECK 2: Integration Count (Claimed: 3-12 packages)');
    for (const fw of this.frameworks) {
      const total = fw.totalUnique;
      const status = (total >= 3 && total <= 12) ? '✅' : '❌';
      console.log(`  ${status} ${fw.fileName}: ${total} integrations`);
      if (total < 3 || total > 12) {
        issues.push({
          check: 'Integration Range',
          severity: 'HIGH',
          issue: `${fw.fileName} has ${total} integrations (outside 3-12 range)`,
        });
      }
    }
    console.log('');

    // Check 3: Mock detection (should be production-ready)
    console.log('CHECK 3: Production Readiness (No Mocks)');
    for (const fw of this.frameworks) {
      const status = fw.hasMocks ? '❌' : '✅';
      console.log(`  ${status} ${fw.fileName}: ${fw.hasMocks ? 'Contains mocks' : 'No mocks'}`);
      if (fw.hasMocks) {
        issues.push({
          check: 'Mock Detection',
          severity: 'MEDIUM',
          issue: `${fw.fileName} contains mock implementations`,
        });
      }
    }
    console.log('');

    // Check 4: OTEL in business logic (anti-pattern)
    console.log('CHECK 4: Separation of Concerns (OTEL in business logic)');
    for (const fw of this.frameworks) {
      const status = fw.hasOTEL ? '⚠️' : '✅';
      console.log(`  ${status} ${fw.fileName}: ${fw.hasOTEL ? 'Has OTEL' : 'Clean'}`);
      if (fw.hasOTEL) {
        issues.push({
          check: 'OTEL Separation',
          severity: 'LOW',
          issue: `${fw.fileName} may have OTEL in business logic`,
        });
      }
    }
    console.log('');

    // Check 5: File size (should be <500 lines per CLAUDE.md)
    console.log('CHECK 5: File Size (Recommended: <500 lines)');
    for (const fw of this.frameworks) {
      const status = fw.lineCount <= 500 ? '✅' : '⚠️';
      console.log(`  ${status} ${fw.fileName}: ${fw.lineCount} lines`);
      if (fw.lineCount > 500) {
        issues.push({
          check: 'File Size',
          severity: 'LOW',
          issue: `${fw.fileName} has ${fw.lineCount} lines (>500)`,
        });
      }
    }
    console.log('');

    return issues;
  }

  /**
   * Generate final report
   */
  generateReport() {
    const issues = this.adversarialCheck();

    console.log('\n╔════════════════════════════════════════════════════════════╗');
    console.log('║ ARCHITECTURE COHERENCE SCORE                               ║');
    console.log('╚════════════════════════════════════════════════════════════╝\n');

    // Scoring weights
    const scores = {
      frameworkCount: this.frameworks.length >= 10 ? 20 : (this.frameworks.length / 10) * 20,
      integrationRange: 0,
      productionReady: 0,
      separation: 0,
      fileSize: 0,
    };

    // Integration range score
    const validIntegrations = this.frameworks.filter(fw =>
      fw.totalUnique >= 3 && fw.totalUnique <= 12
    ).length;
    scores.integrationRange = (validIntegrations / this.frameworks.length) * 25;

    // Production ready score
    const noMocks = this.frameworks.filter(fw => !fw.hasMocks).length;
    scores.productionReady = (noMocks / this.frameworks.length) * 20;

    // Separation of concerns score
    const noOTEL = this.frameworks.filter(fw => !fw.hasOTEL).length;
    scores.separation = (noOTEL / this.frameworks.length) * 20;

    // File size score
    const goodSize = this.frameworks.filter(fw => fw.lineCount <= 500).length;
    scores.fileSize = (goodSize / this.frameworks.length) * 15;

    const totalScore = Math.round(
      scores.frameworkCount +
      scores.integrationRange +
      scores.productionReady +
      scores.separation +
      scores.fileSize
    );

    console.log('Score Breakdown:');
    console.log(`  Framework Count (20): ${Math.round(scores.frameworkCount)}/20`);
    console.log(`  Integration Range (25): ${Math.round(scores.integrationRange)}/25`);
    console.log(`  Production Ready (20): ${Math.round(scores.productionReady)}/20`);
    console.log(`  Separation of Concerns (20): ${Math.round(scores.separation)}/20`);
    console.log(`  File Size (15): ${Math.round(scores.fileSize)}/15`);
    console.log(`\n  TOTAL COHERENCE SCORE: ${totalScore}/100`);

    console.log(`\n  Grade: ${this.getGrade(totalScore)}`);
    console.log(`  Status: ${totalScore >= 80 ? '✅ PASS' : '❌ FAIL'}\n`);

    // Risk assessment
    console.log('╔════════════════════════════════════════════════════════════╗');
    console.log('║ RISK ASSESSMENT                                            ║');
    console.log('╚════════════════════════════════════════════════════════════╝\n');

    const critical = issues.filter(i => i.severity === 'CRITICAL');
    const high = issues.filter(i => i.severity === 'HIGH');
    const medium = issues.filter(i => i.severity === 'MEDIUM');
    const low = issues.filter(i => i.severity === 'LOW');

    console.log(`Issues Found: ${issues.length}`);
    console.log(`  - CRITICAL: ${critical.length}`);
    console.log(`  - HIGH: ${high.length}`);
    console.log(`  - MEDIUM: ${medium.length}`);
    console.log(`  - LOW: ${low.length}\n`);

    if (critical.length > 0) {
      console.log('CRITICAL ISSUES:');
      critical.forEach(i => console.log(`  ❌ ${i.issue}`));
      console.log('');
    }

    if (high.length > 0) {
      console.log('HIGH PRIORITY ISSUES:');
      high.forEach(i => console.log(`  ⚠️  ${i.issue}`));
      console.log('');
    }

    return {
      score: totalScore,
      issues,
      frameworks: this.frameworks,
      recommendation: this.getRecommendation(totalScore, issues),
    };
  }

  getGrade(score) {
    if (score >= 90) return 'A (Excellent)';
    if (score >= 80) return 'B (Good)';
    if (score >= 70) return 'C (Acceptable)';
    if (score >= 60) return 'D (Needs Improvement)';
    return 'F (Failing)';
  }

  getRecommendation(score, issues) {
    const critical = issues.filter(i => i.severity === 'CRITICAL').length;

    if (critical > 0) {
      return 'BLOCK: Critical issues must be resolved before proceeding.';
    } else if (score < 80) {
      return 'REVIEW REQUIRED: Architecture does not meet quality standards.';
    } else {
      return 'APPROVED: Architecture meets quality standards.';
    }
  }
}

// Run analysis
const analyzer = new ArchitectureAnalyzer();
analyzer.analyze();
analyzer.generateDependencyMatrix();
const report = analyzer.generateReport();

console.log('╔════════════════════════════════════════════════════════════╗');
console.log('║ FINAL RECOMMENDATION                                       ║');
console.log('╚════════════════════════════════════════════════════════════╝\n');
console.log(`  ${report.recommendation}\n`);

// Exit with appropriate code
process.exit(report.score >= 80 && report.issues.filter(i => i.severity === 'CRITICAL').length === 0 ? 0 : 1);
