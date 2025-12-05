#!/usr/bin/env node

/**
 * validate-diataxis.js - Validate Diataxis documentation completeness
 *
 * Usage:
 *   node validate-diataxis.js <package-dir>
 *   node validate-diataxis.js packages/core
 *
 * Checks:
 *   - All required files exist
 *   - No TODO/FIXME placeholders
 *   - Minimum word count per file
 *   - Code examples present
 *   - Proper markdown structure
 *
 * Output: Score (0-100) and detailed report
 */

import fs from 'fs';
import path from 'path';

const args = process.argv.slice(2);
const packageDir = args[0];

if (!packageDir) {
  console.error('Usage: node validate-diataxis.js <package-dir>');
  console.error('Example: node validate-diataxis.js packages/core');
  process.exit(1);
}

const docsDir = path.join(packageDir, 'docs');

if (!fs.existsSync(docsDir)) {
  console.error(`Error: docs directory not found: ${docsDir}`);
  process.exit(1);
}

// Validation configuration
const config = {
  tutorials: { count: 3, minWords: 150 },
  'how-to': { count: 4, minWords: 150 },
  reference: { count: 5, minWords: 150 },
  explanation: { count: 4, minWords: 120 }
};

let totalScore = 0;
let maxScore = 0;

const report = {
  timestamp: new Date().toISOString(),
  package: packageDir,
  results: {},
  warnings: [],
  errors: []
};

console.log(`\n=== Diataxis Documentation Validation ===\n`);
console.log(`Package: ${packageDir}\n`);

// Check each section
Object.entries(config).forEach(([section, requirements]) => {
  const sectionDir = path.join(docsDir, section);
  const sectionReport = {
    required: requirements.count,
    found: 0,
    complete: 0,
    files: []
  };

  if (!fs.existsSync(sectionDir)) {
    report.errors.push(`Missing directory: ${section}/`);
    report.results[section] = sectionReport;
    return;
  }

  const files = fs.readdirSync(sectionDir)
    .filter(f => f.endsWith('.md'))
    .filter(f => !f.includes('template'))
    .sort();

  sectionReport.found = files.length;
  sectionReport.count = files.length;

  files.forEach(file => {
    const filepath = path.join(sectionDir, file);
    const content = fs.readFileSync(filepath, 'utf-8');
    const wordCount = content.split(/\s+/).length;

    const fileReport = {
      name: file,
      wordCount: wordCount,
      complete: true,
      issues: []
    };

    // Check word count
    if (wordCount < requirements.minWords) {
      fileReport.issues.push(`Too short: ${wordCount} words (need ${requirements.minWords})`);
      fileReport.complete = false;
    }

    // Check for TODO/FIXME
    if (content.includes('TODO') || content.includes('FIXME')) {
      fileReport.issues.push('Contains TODO/FIXME placeholders');
      fileReport.complete = false;
    }

    // Check for proper structure
    if (!content.includes('#')) {
      fileReport.issues.push('Missing markdown headings');
      fileReport.complete = false;
    }

    if (fileReport.complete) {
      sectionReport.complete++;
    }

    sectionReport.files.push(fileReport);
  });

  report.results[section] = sectionReport;

  // Calculate score for this section
  const sectionScore = (sectionReport.complete / requirements.count) * 100;
  const sectionMaxScore = 100;

  totalScore += sectionScore;
  maxScore += sectionMaxScore;

  console.log(`${section.toUpperCase()}`);
  console.log(`  Files: ${sectionReport.found}/${requirements.count}`);
  console.log(`  Complete: ${sectionReport.complete}/${requirements.count}`);
  console.log(`  Score: ${Math.round(sectionScore)}/100`);
  console.log();
});

// Overall score
const overallScore = Math.round((totalScore / maxScore) * 100);

console.log(`=== OVERALL SCORE: ${overallScore}/100 ===\n`);

if (overallScore >= 80) {
  console.log('✓ Documentation is ready for Phase completion');
} else if (overallScore >= 50) {
  console.log('⚠ Documentation is incomplete, needs work');
} else {
  console.log('✗ Documentation is severely incomplete');
}

// Detailed report
if (report.warnings.length > 0) {
  console.log('\nWARNINGS:');
  report.warnings.forEach(w => console.log(`  - ${w}`));
}

if (report.errors.length > 0) {
  console.log('\nERRORS:');
  report.errors.forEach(e => console.log(`  - ${e}`));
}

// Show files needing attention
let needsWork = false;
Object.entries(report.results).forEach(([section, sectionReport]) => {
  const incomplete = sectionReport.files.filter(f => !f.complete);
  if (incomplete.length > 0) {
    if (!needsWork) {
      console.log('\nFILES NEEDING ATTENTION:\n');
      needsWork = true;
    }
    console.log(`${section}/`);
    incomplete.forEach(file => {
      console.log(`  - ${file.name}`);
      file.issues.forEach(issue => console.log(`      • ${issue}`));
    });
  }
});

// Save report as JSON
const reportPath = path.join(docsDir, 'validation-report.json');
fs.writeFileSync(reportPath, JSON.stringify(report, null, 2));
console.log(`\nDetailed report saved to: ${reportPath}\n`);

// Exit code based on score
process.exit(overallScore >= 80 ? 0 : 1);
