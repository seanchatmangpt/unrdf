#!/usr/bin/env node

/**
 * @file run-validator.mjs
 * Run evidence validator on phase 1-3 evidence, produce final report
 */

import { readFileSync, writeFileSync } from 'fs';
import { EvidenceValidator, RejectionAnalyzer } from './evidence-validator.mjs';

console.log('\n🔬 MEGA-PROMPT EVIDENCE VALIDATOR');
console.log('━'.repeat(80));

// 1. Load evidence
console.log('\n📂 Loading evidence from phase-1.json...');
const rawEvidence = JSON.parse(readFileSync('./evidence-phase-1.json', 'utf-8'));
console.log(`✓ Loaded ${rawEvidence.length} evidence items`);

// 2. Create validator
console.log('\n⚙️  Initializing validator (threshold: 70/100)...');
const validator = new EvidenceValidator({ threshold: 70 });

// 3. Ingest and score all evidence
console.log('\n📊 Scoring evidence...');
const batchResult = validator.ingestBatch(rawEvidence);
console.log(`✓ Total: ${batchResult.total}`);
console.log(`✓ Accepted (≥70): ${batchResult.accepted}`);
console.log(`✓ Rejected (<70): ${batchResult.rejected}`);

// 4. Generate report
console.log('\n📈 Generating final report...');
const report = validator.generateReport();

// 5. Print summary
console.log('\n' + '═'.repeat(80));
console.log('EVIDENCE VALIDATION REPORT');
console.log('═'.repeat(80));

console.log('\n📋 METADATA');
console.log(`  Timestamp: ${report.metadata.timestamp}`);
console.log(`  Total Evidence: ${report.metadata.totalEvidence}`);
console.log(`  Accepted: ${report.metadata.acceptedEvidence}`);
console.log(`  Rejected: ${report.metadata.rejectedEvidence}`);
console.log(`  Acceptance Rate: ${report.metadata.acceptanceRate}`);
console.log(`  Falsifications Found: ${report.metadata.falsifications}`);
console.log(`  Supporting Items: ${report.metadata.supportingItems}`);
console.log(`  Comprehensiveness: ${report.metadata.comprehensiveness}%`);
console.log(`  Status: ${report.metadata.comprehensivenessStatus}`);

console.log('\n🚨 FALSIFICATIONS (Published First)');
if (report.falsifications.count === 0) {
  console.log('  ✓ No strong falsifications (score ≥70)');
} else {
  console.log(`  Found: ${report.falsifications.count} counter-claim(s)\n`);
  report.falsifications.items.forEach((item, i) => {
    console.log(`  ${i + 1}. [${item.strength.toUpperCase()}] ${item.claim}`);
    console.log(`     Source: ${item.source}`);
    console.log(`     Axiom violated: ${item.axiom}`);
    console.log(`     Score: ${item.score}/100`);
    console.log(`     ${item.interpretation}`);
    console.log('');
  });
}

console.log('\n✅ AXIOM ASSESSMENT');
Object.entries(report.axiomAssessment).forEach(([axiom, assessment]) => {
  const icon =
    assessment.status === 'WELL_COVERED' ? '✓' :
    assessment.status === 'PARTIALLY_COVERED' ? '◐' : '✗';
  console.log(`  ${icon} ${axiom}: ${assessment.status}`);
  console.log(`     Supporting: ${assessment.supportingItems} items (avg score: ${assessment.averageScore})`);
  console.log(`     Falsifying: ${assessment.falsifyingItems} items`);
});

console.log('\n📊 EVIDENCE STATISTICS');
console.log(`  Average Score: ${report.statistics.averageScore}/100`);
console.log(`  Score Distribution:`);
Object.entries(report.statistics.scoreDistribution).forEach(([bucket, count]) => {
  console.log(`    ${bucket}: ${count} items`);
});
console.log(`  Evidence Class Breakdown:`);
Object.entries(report.statistics.evidenceClassBreakdown).forEach(([cls, count]) => {
  console.log(`    Class ${cls}: ${count} items`);
});

if (report.gaps.underRepresented.length > 0) {
  console.log('\n⚠️  GAPS');
  report.gaps.underRepresented.forEach(gap => {
    console.log(`  ${gap.axiom}: ${gap.itemsFound} items (need ${gap.itemsNeeded} more)`);
  });
} else {
  console.log('\n✓ No axiom gaps - all 5 axioms well-covered');
}

console.log('\n📋 SUPPORTING EVIDENCE BY AXIOM');
Object.entries(report.supportingEvidence.byAxiom).forEach(([axiom, items]) => {
  if (items.length > 0) {
    console.log(`\n  ${axiom} (${items.length} items):`);
    items.forEach((item, i) => {
      console.log(`    ${i + 1}. ${item.claim}`);
      console.log(`       Score: ${item.score}/100 | Category: ${item.category}`);
    });
  }
});

console.log('\n🎯 NEXT STEPS');
report.nextSteps.forEach((step, i) => {
  console.log(`  ${i + 1}. ${step}`);
});

// 6. Save full report to JSON
const reportPath = './evidence-report-final.json';
writeFileSync(reportPath, JSON.stringify(report, null, 2));
console.log(`\n💾 Full report saved to: ${reportPath}`);

// 7. Analyze rejections
console.log('\n' + '─'.repeat(80));
console.log('REJECTION ANALYSIS');
console.log('─'.repeat(80));
const analyzer = new RejectionAnalyzer(validator);
const rejectionAnalysis = analyzer.analyze();
console.log(`\nTotal Rejected: ${rejectionAnalysis.totalRejected}`);
console.log('Rejection Reasons:');
Object.entries(rejectionAnalysis.rejectionReasonBreakdown).forEach(([reason, count]) => {
  console.log(`  ${reason}: ${count}`);
});

if (rejectionAnalysis.suggestions.length > 0) {
  console.log('\nSuggestions for Improvement:');
  rejectionAnalysis.suggestions.slice(0, 5).forEach((suggestion, i) => {
    console.log(`  ${i + 1}. ${suggestion}`);
  });
  if (rejectionAnalysis.suggestions.length > 5) {
    console.log(`  ... and ${rejectionAnalysis.suggestions.length - 5} more`);
  }
}

console.log('\n' + '═'.repeat(80));
console.log('✅ VALIDATION COMPLETE');
console.log('═'.repeat(80) + '\n');

// Return report for programmatic use
export default report;
