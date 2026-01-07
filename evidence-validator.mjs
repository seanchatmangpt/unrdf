/**
 * @file evidence-validator.mjs
 * @module mega-prompt-validator
 * @description Runtime validator for evidence-gathering swarm.
 *
 * Enforces scoring rubric, rejects weak evidence, produces final report.
 * Can be invoked standalone or integrated into orchestration system.
 */

import { SCORING_RUBRIC, scoreEvidence } from './evidence-scoring-rubric.mjs';
import { AXIOMS } from './swarm-mega-prompt-roles.mjs';

/**
 * EVIDENCE VALIDATOR CLASS
 * ======================
 *
 * Receives evidence items from all agents.
 * Applies rubric scoring.
 * Produces final report.
 */

export class EvidenceValidator {
  constructor(config = {}) {
    this.rubric = config.rubric || SCORING_RUBRIC;
    this.axioms = config.axioms || AXIOMS;
    this.acceptanceThreshold = config.threshold || 70;

    this.allEvidence = [];
    this.acceptedEvidence = [];
    this.rejectedEvidence = [];
    this.falsifications = [];
    this.supportingEvidence = {};

    // Initialize supporting evidence buckets by axiom
    Object.keys(this.axioms).forEach(axiom => {
      this.supportingEvidence[axiom] = [];
    });
  }

  /**
   * Ingest and score a single evidence item
   * @param {Object} evidence
   * @returns {Object} {item, score, breakdown, accepted}
   */
  ingestEvidence(evidence) {
    const result = scoreEvidence(evidence, this.rubric);
    const item = { ...evidence, score: result.score, breakdown: result.breakdown };

    this.allEvidence.push(item);

    // Check global rejection gates
    if (result.score === 0) {
      item.acceptanceStatus = 'REJECTED';
      item.rejectionReason = result.reason;
      this.rejectedEvidence.push(item);
      return { item, accepted: false, score: 0, reason: result.reason };
    }

    // Check threshold
    if (result.score < this.acceptanceThreshold) {
      item.acceptanceStatus = 'REJECTED';
      item.rejectionReason = `Score ${result.score} < threshold ${this.acceptanceThreshold}`;
      this.rejectedEvidence.push(item);
      return { item, accepted: false, score: result.score, reason: item.rejectionReason };
    }

    // ACCEPTANCE
    item.acceptanceStatus = 'ACCEPTED';
    this.acceptedEvidence.push(item);

    // Categorize
    if (evidence.falsificationStrength && evidence.falsificationStrength !== 'NONE') {
      this.falsifications.push(item);
    } else {
      // Supporting evidence
      const axiom = evidence.relevantAxiom;
      if (axiom && this.supportingEvidence[axiom]) {
        this.supportingEvidence[axiom].push(item);
      }
    }

    return { item, accepted: true, score: result.score, reason: 'ACCEPTED' };
  }

  /**
   * Batch ingest multiple evidence items
   * @param {Array} evidenceArray
   * @returns {Object} summary
   */
  ingestBatch(evidenceArray) {
    const results = [];
    evidenceArray.forEach(item => {
      results.push(this.ingestEvidence(item));
    });

    return {
      total: evidenceArray.length,
      accepted: results.filter(r => r.accepted).length,
      rejected: results.filter(r => !r.accepted).length,
      details: results,
    };
  }

  /**
   * Generate final report
   * @returns {Object} comprehensive report
   */
  generateReport() {
    const timestamp = new Date().toISOString();
    const comprehensiveness = this.calculateComprehensiveness();
    const gaps = this.identifyGaps();

    // FALSIFICATIONS FIRST (publication order)
    const reportFalsifications = this.falsifications.map(item => ({
      strength: item.falsificationStrength,
      axiom: item.relevantAxiom,
      claim: item.claim,
      source: item.source,
      score: item.score,
      interpretation: item.interpretation || '',
      formalStatement: item.formalStatement || '',
    }));

    // SUPPORTING EVIDENCE (grouped by axiom)
    const reportSupporting = {};
    Object.keys(this.supportingEvidence).forEach(axiom => {
      reportSupporting[axiom] = this.supportingEvidence[axiom].map(item => ({
        claim: item.claim,
        source: item.source,
        score: item.score,
        category: item.category || 'GENERAL',
        formalStatement: item.formalStatement || '',
      }));
    });

    return {
      metadata: {
        timestamp,
        totalEvidence: this.allEvidence.length,
        acceptedEvidence: this.acceptedEvidence.length,
        rejectedEvidence: this.rejectedEvidence.length,
        acceptanceRate: (this.acceptedEvidence.length / this.allEvidence.length * 100).toFixed(1) + '%',
        falsifications: this.falsifications.length,
        supportingItems: this.acceptedEvidence.length - this.falsifications.length,
        comprehensiveness,
        comprehensivenessStatus: comprehensiveness >= 60 ? 'ADEQUATE' : 'INCOMPLETE',
      },

      falsifications: {
        order: 'FIRST (before supporting evidence per mega-prompt)',
        count: reportFalsifications.length,
        items: reportFalsifications,
        summary: reportFalsifications.length > 0
          ? `${reportFalsifications.length} strong counter-claim(s) found - thesis requires revision`
          : 'No strong falsifications (all major counter-claims failed)',
      },

      supportingEvidence: {
        byAxiom: reportSupporting,
        summary: this.summarizeSupportingEvidence(),
      },

      axiomAssessment: {
        SCALE: this.assessAxiom('SCALE'),
        REVERSIBILITY: this.assessAxiom('REVERSIBILITY'),
        DETERMINISM: this.assessAxiom('DETERMINISM'),
        COORDINATION: this.assessAxiom('COORDINATION'),
        MINIMALITY: this.assessAxiom('MINIMALITY'),
      },

      gaps: {
        underRepresented: gaps.underRepresented,
        recommendation: gaps.recommendation,
      },

      statistics: {
        averageScore: (this.acceptedEvidence.reduce((a, b) => a + b.score, 0) / this.acceptedEvidence.length).toFixed(1),
        scoreDistribution: this.getScoreDistribution(),
        evidenceClassBreakdown: this.getClassBreakdown(),
        domainCoverage: this.getDomainCoverage(),
      },

      nextSteps: this.recommendNextSteps(),
    };
  }

  /**
   * Calculate comprehensiveness (% of thesis space covered)
   * @returns {number} 0-100
   */
  calculateComprehensiveness() {
    const targetDomains = 15; // From mega-prompt
    const coveredDomains = new Set();

    this.acceptedEvidence.forEach(item => {
      if (item.relevantAxiom) coveredDomains.add(item.relevantAxiom);
      if (item.domain) coveredDomains.add(item.domain);
    });

    return Math.min(100, Math.round((coveredDomains.size / targetDomains) * 100));
  }

  /**
   * Identify gaps (axioms with <3 supporting items)
   * @returns {Object}
   */
  identifyGaps() {
    const gaps = [];
    const recommendation = [];

    Object.keys(this.supportingEvidence).forEach(axiom => {
      const count = this.supportingEvidence[axiom].length;
      if (count < 3) {
        gaps.push({ axiom, itemsFound: count, itemsNeeded: 3 - count });
        recommendation.push(`${axiom}: acquire ${3 - count} more evidence item(s)`);
      }
    });

    return { underRepresented: gaps, recommendation };
  }

  /**
   * Assess coverage for a single axiom
   * @param {string} axiom
   * @returns {Object}
   */
  assessAxiom(axiom) {
    const supporting = this.supportingEvidence[axiom] || [];
    const falsifyingThisAxiom = this.falsifications.filter(f => f.relevantAxiom === axiom);

    return {
      axiom,
      supportingItems: supporting.length,
      falsifyingItems: falsifyingThisAxiom.length,
      averageScore: supporting.length > 0
        ? (supporting.reduce((a, b) => a + b.score, 0) / supporting.length).toFixed(1)
        : 'N/A',
      status: supporting.length >= 3
        ? 'WELL_COVERED'
        : supporting.length > 0
          ? 'PARTIALLY_COVERED'
          : 'UNCOVERED',
    };
  }

  /**
   * Summarize supporting evidence by strength
   * @returns {Object}
   */
  summarizeSupportingEvidence() {
    const strong = this.acceptedEvidence.filter(e => e.score >= 85).length;
    const medium = this.acceptedEvidence.filter(e => e.score >= 70 && e.score < 85).length;

    return {
      strongItems: strong,
      mediumItems: medium,
      totalSupporting: strong + medium,
      thesis: strong > medium
        ? 'Thesis well-supported by high-quality evidence'
        : 'Thesis supported by moderate-quality evidence',
    };
  }

  /**
   * Score distribution
   * @returns {Object}
   */
  getScoreDistribution() {
    const buckets = {
      '90-100': 0,
      '80-89': 0,
      '70-79': 0,
      '<70': 0,
    };

    this.allEvidence.forEach(e => {
      if (e.score >= 90) buckets['90-100']++;
      else if (e.score >= 80) buckets['80-89']++;
      else if (e.score >= 70) buckets['70-79']++;
      else buckets['<70']++;
    });

    return buckets;
  }

  /**
   * Evidence class breakdown
   * @returns {Object}
   */
  getClassBreakdown() {
    const breakdown = { A: 0, B: 0, C: 0 };
    this.allEvidence.forEach(e => {
      if (e.evidenceClass === 'A') breakdown.A++;
      else if (e.evidenceClass === 'B') breakdown.B++;
      else breakdown.C++;
    });
    return breakdown;
  }

  /**
   * Domain coverage map
   * @returns {Object}
   */
  getDomainCoverage() {
    const domains = {};
    this.acceptedEvidence.forEach(e => {
      const domain = e.domain || 'uncategorized';
      domains[domain] = (domains[domain] || 0) + 1;
    });
    return domains;
  }

  /**
   * Recommend next steps based on findings
   * @returns {Array<string>}
   */
  recommendNextSteps() {
    const steps = [];

    if (this.falsifications.length > 0) {
      steps.push('CRITICAL: Publish falsifications immediately and revise thesis');
      steps.push(`Found ${this.falsifications.length} counter-claim(s) that score ≥${this.acceptanceThreshold}`);
    } else {
      steps.push('No strong falsifications found - thesis appears viable');
    }

    const gaps = this.identifyGaps();
    if (gaps.underRepresented.length > 0) {
      steps.push(`Fill axiom gaps: ${gaps.recommendation.join(', ')}`);
    }

    if (this.calculateComprehensiveness() < 60) {
      steps.push('Comprehensiveness <60% - recommend Phase 5 domain expansion');
    } else {
      steps.push('Comprehensiveness adequate - thesis space reasonably explored');
    }

    steps.push('Conduct peer review of evidence quality and scoring consistency');

    return steps;
  }

  /**
   * Export report to JSON
   * @returns {string} JSON
   */
  toJSON() {
    return JSON.stringify(this.generateReport(), null, 2);
  }
}

/**
 * STANDALONE EXECUTION EXAMPLE
 * ===========================
 *
 * Usage:
 *   const validator = new EvidenceValidator();
 *   validator.ingestBatch(allEvidenceArray);
 *   const report = validator.generateReport();
 *   console.log(JSON.stringify(report, null, 2));
 */

export async function runValidationPipeline(evidenceFilePath) {
  try {
    // Read evidence file
    const { readFileSync } = await import('fs');
    const rawData = readFileSync(evidenceFilePath, 'utf-8');
    const allEvidence = JSON.parse(rawData);

    // Create validator
    const validator = new EvidenceValidator({ threshold: 70 });

    // Ingest and score
    const batchResult = validator.ingestBatch(allEvidence);
    console.log(`\n📊 INGESTION SUMMARY`);
    console.log(`   Total items: ${batchResult.total}`);
    console.log(`   Accepted: ${batchResult.accepted}`);
    console.log(`   Rejected: ${batchResult.rejected}`);

    // Generate report
    const report = validator.generateReport();

    // Output
    console.log(`\n📈 VALIDATION REPORT`);
    console.log(`   Timestamp: ${report.metadata.timestamp}`);
    console.log(`   Comprehensiveness: ${report.metadata.comprehensiveness}%`);
    console.log(`   Falsifications: ${report.falsifications.count}`);
    console.log(`   Supporting items: ${report.metadata.supportingItems}`);

    if (report.falsifications.count > 0) {
      console.log(`\n🚨 FALSIFICATIONS (PUBLISH FIRST):`);
      report.falsifications.items.forEach((item, i) => {
        console.log(`   ${i + 1}. [${item.strength}] ${item.claim}`);
        console.log(`      Source: ${item.source}`);
        console.log(`      Score: ${item.score}/100`);
      });
    }

    console.log(`\n✅ AXIOM ASSESSMENT`);
    Object.entries(report.axiomAssessment).forEach(([axiom, assessment]) => {
      console.log(`   ${axiom}: ${assessment.status} (${assessment.supportingItems} items)`);
    });

    console.log(`\n📋 NEXT STEPS`);
    report.nextSteps.forEach((step, i) => {
      console.log(`   ${i + 1}. ${step}`);
    });

    // Save full report
    const { writeFileSync } = await import('fs');
    const reportPath = evidenceFilePath.replace('.json', '-report.json');
    writeFileSync(reportPath, JSON.stringify(report, null, 2));
    console.log(`\n💾 Full report saved to: ${reportPath}`);

    return report;
  } catch (error) {
    console.error('❌ Validation failed:', error.message);
    throw error;
  }
}

/**
 * BATCH REJECTION ANALYZER
 * ========================
 *
 * Analyzes why items were rejected and suggests improvements
 */

export class RejectionAnalyzer {
  constructor(validator) {
    this.validator = validator;
  }

  analyze() {
    const rejectionReasons = {};

    this.validator.rejectedEvidence.forEach(item => {
      const reason = item.rejectionReason || 'unknown';
      rejectionReasons[reason] = (rejectionReasons[reason] || 0) + 1;
    });

    const suggestions = this.suggestImprovements();

    return {
      totalRejected: this.validator.rejectedEvidence.length,
      rejectionReasonBreakdown: rejectionReasons,
      suggestions,
    };
  }

  suggestImprovements() {
    const suggestions = [];

    this.validator.rejectedEvidence.forEach(item => {
      const breakdown = item.breakdown || {};

      if (breakdown.classA === 0) {
        suggestions.push(`${item.claim}: Find peer-reviewed primary sources`);
      }
      if (breakdown.primarySource < 10) {
        suggestions.push(`${item.claim}: Increase primary source content (currently <30%)`);
      }
      if (breakdown.quantitative === 0) {
        suggestions.push(`${item.claim}: Add quantitative bounds or measurements`);
      }
      if (breakdown.relevance === 0) {
        suggestions.push(`${item.claim}: Connect to at least one axiom (SCALE|REVERSIBILITY|...)`);
      }
    });

    return suggestions;
  }
}
