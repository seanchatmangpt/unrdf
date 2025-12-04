/**
 * @file useQInvariants.mjs
 * @description React hook for Q-invariants: preservation laws for thesis quality
 * Validates 7 core scholarly invariants across all shards
 */

import { _useState, useCallback, useMemo } from 'react';

/**
 * useQInvariants hook
 *
 * Manages Q-invariants: 7 preservation laws that every thesis must satisfy.
 *
 * Q1: Truthfulness  - Claims backed by evidence or citation
 * Q2: Clarity       - Readable, accessible to target audience
 * Q3: Novelty       - Not just repeating prior work
 * Q4: Coherence     - Sections logically connect
 * Q5: Completeness  - All research questions answered
 * Q6: Proportionality - Important topics get proportional space
 * Q7: Accessibility - Meets audience comprehension level
 *
 * @param {Δ_Shard[]} shards - All shards
 * @param {Object} config - Configuration options
 * @returns {Object} Q-invariant operations
 * @returns {Object} return.invariants - All 7 invariants with scores
 * @returns {Object} return.scores - Individual scores (0-1)
 * @returns {string[]} return.failing - List of failing invariants
 * @returns {Function} return.checkAll - Validate all invariants
 * @returns {number} return.overallScore - Weighted average (0-1)
 *
 * @example
 * const { scores, failing, checkAll } = useQInvariants(shards);
 * if (Object.values(scores).every(s => s > 0.8)) {
 *   console.log('All invariants satisfied!');
 * }
 */
export function useQInvariants(shards = [], config = {}) {
  const { truthThreshold = 0.8, noveltyThreshold = 0.7, clarityThreshold = 0.75 } = config;

  /**
   * Q1: Truthfulness
   * Claims must be backed by evidence or proper citation
   */
  const checkTruthfulness = useCallback(() => {
    const violations = [];

    for (const shard of shards) {
      if (['Claim', 'Proof', 'Result', 'Analysis'].includes(shard.type)) {
        const content = shard.content || '';

        // Check for citation patterns: [Author, Year] or @cite
        const citationPattern = /\[.*?(?:19|20)\d{2}.*?\]|@cite|cited|according to|studies show/i;
        const hasCitation = citationPattern.test(content);

        if (!hasCitation && shard.type !== 'Proof') {
          violations.push({
            shard: shard.id,
            type: shard.type,
            issue: 'Missing citation or evidence reference',
          });
        }
      }
    }

    const score = violations.length === 0 ? 1.0 : Math.max(0, 1 - violations.length * 0.1);
    return { score: Math.min(score, truthThreshold), violations };
  }, [shards, truthThreshold]);

  /**
   * Q2: Clarity
   * Writing should be clear and accessible
   */
  const checkClarity = useCallback(() => {
    const issues = [];

    for (const shard of shards) {
      const content = shard.content || '';

      // Heuristics for clarity
      const sentenceCount = (content.match(/[.!?]/g) || []).length;
      const wordCount = content.split(/\s+/).length;
      const avgSentenceLength = wordCount / Math.max(sentenceCount, 1);

      // Red flag: very long sentences (>30 words average)
      if (avgSentenceLength > 30) {
        issues.push({
          shard: shard.id,
          type: shard.type,
          issue: `Long sentences (avg ${avgSentenceLength.toFixed(1)} words). Consider breaking up.`,
          severity: 'warning',
        });
      }

      // Check for jargon clusters
      const technicalTerms = (content.match(/\b[A-Z]{2,}\b/g) || []).length;
      const jargonDensity = technicalTerms / Math.max(wordCount / 100, 1);

      if (jargonDensity > 5) {
        issues.push({
          shard: shard.id,
          type: shard.type,
          issue: 'High jargon density. Add plain-language explanations.',
          severity: 'warning',
        });
      }
    }

    const score = Math.max(0, 1 - issues.length * 0.15);
    return { score: Math.min(score, clarityThreshold), issues };
  }, [shards, clarityThreshold]);

  /**
   * Q3: Novelty
   * Work should advance beyond prior research
   */
  const checkNovelty = useCallback(() => {
    const issues = [];

    const claim = shards.find(s => s.type === 'Claim');
    const gap = shards.find(s => s.type === 'Gap');
    const design = shards.find(s => s.type === 'Design');

    if (!claim) {
      issues.push({ issue: 'Missing explicit claim. How is this novel?' });
    }

    if (!gap) {
      issues.push({
        issue: 'No identified gap. Where is the novelty relative to prior work?',
      });
    }

    if (!design && !shards.find(s => s.type === 'Artifact')) {
      issues.push({
        issue: 'No new design or artifact. What is novel about this work?',
      });
    }

    // Check for sufficient differentiation language
    const canon = shards.find(s => s.type === 'Canon');
    if (canon) {
      const differentiation = /unlike|novel|new|advance|extend|improve|beyond/i;
      if (!differentiation.test(canon.content || '')) {
        issues.push({
          issue: 'Canon section lacks clear differentiation from prior work.',
        });
      }
    }

    const score = 1 - Math.min(issues.length * 0.2, 1);
    return {
      score: Math.max(score, noveltyThreshold),
      issues,
    };
  }, [shards, noveltyThreshold]);

  /**
   * Q4: Coherence
   * Sections should logically connect and support each other
   */
  const checkCoherence = useCallback(() => {
    const issues = [];

    // Check for key connectors across shards
    const connectorPattern =
      /therefore|thus|as a result|consequently|in conclusion|moreover|furthermore/i;

    let hasConnectors = false;
    for (const shard of shards) {
      if (connectorPattern.test(shard.content || '')) {
        hasConnectors = true;
        break;
      }
    }

    if (!hasConnectors) {
      issues.push({
        issue: 'Few logical connectors between sections. Consider adding transition language.',
      });
    }

    // Check for forward/backward references
    let hasReferences = false;
    for (const shard of shards) {
      if (
        /(see .*section|as discussed|mentioned earlier|noted above|below)/i.test(
          shard.content || ''
        )
      ) {
        hasReferences = true;
        break;
      }
    }

    if (!hasReferences) {
      issues.push({
        issue: 'Few cross-references between sections. Add internal references for coherence.',
      });
    }

    const score = 1 - issues.length * 0.25;
    return { score: Math.max(0, score), issues };
  }, [shards]);

  /**
   * Q5: Completeness
   * All research questions should be answered
   */
  const checkCompleteness = useCallback(() => {
    const issues = [];

    // Check if intro has RQ
    const intro = shards.find(s => s.type === 'Intro');
    const rqFound = intro && /question|objective|research|purpose|aim/i.test(intro.content || '');

    if (!rqFound) {
      issues.push({
        issue: 'No clear research question identified in Introduction.',
      });
    }

    // Check if conclusion answers RQ
    const conclusion = shards.find(s => s.type === 'Conclusion');
    const answerFound =
      conclusion && /answer|address|address|resolve|conclude/i.test(conclusion.content || '');

    if (!answerFound) {
      issues.push({
        issue: 'Conclusion does not explicitly answer the research question.',
      });
    }

    // Check for results section
    if (!shards.find(s => s.type === 'Result')) {
      issues.push({ issue: 'No results section. How are findings presented?' });
    }

    const score = 1 - issues.length * 0.3;
    return { score: Math.max(0, score), issues };
  }, [shards]);

  /**
   * Q6: Proportionality
   * Important topics should get space proportional to importance
   */
  const checkProportionality = useCallback(() => {
    const issues = [];
    const totalWords = shards.reduce((sum, s) => sum + (s.wordCount || 0), 0);

    const expectedProportions = {
      Intro: 0.1, // 10%
      Method: 0.15, // 15%
      Result: 0.2, // 20%
      Discussion: 0.25, // 25%
      Conclusion: 0.1, // 10%
    };

    for (const [type, expectedProp] of Object.entries(expectedProportions)) {
      const shard = shards.find(s => s.type === type);
      if (!shard) continue;

      const actualProp = shard.wordCount / totalWords;
      const proportionDiff = Math.abs(actualProp - expectedProp);

      if (proportionDiff > 0.1) {
        // >10% difference is flag
        issues.push({
          type,
          expected: `${(expectedProp * 100).toFixed(0)}%`,
          actual: `${(actualProp * 100).toFixed(0)}%`,
          issue: `${type} is ${actualProp > expectedProp ? 'over' : 'under'}-represented.`,
        });
      }
    }

    const score = 1 - Math.min(issues.length * 0.15, 1);
    return { score: Math.max(0, score), issues };
  }, [shards]);

  /**
   * Q7: Accessibility
   * Work should be understandable by target audience
   */
  const checkAccessibility = useCallback(() => {
    const issues = [];

    // Check for glossary of terms
    const hasGlossary = shards.some(
      s => s.type === 'Intro' && /defined as|means|refers to/i.test(s.content || '')
    );

    if (!hasGlossary) {
      issues.push({
        issue: 'No key terms are defined. Add glossary or term definitions.',
      });
    }

    // Check for examples or illustrations
    const hasExamples = shards.some(s =>
      /example|figure|table|illustration/i.test(s.content || '')
    );

    if (!hasExamples) {
      issues.push({
        issue: 'No examples or illustrations. Consider adding concrete illustrations.',
      });
    }

    // Check for summary/recap sections
    const hasRecaps = shards.some(s =>
      /summary|recap|in brief|to summarize/i.test(s.content || '')
    );

    if (!hasRecaps) {
      issues.push({
        issue: 'No section summaries. Add brief recaps for accessibility.',
      });
    }

    const score = 1 - issues.length * 0.2;
    return { score: Math.max(0, score), issues };
  }, [shards]);

  /**
   * Check all 7 invariants
   */
  const checkAll = useCallback(() => {
    return {
      Q1_Truthfulness: checkTruthfulness(),
      Q2_Clarity: checkClarity(),
      Q3_Novelty: checkNovelty(),
      Q4_Coherence: checkCoherence(),
      Q5_Completeness: checkCompleteness(),
      Q6_Proportionality: checkProportionality(),
      Q7_Accessibility: checkAccessibility(),
    };
  }, [
    checkTruthfulness,
    checkClarity,
    checkNovelty,
    checkCoherence,
    checkCompleteness,
    checkProportionality,
    checkAccessibility,
  ]);

  /**
   * Compute all scores
   */
  const invariants = useMemo(() => checkAll(), [checkAll]);

  /**
   * Extract scores only
   */
  const scores = useMemo(
    () => ({
      Q1_Truthfulness: invariants.Q1_Truthfulness.score,
      Q2_Clarity: invariants.Q2_Clarity.score,
      Q3_Novelty: invariants.Q3_Novelty.score,
      Q4_Coherence: invariants.Q4_Coherence.score,
      Q5_Completeness: invariants.Q5_Completeness.score,
      Q6_Proportionality: invariants.Q6_Proportionality.score,
      Q7_Accessibility: invariants.Q7_Accessibility.score,
    }),
    [invariants]
  );

  /**
   * Find failing invariants
   */
  const failing = useMemo(() => {
    return Object.entries(scores)
      .filter(([_, score]) => score < 0.8)
      .map(([name, score]) => ({ name, score }));
  }, [scores]);

  /**
   * Overall score (weighted average)
   */
  const overallScore = useMemo(
    () => Object.values(scores).reduce((sum, score) => sum + score, 0) / 7,
    [scores]
  );

  return {
    // Data
    invariants,
    scores,
    failing,
    overallScore,

    // Operations
    checkAll,
  };
}

export default useQInvariants;
