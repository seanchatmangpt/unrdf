/**
 * Socratic Command - Intelligent Assumption Extraction
 *
 * Implements Socratic AI for challenging assumptions and preventing groupthink.
 *
 * Key Capabilities:
 * - Extract implicit assumptions from statements
 * - Generate Socratic questions for clarification
 * - Evidence-based reasoning
 * - Detect logical fallacies and vague claims
 *
 * Based on 2030 capability: "Death of Groupthink"
 *
 * @module cli/commands/socratic
 */

import { defineCommand } from 'citty';
import { SocraticAgent } from '@unrdf/decision-fabric';

/**
 * Format Socratic analysis result for display
 */
function formatAnalysis(analysis) {
  const lines = [];

  lines.push('');
  lines.push('═══════════════════════════════════════════════════════');
  lines.push('  SOCRATIC AI - ASSUMPTION ANALYSIS');
  lines.push('═══════════════════════════════════════════════════════');
  lines.push('');

  // Original Statement
  lines.push('Statement:');
  lines.push(`  "${analysis.original_statement}"`);
  lines.push('');

  // Assumptions
  if (analysis.assumptions && analysis.assumptions.length > 0) {
    lines.push('─────────────────────────────────────────────────────');
    lines.push(`ASSUMPTIONS EXTRACTED: ${analysis.assumptions.length}`);
    lines.push('─────────────────────────────────────────────────────');
    lines.push('');

    analysis.assumptions.forEach((assumption, i) => {
      const classEmoji = {
        'STRONG': '🟢',
        'MODERATE': '🟡',
        'WEAK': '🟠',
        'UNVALIDATED': '🔴',
        'REFUTED': '⛔'
      };

      lines.push(`${i + 1}. ${classEmoji[assumption.classification]} [${assumption.classification}] ${assumption.statement}`);
      lines.push(`   Confidence: ${(assumption.confidence * 100).toFixed(0)}%`);

      if (assumption.evidence_for.length > 0) {
        lines.push(`   Evidence FOR: ${assumption.evidence_for.length} sources`);
      }

      if (assumption.evidence_against.length > 0) {
        lines.push(`   Evidence AGAINST: ${assumption.evidence_against.length} sources`);
      }

      if (assumption.evidence_for.length > 0 || assumption.evidence_against.length > 0) {
        lines.push(`   Evidence Strength: ${(assumption.evidenceStrength * 100).toFixed(0)}%`);
      }

      lines.push('');
    });
  } else {
    lines.push('✅ No problematic assumptions detected');
    lines.push('');
  }

  // Challenges
  if (analysis.challenges && analysis.challenges.length > 0) {
    lines.push('─────────────────────────────────────────────────────');
    lines.push(`SOCRATIC CHALLENGES: ${analysis.challenges.length}`);
    lines.push('─────────────────────────────────────────────────────');
    lines.push('');

    const bySeverity = {
      HIGH: [],
      MEDIUM: [],
      LOW: []
    };

    analysis.challenges.forEach(c => {
      bySeverity[c.severity].push(c);
    });

    // High severity first
    if (bySeverity.HIGH.length > 0) {
      lines.push('🔴 HIGH SEVERITY (BLOCKERS):');
      lines.push('');
      bySeverity.HIGH.forEach((c, i) => {
        lines.push(`  ${i + 1}. [${c.type}] ${c.question}`);
        lines.push('');
      });
    }

    // Medium severity
    if (bySeverity.MEDIUM.length > 0) {
      lines.push('🟡 MEDIUM SEVERITY (WARNINGS):');
      lines.push('');
      bySeverity.MEDIUM.forEach((c, i) => {
        lines.push(`  ${i + 1}. [${c.type}] ${c.question}`);
        lines.push('');
      });
    }

    // Low severity
    if (bySeverity.LOW.length > 0) {
      lines.push('🟢 LOW SEVERITY (INFORMATIONAL):');
      lines.push('');
      bySeverity.LOW.forEach((c, i) => {
        lines.push(`  ${i + 1}. [${c.type}] ${c.question}`);
        lines.push('');
      });
    }
  } else {
    lines.push('✅ No challenges generated - statement is well-formed');
    lines.push('');
  }

  // Alternatives
  if (analysis.alternatives && analysis.alternatives.length > 0) {
    lines.push('─────────────────────────────────────────────────────');
    lines.push('EVIDENCE-BASED ALTERNATIVES');
    lines.push('─────────────────────────────────────────────────────');
    lines.push('');

    analysis.alternatives.forEach((alt, i) => {
      lines.push(`${i + 1}. ${alt.solution}`);
      lines.push(`   Value: ${alt.value}  Cost: ${alt.cost}  Efficiency: ${alt.efficiency.toFixed(2)}`);
      lines.push(`   Evidence Strength: ${(alt.evidence_strength * 100).toFixed(0)}%`);
      lines.push('');
    });
  }

  // Recommendation
  lines.push('─────────────────────────────────────────────────────');
  lines.push('RECOMMENDATION');
  lines.push('─────────────────────────────────────────────────────');
  lines.push('');

  const rec = analysis.recommendation;

  if (rec.proceed) {
    lines.push('✅ PROCEED');
  } else {
    lines.push('⛔ DO NOT PROCEED - ADDRESS CHALLENGES FIRST');
  }

  lines.push('');
  lines.push(`Reason: ${rec.reason}`);
  lines.push(`Action: ${rec.action}`);

  if (rec.alternatives) {
    lines.push('');
    lines.push(`Alternatives: ${rec.alternatives}`);
  }

  lines.push('');
  lines.push('═══════════════════════════════════════════════════════');
  lines.push('');

  return lines.join('\n');
}

/**
 * Main socratic command
 */
export const socraticCommand = defineCommand({
  meta: {
    name: 'socratic',
    description: 'Challenge assumptions using Socratic AI reasoning',
  },
  args: {
    statement: {
      type: 'positional',
      description: 'Statement to analyze for assumptions',
      required: true,
    },
    output: {
      type: 'string',
      description: 'Output format (text|json)',
      default: 'text',
      alias: 'o',
    },
  },
  async run({ args }) {
    try {
      console.error('Analyzing statement with Socratic AI...');
      console.error('');

      // Create Socratic agent
      const agent = new SocraticAgent({ knowledgeStore: null });

      // Analyze statement
      const analysis = await agent.analyze(args.statement);

      // Output result
      if (args.output === 'json') {
        console.log(JSON.stringify(analysis, null, 2));
      } else {
        console.log(formatAnalysis(analysis));
      }

      // Exit code based on recommendation
      if (!analysis.recommendation.proceed) {
        process.exit(1);
      }

    } catch (error) {
      console.error('');
      console.error('❌ Error analyzing statement:');
      console.error(error.message);
      console.error('');
      process.exit(1);
    }
  },
});
