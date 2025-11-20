/**
 * @fileoverview HTF (Hyper-Thesis Framework) - Complete Example
 * Shows how to use Λ-scheduling, Π-merging, and Γ-validation for academic writing
 */

import {
  useLambdaScheduling,
  usePiProfile,
  useGammaChecker,
  useHTFFramework,
  DeltaFamilies,
  StandardInvariants
} from '../src/react-hooks/htf/index.mjs';

// ============================================
// EXAMPLE: PhD Thesis on Knowledge Graphs
// ============================================

/**
 * Define thesis shards (from the 2028 revolution thesis)
 */
const thesisShards = [
  // IMRaD family
  {
    id: 'intro',
    family: 'imrad',
    label: 'Introduction: Knowledge Graph Limitations',
    content: 'Current knowledge graphs are centralized, static, and lack semantic understanding. We propose a unified architecture addressing these limitations...',
    weight: 0.8,
    dependencies: ['problem', 'gap'],
    metadata: { pages: 15 }
  },
  {
    id: 'method',
    family: 'imrad',
    label: 'Methods: HTF Architecture Design',
    content: 'The Hyper-Thesis Framework integrates 7 academic modes through three operations: Λ-ordering, Π-merging, Γ-globalization...',
    weight: 0.9,
    dependencies: ['intro'],
    metadata: { pages: 20 }
  },
  {
    id: 'result',
    family: 'imrad',
    label: 'Results: Implementation & Metrics',
    content: 'We implemented the HTF framework with React hooks providing scheduling, coherence analysis, and validation...',
    weight: 0.85,
    dependencies: ['method', 'artifact'],
    metadata: { pages: 18 }
  },
  {
    id: 'discuss',
    family: 'imrad',
    label: 'Discussion: Implications & Future Work',
    content: 'Our results demonstrate that knowledge graphs can be unified across 7 writing modes while maintaining coherence...',
    weight: 0.8,
    dependencies: ['result', 'insight'],
    metadata: { pages: 15 }
  },

  // Argument family
  {
    id: 'claim',
    family: 'argument',
    label: 'Claim: Unified Thesis Framework is Necessary',
    content: 'Current academic writing lacks a unifying framework that handles multiple modes coherently...',
    weight: 0.95,
    dependencies: ['problem'],
    metadata: { support: 'literature' }
  },
  {
    id: 'ground',
    family: 'argument',
    label: 'Grounds: Evidence from 50+ Thesis Studies',
    content: 'Analysis of 50 recent PhD theses shows fragmentation across IMRaD, papers, argument, and narrative...',
    weight: 0.85,
    dependencies: ['claim', 'canon'],
    metadata: { sources: 50 }
  },
  {
    id: 'proof',
    family: 'argument',
    label: 'Proof: HTF Achieves >0.9 Coherence',
    content: 'We demonstrate that our framework achieves 91% coherence across 7 modes with Π-merging...',
    weight: 0.9,
    dependencies: ['ground', 'result'],
    metadata: { empirical: true }
  },
  {
    id: 'objection',
    family: 'argument',
    label: 'Objection: Complexity & Learning Curve',
    content: 'Critics may argue that HTF is too complex for thesis writers accustomed to linear narratives...',
    weight: 0.6,
    dependencies: ['proof'],
    metadata: { counterargument: true }
  },
  {
    id: 'reply',
    family: 'argument',
    label: 'Reply: Automation Reduces Complexity',
    content: 'We show that automated Λ-scheduling and Γ-validation reduce effective complexity...',
    weight: 0.85,
    dependencies: ['objection'],
    metadata: { rebuttal: true }
  },

  // Contribution family
  {
    id: 'gap',
    family: 'contribution',
    label: 'Gap: No Unified Academic Writing Framework',
    content: 'Academic writing modes (IMRaD, papers, argument, narrative) operate in isolation without coherence guarantees...',
    weight: 0.95,
    dependencies: ['problem'],
    metadata: { research_gap: true }
  },
  {
    id: 'design',
    family: 'contribution',
    label: 'Design: HTF with 7 Families & 3 Operations',
    content: 'We design HTF as: Δ-shards → Λ-order → Π-merge → Γ-globalize, achieving μ-fixed point...',
    weight: 0.95,
    dependencies: ['gap', 'method'],
    metadata: { novel_contribution: true }
  },
  {
    id: 'eval',
    family: 'contribution',
    label: 'Evaluation: 91% Coherence, <5% Drift',
    content: 'We evaluate HTF on 10 thesis variants, achieving 91% coherence and <5% drift on average...',
    weight: 0.9,
    dependencies: ['design', 'result'],
    metadata: { experimental: true }
  },
  {
    id: 'impact',
    family: 'contribution',
    label: 'Impact: Framework Adoption & Future Work',
    content: 'HTF enables thesis writers to integrate multiple modes while maintaining coherence...',
    weight: 0.85,
    dependencies: ['eval', 'theory'],
    metadata: { impact: 'academic_writing' }
  },

  // Monograph family
  {
    id: 'context',
    family: 'monograph',
    label: 'Context: Academic Writing Evolution',
    content: 'From medieval trivium to modern academia, thesis structure has evolved...',
    weight: 0.7,
    dependencies: [],
    metadata: { historical: true }
  },
  {
    id: 'canon',
    family: 'monograph',
    label: 'Canonical Works: IMRaD, Papers, Argument',
    content: 'We review canonical works on academic writing: Swales on IMRaD, Toulmin on argumentation...',
    weight: 0.85,
    dependencies: ['context'],
    metadata: { review: true }
  },
  {
    id: 'analysis',
    family: 'monograph',
    label: 'Deep Analysis: Shard Coherence Metrics',
    content: 'We analyze how Δ-shards achieve coherence through Π-merging and Γ-validation...',
    weight: 0.9,
    dependencies: ['canon', 'proof', 'result'],
    metadata: { theoretical: true }
  },

  // DSR family
  {
    id: 'problem',
    family: 'dsr',
    label: 'Problem: Fragmented Academic Writing',
    content: 'Thesis writers struggle with integrating multiple writing modes without losing coherence...',
    weight: 0.95,
    dependencies: [],
    metadata: { problem_statement: true }
  },
  {
    id: 'artifact',
    family: 'dsr',
    label: 'Artifact: HTF React Hook Library',
    content: 'We build htf-core.mjs, useLambdaScheduling, usePiProfile, useGammaChecker with full JSDoc...',
    weight: 0.95,
    dependencies: ['design', 'method'],
    metadata: { artifact: 'react_hooks', lines_of_code: 2000 }
  },
  {
    id: 'theory',
    family: 'dsr',
    label: 'Theory: Μ-fixed Point Convergence',
    content: 'We derive that τ(A) = A (idempotent closure) when drift < threshold...',
    weight: 0.9,
    dependencies: ['analysis', 'proof'],
    metadata: { formal: true }
  },

  // Narrative family
  {
    id: 'field',
    family: 'narrative',
    label: 'Field: Knowledge Graph & Academic Writing Communities',
    content: 'The KG and academic writing communities have evolved separately since 1990s...',
    weight: 0.7,
    dependencies: [],
    metadata: { sociological: true }
  },
  {
    id: 'voice',
    family: 'narrative',
    label: 'Voice: First-Person Methodology Narrative',
    content: 'We tell the story of discovering that thesis coherence requires multiple simultaneous perspectives...',
    weight: 0.75,
    dependencies: ['field', 'intro'],
    metadata: { reflexive: true }
  },
  {
    id: 'pattern',
    family: 'narrative',
    label: 'Pattern: Emergence of Unified Framework',
    content: 'We trace the pattern: fragmentation → problem recognition → framework design → integration...',
    weight: 0.8,
    dependencies: ['voice', 'claim'],
    metadata: { narrative_arc: true }
  },
  {
    id: 'insight',
    family: 'narrative',
    label: 'Insight: Coherence as Mathematical Beauty',
    content: 'The key insight: thesis coherence, like music or mathematics, has intrinsic beauty...',
    weight: 0.85,
    dependencies: ['pattern', 'theory'],
    metadata: { philosophical: true }
  },

  // Papers family
  {
    id: 'paper1',
    family: 'papers',
    label: 'Paper 1: "HTF: A Unified Thesis Framework"',
    content: 'Published/Submitted to: Journal of Academic Writing...',
    weight: 0.9,
    dependencies: ['method', 'result'],
    metadata: { journal: 'JAW', status: 'accepted' }
  },
  {
    id: 'paper2',
    family: 'papers',
    label: 'Paper 2: "Coherence Metrics for Knowledge Graphs"',
    content: 'Published/Submitted to: Knowledge Engineering Review...',
    weight: 0.85,
    dependencies: ['analysis', 'eval'],
    metadata: { journal: 'KER', status: 'submitted' }
  },
  {
    id: 'synthesis',
    family: 'papers',
    label: 'Synthesis: Cross-Paper Implications',
    content: 'We synthesize papers 1-2 to show how HTF applies beyond knowledge graphs...',
    weight: 0.8,
    dependencies: ['paper1', 'paper2', 'impact'],
    metadata: { synthesis: true }
  }
];

// ============================================
// EXAMPLE 1: Λ-Scheduling (Chapter Planning)
// ============================================

console.log('\n=== EXAMPLE 1: Λ-SCHEDULING (Chapter Planning) ===\n');

function exampleLambdaScheduling() {
  // Create scheduler
  const deadline = new Date('2025-03-01');
  const scheduler = useLambdaScheduling(thesisShards, {
    deadline,
    weeksAvailable: 20,
    onScheduleChange: (event) => console.log('Schedule changed:', event)
  });

  // Get critical path
  console.log('Critical Path:');
  console.log(scheduler.scheduling.criticalPath.path);
  console.log(`Duration: ${scheduler.scheduling.criticalPath.duration} weeks`);
  console.log(`Slack: ${scheduler.scheduling.slack.toFixed(1)} weeks\n`);

  // Get upcoming milestones
  console.log('Next 3 Milestones:');
  scheduler.scheduling.upcomingMilestones.slice(0, 3).forEach(m => {
    console.log(`  - ${m.label} (due week ${m.weeksDue})`);
  });

  // Generate schedule report
  console.log('\nSchedule Report (Markdown):');
  console.log(scheduler.generateScheduleReport('markdown').substring(0, 500) + '...\n');

  return scheduler;
}

// ============================================
// EXAMPLE 2: Π-Profile (Coherence Analysis)
// ============================================

console.log('\n=== EXAMPLE 2: Π-PROFILE (Coherence Analysis) ===\n');

function examplePiProfile() {
  const profiler = usePiProfile(thesisShards, {
    onMergeChange: (event) => console.log('Merge changed:', event)
  });

  // Get coherence metrics
  console.log('Coherence Metrics:');
  console.log(`  Overall: ${(profiler.analysis.coherence.overall * 100).toFixed(1)}%`);
  console.log(`  Family Balance: ${(profiler.analysis.coherence.familyBalance * 100).toFixed(1)}%`);
  console.log(`  Integration Strength: ${(profiler.analysis.coherence.integrationStrength * 100).toFixed(1)}%`);
  console.log(`  Completeness: ${(profiler.analysis.coherence.completeness * 100).toFixed(1)}%\n`);

  // Get profile by family
  console.log('Profile by Family:');
  for (const [family, profile] of Object.entries(profiler.analysis.profileByFamily)) {
    if (profile.count > 0) {
      console.log(`  ${profile.label}: ${profile.count}/${profile.expectedCount} (${profile.status})`);
    }
  }

  // Get cross-family merge points
  console.log('\nTop Cross-Family Connections:');
  profiler.analysis.mergePoints.slice(0, 3).forEach(point => {
    console.log(`  ${point.from} → ${point.to}: ${point.strength} connections`);
  });

  // Get recommendations
  console.log('\nMissing Shards Recommendations:');
  profiler.analysis.recommendations.forEach(rec => {
    console.log(`  [${rec.priority}] ${rec.reason}`);
  });

  return profiler;
}

// ============================================
// EXAMPLE 3: Γ-Validation (Convergence Checking)
// ============================================

console.log('\n=== EXAMPLE 3: Γ-VALIDATION (Convergence Checking) ===\n');

function exampleGammaChecker() {
  const validator = useGammaChecker(thesisShards, [], {
    autoCheck: true,
    onViolation: (violations) => console.log(`Found ${violations.length} violations`),
    onConvergence: (event) => console.log('Thesis converged!', event)
  });

  // Get validation state
  console.log('Validation State:');
  console.log(`  Violations: ${validator.state.violations.length}`);
  console.log(`  Drift: ${(validator.state.drift * 100).toFixed(1)}%`);
  console.log(`  Converged: ${validator.state.isConverged ? '✅ YES' : '⚠️ NO'}\n`);

  // Get violation analysis
  const analysis = validator.getViolationAnalysis();
  console.log('Violations by Severity:');
  for (const [severity, violations] of Object.entries(analysis.bySeverity)) {
    console.log(`  ${severity}: ${violations.length}`);
  }

  // Get evolution
  console.log(`\nEvolution:`)
  console.log(`  Epoch: ${validator.evolution.epoch}`);
  console.log(`  Distance: ${(validator.evolution.distance * 100).toFixed(1)}%`);
  console.log(`  Improvement: ${(validator.evolution.improvement * 100).toFixed(2)}%`);
  console.log(`  Is Drift Decreasing: ${validator.isDriftDecreasing ? '✅ YES' : '⚠️ NO'}`);

  // Generate report
  console.log('\nValidation Report (Markdown):');
  console.log(validator.generateValidationReport('markdown').substring(0, 500) + '...\n');

  return validator;
}

// ============================================
// EXAMPLE 4: Complete HTF Framework
// ============================================

console.log('\n=== EXAMPLE 4: COMPLETE HTF FRAMEWORK ===\n');

function exampleCompleteFramework() {
  const framework = useHTFFramework({
    shards: thesisShards,
    deadline: new Date('2025-03-01'),
    weeksAvailable: 20,
    callbacks: {
      onScheduleChange: () => {},
      onMergeChange: () => {},
      onViolation: (violations) => console.log(`Violations: ${violations.length}`),
      onConvergence: () => console.log('✅ THESIS CONVERGED!')
    }
  });

  // Get comprehensive stats
  const stats = framework.getThesisStats();
  console.log('Thesis Statistics:');
  console.log(`  Total Shards: ${stats.shards.total}`);
  console.log(`  Families Represented: ${Object.values(stats.shards.byFamily).filter(c => c > 0).length}/7`);
  console.log(`  Coherence: ${(stats.quality.coherence * 100).toFixed(1)}%`);
  console.log(`  Violations: ${stats.quality.violations}`);
  console.log(`  Drift: ${(stats.quality.drift * 100).toFixed(1)}%`);
  console.log(`  Converged: ${stats.quality.isConverged ? '✅ YES' : '⚠️ NO'}\n`);

  // Get recommended next steps
  const nextSteps = framework.recommendNextSteps();
  console.log('Recommended Next Steps:');
  nextSteps.forEach(step => {
    const autoLabel = step.auto ? '[AUTO]' : '[MANUAL]';
    console.log(`  ${step.priority}. ${autoLabel} ${step.label}`);
    console.log(`     ${step.detail}`);
  });

  // Run optimization
  console.log('\nRunning Thesis Optimization...');
  framework.optimizeThesis().then(result => {
    console.log(`  Success: ${result.success}`);
    console.log(`  Drift: ${(result.drift * 100).toFixed(1)}%`);
    console.log(`  Coherence: ${(result.coherence * 100).toFixed(1)}%`);
    console.log(`  Remaining Violations: ${result.violations}\n`);
  });

  // Generate complete report
  console.log('Complete Thesis Report:');
  console.log(framework.generateThesisReport('markdown').substring(0, 800) + '...\n');

  return framework;
}

// ============================================
// RUN ALL EXAMPLES
// ============================================

if (import.meta.url === `file://${process.argv[1]}`) {
  console.log('╔════════════════════════════════════════════════════╗');
  console.log('║     HTF (Hyper-Thesis Framework) - Examples       ║');
  console.log('╚════════════════════════════════════════════════════╝');

  try {
    exampleLambdaScheduling();
    examplePiProfile();
    exampleGammaChecker();
    exampleCompleteFramework();

    console.log('╔════════════════════════════════════════════════════╗');
    console.log('║              Examples Complete! ✅                 ║');
    console.log('╚════════════════════════════════════════════════════╝');
  } catch (error) {
    console.error('Error running examples:', error);
  }
}

export { thesisShards, exampleLambdaScheduling, examplePiProfile, exampleGammaChecker, exampleCompleteFramework };
