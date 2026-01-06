/**
 * @file swarm-orchestration-plan.mjs
 * @module mega-prompt-orchestration
 * @description Execution plan for 7-agent swarm gathering civilizational-scale evidence.
 *
 * Dependencies, sequencing, checkpoints, and failure modes.
 */

export const ORCHESTRATION_PLAN = {
  name: 'Mega-Prompt Evidence Gathering Swarm',
  duration: '6-8 hours total wall-clock time',
  agents: 7,
  axioms: 5,
  targetDomains: 15,
  phases: 4,

  /**
   * PHASE 1: FOUNDATIONAL RESEARCH (2-3 hours, PARALLEL)
   * ===================================================
   *
   * Three independent agents establish ground truth on core constraints.
   * No dependencies between agents.
   * GATE: All Phase 1 agents must score ≥70 on at least 1 evidence item each before proceeding.
   */
  phase1: {
    name: 'Foundational Research',
    duration: '2-3 hours (parallel)',
    parallelFactor: 3,
    agents: [
      {
        id: 'scale-collapse',
        name: 'Scale Collapse Analyst',
        axiom: 'SCALE',
        targets: 3,
        deadline: '2.5 hours',
        keyQuestions: [
          'What is the empirical throughput limit for human-mediated decisions?',
          'Why does committee-based governance fail at scale?',
          'What hard evidence exists on operational system limits?',
        ],
        acceptanceCriteria: [
          'At least 1 evidence item with score ≥70',
          'Covers ≥2 of 3 target domains (human limits, operational throughput, consensus collapse)',
          'Primary sources: ≥80% of citations',
        ],
        successMetric: 'Evidence count: minimum 3, target 5+ items with ≥70 score',
      },
      {
        id: 'reversibility-safety',
        name: 'Reversibility & Safety Theorist',
        axiom: 'REVERSIBILITY',
        targets: 3,
        deadline: '2.5 hours',
        keyQuestions: [
          'What formal theorems prove irreversibility dominates expectation-based correction?',
          'Why is post-hoc verification impossible under irreversibility?',
          'What do safety-critical systems say about learning after execution?',
        ],
        acceptanceCriteria: [
          'At least 1 formal theorem or proof (score ≥70)',
          'Covers ≥2 of 3 target domains (control theory, safety-critical, information theory)',
          'Quantitative rigor: explicit bounds or impossibility arguments',
        ],
        successMetric: 'Evidence count: minimum 3, target 5+ items with ≥70 score',
      },
      {
        id: 'fuller-lineage',
        name: 'Fuller Lineage Validator',
        axiom: 'MINIMALITY (grounding)',
        targets: 3,
        deadline: '2.5 hours',
        keyQuestions: [
          'What did Buckminster Fuller actually assert (primary texts)?',
          'What formal gaps exist in Fuller\'s program?',
          'How do modern systems complete Fuller\'s vision?',
        ],
        acceptanceCriteria: [
          'At least 2 direct Fuller quotes (page numbers)',
          'At least 1 identified formal gap',
          'At least 1 modern system that extends Fuller',
        ],
        successMetric: 'Fuller grounding: 100% primary source verification',
      },
    ],
    checkpoint: {
      name: 'Phase 1 Completion Gate',
      condition: 'All 3 agents report ≥1 item with score ≥70, AND ≥2/3 target domains covered',
      onFailure: 'Extend Phase 1 by 1 hour; re-task agent on weakest domain',
    },
  },

  /**
   * PHASE 2: CONSTRAINT DERIVATION (2-3 hours, PARALLEL)
   * ====================================================
   *
   * Two agents build on Phase 1 results to derive implications.
   * Dependencies: Both agents read Phase 1 outputs before starting.
   * GATE: At least 1 agent must produce novel constraint argument not in Phase 1.
   */
  phase2: {
    name: 'Constraint Derivation',
    duration: '2-3 hours (parallel)',
    parallelFactor: 2,
    dependencies: ['All Phase 1 agents must report completion'],
    agents: [
      {
        id: 'deterministic-projection',
        name: 'Deterministic Projection Validator',
        axiom: 'DETERMINISM',
        targets: 3,
        deadline: '2.5 hours',
        keyQuestions: [
          'What theorem proves determinism is required for irreversible systems?',
          'Why does expectation ≠ safety under non-determinism?',
          'Can A = μ(O) be proven minimal?',
        ],
        dependsOn: ['reversibility-safety (Phase 1)'],
        acceptanceCriteria: [
          'At least 1 formal theorem on determinism requirement',
          'Explains connection between irreversibility and determinism',
          'Covers ≥2 of 3 target domains',
        ],
        successMetric: 'Evidence count: minimum 3, target 5+ items with ≥70 score',
      },
      {
        id: 'coordination-sharding',
        name: 'Coordination & Sharding Theorist',
        axiom: 'COORDINATION',
        targets: 3,
        deadline: '2.5 hours',
        keyQuestions: [
          'What lower bounds exist on coordination in non-shardable systems?',
          'Is commutativity a necessary condition for sharding?',
          'Why are minimal operator sets unique up to isomorphism?',
        ],
        dependsOn: ['scale-collapse (Phase 1)'],
        acceptanceCriteria: [
          'At least 1 lower-bound proof (Ω or explicit threshold)',
          'Explains sharding requirements',
          'Covers ≥2 of 3 target domains',
        ],
        successMetric: 'Evidence count: minimum 3, target 5+ items with ≥70 score',
      },
    ],
    checkpoint: {
      name: 'Phase 2 Completion Gate',
      condition: 'Both agents report ≥1 item with score ≥70, AND 1 novel constraint not in Phase 1',
      onFailure: 'Extend Phase 2 by 1 hour; focus on lowest-scoring agent',
    },
  },

  /**
   * PHASE 3: ACTIVE FALSIFICATION (1-2 hours, SEQUENTIAL)
   * =====================================================
   *
   * Single agent hunts for counter-examples and alternative calculi.
   * CRITICAL: This agent MUST test against ALL 5 axioms, not cherry-pick.
   * Dependency: Receives constraint list from Phases 1-2.
   * GATE: Must attempt ≥5 different counter-claim directions.
   */
  phase3: {
    name: 'Active Falsification',
    duration: '1-2 hours (sequential)',
    serialization: 'Required',
    dependsOn: ['All Phase 1 + Phase 2 agents completed'],
    agents: [
      {
        id: 'active-falsification',
        name: 'Active Falsification Scout',
        axiom: 'All (tries to break each)',
        targets: 3,
        deadline: '1.5 hours',
        keyQuestions: [
          'Is there any system that violates SCALE without failing?',
          'Can irreversible actions be corrected? Any counter-example?',
          'Do non-deterministic controllers have formal safety guarantees?',
          'Can non-commutative systems shard? Counter-example?',
          'Are there alternative minimal calculi?',
        ],
        inputConstraints: 'All constraint statements from Phases 1-2 (5 axioms, ≥15 supporting evidence items)',
        acceptanceCriteria: [
          'Attempts ≥5 different counter-claim directions',
          'Each attempt has formal argument or primary source',
          'Explicitly states why each counter-claim fails or succeeds',
          'Any strong counter-claim (score ≥70) is flagged for publication first',
        ],
        successMetric: 'Falsification count: ≥3 counter-claims with formal analysis, ≤1 score ≥70 (if any)',
      },
    ],
    checkpoint: {
      name: 'Phase 3 Completion Gate',
      condition: 'Agent attempts ≥5 counter-claims and produces structured analysis',
      onFailure: 'Agent continues until deadline or finds strong counter-example',
      strongCounterFound: {
        action: 'IMMEDIATE ESCALATION - flag for publication, continue Phase 4 as normal',
        rationale: 'Falsifications published before supporting evidence per mega-prompt rules',
      },
    },
  },

  /**
   * PHASE 4: EVIDENCE AGGREGATION & REPORT (1 hour, SEQUENTIAL)
   * ==========================================================
   *
   * Curator collects all evidence, applies rubric, produces final report.
   * Dependency: All Phases 1-3 completed.
   * GATE: Report must show comprehensiveness score ≥60%.
   */
  phase4: {
    name: 'Evidence Aggregation & Report',
    duration: '1 hour (sequential)',
    serialization: 'Required',
    dependsOn: ['All Phase 1, 2, 3 agents completed'],
    agents: [
      {
        id: 'evidence-curator',
        name: 'Evidence Curator & Rubric Enforcer',
        axiom: 'N/A (meta)',
        targets: 'All evidence from Phases 1-3',
        deadline: '1 hour',
        keyResponsibilities: [
          'Aggregate all evidence items from 6 agents',
          'Score each item against rubric (0-100)',
          'Reject items with score <70',
          'Identify and publish falsifications FIRST',
          'Group supporting evidence by axiom',
          'Measure comprehensiveness (% of domains covered)',
          'Flag gaps (axioms with <3 supporting items)',
          'Produce summary statistics',
          'Recommend next research directions',
        ],
        inputs: [
          'Phase 1: SCALE, REVERSIBILITY, MINIMALITY (grounding)',
          'Phase 2: DETERMINISM, COORDINATION',
          'Phase 3: Falsification attempts + any strong counters',
        ],
        outputs: {
          format: 'evidence-report.json',
          sections: [
            'metadata (timestamp, agent count, total items)',
            'falsifications (if any, published first)',
            'supporting evidence by axiom',
            'gaps and unknowns',
            'next steps',
          ],
        },
        acceptanceCriteria: [
          'All 6 Phase 1-3 agents aggregated',
          'Rubric applied consistently (scoring variance <5%)',
          'Falsifications (if any) published first in report',
          'Comprehensiveness score ≥60% (minimum)',
          'Gaps explicitly identified',
        ],
        successMetric: 'Report completeness: 100% of input evidence processed, no >10% discrepancies',
      },
    ],
    checkpoint: {
      name: 'Final Report Gate',
      condition: 'Report complete, comprehensiveness ≥60%, all axioms assessed',
      onSuccess: 'SWARM COMPLETE - Report ready for external review',
      onFailure: 'Extend Phase 4 by 30 min; curator fills largest gaps',
    },
  },

  /**
   * COORDINATION & COMMUNICATION
   * ============================
   *
   * Between Phases, agents communicate via shared evidence file (JSON):
   *   .evidence-phase-1.json (Phase 1 output → Phase 2/3 input)
   *   .evidence-phase-2.json (Phase 2 output → Phase 3 input)
   *   .evidence-final.json (All agents → Curator input)
   *
   * Real-time metrics:
   *   - Agent progress (% domain coverage)
   *   - Evidence quality (avg score, rejection rate)
   *   - Constraint satisfaction (axioms covered)
   *
   * Failure modes & recovery:
   *   1. Phase 1 agent fails to score ≥70 on any domain
   *      → Extend by 1 hour, re-task on weakest domain
   *   2. Phase 2 agent unable to build on Phase 1
   *      → Reduce target from 5 to 3 evidence items
   *   3. Phase 3 finds strong counter-example (score ≥70)
   *      → PUBLISH FIRST, continue Phase 4 as normal
   *   4. Phase 4 comprehensiveness <60%
   *      → Identify 1-2 lowest-coverage axioms, recommend Phase 5 focus
   */

  communicationProtocol: {
    phase1ToPhase2: {
      format: 'JSON evidence array',
      schema: {
        axiom: 'SCALE|REVERSIBILITY|MINIMALITY',
        claims: '[{claim, source, score, category}]',
        constraints: '[{constraint statement, supporting score}]',
      },
      deadline: 'Within 30 min of Phase 1 completion',
    },
    phase2ToPhase3: {
      format: 'JSON constraint list',
      schema: {
        axioms: '[{name, statement, supporting evidence count}]',
        gaps: '[{axiom, gaps identified}]',
      },
      deadline: 'Within 30 min of Phase 2 completion',
    },
    allToPhase4: {
      format: 'JSON evidence archive',
      schema: {
        phases: {
          phase1: '[evidence items]',
          phase2: '[evidence items]',
          phase3: '[falsification attempts + results]',
        },
      },
      deadline: 'At Phase 3 completion',
    },
  },

  timelineAndScheduling: {
    totalWallClockTime: '6-8 hours',
    breakdown: {
      phase1: '2-3 hours (3 agents in parallel → 0.67-1 hours per agent)',
      phase2: '2-3 hours (2 agents in parallel, depends on Phase 1)',
      phase3: '1-2 hours (1 agent, depends on Phases 1-2)',
      phase4: '1 hour (1 agent, depends on all prior)',
      buffers: '~1 hour contingency for checkpoint extensions',
    },
    criticalPath: 'Phase 1 → Phase 2 → Phase 3 → Phase 4 (no parallelization shortcuts)',
    parallelizationGains: 'Phase 1 saves 2 hours (3 agents in parallel), Phase 2 saves 1 hour (2 agents parallel)',
  },

  successMetrics: {
    global: {
      'Evidence items (≥70 score)': 'Target ≥20 total',
      'Axiom coverage': 'All 5 axioms with ≥3 supporting items',
      'Falsification attempts': '≥5 distinct counter-claims',
      'Comprehensiveness': '≥60% of thesis space covered',
    },
    perPhase: {
      phase1: 'Each agent: ≥3 items, ≥70 score on ≥1',
      phase2: 'Each agent: ≥3 items, ≥70 score on ≥1',
      phase3: 'Agent: ≥5 counter-claims with formal analysis',
      phase4: 'Curator: 100% input processed, no >10% score variance',
    },
  },

  escalationProcedure: {
    weakAgent: {
      trigger: 'Agent score <50 avg after 1 hour',
      action: 'Refocus on highest-impact domain, extend deadline by 30 min',
    },
    strongCounter: {
      trigger: 'Falsification attempt scores ≥70',
      action: 'IMMEDIATE ESCALATION - flag for publication before supporting evidence',
    },
    phaseFailure: {
      trigger: 'Checkpoint not met by deadline',
      action: 'Extend by 1 hour OR reduce scope (e.g., 3 domains → 2)',
    },
    reportIncomplete: {
      trigger: 'Phase 4 comprehensiveness <60% at deadline',
      action: 'Deliver as-is with "incomplete" flag, recommend Phase 5 focus areas',
    },
  },

  nextSteps: {
    ifThesisSupportedByEvidence: [
      '1. Commission peer review of evidence quality (external auditors)',
      '2. Publish falsifications (if any) with full context',
      '3. Identify axioms with <3 supporting items (gaps)',
      '4. Recommend follow-up research on gaps',
    ],
    ifStrongCounterFound: [
      '1. Publish counter-claim immediately (before supporting evidence)',
      '2. Analyze why thesis fails and under what conditions',
      '3. Refine thesis or declare alternative calculus viable',
      '4. Conduct Phase 5: deep dive on counter-example',
    ],
    ifNoConclusion: [
      '1. Identify 2-3 highest-value research directions',
      '2. Recommend Phase 5 focus (domain deepening or new domains)',
      '3. Cost-benefit analysis: is further research justified?',
    ],
  },
};

/**
 * AGENT HANDOFF TEMPLATE
 * ======================
 *
 * Each agent uses this template for evidence items:
 */

export const EVIDENCE_TEMPLATE = {
  id: 'auto-generated-uuid',
  agentId: 'scale-collapse | reversibility-safety | ... | evidence-curator',
  phase: 1,
  timestamp: 'ISO 8601',

  // EVIDENCE SUBSTANCE
  claim: 'Specific, falsifiable statement about thesis or axiom',
  source: 'Primary citation (book, paper, code), with page/section number',
  quote: 'Verbatim text if applicable',
  link: 'GitHub, arXiv, DOI link if available',

  // SCORING
  evidenceClass: 'A | B | C',
  primarySourcePercent: 0,
  quantitativeRigor: 0,
  relevantAxiom: 'SCALE | REVERSIBILITY | DETERMINISM | COORDINATION | MINIMALITY',
  falsificationStrength: 'STRONG | MEDIUM | WEAK | NONE (supporting only)',

  // ANALYSIS
  formalStatement: 'Theorem, bound, or equation if applicable (LaTeX OK)',
  interpretation: 'Why this supports/falsifies thesis',
  counterArguments: 'Potential objections or limitations',

  // METADATA
  rubricScore: 0, // 0-100, auto-calculated by Phase 4
  rubricBreakdown: {}, // {classA: 20, primarySource: 20, ...}
  acceptanceStatus: 'PENDING | ACCEPTED | REJECTED',
  rejectionReason: 'if applicable',
};

/**
 * EXECUTION CHECKLIST
 * ===================
 *
 * Before starting Phase 1:
 *   [ ] All 7 agents have role definitions
 *   [ ] Evidence template distributed to all agents
 *   [ ] Rubric explained + example evidence reviewed
 *   [ ] Shared JSON output directory created
 *   [ ] Timeline posted (6-8 hours wall-clock)
 *
 * Phase 1 → Phase 2:
 *   [ ] Phase 1 output JSON file generated
 *   [ ] All constraints extracted into Phase 2 input
 *   [ ] Phase 2 agents acknowledge receipt
 *
 * Phase 2 → Phase 3:
 *   [ ] Phase 2 output JSON file generated
 *   [ ] Constraint list distilled (5 axioms, key bounds)
 *   [ ] Phase 3 agent studies constraints for 15 min
 *   [ ] Phase 3 begins with explicit counter-claim targeting strategy
 *
 * Phase 3 → Phase 4:
 *   [ ] Phase 3 output JSON file generated
 *   [ ] All evidence (Phases 1-3) merged into single file
 *   [ ] Curator runs rubric scoring on all items
 *   [ ] Falsifications (if any) flagged for publication-first ordering
 *
 * Phase 4 → Final Report:
 *   [ ] Report JSON generated
 *   [ ] Comprehensiveness score calculated
 *   [ ] Gaps analysis complete
 *   [ ] Next steps recommended
 *   [ ] Report ready for external review
 */
