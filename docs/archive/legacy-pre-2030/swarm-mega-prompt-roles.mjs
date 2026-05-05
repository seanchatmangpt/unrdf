/**
 * @file swarm-mega-prompt-roles.mjs
 * @module mega-prompt-swarm
 * @description Autonomous swarm role definitions for civilizational-scale information control evidence gathering.
 *
 * 7 parallel agents, each with formal responsibilities, evidence targets, and rejection criteria.
 * Designed for adversarial validation of irreversible systems thesis.
 */

/**
 * SWARM ORCHESTRATION PHASES:
 *
 * PHASE 1 (Parallel, ~2-3 hours)
 *   - Scale Collapse Analyst
 *   - Reversibility & Safety Theorist
 *   - Fuller Lineage Validator
 *
 * PHASE 2 (Parallel, ~2-3 hours, depends on Phase 1)
 *   - Deterministic Projection Validator
 *   - Coordination & Sharding Theorist
 *
 * PHASE 3 (Sequential, ~1-2 hours, depends on Phases 1-2)
 *   - Active Falsification Scout
 *
 * PHASE 4 (Sequential, ~1 hour)
 *   - Evidence Curator & Rubric Enforcer
 */

export const SWARM_ROLES = {
  /**
   * AGENT 1: Scale Collapse Analyst
   * ================================
   * Responsibility: Throughput limits, human-in-loop failures, governance collapse
   * Thesis axiom: Human review cannot scale beyond ~10³-10⁴ decisions/sec
   */
  scaleCollapseAnalyst: {
    id: 'scale-collapse',
    name: 'Scale Collapse Analyst',
    phase: 1,
    parallelizable: true,
    targets: [
      {
        domain: 'operational throughput limits',
        sources: ['peer-reviewed OPS papers', 'financial exchange regulations', 'air-traffic control'],
        extract: ['ops/sec limits', 'latency bounds', 'failure cascade studies'],
      },
      {
        domain: 'human-mediated systems',
        sources: ['FAA incident reports', 'trading floor blow-ups', 'medical error analysis'],
        extract: ['review bottleneck measurements', 'error rates at scale', 'callback delays'],
      },
      {
        domain: 'committee/consensus collapse',
        sources: ['distributed systems papers', 'organizational theory', 'governance failures'],
        extract: ['consensus latency (Paxos, Raft)', 'partition tolerance proofs', 'byzantine thresholds'],
      },
    ],
    rejectionCriteria: [
      'opinion/essay (not quantified)',
      'best-practice blog posts',
      'lacks concrete metrics (ops/sec, latency)',
      'no primary source or benchmark code',
    ],
    outputFormat: {
      claim: 'string',
      source: 'citation + link',
      metric: 'number with unit (e.g. "10^6 ops/sec")',
      bounds: 'O(n) or explicit threshold',
      falsifiesThesis: 'boolean - does this show scale IS possible?',
    },
  },

  /**
   * AGENT 2: Reversibility & Safety Theorist
   * ========================================
   * Responsibility: Irreversibility dominates, first-error failures, post-hoc verification limits
   * Thesis axiom: Systems with irreversible actions CANNOT rely on correction after execution
   */
  reversibilityTheorist: {
    id: 'reversibility-safety',
    name: 'Reversibility & Safety Theorist',
    phase: 1,
    parallelizable: true,
    targets: [
      {
        domain: 'irreversible action dominance',
        sources: [
          'control theory (stability, Lyapunov)',
          'safety-critical systems (aerospace, nuclear)',
          'financial crash analysis (Black Monday, 2008)',
        ],
        extract: [
          'theorems on irreversible state',
          'first-error dominance proofs',
          'why reversal fails in real time',
        ],
      },
      {
        domain: 'post-hoc verification failure',
        sources: [
          'formal methods (model checking)',
          'distributed consensus papers',
          'transaction ACID properties',
        ],
        extract: [
          'why learning after execution fails',
          'impossibility proofs for backward causality',
          'safety constraints on stochastic systems',
        ],
      },
      {
        domain: 'irreversibility at scale',
        sources: [
          'quantum mechanics (no-cloning theorem)',
          'thermodynamics (entropy)',
          'information theory (mutual information loss)',
        ],
        extract: [
          'physical bounds on reversibility',
          'information loss proofs',
          'why scale makes reversal harder',
        ],
      },
    ],
    rejectionCriteria: [
      'ethics essays (e.g., "AI safety")',
      'lacks formal theorem or proof sketch',
      'opinion on what "should" happen',
      'no quantitative bound',
    ],
    outputFormat: {
      claim: 'string',
      theorem: 'formal statement (LaTeX OK)',
      source: 'primary citation',
      significance: 'why this makes post-hoc correction impossible',
      falsifiesThesis: 'boolean - can systems correct after irreversible action?',
    },
  },

  /**
   * AGENT 3: Fuller Lineage Validator
   * =================================
   * Responsibility: Buckminster Fuller's program + formal gaps
   * Thesis axiom: Modern calculus either completes or contradicts Fuller's vision
   */
  fullerValidator: {
    id: 'fuller-lineage',
    name: 'Fuller Lineage Validator',
    phase: 1,
    parallelizable: true,
    targets: [
      {
        domain: 'Fuller primary texts',
        sources: [
          'Synergetics (1975, 1979)',
          'Operating Manual for Spaceship Earth',
          'Critical Path',
          'World Game',
        ],
        extract: [
          'direct quotes on determinism/anticipation',
          'geometric axioms (tetrahedral, geodesic)',
          'statements on closure/replayability',
          'what Fuller DID NOT formalize',
        ],
      },
      {
        domain: 'Fuller followers & formal extensions',
        sources: [
          'Medawar critical analysis',
          'mathematical synergetics papers',
          'systems theory (Laszlo, Bertalanffy)',
        ],
        extract: [
          'which Fuller ideas were proved',
          'which remain conjecture',
          'formal gaps in synergetics axioms',
        ],
      },
      {
        domain: 'modern systems completing Fuller',
        sources: [
          'event sourcing literature',
          'temporal databases',
          'distributed ledgers (blockchain analysis)',
        ],
        extract: [
          'where modern tech answers Fuller\'s questions',
          'what he lacked 50 years ago',
          'alignment with CADS (Comprehensive Anticipatory Design Science)',
        ],
      },
    ],
    rejectionCriteria: [
      'interpretation/hagiography (not direct text)',
      'opinion on Fuller\'s "vision"',
      'lacks specific quotes + page numbers',
      'no comparison to formal modern work',
    ],
    outputFormat: {
      claim: 'string',
      fullerQuote: 'verbatim + source (book, page)',
      formalGap: 'what Fuller lacked (theorem, bound, algorithm)',
      modernCompletion: 'how is this gap filled today?',
      aligns: 'boolean - does modern work support or contradict Fuller?',
    },
  },

  /**
   * AGENT 4: Deterministic Projection Validator
   * ===========================================
   * Responsibility: Why determinism is required, what non-determinism fails to guarantee
   * Thesis axiom: A = μ(O) is the only provably safe projection calculus
   * Depends on: Phase 1 (reversibility results)
   */
  deterministicValidator: {
    id: 'deterministic-projection',
    name: 'Deterministic Projection Validator',
    phase: 2,
    parallelizable: true,
    dependencies: ['reversibility-safety'],
    targets: [
      {
        domain: 'probabilistic systems on irreversible actions',
        sources: [
          'stochastic control theory',
          'reinforcement learning safety papers',
          'Markov decision process bounds',
        ],
        extract: [
          'when expectation ≠ safety',
          'why probability of failure is > 0',
          'bounds on rare-event guarantees',
        ],
      },
      {
        domain: 'deterministic guarantees',
        sources: [
          'formal verification (model checking)',
          'deterministic simulations',
          'blockchain consensus (deterministic replay)',
        ],
        extract: [
          'state reachability proofs',
          'why determinism enables proof',
          'formal completeness of deterministic projections',
        ],
      },
      {
        domain: 'idempotence as safety requirement',
        sources: [
          'transaction systems (ACID)',
          'database replayability',
          'idempotent APIs (HTTP PUT/DELETE)',
        ],
        extract: [
          'why idempotence is needed',
          'what breaks without it',
          'complexity of idempotent-free systems',
        ],
      },
    ],
    rejectionCriteria: [
      'argues probabilistic "is fine" without bounds',
      'opinion on machine learning safety',
      'lacks formal safety threshold',
      'confuses expected value with safety guarantees',
    ],
    outputFormat: {
      claim: 'string',
      formalStatement: 'theorem or proof sketch (determinism required for X)',
      source: 'primary paper + equation number',
      counterexample: 'any system breaking determinism requirement (if found)',
      falsifiesThesis: 'boolean - can probabilistic systems guarantee safety?',
    },
  },

  /**
   * AGENT 5: Coordination & Sharding Theorist
   * ========================================
   * Responsibility: Distributed scaling, consensus costs, minimality
   * Thesis axiom: Only shardable, commutative systems scale. Minimality is unique.
   * Depends on: Phase 1 (scale limits)
   */
  coordinationTheorist: {
    id: 'coordination-sharding',
    name: 'Coordination & Sharding Theorist',
    phase: 2,
    parallelizable: true,
    dependencies: ['scale-collapse'],
    targets: [
      {
        domain: 'sharding boundaries',
        sources: [
          'distributed systems theory',
          'blockchain sharding (Polkadot, Cosmos)',
          'database partitioning (MySQL sharding)',
          'CAP theorem + extensions',
        ],
        extract: [
          'what properties allow sharding',
          'coordination cost (Ω(n²) lower bounds)',
          'impossibility theorems (CAP, FLP)',
        ],
      },
      {
        domain: 'minimal operator sets',
        sources: [
          'lattice theory',
          'category theory (initial objects)',
          'abstract algebra (group axioms)',
        ],
        extract: [
          'what makes an operator set minimal',
          'uniqueness of minimal sets (up to isomorphism)',
          'how to prove minimality',
        ],
      },
      {
        domain: 'commutativity & state reconciliation',
        sources: [
          'CRDT literature (Shapiro et al.)',
          'operational transformation',
          'eventual consistency proofs',
        ],
        extract: [
          'when commutativity holds',
          'why it enables sharding',
          'cost of enforcing commutativity',
        ],
      },
    ],
    rejectionCriteria: [
      'ignores commutativity requirement',
      'lacks lower-bound proof',
      'opinion on blockchain "efficiency"',
      'no formal sharding boundary definition',
    ],
    outputFormat: {
      claim: 'string',
      lowerBound: 'asymptotic cost (e.g., "Ω(n log n) communication")',
      source: 'paper + theorem number',
      minimality: 'is this the smallest operator set satisfying constraints?',
      falsifiesThesis: 'boolean - does non-minimal system still scale?',
    },
  },

  /**
   * AGENT 6: Active Falsification Scout
   * ==================================
   * Responsibility: Hunt for counter-examples, alternative calculi, exceptions
   * Thesis axiom: No other calculus survives all constraints. PROVE ME WRONG.
   * Depends on: Phases 1-2 (all constraints established)
   */
  falsificationScout: {
    id: 'active-falsification',
    name: 'Active Falsification Scout',
    phase: 3,
    parallelizable: false,
    dependencies: ['scale-collapse', 'reversibility-safety', 'deterministic-projection', 'coordination-sharding'],
    targets: [
      {
        domain: 'alternative control paradigms',
        sources: [
          'adaptive control literature',
          'machine learning on irreversible systems',
          'human-in-loop safeguards',
        ],
        extract: [
          'any system that works WITHOUT deterministic projection',
          'proof that non-determinism can guarantee safety',
          'alternative idempotence mechanisms',
        ],
      },
      {
        domain: 'scale exceptions',
        sources: [
          'biological systems (evolution)',
          'human organizations (management)',
          'quantum systems',
        ],
        extract: [
          'any civilization-scale system that succeeded without determinism',
          'proof that post-hoc correction worked at scale',
          'alternative to replayability',
        ],
      },
      {
        domain: 'mathematical counterexamples',
        sources: [
          'category theory (alternative base categories)',
          'type theory (alternative projections)',
          'proof assistants (Coq, Lean formalized contradictions)',
        ],
        extract: [
          'formal proof that A ≠ μ(O) can still work',
          'calculus with different axioms but same guarantees',
          'cases where uniqueness fails',
        ],
      },
    ],
    rejectionCriteria: [
      'strawman counter-examples',
      'lacks formal proof of equivalence',
      'opinion that "alternatives might exist"',
      'does not test against ALL five constraints',
    ],
    outputFormat: {
      claim: 'string - specific counter-example',
      source: 'primary reference',
      formalRebuttal: 'why this violates or respects the thesis constraints',
      strength: 'weak / medium / strong counter-example',
      publishFirst: 'boolean - if true, report this BEFORE supporting evidence',
    },
  },

  /**
   * AGENT 7: Evidence Curator & Rubric Enforcer
   * ==========================================
   * Responsibility: Aggregate all evidence, enforce scoring rubric, produce final report
   * Depends on: All agents (Phases 1-3)
   */
  evidenceCurator: {
    id: 'evidence-curator',
    name: 'Evidence Curator & Rubric Enforcer',
    phase: 4,
    parallelizable: false,
    dependencies: ['scale-collapse', 'reversibility-safety', 'fuller-lineage', 'deterministic-projection', 'coordination-sharding', 'active-falsification'],
    responsibilities: [
      'Aggregate evidence from all 6 agents',
      'Score each evidence item against rubric (scale 0-100)',
      'Reject evidence that scores <70 OR contradicts formal axioms',
      'Publish falsifications FIRST (regardless of score)',
      'Produce summary statistics (% supporting, % falsifying, gaps)',
      'Identify remaining unknowns',
      'Recommend next research directions',
    ],
    outputFormat: {
      format: 'evidence-report.json',
      structure: {
        metadata: {
          timestamp: 'ISO 8601',
          agentsQueried: 7,
          totalEvidenceItems: 'number',
          comprehensiveness: '0-100 (% of target space covered)',
        },
        falsifications: [
          {
            strength: 'strong / medium / weak',
            claim: 'what thesis axiom is violated?',
            source: 'primary reference',
            evidenceScore: '0-100',
            publishOrder: 1,
          },
        ],
        supportingEvidence: [
          {
            axiom: 'SCALE | REVERSIBILITY | DETERMINISM | COORDINATION | MINIMALITY',
            claim: 'string',
            source: 'citation',
            evidenceScore: '0-100 (must be ≥70)',
            category: 'PROOF | BENCHMARK | IMPOSSIBILITY | COUNTEREXAMPLE',
          },
        ],
        gaps: ['unresolved questions', 'missing evidence domains'],
        nextSteps: ['recommended follow-up research'],
      },
    },
  },
};

/**
 * AXIOMS UNDER TEST (5 constraints)
 * =================================
 *
 * SCALE (S)
 *   Human-in-loop systems collapse at ~10^4 decisions/sec
 *   Post-hoc verification is insufficient for irreversible actions
 *
 * REVERSIBILITY (R)
 *   Irreversible actions CANNOT be corrected after execution
 *   First-error dominates expectation-based correction
 *
 * DETERMINISM (D)
 *   Probabilistic systems cannot guarantee safety on irreversible systems
 *   A = μ(O) (deterministic projection) is mandatory
 *
 * COORDINATION (C)
 *   Shardable systems require commutativity
 *   Coordination cost is Ω(n²) in non-shardable case
 *
 * MINIMALITY (M)
 *   The operator set {determinism, idempotence, replayability, closure, ...} is minimal
 *   No alternative calculus satisfies all constraints
 */

export const AXIOMS = {
  SCALE: {
    statement: 'Human-mediated systems have hard throughput limits (~10^4 ops/sec)',
    requires: ['quantitative throughput bounds', 'failure cascade studies'],
  },
  REVERSIBILITY: {
    statement: 'Irreversible actions cannot be corrected after execution',
    requires: ['formal theorem or proof', 'first-error dominance argument'],
  },
  DETERMINISM: {
    statement: 'Non-deterministic controllers cannot guarantee safety on irreversible systems',
    requires: ['probabilistic safety bounds', 'equivalence proof of A = μ(O)'],
  },
  COORDINATION: {
    statement: 'Sharding requires commutativity; non-shardable systems cost Ω(n²)',
    requires: ['lower-bound proofs', 'commutativity requirements'],
  },
  MINIMALITY: {
    statement: 'No alternative calculus satisfies all constraints',
    requires: ['exhaustive falsification search', 'uniqueness proof up to isomorphism'],
  },
};

export const SWARM_EXECUTION_PLAN = {
  phase1: {
    name: 'Parallel Foundational Research',
    duration: '2-3 hours',
    agents: ['scale-collapse', 'reversibility-safety', 'fuller-lineage'],
    description: 'Independent evidence gathering on fundamental constraints',
  },
  phase2: {
    name: 'Parallel Constraint Derivation',
    duration: '2-3 hours',
    agents: ['deterministic-projection', 'coordination-sharding'],
    description: 'Build on Phase 1 to derive implications',
  },
  phase3: {
    name: 'Active Falsification',
    duration: '1-2 hours',
    agents: ['active-falsification'],
    description: 'Hunt for counter-examples using all prior constraints',
  },
  phase4: {
    name: 'Evidence Aggregation & Report',
    duration: '1 hour',
    agents: ['evidence-curator'],
    description: 'Score, filter, and produce final report',
  },
};
