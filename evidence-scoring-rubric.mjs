/**
 * @file evidence-scoring-rubric.mjs
 * @module mega-prompt-validation
 * @description Quantitative scoring rubric for civilizational-scale evidence.
 *
 * Each piece of evidence is scored 0-100.
 * Scores <70 are rejected.
 * Any evidence contradicting axioms is published first.
 */

/**
 * EVIDENCE CLASSIFICATION HIERARCHY
 * ==================================
 *
 * Class A (Direct Evidence) - 20 points
 *   - Formal proofs, theorems with page numbers
 *   - Peer-reviewed benchmarks with reproducible code
 *   - Primary source quantitative data (not summarized)
 *   - Mathematical bounds proven rigorously
 *
 * Class B (Secondary Analysis) - 10 points
 *   - Meticulously cited analysis of Class A sources
 *   - Experimental data with error bars and methodology
 *   - Domain expert interpretation with clear methodology
 *   - Published but not peer-reviewed (ArXiv, technical reports)
 *
 * Class C (Tertiary/Opinion) - 0 points [AUTO-REJECT]
 *   - Blog posts, opinion essays, "best practices"
 *   - Summaries without source verification
 *   - Ethical arguments (not technical)
 *   - Speculation, "could", "might"
 */

/**
 * SCORING COMPONENTS (each 0-20 points, equally weighted)
 * ========================================================
 *
 * 1. EVIDENCE CLASS
 *    20 = Class A (formal proof or rigorous benchmark)
 *    10 = Class B (secondary analysis of Class A)
 *     0 = Class C (opinion, blog, speculation) [REJECT IMMEDIATELY]
 *
 * 2. PRIMARY SOURCE REQUIREMENT
 *    20 = >80% primary sources (equations, theorems, experimental data)
 *    15 = 50-80% primary
 *    10 = 30-50% primary
 *     5 = <30% primary [REJECT if <30%]
 *     0 = No primary sources
 *
 * 3. QUANTITATIVE RIGOR
 *    20 = Explicit bounds, equations, or benchmarks (with units/precision)
 *    15 = Measurable but approximate
 *    10 = Qualitative with numbers
 *     5 = Mostly qualitative
 *     0 = No quantification [REJECT]
 *
 * 4. RELEVANCE TO THESIS AXIOMS
 *    20 = Directly supports/falsifies one axiom (clear connection)
 *    15 = Constrains multiple axioms
 *    10 = Tangentially related
 *     5 = Weak connection
 *     0 = Irrelevant [REJECT]
 *
 * 5. FALSIFICATION STRENGTH (if counter-claim)
 *    20 = Directly contradicts axiom with formal proof
 *    15 = Counter-example that violates stated constraint
 *    10 = Suggests alternative but not proven
 *     5 = Weak counter-example
 *     0 = Not a falsification
 *
 * TOTAL: 100 points (5 × 20)
 * THRESHOLD: ≥70 to accept (must pass at least 3.5 of 5 criteria)
 */

export const SCORING_RUBRIC = {
  criteria: [
    {
      name: 'Evidence Class',
      weight: 20,
      levels: [
        {
          score: 20,
          description: 'Class A: Formal proof, theorem, peer-reviewed benchmark with code',
          examples: [
            'Equation: "Theorem 3.2 (page 47, Smith et al., 2022)"',
            'Benchmark: "Latency = 0.347ms ± 0.012ms, N=10,000 trials, code: github.com/..."',
            'Impossibility proof: "By CAP theorem, [constraint]"',
          ],
        },
        {
          score: 10,
          description: 'Class B: Secondary analysis of Class A, experimental data, domain report',
          examples: [
            'Analysis: "According to Lemma 4 (Jones et al., 2020), this implies..."',
            'Experiment: "Our test of Paxos consensus showed latency of 5ms on AWS"',
            'Technical report: "FAA incident database shows 23 errors per 10K operations"',
          ],
        },
        {
          score: 0,
          description: 'Class C: Opinion, blog, speculation, ethics essay [IMMEDIATE REJECT]',
          examples: [
            '"I think deterministic systems are safer..."',
            '"This blog suggests that human oversight helps"',
            '"Machine learning could solve this problem"',
          ],
        },
      ],
    },
    {
      name: 'Primary Source Requirement',
      weight: 20,
      levels: [
        {
          score: 20,
          description: '>80% primary (equations, experimental data, code, theorems)',
        },
        {
          score: 15,
          description: '50-80% primary (good mix of primary + secondary analysis)',
        },
        {
          score: 10,
          description: '30-50% primary (more analysis than raw data)',
        },
        {
          score: 5,
          description: '<30% primary [REJECT - insufficient primary sources]',
        },
        {
          score: 0,
          description: 'No primary sources [AUTO-REJECT]',
        },
      ],
    },
    {
      name: 'Quantitative Rigor',
      weight: 20,
      levels: [
        {
          score: 20,
          description: 'Explicit bounds (O(n), Ω(n²)), latency/throughput with units, equations',
          examples: [
            'Consensus: "O(log n) rounds, Ω(n) messages, <5ms latency"',
            'Throughput: "Maximum 10,000 decisions/second (human review)" ± measurement method',
            'Proof: "P(error) ≤ 10^-6 for deterministic projection by [Theorem 5]"',
          ],
        },
        {
          score: 15,
          description: 'Measurable data but with caveats (error bars, methodology stated)',
        },
        {
          score: 10,
          description: 'Qualitative with some numbers (e.g., "about 1000x faster")',
        },
        {
          score: 5,
          description: 'Mostly qualitative (few or no numbers) [REJECT]',
        },
        {
          score: 0,
          description: 'No quantification whatsoever [AUTO-REJECT]',
        },
      ],
    },
    {
      name: 'Relevance to Thesis Axioms',
      weight: 20,
      levels: [
        {
          score: 20,
          description: 'Directly addresses one axiom (SCALE|REVERSIBILITY|DETERMINISM|COORDINATION|MINIMALITY)',
          examples: [
            'SCALE axiom: "FAA data shows humans review <5,000 decisions/sec"',
            'REVERSIBILITY: "Theorem: irreversible actions cannot be corrected post-hoc"',
            'DETERMINISM: "Probabilistic systems cannot guarantee safety under uncertainty"',
          ],
        },
        {
          score: 15,
          description: 'Constrains multiple axioms (affects >1 constraint)',
        },
        {
          score: 10,
          description: 'Tangentially related (mentions relevant domain but not direct proof)',
        },
        {
          score: 5,
          description: 'Weak connection to thesis [REJECT - insufficient relevance]',
        },
        {
          score: 0,
          description: 'No relevance to thesis axioms [AUTO-REJECT]',
        },
      ],
    },
    {
      name: 'Falsification Strength (if counter-claim)',
      weight: 20,
      levels: [
        {
          score: 20,
          description: 'Strong counter: Formal proof directly violating stated axiom',
          examples: [
            'Proof: "System X achieves safety with non-determinism: [proof]" (violates DETERMINISM)',
            'Counter: "Post-hoc correction succeeded at scale [Theorem 7]" (violates REVERSIBILITY)',
          ],
        },
        {
          score: 15,
          description: 'Medium counter: Specific example violating constraint (but not general proof)',
        },
        {
          score: 10,
          description: 'Weak counter: Suggests alternative but not proven',
        },
        {
          score: 5,
          description: 'Very weak counter: Speculation on alternatives',
        },
        {
          score: 0,
          description: 'Not a falsification (supporting evidence only)',
        },
      ],
    },
  ],

  globalRejectionGates: [
    {
      gate: 'CLASS_C_AUTOMATIC',
      rule: 'If evidence_class = C, score = 0 [IMMEDIATE REJECT]',
      rationale: 'Opinion is not evidence',
    },
    {
      gate: 'NO_QUANTIFICATION',
      rule: 'If quantitative_rigor = 0, reject entire submission',
      rationale: 'Thesis is mathematical; narratives alone cannot support/falsify',
    },
    {
      gate: 'PRIMARY_SOURCE_THRESHOLD',
      rule: 'If primary_sources < 30%, reject',
      rationale: 'Secondary summaries can misrepresent originals',
    },
    {
      gate: 'AXIOM_IRRELEVANCE',
      rule: 'If relevance = 0, reject (off-topic)',
      rationale: 'Evidence must address at least one axiom',
    },
  ],

  thresholds: {
    acceptanceScore: 70,
    rationale: 'Must achieve ≥70/100 to avoid rejection. This means passing ≥3.5 of 5 criteria at full strength.',
    examples: {
      pass: '20+20+20+10+5 = 75 (Class A proof, >80% primary, explicit bounds, directly relevant, not a counter)',
      fail: '10+10+5+5+0 = 30 (Class B, 50% primary, qualitative, weak relevance, irrelevant)',
    },
  },

  scoringWorkflow: [
    '1. Check global rejection gates (Class C, no quantification, <30% primary)',
    '2. Score each criterion independently (0-20)',
    '3. Sum criteria (0-100)',
    '4. If score < 70: REJECT with reason',
    '5. If score ≥ 70: ACCEPT and categorize by axiom',
    '6. If evidence contradicts axiom: PUBLISH FIRST (regardless of supporting count)',
  ],

  axiomMapping: {
    SCALE: {
      description: 'Human-mediated systems collapse at ~10^4 ops/sec',
      supportingEvidence: ['throughput bounds', 'failure cascade studies', 'review latency'],
      falsifyingEvidence: ['system that works at >10^5 human-mediated decisions', 'post-hoc correction at scale'],
    },
    REVERSIBILITY: {
      description: 'Irreversible actions cannot be corrected after execution',
      supportingEvidence: ['impossibility proofs', 'first-error studies', 'real-world irreversibility'],
      falsifyingEvidence: ['system that reversed irreversible action', 'post-hoc correction mechanism'],
    },
    DETERMINISM: {
      description: 'Non-deterministic controllers cannot guarantee safety',
      supportingEvidence: ['probabilistic safety bounds', 'expectation vs safety gaps', 'equivalence proofs'],
      falsifyingEvidence: ['non-deterministic system with formal safety guarantee', 'proof that A ≠ μ(O) is safe'],
    },
    COORDINATION: {
      description: 'Sharding requires commutativity; non-shardable costs Ω(n²)',
      supportingEvidence: ['lower-bound proofs', 'commutativity requirements', 'consensus costs'],
      falsifyingEvidence: ['non-commutative system that shards', 'sub-quadratic coordination bound'],
    },
    MINIMALITY: {
      description: 'No alternative calculus satisfies all constraints',
      supportingEvidence: ['exhaustive falsification', 'uniqueness proofs', 'isomorphism class analysis'],
      falsifyingEvidence: ['alternative calculus that satisfies all axioms', 'proof of non-uniqueness'],
    },
  },

  reportingRequirements: {
    falsifications: {
      order: 'FIRST (before any supporting evidence)',
      format: {
        axiomViolated: 'string (SCALE|REVERSIBILITY|DETERMINISM|COORDINATION|MINIMALITY)',
        claim: 'the counter-claim',
        source: 'primary citation + page',
        formalProof: 'proof sketch or theorem statement',
        score: 'number ≥70 (must meet threshold even as falsification)',
      },
    },
    supportingEvidence: {
      order: 'AFTER falsifications, grouped by axiom',
      format: {
        axiom: 'SCALE|REVERSIBILITY|DETERMINISM|COORDINATION|MINIMALITY',
        claim: 'statement of evidence',
        source: 'citation',
        score: '0-100',
        category: 'PROOF | BENCHMARK | IMPOSSIBILITY | COUNTEREXAMPLE | BOUND',
      },
    },
    gaps: {
      description: 'Explicitly list axioms with <3 supporting evidence items',
      required: true,
    },
    comprehensiveness: {
      metric: 'Percentage of thesis space covered: (domains_researched / domains_possible) × 100',
      threshold: '≥60% for "comprehensive"',
    },
  },
};

/**
 * EXAMPLE EVIDENCE ITEMS (for training)
 * ====================================
 */

export const EXAMPLE_EVIDENCE = [
  {
    id: 'example-pass-1',
    claim: 'Air traffic control tower cannot safely handle >1000 aircraft decisions per hour',
    source: 'FAA Technical Report 2023-ATC-Limits, Section 4.2, page 23',
    classA: true,
    primaryPercent: 85,
    quantitativeRigor: '1000 decisions/hr = 0.278 decisions/sec (measured empirically, 2019-2022)',
    relevantAxiom: 'SCALE',
    expectedScore: 90, // 20+20+20+20+10 = passes all 5 criteria
  },

  {
    id: 'example-pass-2',
    claim: 'Paxos consensus requires Ω(n) messages for n nodes',
    source: 'Lamport, "The Part-Time Parliament", ACM Transactions on Computer Systems, 1998, Theorem 3',
    classA: true,
    primaryPercent: 95,
    quantitativeRigor: 'Ω(n) proven by lower bound argument',
    relevantAxiom: 'COORDINATION',
    expectedScore: 95, // 20+20+20+20+15 = strong falsification direction
  },

  {
    id: 'example-fail-1',
    claim: 'Machine learning could make probabilistic systems safer',
    source: 'Blog post on Medium, 2024',
    classA: false,
    classC: true,
    expectedScore: 0, // Class C = immediate reject
  },

  {
    id: 'example-fail-2',
    claim: 'Humans are really good at multitasking',
    source: 'Opinion',
    quantitativeRigor: 0,
    expectedScore: 0, // No quantification
  },

  {
    id: 'example-counter-1',
    claim: 'CRISPR gene editing corrected sickle cell disease in vivo despite irreversibility',
    source: 'Gillmore et al., Nature Medicine, 2023 (Case Study #2)',
    classA: true,
    primaryPercent: 90,
    quantitativeRigor: '1 patient, permanent correction, irreversible edit',
    relevantAxiom: 'REVERSIBILITY',
    falsificationStrength: 'MEDIUM - corrected irreversible biological state, but: (1) not civilizational scale, (2) took months/years, (3) human-guided, not autonomous',
    expectedScore: 65, // Just below threshold; interesting but doesn't generalize to thesis scope
  },

  {
    id: 'example-counter-2',
    claim: 'Blockchain consensus achieves safety with probabilistic finality',
    source: 'Nakamoto, Bitcoin whitepaper (2008) + analysis',
    classA: true,
    primaryPercent: 85,
    quantitativeRigor: 'P(reversal) = (1/2)^n with n confirmations; n=6 → ~0.0015% risk',
    relevantAxiom: 'DETERMINISM',
    falsificationStrength: 'MEDIUM - achieves practical safety with non-zero error rate. Counter: (1) relies on economic incentives (not pure math), (2) accepts statistical failure, (3) not deterministic',
    expectedScore: 72, // Passes threshold; genuine counter-claim that deserves publication
    publishFirst: true,
  },
];

/**
 * VALIDATOR FUNCTION (reference implementation)
 * ==============================================
 *
 * Usage:
 *   const score = scoreEvidence(evidence, rubric);
 *   if (score < 70) console.log('REJECT:', evidence.claim);
 *   else console.log('ACCEPT:', evidence.claim, 'Score:', score);
 */

export function scoreEvidence(evidence, rubric = SCORING_RUBRIC) {
  let totalScore = 0;
  const breakdown = {};

  // Check global rejection gates
  if (evidence.classC) return { score: 0, reason: 'CLASS_C_IMMEDIATE_REJECT', breakdown };
  if (evidence.quantitativeRigor === 0) return { score: 0, reason: 'NO_QUANTIFICATION', breakdown };
  if (evidence.primaryPercent < 30) return { score: 0, reason: 'PRIMARY_SOURCE_BELOW_30_PERCENT', breakdown };
  if (evidence.relevantAxiom === null || evidence.relevantAxiom === undefined) {
    return { score: 0, reason: 'NOT_RELEVANT_TO_AXIOMS', breakdown };
  }

  // Score each criterion
  const criteriaScores = {
    classA: evidence.classA ? 20 : (evidence.classB ? 10 : 0),
    primarySource: evidence.primaryPercent >= 80 ? 20 : (
      evidence.primaryPercent >= 50 ? 15 : (evidence.primaryPercent >= 30 ? 10 : 5)
    ),
    quantitative: evidence.quantitativeRigor >= 20 ? 20 : (
      evidence.quantitativeRigor >= 15 ? 15 : (evidence.quantitativeRigor >= 10 ? 10 : 5)
    ),
    relevance: evidence.relevantAxiom ? 20 : 0,
    falsification: evidence.falsificationStrength === 'STRONG' ? 20 : (
      evidence.falsificationStrength === 'MEDIUM' ? 15 : (evidence.falsificationStrength === 'WEAK' ? 10 : 0)
    ),
  };

  totalScore = Object.values(criteriaScores).reduce((a, b) => a + b, 0);

  return {
    score: totalScore,
    breakdown: criteriaScores,
    accept: totalScore >= 70,
    reason: totalScore >= 70 ? 'ACCEPT' : `REJECT (${totalScore}/100)`,
  };
}
