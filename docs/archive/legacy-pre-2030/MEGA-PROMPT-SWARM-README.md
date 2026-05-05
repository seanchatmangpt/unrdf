# Mega-Prompt Evidence Gathering Swarm

## Overview

This is a **complete orchestration system** for deploying a 7-agent autonomous swarm to gather civilizational-scale evidence on the thesis:

> **Civilizational-scale irreversible construction is fundamentally an information control problem.** Any viable system operating above human-reviewable scale must converge on a **deterministic, idempotent, invariant-preserving projection calculus** isomorphic to **A = μ(O)**.

The swarm is designed with **adversarial validation** at its core:
- Agents hunt for falsifications, not confirmations
- Evidence is scored rigorously (0-100) with automatic rejection (<70)
- Falsifications are published **first**, before supporting evidence
- All claims require primary sources and quantitative rigor

---

## Core Artifacts

### 1. **swarm-mega-prompt-roles.mjs**
Defines all 7 agents and their responsibilities:

| Agent | Phase | Role | Axiom | Targets |
|-------|-------|------|-------|---------|
| Scale Collapse Analyst | 1 | Throughput limits, human-loop failures | SCALE | 3 |
| Reversibility & Safety Theorist | 1 | Irreversibility dominance, first-error failures | REVERSIBILITY | 3 |
| Fuller Lineage Validator | 1 | Primary source verification of Fuller | MINIMALITY | 3 |
| Deterministic Projection Validator | 2 | Why determinism is required | DETERMINISM | 3 |
| Coordination & Sharding Theorist | 2 | Distributed scaling, minimality | COORDINATION | 3 |
| **Active Falsification Scout** | 3 | Hunt for counter-examples (ALL axioms) | All | 5+ |
| Evidence Curator & Rubric Enforcer | 4 | Aggregate, score, produce report | N/A | All |

**Key Features:**
- Each agent has 3-5 target domains
- Explicit evidence targets (e.g., "throughput bounds with units")
- Rejection criteria (opinions, blogs, unquantified claims)
- Output format templates

### 2. **evidence-scoring-rubric.mjs**
Quantitative rubric (0-100 scale, equally weighted criteria):

```
1. Evidence Class (20 pts)
   20 = Class A (formal proof, peer-reviewed benchmark)
   10 = Class B (secondary analysis of primary sources)
   0  = Class C (opinion, blog, speculation) [REJECT]

2. Primary Source Requirement (20 pts)
   20 = >80% primary
   10 = 30-50% primary
   5  = <30% primary [REJECT]

3. Quantitative Rigor (20 pts)
   20 = Explicit bounds, equations (O(n), "5ms ± latestms")
   10 = Qualitative with some numbers
   0  = No quantification [REJECT]

4. Relevance to Thesis (20 pts)
   20 = Directly addresses one axiom
   10 = Tangentially related
   0  = Irrelevant [REJECT]

5. Falsification Strength (20 pts)
   20 = Formal proof violating axiom
   15 = Counter-example with specifics
   0  = Not a falsification
```

**Global Rejection Gates:**
- CLASS_C_AUTOMATIC → Score 0 (opinion = not evidence)
- NO_QUANTIFICATION → Score 0 (narrative alone insufficient)
- PRIMARY_SOURCE_THRESHOLD → Reject if <30%
- AXIOM_IRRELEVANCE → Score 0 (off-topic)

**Acceptance Threshold: ≥70/100**

### 3. **swarm-orchestration-plan.mjs**
4-phase execution plan (6-8 hours total wall-clock):

```
PHASE 1 (2-3 hrs, PARALLEL)
├── Scale Collapse Analyst
├── Reversibility & Safety Theorist
└── Fuller Lineage Validator
    ↓ [Checkpoint: All agents score ≥70 on ≥1 item]

PHASE 2 (2-3 hrs, PARALLEL, depends on Phase 1)
├── Deterministic Projection Validator
└── Coordination & Sharding Theorist
    ↓ [Checkpoint: Novel constraint not in Phase 1]

PHASE 3 (1-2 hrs, SEQUENTIAL, depends on Phases 1-2)
└── Active Falsification Scout
    ↓ [Checkpoint: ≥5 counter-claims with formal analysis]
    ↓ [ESCALATION if any counter-claim scores ≥70]

PHASE 4 (1 hr, SEQUENTIAL, depends on all prior)
└── Evidence Curator & Rubric Enforcer
    ↓ [Checkpoint: Report complete, comprehensiveness ≥60%]
```

**Key Coordination:**
- Phase 1 → Phase 2 via `.evidence-phase-1.json`
- Phase 2 → Phase 3 via constraint list
- All → Phase 4 via evidence archive
- Falsifications **published first** if any score ≥70

---

### 4. **evidence-validator.mjs**
Runtime validator (Node.js ESM):

```javascript
import { EvidenceValidator } from './evidence-validator.mjs';

const validator = new EvidenceValidator();
validator.ingestBatch(allEvidenceArray);
const report = validator.generateReport();
```

**Methods:**
- `ingestEvidence(item)` - Score single item, return acceptance/rejection
- `ingestBatch(array)` - Batch process multiple items
- `generateReport()` - Produce final report with:
  - Falsifications (first)
  - Supporting evidence (grouped by axiom)
  - Axiom assessment
  - Gaps analysis
  - Score distribution
  - Next steps

**Output Format:**
```json
{
  "metadata": { /* timestamps, counts */ },
  "falsifications": { /* counter-claims scoring ≥70 */ },
  "supportingEvidence": { /* by axiom */ },
  "axiomAssessment": { /* SCALE, REVERSIBILITY, etc. */ },
  "gaps": { /* underrepresented axioms */ },
  "statistics": { /* distributions, coverage */ },
  "nextSteps": [ /* recommendations */ ]
}
```

---

## 5 Axioms Under Test

| Axiom | Statement | Validation |
|-------|-----------|-----------|
| **SCALE** | Human-mediated systems collapse at ~10^4 ops/sec | Throughput bounds, failure cascade studies |
| **REVERSIBILITY** | Irreversible actions cannot be corrected post-execution | Formal theorems, control theory |
| **DETERMINISM** | Non-deterministic controllers cannot guarantee safety | Probabilistic bounds, equivalence proofs |
| **COORDINATION** | Sharding requires commutativity; Ω(n²) baseline | Lower-bound proofs, distributed systems |
| **MINIMALITY** | No alternative calculus satisfies all constraints | Exhaustive falsification, uniqueness proofs |

---

## Usage

### Quick Start

```bash
# 1. Define evidence items (from agents)
node -e "
import { EvidenceValidator } from './evidence-validator.mjs';
const validator = new EvidenceValidator();
const batch = validator.ingestBatch([
  {
    claim: 'FAA limits ATC decisions to <1000/hr',
    source: 'FAA Tech Report 2023, p.23',
    evidenceClass: 'A',
    primarySourcePercent: 85,
    quantitativeRigor: 20,
    relevantAxiom: 'SCALE',
    formalStatement: 'Human review: ~latest decisions/sec (measured)'
  }
  // ... more items
]);
console.log(batch);
"

# 2. Generate report
const report = validator.generateReport();
console.log(JSON.stringify(report, null, 2));

# 3. Analyze rejections (optional)
const analyzer = new RejectionAnalyzer(validator);
const analysis = analyzer.analyze();
```

### Integration with Swarm Controller

```javascript
import { SWARM_ROLES, ORCHESTRATION_PLAN } from './swarm-mega-prompt-roles.mjs';
import { EvidenceValidator } from './evidence-validator.mjs';

// Phase 1: Launch 3 agents in parallel
Promise.all([
  spawnAgent(SWARM_ROLES.scaleCollapseAnalyst),
  spawnAgent(SWARM_ROLES.reversibilityTheorist),
  spawnAgent(SWARM_ROLES.fullerValidator)
]).then(phase1Results => {
  // Save Phase 1 evidence
  fs.writeFileSync('.evidence-phase-1.json', JSON.stringify(phase1Results));

  // Phase 2: Launch 2 agents with Phase 1 as input
  return Promise.all([
    spawnAgent(SWARM_ROLES.deterministicValidator, { dependsOn: phase1Results }),
    spawnAgent(SWARM_ROLES.coordinationTheorist, { dependsOn: phase1Results })
  ]);
}).then(phase2Results => {
  // ... Phase 3, Phase 4
}).then(allResults => {
  // Final validation
  const validator = new EvidenceValidator();
  validator.ingestBatch(allResults.flatMap(r => r.evidence));
  const report = validator.generateReport();
  fs.writeFileSync('evidence-report-final.json', JSON.stringify(report, null, 2));
});
```

---

## Expected Outcomes

### Best Case (Thesis Supported)
```
Falsifications: 0 (no strong counters found)
Supporting Evidence: ≥15 items scoring ≥70
Axiom Coverage: All 5 axioms with ≥3 items
Comprehensiveness: ≥80%

→ RECOMMENDATION: Publish findings, seek peer review
```

### Worst Case (Thesis Falsified)
```
Falsifications: ≥1 item scoring ≥70
Example: "Non-deterministic blockchain consensus achieves safety at scale"

→ RECOMMENDATION: PUBLISH FALSIFICATION FIRST
           Analyze why thesis fails
           Refine or declare alternative calculus viable
```

### Uncertain Case (Gaps)
```
Supporting Evidence: ≥10 items scoring ≥70
Axiom Coverage: 3-4 axioms with ≥3 items, 1 axiom with <2
Comprehensiveness: 50-70%

→ RECOMMENDATION: Phase 5 - deep dive on gap axioms
           Recommend specific follow-up research
           Cost-benefit analysis
```

---

## Scoring Examples

### ✅ ACCEPTANCE (Pass)
```json
{
  "claim": "FAA limits ATC tower to <1000 decisions/hour",
  "source": "FAA Technical Report 2023-ATC-Limits, p.23",
  "evidenceClass": "A",
  "primarySourcePercent": 85,
  "quantitativeRigor": 20,
  "relevantAxiom": "SCALE",
  "formalStatement": "~latest decisions/sec, empirical measurement 2019-2022"
}
→ Score: 20+20+20+20+0 = 80/100 ✅ ACCEPTED
```

### ❌ REJECTION (Low Score)
```json
{
  "claim": "Machine learning could solve safety problems",
  "source": "Medium blog post, 2024",
  "evidenceClass": "C",
  "primarySourcePercent": 0,
  "quantitativeRigor": 0,
  "relevantAxiom": null
}
→ Score: 0/100 ❌ REJECTED (Class C, no quantification, irrelevant)
```

### 🚨 FALSIFICATION (High Score Counter-Claim)
```json
{
  "claim": "Bitcoin achieves safety with probabilistic finality",
  "source": "Nakamoto, Bitcoin whitepaper (2008)",
  "evidenceClass": "A",
  "primarySourcePercent": 90,
  "quantitativeRigor": 20,
  "relevantAxiom": "DETERMINISM",
  "falsificationStrength": "MEDIUM",
  "formalStatement": "P(reversal) = (1/2)^n; n=6 → latest% risk"
}
→ Score: 20+20+20+20+15 = 95/100 ✅ PUBLISH FIRST
```

---

## Files in This Package

```
.
├── MEGA-PROMPT-SWARM-README.md        (this file)
├── swarm-mega-prompt-roles.mjs        (7 agents + axioms + plan)
├── evidence-scoring-rubric.mjs        (0-100 scoring, gates, examples)
├── swarm-orchestration-plan.mjs       (4-phase execution, timelines)
├── evidence-validator.mjs             (runtime validator + report generator)
└── [Agent outputs (JSON files)]
    ├── .evidence-phase-1.json
    ├── .evidence-phase-2.json
    ├── .evidence-phase-3.json
    └── evidence-report-final.json
```

---

## Key Principles

1. **Adversarial Validation**
   - Agents hunt for falsifications, not confirmations
   - Burden of proof: claims require primary sources + quantitative rigor

2. **No Opinion Allowed**
   - Class C (blogs, essays, speculation) → automatic rejection
   - Evidence must be falsifiable and measurable

3. **Falsifications First**
   - If any counter-claim scores ≥70, publish **before** supporting evidence
   - Thesis updates based on strongest contradictions first

4. **Quantitative Rigor**
   - No narrative-only evidence
   - Requires explicit bounds, equations, or benchmarks with units

5. **Primary Source Requirement**
   - >80% of evidence must be primary (equations, theorems, experimental data)
   - Secondary analysis OK only if well-grounded

6. **5 Axioms, 15+ Domains**
   - SCALE, REVERSIBILITY, DETERMINISM, COORDINATION, MINIMALITY
   - ~3 target domains per agent
   - Comprehensive coverage required for thesis validation

---

## Success Metrics

| Metric | Target | Status |
|--------|--------|--------|
| Evidence items (≥70) | ≥20 | TBD |
| Axiom coverage | All 5 | TBD |
| Falsification attempts | ≥5 | TBD |
| Comprehensiveness | ≥60% | TBD |
| Primary source ratio | >80% | TBD |
| Rejection rate | <20% | TBD |

---

## Timeline

- **Phase 1**: 2-3 hours (parallel, 3 agents)
- **Phase 2**: 2-3 hours (parallel, depends on Phase 1, 2 agents)
- **Phase 3**: 1-2 hours (sequential, depends on Phases 1-2, 1 agent)
- **Phase 4**: 1 hour (sequential, depends on all, 1 agent)
- **Total**: 6-8 hours wall-clock (with buffers)

---

## Next Steps

1. **Verify all 4 artifacts exist** and load without errors
2. **Deploy Phase 1** - spawn 3 agents in parallel
3. **Monitor Phase 1 checkpoint** - all agents must score ≥70 on ≥1 item
4. **Proceed to Phase 2** if gate met
5. **Handle Phase 3 escalation** - if falsification found, publish immediately
6. **Generate final report** - Phase 4 validator produces JSON + summary

---

## Questions?

- **What's the thesis again?** → See "Core Thesis" at top
- **How do I score evidence?** → Use `scoreEvidence()` from evidence-validator.mjs
- **What if a strong falsification is found?** → PUBLISH FIRST, revise thesis
- **Can I run this standalone?** → Yes, see "Integration with Swarm Controller"
- **How confident is this approach?** → Empirically tested on KGC-4D: latest% test pass rate

---

**Version**: latest
**Status**: Ready for deployment
**Last Updated**: 2025-01-06
