# HTF: Hyper-Thesis Framework
## A Unified μ-Architecture for Academic Writing

**Version:** 1.0
**Date:** November 18, 2024
**Status:** Specification Complete, React Implementation Available

---

## Table of Contents

1. [Vision](#vision)
2. [The Seven Academic Writing Modes](#the-seven-academic-writing-modes)
3. [Core Concepts](#core-concepts)
4. [Three Operations: Λ, Π, Γ](#three-operations)
5. [React Hooks API](#react-hooks-api)
6. [Usage Examples](#usage-examples)
7. [Advanced Topics](#advanced-topics)
8. [FAQ](#faq)

---

## Vision

**Problem:** Academic thesis writing typically follows one of seven distinct modes (IMRaD, Papers, Argument, Contribution, Monograph, DSR, Narrative). Thesis writers struggle to integrate multiple modes while maintaining coherence.

**Solution:** The Hyper-Thesis Framework (HTF) provides a unified μ-architecture that:
1. Represents thesis content as Δ-shards (modular chunks)
2. Orders shards with Λ-scheduling (total ordering respecting dependencies)
3. Merges shards with Π-profiles (ensuring cross-family coherence)
4. Validates shards with Γ-checking (enforcing Q-invariants)
5. Converges to μ-fixed point (idempotent closure where thesis stabilizes)

**Result:** Thesis writers get automatic:
- Chapter scheduling with critical path analysis
- Coherence metrics across 7 writing modes
- Violation detection and auto-fixing
- Progress tracking toward convergence

---

## The Seven Academic Writing Modes

### 1. **IMRaD** - Introduction, Method, Results, Discussion
**Best for:** Empirical research, experiments, studies
**Canonical shards:** intro, method, result, discuss
**Goal:** Present findings in standardized format for reproducibility

### 2. **Thesis-by-Papers** - Multi-article modularity
**Best for:** Compilation theses, article-based PhDs
**Canonical shards:** paper1, paper2, paper3, synthesis
**Goal:** Integrate multiple publications into unified thesis

### 3. **Argument** - Toulmin reasoning
**Best for:** Philosophy, humanities, critical analysis
**Canonical shards:** claim, ground, proof, objection, reply
**Goal:** Build logically sound arguments with counterargument handling

### 4. **Contribution** - Problem-solution arc
**Best for:** Engineering, design, innovation-focused research
**Canonical shards:** gap, design, eval, impact
**Goal:** Show how research fills a specific gap and creates impact

### 5. **Monograph** - Deep contextual narrative
**Best for:** Literary studies, historical analysis, comprehensive reviews
**Canonical shards:** context, canon, method, analysis, conclusion
**Goal:** Provide deep, historically-grounded analysis

### 6. **DSR** - Design Science Research
**Best for:** Systems design, software engineering, artifact-centric work
**Canonical shards:** problem, artifact, eval, theory
**Goal:** Design and evaluate artifacts, contributing theory

### 7. **Narrative** - Experiential knowledge creation
**Best for:** Reflective practice, qualitative research, autoethnography
**Canonical shards:** field, voice, pattern, insight
**Goal:** Create knowledge through experiential storytelling

---

## Core Concepts

### Δ-Shard (Delta-Shard)
A modular piece of thesis content with:
- **id:** Unique identifier
- **family:** Which of the 7 modes (imrad, papers, argument, contribution, monograph, dsr, narrative)
- **label:** Human-readable title
- **content:** The actual text/content
- **weight:** Importance (0-1)
- **dependencies:** Other shards that must come before

```javascript
{
  id: 'intro',
  family: 'imrad',
  label: 'Introduction: Research Problem',
  content: 'This thesis addresses the challenge of...',
  weight: 0.8,
  dependencies: ['problem'],
  metadata: { pages: 15 }
}
```

### Q-Invariants (Query Invariants)
Constraints that MUST be preserved across all layers:

```javascript
StandardInvariants = [
  {
    id: 'Q-coherence',
    description: 'Each shard must contribute meaningfully (min 100 chars)',
    predicate: (shard) => shard.content?.length > 100,
    appliesTo: ['imrad', 'papers', 'argument', 'contribution', 'monograph', 'dsr', 'narrative']
  },
  {
    id: 'Q-positioning',
    description: 'Shard dependencies must be valid and acyclic',
    predicate: (shard) => Array.isArray(shard.dependencies),
    appliesTo: ['*']
  },
  {
    id: 'Q-contribution',
    description: 'Shard must have positive weight',
    predicate: (shard) => shard.weight > 0,
    appliesTo: ['*']
  }
]
```

### μ-Fixed Point (Mu-Fixed Point)
The convergence target where:
- τ(A) = A (thesis is idempotent)
- drift < 0.05 (< 5% deviation)
- All Q-invariants satisfied
- All families represented coherently

---

## Three Operations

### 1. **Λ (Lambda) - Total Ordering**

Creates a complete schedule of all shards respecting dependencies and deadlines.

**Input:** Δ-shards, deadline, available weeks
**Process:**
1. Topological sort respecting dependencies
2. Follow canonical order when possible (IntroΔ → MethodΔ → ResultΔ → DiscussΔ → ...)
3. Compute critical path (longest dependency chain)
4. Allocate time proportionally to shard weights
5. Buffer time for integration (default 10%)

**Output:** LambdaOrder
```javascript
{
  chain: ['problem', 'gap', 'claim', 'intro', ...],  // Total order
  positions: Map { 'intro' => 3, 'method' => 4, ... },
  criticalPath: ['problem', 'artifact', 'theory', 'conclusion']
}
```

**React Hook:** `useLambdaScheduling`
```javascript
const scheduler = useLambdaScheduling(shards, {
  deadline: new Date('2025-03-01'),
  weeksAvailable: 20
});

// Get schedule
scheduler.state.schedule;  // Week-by-week schedule
scheduler.scheduling.criticalPath;  // Critical path
scheduler.scheduling.slack;  // Weeks of slack before deadline
```

### 2. **Π (Pi) - Merge**

Merges all Δ-shards from different families into a single coherent A (argument/architecture).

**Input:** Δ-shards with family assignments
**Process:**
1. Group shards by family
2. Find cross-family dependencies (merge points)
3. Calculate coherence metrics:
   - Family balance (even weight distribution)
   - Family coverage (% of families represented)
   - Integration strength (cross-family connections)
   - Completeness (% of expected shards)
4. Overall coherence = weighted combination (0-1)

**Output:** PiMerge
```javascript
{
  unifiedId: 'unified-thesis-42',
  components: ['intro', 'method', 'result', ...],
  mergePoints: {
    'imrad:method_to_dsr:artifact': {
      fromShard: 'method',
      toShard: 'artifact',
      strength: 1
    }
  },
  coherence: 0.91  // 0-1 scale
}
```

**React Hook:** `usePiProfile`
```javascript
const profiler = usePiProfile(shards);

// Get coherence metrics
profiler.analysis.coherence;  // { overall: 0.91, familyBalance: 0.88, ... }

// Get profile by family
profiler.analysis.profileByFamily;  // { imrad: {...}, papers: {...}, ... }

// Get recommendations for improvement
profiler.analysis.recommendations;  // Missing shards, reweighting suggestions

// Visualize for chord/sankey diagram
profiler.getMergeVisualizationData();
```

### 3. **Γ (Gamma) - Globalization**

Validates all Δ-shards against Q-invariants and tracks convergence to μ-fixed point.

**Input:** Δ-shards, Q-invariants
**Process:**
1. For each invariant and shard:
   - Check if invariant applies to shard's family
   - Run invariant predicate
   - Record violations if fail
2. Compute drift = (violations / total shards) × severity-weighted
3. Track evolution: drift should decrease epoch → epoch
4. Converged when: drift < 0.05 AND violations = 0

**Output:** GammaGlobalization
```javascript
{
  thesisId: 'thesis-2024-11-18',
  shardMap: Map { 'intro' => 'section-1', 'method' => 'section-2', ... },
  invariants: ['Q-coherence', 'Q-positioning', 'Q-contribution', ...],
  drift: 0.03,  // 3% (converged!)
  violations: []
}
```

**React Hook:** `useGammaChecker`
```javascript
const validator = useGammaChecker(shards, customInvariants, {
  autoCheck: true,
  onViolation: (violations) => console.log('Found issues:', violations),
  onConvergence: () => console.log('Thesis converged!')
});

// Get validation state
validator.state.violations;  // All violations
validator.state.drift;  // % drift from fixed point
validator.state.isConverged;  // Boolean

// Auto-fix violations
validator.fixViolations();

// Get detailed analysis
validator.getViolationAnalysis();  // By severity, by family, by invariant

// Track evolution
validator.evolution;  // { epoch: 5, distance: 0.03, isConverged: true }
```

---

## React Hooks API

### `useLambdaScheduling(shards, options)`

**Purpose:** Create total order (Λ-chain) and schedule shards across timeline

**Returns:**
- `state.shards` - Current shards
- `state.lambdaOrder` - Computed Λ-ordering
- `state.schedule` - Week-by-week schedule
- `state.progress` - Completion status per shard
- `state.totalWeeks` - Weeks until deadline
- `state.overallProgress` - % complete (0-1)

**Methods:**
- `addShard(shard)` - Add new shard
- `updateShard(shardId, updates)` - Modify shard
- `removeShard(shardId)` - Remove shard
- `reorderShards(newOrder)` - Adjust Λ-chain manually
- `adjustTimings(buffer)` - Auto-optimize schedule
- `generateScheduleReport(format)` - Export schedule (markdown/json/gantt)

**Example:**
```javascript
const { state, scheduling, addShard, generateScheduleReport } = useLambdaScheduling(shards, {
  deadline: new Date('2025-03-01'),
  onScheduleChange: (event) => console.log(event)
});

// Get critical path
scheduling.criticalPath.path;  // Array of shard IDs
scheduling.criticalPath.duration;  // Weeks required

// Add buffer time for final integration
scheduling.upcomingMilestones;  // Next 5 deadlines

// Export as markdown
const markdown = generateScheduleReport('markdown');
```

### `usePiProfile(shards, options)`

**Purpose:** Analyze and visualize Π-merge (how families integrate coherently)

**Returns:**
- `state.shards` - Current shards
- `state.weights` - Custom weights per shard
- `state.mergeStrategy` - Current strategy (balanced/emphasize-gaps/etc)
- `state.piMerge` - Computed merge structure

**Methods:**
- `setShardWeight(shardId, weight)` - Adjust importance
- `suggestReweighting()` - AI suggest weight changes
- `applyMergeStrategy(strategy)` - Apply pre-defined strategy
- `recommendMissingShards()` - What's missing?
- `generateProfileReport(format)` - Export profile
- `getMergeVisualizationData()` - For visualization

**Example:**
```javascript
const { analysis, profileByFamily, mergePoints, setShardWeight, applyMergeStrategy } = usePiProfile(shards);

// Check family representation
analysis.profileByFamily['imrad'];  // { count: 4, status: 'complete', ... }

// Get cross-family connections
mergePoints;  // Array of { from, to, strength, connections }

// Improve coherence
applyMergeStrategy('emphasize-gaps');  // Boost contribution family

// Get metrics
analysis.coherence;  // { overall: 0.91, familyBalance: 0.88, ... }
```

### `useGammaChecker(shards, customInvariants, options)`

**Purpose:** Validate and track convergence toward μ-fixed point

**Returns:**
- `state.violations` - All violations found
- `state.drift` - Distance from fixed point
- `state.isConverged` - Boolean
- `evolution` - Epoch tracking, distance history

**Methods:**
- `validateShards()` - Manual validation
- `addCustomInvariant(invariant)` - Add Q to check
- `removeCustomInvariant(id)` - Remove custom Q
- `suggestFixesForViolation(violationId)` - Get fix suggestions
- `fixViolations(violationIds)` - Auto-apply fixes
- `generateValidationReport(format)` - Export report
- `getViolationAnalysis()` - By severity/family/invariant

**Example:**
```javascript
const { state, evolution, fixViolations, isDriftDecreasing } = useGammaChecker(shards, [], {
  autoCheck: true,
  onConvergence: () => window.alert('✅ Thesis converged!')
});

// Track progress
console.log(`Epoch ${evolution.epoch}: drift = ${(state.drift * 100).toFixed(1)}%`);

// Are we improving?
if (isDriftDecreasing) {
  console.log('✅ Drift is decreasing - thesis converging!');
}

// Fix all violations
if (state.violations.length > 0) {
  fixViolations();  // Auto-fix
}

// Converged?
if (state.isConverged) {
  console.log('✅ THESIS READY FOR SUBMISSION');
}
```

### `useHTFFramework(config)`

**Purpose:** Master hook combining Λ, Π, Γ into unified interface

**Parameters:**
```javascript
{
  shards: [],  // Initial shards
  deadline: new Date(),
  weeksAvailable: null,
  callbacks: {
    onScheduleChange: () => {},
    onMergeChange: () => {},
    onViolation: () => {},
    onConvergence: () => {}
  }
}
```

**Returns:**
- `state` - Current shards and mode
- `scheduling` - Λ subsystem
- `profile` - Π subsystem
- `validation` - Γ subsystem
- `addShard/updateShard/removeShard` - Master operations
- `optimizeThesis()` - Run full optimization
- `recommendNextSteps()` - What to do next?
- `generateThesisReport(format)` - Complete report
- `getThesisStats()` - Summary statistics

**Example:**
```javascript
const framework = useHTFFramework({
  shards: thesisShards,
  deadline: new Date('2025-03-01')
});

// Get overall status
const stats = framework.getThesisStats();
console.log(`Coherence: ${(stats.quality.coherence * 100).toFixed(1)}%`);
console.log(`Violations: ${stats.quality.violations}`);
console.log(`Converged: ${stats.quality.isConverged}`);

// Get recommendations
framework.recommendNextSteps().forEach(step => {
  console.log(`${step.priority}. ${step.label} - ${step.detail}`);
});

// Run optimization
await framework.optimizeThesis();

// Export final report
const report = framework.generateThesisReport('markdown');
```

---

## Usage Examples

### Example 1: Create and Schedule a Thesis

```javascript
import { useLambdaScheduling, DeltaFamilies } from 'unrdf/htf';

function MyThesisApp() {
  const [shards, setShards] = useState([
    {
      id: 'problem',
      family: 'dsr',
      label: 'Research Problem',
      content: 'Problem statement...',
      weight: 0.95,
      dependencies: []
    },
    {
      id: 'artifact',
      family: 'dsr',
      label: 'Proposed Solution',
      content: 'Solution design...',
      weight: 0.95,
      dependencies: ['problem']
    }
    // ... more shards
  ]);

  const scheduler = useLambdaScheduling(shards, {
    deadline: new Date('2025-06-01'),
    weeksAvailable: 26
  });

  return (
    <div>
      <h2>Thesis Schedule</h2>
      <p>Critical Path: {scheduler.scheduling.criticalPath.duration} weeks</p>
      <p>Slack Available: {scheduler.scheduling.slack.toFixed(1)} weeks</p>
      <button onClick={() => {
        const md = scheduler.generateScheduleReport('markdown');
        console.log(md);
      }}>Export Schedule</button>
    </div>
  );
}
```

### Example 2: Analyze Cross-Family Coherence

```javascript
import { usePiProfile } from 'unrdf/htf';

function ThesisCoherenceAnalyzer({ shards }) {
  const profiler = usePiProfile(shards);

  return (
    <div>
      <h2>Thesis Coherence Analysis</h2>
      <p>Overall: {(profiler.analysis.coherence.overall * 100).toFixed(1)}%</p>

      <h3>By Family:</h3>
      {Object.entries(profiler.analysis.profileByFamily).map(([family, profile]) => (
        <div key={family}>
          <strong>{profile.label}:</strong> {profile.count}/{profile.expectedCount}
        </div>
      ))}

      <h3>Missing Shards:</h3>
      {profiler.analysis.recommendations.map((rec, i) => (
        <div key={i}>[{rec.priority}] {rec.reason}</div>
      ))}
    </div>
  );
}
```

### Example 3: Validate and Auto-Fix

```javascript
import { useGammaChecker } from 'unrdf/htf';

function ThesisValidator({ shards, onReady }) {
  const validator = useGammaChecker(shards, [], {
    autoCheck: true,
    onConvergence: () => onReady()
  });

  return (
    <div>
      <h2>Thesis Validation</h2>
      <p>Status: {validator.state.isConverged ? '✅ Ready' : '⚠️ In Progress'}</p>
      <p>Drift: {(validator.state.drift * 100).toFixed(1)}%</p>
      <p>Violations: {validator.state.violations.length}</p>

      {validator.state.violations.length > 0 && (
        <button onClick={() => validator.fixViolations()}>
          Auto-Fix All Violations
        </button>
      )}
    </div>
  );
}
```

### Example 4: Complete Thesis Dashboard

```javascript
import { useHTFFramework } from 'unrdf/htf';

function ThesisDashboard({ shards }) {
  const framework = useHTFFramework({
    shards,
    deadline: new Date('2025-06-01')
  });

  const stats = framework.getThesisStats();
  const nextSteps = framework.recommendNextSteps();

  return (
    <div>
      <h1>Thesis Dashboard</h1>

      <div className="stats">
        <div>Coherence: {(stats.quality.coherence * 100).toFixed(1)}%</div>
        <div>Progress: {(framework.getProgress() * 100).toFixed(0)}%</div>
        <div>Status: {framework.isConverged() ? '✅ Converged' : '⚠️ In Progress'}</div>
      </div>

      <div className="schedule">
        <h3>Schedule</h3>
        <p>Critical Path: {framework.scheduling.scheduling.criticalPath.duration} weeks</p>
        <p>Slack: {framework.scheduling.scheduling.slack.toFixed(1)} weeks</p>
      </div>

      <div className="recommendations">
        <h3>Next Steps</h3>
        {nextSteps.map((step, i) => (
          <div key={i}>
            {step.priority}. {step.label} {step.auto ? '(auto)' : '(manual)'}
          </div>
        ))}
      </div>

      <button onClick={async () => {
        const result = await framework.optimizeThesis();
        console.log('Optimization result:', result);
      }}>Optimize Thesis</button>

      <button onClick={() => {
        const report = framework.generateThesisReport('markdown');
        download(report, 'thesis-report.md');
      }}>Export Report</button>
    </div>
  );
}
```

---

## Advanced Topics

### Custom Invariants

Add your own constraints beyond the standards:

```javascript
const customInvariant = {
  id: 'Q-page-limit',
  description: 'Each shard must not exceed max pages',
  predicate: (shard) => (shard.metadata?.pages || 0) <= 50,
  appliesTo: ['imrad', 'papers', 'monograph']
};

validator.addCustomInvariant(customInvariant);
```

### Merge Strategies

Pre-configured weighting strategies:

```javascript
// Balanced: Equal weight per family
profile.applyMergeStrategy('balanced');

// Emphasize gaps: Prioritize contribution family
profile.applyMergeStrategy('emphasize-gaps');

// Emphasize methods: Prioritize methodology
profile.applyMergeStrategy('emphasize-methods');

// Emphasize narrative: Prioritize storytelling
profile.applyMergeStrategy('emphasize-narrative');
```

### Evolution Tracking

Monitor convergence over time:

```javascript
// Track how drift decreases
if (validator.isDriftDecreasing) {
  console.log('Thesis converging!');
}

// See improvement each epoch
console.log(`Epoch ${evolution.epoch}: ${(evolution.distance * 100).toFixed(1)}% away`);
console.log(`Improvement: ${(evolution.improvement * 100).toFixed(2)}%`);

// Estimate time to convergence
const estimatedEpochsToConverge = evolution.distance * 20;
```

### Rollback & History

Track changes and revert if needed:

```javascript
// Automatic fix history
const history = validator.fixHistory;

// Rollback to previous epoch
const previousShards = validator.rollbackToEpoch(2);
```

---

## FAQ

**Q: How do I know which family to choose for my shard?**
A: Choose the family that best matches the shard's primary function:
- Presenting methods? → imrad
- Building logical argument? → argument
- Showing research contribution? → contribution
- Telling experiential story? → narrative

**Q: Can I use multiple families in one thesis?**
A: Yes! That's the whole point of HTF. Mix families strategically. The Π-merge will analyze cross-family coherence.

**Q: What if I don't have all 7 families?**
A: That's fine. HTF calculates coverage and recommends missing families. You can add them incrementally.

**Q: How is coherence calculated?**
A: `overall = 0.25 × familyBalance + 0.25 × familyCoverage + 0.25 × integrationStrength + 0.25 × completeness`

**Q: What drift < 0.05 means?**
A: Less than 5% deviation from canonical form. Generally, drift < 0.05 = thesis is converged and ready.

**Q: Can I customize the canonical Λ-order?**
A: Yes, with `reorderShards(newOrder)`. But HTF will warn if dependencies are violated.

**Q: How do I export my thesis?**
A: Use `generateThesisReport('markdown')` to export a complete analysis, or `generateScheduleReport('gantt')` for scheduling visualization.

**Q: What's τ (tau)?**
A: Time/evolution. τ(A) is thesis A after one epoch of refinement. At convergence, τ(A) = A (idempotent).

---

## Related Concepts

- **RDF Knowledge Graphs:** HTF shares concepts with RDF (triples → shards, reasoning → validation)
- **SPARC Methodology:** HTF implements SPARC (Specification, Pseudocode, Architecture, Refinement, Code)
- **Goal-Oriented Action Planning:** HTF uses GOAP-like planning for scheduling
- **Sheaf Theory:** Γ-globalization inspired by sheaves (gluing local data)

---

## Citation

If you use HTF in your research, please cite:

```bibtex
@software{htf2024,
  title = {HTF: Hyper-Thesis Framework - Unified Architecture for Academic Writing},
  author = {UNRDF Research Collective},
  year = {2024},
  url = {https://github.com/unrdf/unrdf}
}
```

---

## License

MIT License - Use freely in academic and commercial projects

---

## Support & Contributing

- **Issues:** Report via GitHub issues
- **Discussions:** Join our Discord community
- **Contributing:** See CONTRIBUTING.md for guidelines
- **Contact:** research@unrdf.io

---

**Last Updated:** November 18, 2024
**Framework Version:** 1.0
**Status:** Production Ready ✅
