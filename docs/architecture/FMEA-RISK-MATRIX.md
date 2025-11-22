# FMEA Risk Matrix - Documentation Quality

## Visual Risk Matrix

```
                     OCCURRENCE (Likelihood)
                     1-2   3-4   5-6   7-8   9-10
                    ┌─────┬─────┬─────┬─────┬─────┐
               9-10 │  M  │  H  │  H  │ CR  │ CR  │
                    ├─────┼─────┼─────┼─────┼─────┤
SEVERITY       7-8  │  L  │  M  │  H  │  H  │ CR  │
(Impact)            ├─────┼─────┼─────┼─────┼─────┤
               5-6  │  L  │  M  │  M  │  H  │  H  │
                    ├─────┼─────┼─────┼─────┼─────┤
               3-4  │  L  │  L  │  M  │  M  │  H  │
                    ├─────┼─────┼─────┼─────┼─────┤
               1-2  │  L  │  L  │  L  │  M  │  M  │
                    └─────┴─────┴─────┴─────┴─────┘

Legend: L = Low | M = Medium | H = High | CR = Critical
```

---

## Failure Mode Mapping

### Plotted on Risk Matrix

```
SEVERITY
    ^
 10 │                              ●FM-03
  9 │                         ●FM-02
  8 │                    ●FM-01    ●FM-05    ●FM-10
  7 │               ●FM-04    ●FM-09
  6 │          ●FM-06
  5 │     ●FM-07
  4 │●FM-08
  3 │
  2 │
  1 │
    └────────────────────────────────────────────> OCCURRENCE
      1    2    3    4    5    6    7    8    9   10

Failure Modes:
  FM-01: Information Unfindable     (S:8, O:6) - HIGH
  FM-02: Outdated Examples          (S:9, O:7) - HIGH
  FM-03: API Drift                  (S:9, O:8) - CRITICAL
  FM-04: Missing Prerequisites      (S:7, O:5) - MEDIUM
  FM-05: Incomplete Coverage        (S:8, O:6) - HIGH
  FM-06: Wrong Audience Level       (S:6, O:5) - MEDIUM
  FM-07: Broken Links               (S:5, O:4) - LOW
  FM-08: Stale Screenshots          (S:4, O:6) - LOW
  FM-09: Ambiguous Instructions     (S:7, O:5) - MEDIUM
  FM-10: Missing Error Context      (S:8, O:6) - HIGH
```

---

## Detailed FMEA Analysis

### FM-03: API Drift (CRITICAL)

| Attribute | Value | Notes |
|-----------|-------|-------|
| **Severity** | 9/10 | Developers cannot trust documentation |
| **Occurrence** | 8/10 | Every API change risks drift |
| **Detection** | 5/10 | Manual review catches some |
| **RPN** | **360** | Highest priority |

```
Risk Timeline:
  Code Change ──────────────────────────────────────> Time
       │
       │ [Day 0]          [Day 7]           [Day 30]
       │    │                │                  │
       ▼    ▼                ▼                  ▼
       ●────●────────────────●──────────────────●
       │    │                │                  │
       │    │                │                  └── User complains
       │    │                └── First confusion report
       │    └── Dev notices but doesn't fix
       └── API changes, docs unchanged

Mitigation Strategy:
  ┌─────────────────────────────────────────────────┐
  │  Code Change                                    │
  │       │                                         │
  │       ▼                                         │
  │  ┌────────────┐    ┌────────────┐              │
  │  │ JSDoc Check│───▶│ API Snapshot│             │
  │  └────────────┘    └────────────┘              │
  │       │                  │                      │
  │       ▼                  ▼                      │
  │  ┌────────────┐    ┌────────────┐              │
  │  │ Doc Gen    │◀───│ Compare    │              │
  │  └────────────┘    └────────────┘              │
  │       │                  │                      │
  │       ▼                  ▼                      │
  │  ┌────────────┐    ┌────────────┐              │
  │  │ PR Blocking│◀───│ Drift Alert│              │
  │  └────────────┘    └────────────┘              │
  └─────────────────────────────────────────────────┘

Expected RPN After Mitigation: 90 (S:9, O:2, D:5)
```

---

### FM-05: Incomplete Coverage (HIGH)

| Attribute | Value | Notes |
|-----------|-------|-------|
| **Severity** | 8/10 | Users cannot find needed info |
| **Occurrence** | 6/10 | New features often undocumented |
| **Detection** | 5/10 | Gaps discovered by users |
| **RPN** | **240** | Second priority |

```
Coverage Gap Visualization:

Module Coverage Status:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

knowledge-engine/core     [████████████████████░░░░░] 80%
knowledge-engine/hooks    [████████████████░░░░░░░░░] 65%
knowledge-engine/stream   [████████████░░░░░░░░░░░░░] 50%
knowledge-engine/fed      [████████░░░░░░░░░░░░░░░░░] 35%
react-hooks/core          [██████████████████████░░░] 88%
react-hooks/streaming     [████████████░░░░░░░░░░░░░] 48%
react-hooks/federation    [██████████░░░░░░░░░░░░░░░] 42%
react-hooks/ai-semantic   [████████░░░░░░░░░░░░░░░░░] 32%
cli/commands              [████████████████████░░░░░] 78%
utils                     [████████████████████████░] 95%

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
OVERALL: 61.3% (Target: 100%)

Gap Analysis:
┌─────────────────────────────────────────────────────────┐
│ Priority 1 (High Impact, Easy):                         │
│   - react-hooks/core remaining 12%                      │
│   - knowledge-engine/core remaining 20%                 │
│                                                         │
│ Priority 2 (High Impact, Medium Effort):                │
│   - knowledge-engine/hooks remaining 35%                │
│   - cli/commands remaining 22%                          │
│                                                         │
│ Priority 3 (Medium Impact):                             │
│   - react-hooks/streaming remaining 52%                 │
│   - react-hooks/federation remaining 58%                │
│                                                         │
│ Priority 4 (Lower Impact):                              │
│   - knowledge-engine/stream remaining 50%               │
│   - knowledge-engine/fed remaining 65%                  │
│   - react-hooks/ai-semantic remaining 68%               │
└─────────────────────────────────────────────────────────┘

Mitigation: Definition of Done includes docs
Expected RPN After Mitigation: 80 (S:8, O:2, D:5)
```

---

### FM-10: Missing Error Context (HIGH)

| Attribute | Value | Notes |
|-----------|-------|-------|
| **Severity** | 8/10 | Debug time significantly increased |
| **Occurrence** | 6/10 | Many errors lack context |
| **Detection** | 6/10 | Users report in issues |
| **RPN** | **288** | Third priority |

```
Error Flow (Current vs. Target):

CURRENT:
  Error Thrown ──▶ Generic Message ──▶ User Confused ──▶ GitHub Issue

  "ValidationError: Invalid input"
       │
       └── User has no idea:
           - What input was invalid?
           - Why is it invalid?
           - How to fix it?

TARGET:
  Error Thrown ──▶ Rich Context ──▶ User Self-Serves ──▶ Problem Solved

  "ValidationError [UNRDF_VALIDATION_003]: Hook name is required

   Field: options.name
   Expected: string (1-128 characters)
   Received: undefined

   Fix: Provide a name when calling defineHook()

   Example:
     defineHook({
       name: 'my-hook',  // <- Add this
       ...
     });

   See: https://unrdf.dev/errors/VALIDATION_003"

Error Catalog Structure:
┌─────────────────────────────────────────────────────────┐
│  UNRDF_[CATEGORY]_[NUMBER]                              │
│                                                         │
│  Categories:                                            │
│    HOOK     - Knowledge hook errors                     │
│    QUERY    - SPARQL query errors                       │
│    VALID    - Validation errors                         │
│    PARSE    - Parsing errors                            │
│    STREAM   - Streaming errors                          │
│    FED      - Federation errors                         │
│    AUTH     - Authentication errors                     │
│    CONFIG   - Configuration errors                      │
│                                                         │
│  Number ranges by severity:                             │
│    001-099  - Fatal (cannot continue)                   │
│    100-199  - Error (operation failed)                  │
│    200-299  - Warning (degraded operation)              │
│    300-399  - Info (recoverable)                        │
└─────────────────────────────────────────────────────────┘

Expected RPN After Mitigation: 96 (S:8, O:2, D:6)
```

---

### FM-01: Information Unfindable (HIGH)

| Attribute | Value | Notes |
|-----------|-------|-------|
| **Severity** | 8/10 | Frustration, support burden |
| **Occurrence** | 6/10 | Common with large docs |
| **Detection** | 4/10 | Often not reported |
| **RPN** | **192** | Fourth priority |

```
User Journey Analysis:

CURRENT (Frustrating):
  User Has Question ──▶ Searches ──▶ No Results ──▶ Browses ──▶ Lost ──▶ GitHub Issue

  Time to answer: 15-30 minutes (or never)

TARGET (Efficient):
  User Has Question ──▶ Searches ──▶ Exact Match ──▶ Answer Found

  Time to answer: <2 minutes

Navigation Improvement Plan:
┌─────────────────────────────────────────────────────────┐
│                                                         │
│  PRIMARY: Full-text search (Algolia)                    │
│    │                                                    │
│    └── Results ranked by:                               │
│        1. Exact match in title                          │
│        2. Match in headings                             │
│        3. Match in content                              │
│                                                         │
│  SECONDARY: DIATAXIS quadrant navigation                │
│    │                                                    │
│    ├── "I want to learn" → Tutorials                    │
│    ├── "I want to do X" → How-to                        │
│    ├── "I need to look up Y" → Reference                │
│    └── "I want to understand Z" → Explanation           │
│                                                         │
│  TERTIARY: Contextual cross-references                  │
│    │                                                    │
│    └── Every page has:                                  │
│        - Related tutorials                              │
│        - Related how-to guides                          │
│        - Related API reference                          │
│        - Related explanations                           │
│                                                         │
│  QUATERNARY: "I want to..." shortcuts                   │
│    │                                                    │
│    └── Common tasks with direct links                   │
│                                                         │
└─────────────────────────────────────────────────────────┘

Expected RPN After Mitigation: 64 (S:8, O:2, D:4)
```

---

### FM-02: Outdated Examples (HIGH)

| Attribute | Value | Notes |
|-----------|-------|-------|
| **Severity** | 9/10 | Completely blocks users |
| **Occurrence** | 7/10 | API evolves, examples don't |
| **Detection** | 3/10 | Often not tested |
| **RPN** | **189** | Fifth priority |

```
Example Validation Pipeline:

┌─────────────────────────────────────────────────────────┐
│                                                         │
│  [1] EXTRACTION                                         │
│      │                                                  │
│      └── Parse markdown ──▶ Find code blocks           │
│                             │                           │
│                             └── language: javascript    │
│                                 no skip-validation tag  │
│                                                         │
│  [2] WRAPPING                                           │
│      │                                                  │
│      └── Add necessary imports                          │
│          │                                              │
│          └── Wrap in async IIFE if needed               │
│              │                                          │
│              └── Add mock dependencies                  │
│                                                         │
│  [3] VALIDATION                                         │
│      │                                                  │
│      ├── Syntax check (always)                          │
│      │                                                  │
│      └── Execution check (--execute flag)               │
│                                                         │
│  [4] REPORTING                                          │
│      │                                                  │
│      └── Pass/Fail with line numbers                    │
│          │                                              │
│          └── Error messages for failures                │
│                                                         │
│  [5] CI INTEGRATION                                     │
│      │                                                  │
│      └── Fail PR if examples broken                     │
│                                                         │
└─────────────────────────────────────────────────────────┘

Expected RPN After Mitigation: 54 (S:9, O:2, D:3)
```

---

## Mitigation Priority Order

Based on RPN and mitigation ROI:

| Priority | FM | Issue | RPN | Mitigation | New RPN | ROI |
|----------|-----|-------|-----|------------|---------|-----|
| 1 | FM-03 | API Drift | 360 | JSDoc automation | 90 | 75% |
| 2 | FM-05 | Incomplete Coverage | 240 | DoD includes docs | 80 | 67% |
| 3 | FM-10 | Missing Error Context | 288 | Error catalog | 96 | 67% |
| 4 | FM-01 | Info Unfindable | 192 | Search + navigation | 64 | 67% |
| 5 | FM-02 | Outdated Examples | 189 | Example CI testing | 54 | 71% |
| 6 | FM-09 | Ambiguous Instructions | 175 | Templates + review | 70 | 60% |
| 7 | FM-04 | Missing Prerequisites | 140 | Checklist templates | 42 | 70% |
| 8 | FM-06 | Wrong Audience Level | 180 | Audience indicators | 90 | 50% |
| 9 | FM-07 | Broken Links | 40 | Link checker CI | 10 | 75% |
| 10 | FM-08 | Stale Screenshots | 72 | Screenshot automation | 36 | 50% |

---

## Implementation Timeline

```
PHASE 1: Quick Wins (Week 1-2)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  [x] Link checker CI (FM-07)
  [x] Example validation tool (FM-02)
  [ ] Checklist templates (FM-04)

PHASE 2: Core Infrastructure (Week 3-4)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  [ ] JSDoc automation (FM-03)
  [ ] Coverage measurement (FM-05)
  [ ] Error catalog structure (FM-10)

PHASE 3: Content Improvement (Week 5-6)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  [ ] Navigation overhaul (FM-01)
  [ ] Templates standardization (FM-09)
  [ ] Audience indicators (FM-06)

PHASE 4: Polish (Week 7-8)
━━━━━━━━━━━━━━━━━━━━━━━━━━━
  [ ] Search integration (FM-01)
  [ ] Screenshot automation (FM-08)
  [ ] Full audit and review
```

---

## Monitoring Dashboard

```
┌────────────────────────────────────────────────────────────────────┐
│                    DOCUMENTATION QUALITY DASHBOARD                  │
├────────────────────────────────────────────────────────────────────┤
│                                                                    │
│  OVERALL HEALTH: 76/100 [███████████████░░░░░] GOOD                │
│                                                                    │
├──────────────────────┬──────────────────────┬──────────────────────┤
│  API COVERAGE        │  EXAMPLE PASS RATE   │  LINK VALIDITY       │
│  ████████████████░░░ │  ████████████████████ │  ████████████████░░░│
│  82%  [Target: 100%] │  100% [Target: 100%] │  97%  [Target: 100%] │
├──────────────────────┼──────────────────────┼──────────────────────┤
│  SEARCH SUCCESS      │  USER SATISFACTION   │  DOC-CODE SYNC       │
│  █████████████████░░ │  ████████████████░░░░│  █████████████░░░░░░░│
│  85%  [Target: 90%]  │  4.2/5 [Target: 4.5] │  67%  [Target: 100%] │
└──────────────────────┴──────────────────────┴──────────────────────┘

ALERTS:
  [!] 3 undocumented exports in streaming module
  [!] 2 broken links detected in tutorials
  [i] Coverage dropped 2% since last week
```

---

## Success Criteria

Mitigation is successful when:

| Metric | Current | Target | Measurement |
|--------|---------|--------|-------------|
| Overall RPN | 1836 | <500 | Sum of all FM RPNs |
| Critical issues | 1 | 0 | RPN > 300 |
| High issues | 5 | 0 | RPN 150-300 |
| API Coverage | 62% | 100% | Automated tool |
| Example Pass Rate | 40% | 100% | CI validation |
| User Satisfaction | Unknown | 4.5/5 | Survey |
