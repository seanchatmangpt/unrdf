# ADR-001: Capability Map Template Design

**Status**: Accepted
**Date**: 2025-12-28
**Deciders**: System Architecture Designer
**Context**: UNRDF Documentation Structure

---

## Context

UNRDF has 64+ packages across the monorepo, each with unique capabilities, runtime support, and integration patterns. Documentation was inconsistent, scattered, and lacked:

1. **Evidence-based claims**: No systematic way to verify documentation accuracy
2. **Progressive learning**: No clear path from beginner to expert
3. **Runtime clarity**: Unclear which features work in Node.js vs Browser vs BEAM
4. **Composition patterns**: No documentation of how capabilities combine
5. **Diataxis compliance**: No structured approach to learning/reference/explanation

**Need**: A standardized template for documenting package capabilities that:
- Follows Diataxis framework (4 quadrants)
- Provides evidence for every claim (file:line references, test links)
- Documents runtime compatibility explicitly
- Shows composition patterns
- Enables progressive learning
- Is maintainable via automation (diataxis-kit integration)

---

## Decision

We created the **Capability Map Template** as the canonical documentation structure for all @unrdf packages.

**Template Structure**:
1. **Overview**: Package purpose, capabilities, dependencies, evidence metrics
2. **Capability Atoms**: Tiered catalog (Core/Advanced/Experimental) with evidence
3. **Composition Patterns**: How atoms combine (C1, C2, etc.)
4. **Diataxis Documentation**:
   - Tutorials (Learning-Oriented)
   - How-To Guides (Task-Oriented)
   - Reference (Information-Oriented)
   - Explanation (Understanding-Oriented)
5. **Runtime Compatibility Matrix**: Node.js/Browser/BEAM support per capability
6. **Examples Index**: Runnable code examples
7. **Testing & Verification**: Test suite, OTEL validation
8. **Evidence & Verification**: Source references, verification commands
9. **Migration & Troubleshooting**: Upgrade guides, common issues

---

## Rationale

### Design Principles

#### 1. Evidence-First

**Decision**: Every capability MUST have:
- Source code reference (file:line)
- Test proof (link to passing test)
- Verification command (reproducible)

**Rationale**:
- **Pro**: Prevents documentation drift (docs linked to code)
- **Pro**: Enables automated verification (CI can check links)
- **Pro**: Builds trust (claims backed by proof)
- **Con**: More work upfront - Accepted because quality matters

**Alternative Considered**: Trust-based documentation
- Rejected: Too easy for docs to become stale

**Evidence**: VALIDATED-CAPABILITIES-MAP.md demonstrated this pattern works (300+ capabilities documented with evidence)

---

#### 2. Diataxis Framework

**Decision**: Organize all documentation into 4 quadrants:
- Tutorials: Learning-oriented
- How-Tos: Problem-oriented
- Reference: Information-oriented
- Explanation: Understanding-oriented

**Rationale**:
- **Pro**: Clear separation of concerns (learning vs working vs understanding)
- **Pro**: Progressive disclosure (beginner → expert path)
- **Pro**: Industry-proven framework (used by Django, Gatsby, etc.)
- **Con**: More structure to maintain - Mitigated by templates and diataxis-kit

**Alternative Considered**: Single README approach
- Rejected: Doesn't scale to complex packages, mixes concerns

**Evidence**: DIATAXIS_360.md analysis showed clear gaps in current docs that Diataxis solves

---

#### 3. Capability Atoms + Compositions

**Decision**: Document capabilities at TWO levels:
1. **Atoms**: Individual, testable building blocks
2. **Compositions**: How atoms combine (patterns C1-C15)

**Rationale**:
- **Pro**: Composability is core to UNRDF design
- **Pro**: Shows how simple atoms solve complex problems
- **Pro**: Enables understanding of cross-package integration
- **Con**: Requires understanding composition patterns - Acceptable for target audience

**Alternative Considered**: Document features in isolation
- Rejected: Doesn't show real-world usage patterns

**Evidence**: CAPABILITY-BASIS.md demonstrated this structure (9 categories × 15 compositions)

---

#### 4. Runtime Compatibility Matrix

**Decision**: Explicitly document Node.js/Browser/BEAM support for EVERY capability

**Rationale**:
- **Pro**: UNRDF targets multiple runtimes (Node, Browser, BEAM via AtomVM)
- **Pro**: Prevents user frustration (know before trying)
- **Pro**: Guides development priorities (where to focus)
- **Con**: Maintenance burden - Automated via test matrix

**Alternative Considered**: Document as "universal" and let users discover limitations
- Rejected: Poor UX, wastes developer time

**Evidence**: AtomVM integration showed need for clear runtime boundaries

---

#### 5. Three-Tier Capability Organization

**Decision**: Organize capabilities into tiers:
- **Tier 1 (Core)**: 20% of capabilities, 80% of usage
- **Tier 2 (Advanced)**: 15% usage
- **Tier 3 (Experimental)**: 5% usage

**Rationale**:
- **Pro**: Pareto principle (80/20) guides users to most valuable features first
- **Pro**: Reduces cognitive load (focus on essentials)
- **Pro**: Enables deprecation path (experimental → advanced → core)
- **Con**: Subjective classification - Mitigated by usage analytics

**Alternative Considered**: Flat list of capabilities
- Rejected: Overwhelming for beginners

**Evidence**: VALIDATED-CAPABILITIES-MAP.md demonstrated tiering based on production usage

---

### Template Sections Justification

#### Overview Section

**Purpose**: 30-second decision point

**Content**:
- Package purpose (2-3 sentences)
- Top 3-5 capabilities
- Dependencies
- Evidence metrics

**Rationale**: Developers need to know immediately if this package solves their problem

**Evidence Required**:
- Test coverage %
- Test pass rate
- OTEL score
- Example count

---

#### Capability Atoms Section

**Purpose**: Complete inventory of building blocks

**Content**:
- Table: Atom | Type | Runtime | Evidence | Compositions
- Verification commands
- Source code links

**Rationale**:
- Enables developers to discover ALL capabilities
- Provides evidence for every claim
- Shows what can be composed

**Automation**: Diataxis-kit reference-extractor.mjs auto-generates this from JSDoc

---

#### Composition Patterns Section

**Purpose**: Show how atoms combine to solve problems

**Content**:
- Pattern ID (C1, C2, etc.)
- Description
- Working code example

**Rationale**:
- Real code is more valuable than abstract descriptions
- Shows cross-package integration
- Demonstrates UNRDF composability philosophy

**Example**: C10 (Receipt-Verified Time-Travel) = C2 (Time-Travel RDF) + C6 (Auditable Workflows)

---

#### Tutorial Section (📚)

**Purpose**: Help beginners learn by doing

**Characteristics**:
- Step-by-step
- Safe environment
- Builds confidence
- One clear path

**Structure**:
- Goal (concrete outcome)
- Prerequisites
- Time estimate
- Steps (code + explanation)
- Success criteria
- Next steps

**Rationale**: Tutorials convert curious visitors into active users

**Evidence**: Getting Started tutorials have highest traffic in existing docs

---

#### How-To Section (🛠️)

**Purpose**: Solve specific problems users face at work

**Characteristics**:
- Problem-focused
- Assumes basic knowledge
- One working solution
- Practical

**Structure**:
- Problem statement
- Context (when to use)
- Prerequisites
- Solution (code)
- Verification
- Common pitfalls
- See also

**Rationale**: How-tos address real pain points and reduce support burden

**Evidence**: "How do I..." questions dominate GitHub issues

---

#### Reference Section (📖)

**Purpose**: Complete, accurate technical information

**Characteristics**:
- Comprehensive
- Dry and precise
- Structured for lookup
- One example per API

**Structure**:
- Function signature
- Parameters (types, defaults, constraints)
- Returns
- Throws
- Example
- Runtime compatibility
- Performance
- Source link
- Test link

**Rationale**: Developers need precise information when implementing

**Auto-Generation**: Can be generated from JSDoc using jsdoc2md or diataxis-kit

---

#### Explanation Section (💡)

**Purpose**: Deepen understanding of WHY and HOW

**Characteristics**:
- Conceptual
- Discursive
- Explores alternatives
- Connects ideas

**Structure**:
- Why package exists
- Architecture decisions (ADRs)
- Core concepts
- Performance model
- Integration patterns
- When to use vs alternatives

**Rationale**:
- Explanations turn users into experts
- Reduces misuse and anti-patterns
- Documents design decisions for future maintainers

**Evidence**: ADRs prevent repeated discussions about settled decisions

---

#### Runtime Compatibility Matrix

**Purpose**: Clear boundaries of what works where

**Content**: Table of capabilities × runtimes with notes

**Rationale**:
- UNRDF supports 3 runtimes (Node, Browser, BEAM)
- Not all features work everywhere (e.g., Git requires Node.js)
- Users need to know BEFORE attempting integration

**Format**:
```markdown
| Capability | Node.js | Browser | BEAM/WASM | Notes |
|------------|---------|---------|-----------|-------|
| createStore() | ✅ | ✅ | ⏳ | WASM bridge planned |
```

---

#### Examples Index

**Purpose**: Catalog of working code

**Content**: List of examples with:
- File path
- What it demonstrates
- How to run
- Expected output

**Rationale**:
- Examples are the fastest way to learn
- Runnable code builds confidence
- Serves as integration tests

**Evidence**: examples/ directories have high engagement

---

#### Testing & Verification

**Purpose**: Prove documentation is accurate

**Content**:
- Test suite structure
- Coverage metrics
- OTEL validation
- Verification commands

**Rationale**:
- Tests are proof of capability
- Enables CI/CD validation
- Builds user trust

**Automation**: Coverage auto-updated by CI

---

#### Evidence & Verification Section

**Purpose**: Make all claims verifiable

**Content**:
- Source code references
- Test evidence
- Benchmark evidence
- Verification commands

**Rationale**:
- Prevents documentation drift
- Enables automated checking
- Reduces support burden (users can verify themselves)

**Standard**: All claims must include file:line reference and passing test

---

### Integration with Diataxis-Kit

**Decision**: Template works with diataxis-kit auto-generation

**Workflow**:
1. Diataxis-kit scans package (inventory.mjs)
2. Collects evidence (evidence.mjs)
3. Classifies into Diataxis types (classify.mjs)
4. Generates scaffold (scaffold.mjs)
5. Developer fills in details using template

**Rationale**:
- Automation reduces manual work
- Evidence collection is systematic
- Confidence scores guide where to focus effort
- Deterministic generation ensures consistency

**Evidence**: Diataxis-kit successfully generated scaffolds for 86 packages

---

## Consequences

### Positive

1. **Consistent Documentation**: All packages follow same structure
2. **Evidence-Based**: Every claim backed by code/tests
3. **Progressive Learning**: Clear path from beginner to expert
4. **Runtime Clarity**: Explicit support matrix
5. **Composability**: Shows how capabilities combine
6. **Maintainable**: Automation reduces manual work
7. **Verifiable**: CI can check documentation accuracy

### Negative

1. **Initial Effort**: More work to create first capability map
   - **Mitigation**: Template and diataxis-kit reduce effort by ~60%
2. **Maintenance Burden**: More documentation to keep updated
   - **Mitigation**: Automation updates evidence, CI checks links
3. **Learning Curve**: Contributors must understand Diataxis
   - **Mitigation**: Usage guide and examples

### Neutral

1. **Template Rigidity**: Might not fit all packages perfectly
   - **Response**: Template is a guide, not a straitjacket
2. **Documentation Size**: Capability maps are long
   - **Response**: Structure enables finding what you need quickly

---

## Alternatives Considered

### Alternative 1: Single README per Package

**Description**: Keep existing README-only approach

**Pros**:
- Simple
- Low maintenance
- Familiar to developers

**Cons**:
- Doesn't scale to complex packages
- Mixes learning/reference/explanation
- No systematic evidence
- No runtime clarity

**Rejected**: Doesn't meet requirements for 64-package monorepo

---

### Alternative 2: External Documentation Site Only

**Description**: Move all docs to Nextra site, no package-level docs

**Pros**:
- Centralized
- Better search
- Consistent styling

**Cons**:
- Package and docs separated (harder to keep in sync)
- Offline use difficult
- No package-specific evidence

**Rejected**: Package-level docs serve different purpose (API reference, quick start)

**Compromise**: Both package capability maps AND Nextra site (cross-linked)

---

### Alternative 3: Auto-Generated Only

**Description**: Fully auto-generate docs from JSDoc, no manual content

**Pros**:
- No manual maintenance
- Always up-to-date with code
- Zero documentation debt

**Cons**:
- No tutorials (can't auto-generate learning content)
- No how-tos (can't auto-generate problem-solving)
- No explanations (can't auto-generate design rationale)
- Dry, hard to learn from

**Rejected**: Auto-generation works for reference only

**Compromise**: Auto-generate reference, manually write tutorials/how-tos/explanations

---

### Alternative 4: Minimal Template

**Description**: Much simpler template (just Overview + API Reference)

**Pros**:
- Less work
- Faster to adopt
- Less to maintain

**Cons**:
- Doesn't address learning needs (no tutorials)
- Doesn't address real problems (no how-tos)
- Doesn't explain design (no explanations)
- Same issues as current state

**Rejected**: Doesn't solve the core problems

---

## Implementation

### Phase 1: Template Creation (Complete)

- [x] Design template structure
- [x] Create CAPABILITY-MAP-TEMPLATE.md
- [x] Create CAPABILITY-MAP-USAGE-GUIDE.md
- [x] Document this ADR

### Phase 2: Reference Implementations (In Progress)

- [ ] @unrdf/oxigraph capability map
- [ ] @unrdf/kgc-4d capability map
- [ ] @unrdf/yawl capability map

### Phase 3: Automation Enhancement

- [ ] Diataxis-kit: Auto-generate capability atoms table
- [ ] Diataxis-kit: Auto-extract composition patterns
- [ ] CI: Verify all file:line links are valid
- [ ] CI: Run all verification commands
- [ ] CI: Check OTEL scores meet threshold

### Phase 4: Rollout

- [ ] Document capability maps for Tier 1 packages (20% = 13 packages)
- [ ] Document capability maps for Tier 2 packages (15% = 10 packages)
- [ ] Update CONTRIBUTING.md with capability map requirements

---

## Success Metrics

### Adoption

- **Target**: 50% of packages (32/64) have capability maps within 90 days
- **Measure**: `find packages -name CAPABILITY-MAP.md | wc -l`

### Quality

- **Target**: 100% of capability maps pass verification
- **Measure**: CI job "verify-capability-maps" green

### User Satisfaction

- **Target**: 80% of new users complete first tutorial successfully
- **Measure**: Track tutorial completion rates

### Support Burden

- **Target**: 30% reduction in "how do I..." GitHub issues
- **Measure**: Compare issue tags before/after

---

## Review

**Next Review**: 2025-03-28 (90 days)

**Review Questions**:
1. Are developers actually using the template?
2. Is diataxis-kit integration working well?
3. Are users finding what they need faster?
4. What sections are most/least valuable?
5. What's the maintenance burden in practice?

---

## References

- **Diataxis Framework**: https://diataxis.fr/
- **VALIDATED-CAPABILITIES-MAP.md**: Evidence this approach works
- **CAPABILITY-BASIS.md**: Demonstrates composition patterns
- **DIATAXIS_360.md**: Analysis showing need for structure
- **Diataxis-kit README**: Automation tooling

---

**Status**: Accepted
**Last Updated**: 2025-12-28
**Deciders**: System Architecture Designer
**Related ADRs**:
- ADR-002: Diataxis-Kit Architecture (future)
- ADR-003: Evidence Standards (future)
