# Implementation Plan: Package Maturity Matrix & Synergistic Capabilities

**Branch**: `006-maturity-matrix` | **Date**: 2025-12-20 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/006-maturity-matrix/spec.md`

## Summary

Define a comprehensive maturity matrix for all 21 UNRDF packages and document synergistic capabilities that emerge from package combinations. This feature delivers:

1. **Maturity Assessment**: 5-level framework (L1 Alpha → L5 Enterprise) with weighted scoring
2. **Synergy Documentation**: 11 categories (A-K) covering all 21 packages
3. **Package Scoring Tool**: CLI command to assess and report package maturity
4. **Synergy Discovery Guide**: Interactive documentation for finding package combinations

**Technical Approach**: Generate maturity data from package.json, test coverage, and OTEL metrics. Store assessments as RDF in `@unrdf/core`. Expose via CLI and documentation.

## Technical Context

**Language/Version**: Node.js 18+ with ES modules (MJS + JSDoc, NO TypeScript in src/)
**Primary Dependencies**: @unrdf/core (RDF storage), @unrdf/cli (CLI commands), @unrdf/validation (OTEL metrics), Vitest (test coverage)
**Storage**: RDF triples in @unrdf/core with maturity ontology schema
**Testing**: Vitest with 80%+ coverage requirement
**Target Platform**: Node.js server, CLI tooling
**Project Type**: Monorepo extension (documentation + CLI + data collection)
**Performance Goals**: Package assessment < 5s per package, full report < 30s
**Constraints**: Must integrate with existing 21-package structure, no breaking changes
**Scale/Scope**: 21 packages × 7 assessment criteria × 5 maturity levels = 735 data points

## Constitution Check

_GATE: Must pass before Phase 0 research. Re-check after Phase 1 design._

**Mandatory Compliance Verification**:

- [x] Feature fits within SPARC 5-phase methodology (no shortcuts)
- [x] RDF/SPARQL/SHACL usage confirmed (maturity data stored as RDF triples)
- [x] Test scope ≥80% coverage confirmed for all deliverables
- [x] No "lean" version planned - delivering production-ready first
- [x] Evidence-based: Planning includes measurement approach (OTEL/metrics for package assessment)
- [x] All related operations will run concurrent (batch package analysis)
- [x] Pattern reuse identified and documented (reusing existing CLI, core, validation patterns)
- [x] No suppression comments or workarounds in scope

**Scope Justification** (if violations listed below):

| Violation                            | Why Needed | Simpler Alternative Rejected |
| ------------------------------------ | ---------- | ---------------------------- |
| (none - constitution fully complied) |            |                              |

## Project Structure

### Documentation (this feature)

```text
specs/006-maturity-matrix/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output - maturity framework research
├── data-model.md        # Phase 1 output - RDF ontology for maturity data
├── quickstart.md        # Phase 1 output - how to assess package maturity
├── contracts/           # Phase 1 output - CLI command schemas
│   └── maturity-cli.json
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
# Existing monorepo structure - extending packages

packages/cli/
├── src/
│   ├── commands/
│   │   └── maturity.mjs      # NEW: Maturity assessment CLI
│   └── lib/
│       └── maturity/         # NEW: Maturity scoring logic
│           ├── collector.mjs   # Data collection from packages
│           ├── scorer.mjs      # Weighted scoring algorithm
│           └── reporter.mjs    # Output formatting

packages/core/
├── src/
│   └── ontologies/
│       └── maturity.ttl      # NEW: Maturity ontology schema

packages/validation/
├── src/
│   └── maturity/             # NEW: OTEL-based maturity metrics
│       └── otel-collector.mjs

docs/
├── maturity-matrix.md        # NEW: Published maturity guide
└── synergy-guide.md          # NEW: Package synergy documentation
```

**Structure Decision**: Extends existing packages (cli, core, validation) with maturity-specific modules. No new packages required - follows monorepo conventions.

## Complexity Tracking

> No violations - all work fits within existing patterns

| Violation | Why Needed | Simpler Alternative Rejected Because |
| --------- | ---------- | ------------------------------------ |
| (none)    | N/A        | N/A                                  |

---

## Phase 0: Research Tasks

### Research Questions (from Technical Context)

1. **Maturity Framework Standards**: What industry standards exist for package maturity? (npm, CNCF, etc.)
2. **Coverage Collection**: How to programmatically collect test coverage from Vitest across 21 packages?
3. **OTEL Integration**: How to use existing @unrdf/validation OTEL for maturity metrics?
4. **RDF Ontology**: Best practices for representing maturity levels in RDF/OWL?
5. **CLI Patterns**: Existing CLI command patterns in @unrdf/cli for consistency?

### Research Output: See `research.md`

---

## Phase 1: Design Deliverables

### 1.1 Data Model (`data-model.md`)

- RDF ontology for maturity assessment
- Package entity with maturity properties
- Synergy relationship definitions
- SHACL shapes for validation

### 1.2 Contracts (`contracts/maturity-cli.json`)

- CLI command specification
- Input/output schemas
- Integration points

### 1.3 Quickstart (`quickstart.md`)

- 5-minute guide to assessing package maturity
- Example commands and outputs
- Common use cases

---

## Agent Context Update

Run after Phase 1 completion:

```bash
.specify/scripts/bash/update-agent-context.sh claude
```

---

## Post-Design Constitution Re-Check

_Re-evaluated after Phase 1 design completion (2025-12-20)_

**Principle I - Adversarial PM**: ✅ Compliant

- Data model includes OTEL spans for assessment metrics (`maturity.package`, `maturity.score`, etc.)
- CLI contract specifies measurable outputs (coverage percentages, level classifications)
- Quickstart includes CI/CD example with verifiable assertions

**Principle II - Pattern Reuse**: ✅ Compliant

- CLI follows existing citty-based command pattern from @unrdf/cli
- RDF ontology extends proven schema.org/SoftwareSourceCode
- Vitest coverage collection uses existing v8 provider configuration

**Principle III - Production-Ready First**: ✅ Compliant

- No "lean" version in scope - delivering complete 21-package assessment
- All 11 synergy categories fully defined with use cases
- Performance requirements specified (< 5s per package, < 30s full report)

**Principle IV - Concurrent Execution**: ✅ Compliant

- Batch package analysis designed (21 packages in parallel)
- CLI commands support concurrent assessment via pnpm filter
- Report generation aggregates all results in single pass

**Principle V - Semantic Web Standards**: ✅ Compliant

- Data model uses OWL ontology with RDF/Turtle format
- SHACL shapes defined for maturity validation
- TTL output format supported in CLI

---

**Status**: ✅ Phase 0 & 1 Complete
**Completed Artifacts**:

- ✅ research.md (Phase 0 - Research)
- ✅ data-model.md (Phase 1 - RDF ontology + SHACL)
- ✅ contracts/maturity-cli.json (Phase 1 - CLI specification)
- ✅ quickstart.md (Phase 1 - Usage guide)
- ✅ update-agent-context.sh executed

**Next**: Phase 2 - Generate tasks.md via `/speckit.tasks` command
