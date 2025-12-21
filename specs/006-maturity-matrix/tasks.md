# Tasks: Package Maturity Matrix & Synergistic Capabilities

**Feature**: 006-maturity-matrix
**Input**: Design documents from `/specs/006-maturity-matrix/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅, contracts/ ✅

**Tests**: OPTIONAL - Only include if explicitly requested. This feature focuses on documentation and CLI tooling.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

---

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and RDF ontology foundation

- [ ] T001 Create maturity module directory structure in packages/cli/src/lib/maturity/
- [ ] T002 [P] Create maturity ontology schema in packages/core/src/ontologies/maturity.ttl
- [ ] T003 [P] Create Zod schemas for maturity types in packages/cli/src/lib/maturity/schemas.mjs
- [ ] T004 [P] Create shared utilities for package discovery in packages/cli/src/lib/maturity/utils.mjs

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T005 Implement coverage collector module in packages/cli/src/lib/maturity/collector.mjs
- [ ] T006 [P] Implement weighted scorer module in packages/cli/src/lib/maturity/scorer.mjs
- [ ] T007 [P] Implement OTEL instrumentation in packages/validation/src/maturity/otel-collector.mjs
- [ ] T008 Create base maturity command with citty in packages/cli/src/commands/maturity.mjs
- [ ] T009 [P] Add maturity command export to packages/cli/src/index.mjs

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Maturity Assessment Framework (Priority: P1) 🎯 MVP

**Goal**: Assess all 21 packages against 7 weighted criteria, assign maturity levels (L1-L5)

**Independent Test**: Run `unrdf maturity assess core` and verify output shows level, score, coverage metrics

### Implementation for User Story 1

- [ ] T010 [P] [US1] Implement package metadata reader in packages/cli/src/lib/maturity/metadata.mjs
- [ ] T011 [P] [US1] Implement Vitest coverage parser in packages/cli/src/lib/maturity/coverage-parser.mjs
- [ ] T012 [P] [US1] Implement API stability analyzer in packages/cli/src/lib/maturity/api-analyzer.mjs
- [ ] T013 [P] [US1] Implement documentation quality checker in packages/cli/src/lib/maturity/doc-checker.mjs
- [ ] T014 [P] [US1] Implement security status scanner in packages/cli/src/lib/maturity/security-scanner.mjs
- [ ] T015 [P] [US1] Implement performance status checker in packages/cli/src/lib/maturity/perf-checker.mjs
- [ ] T016 [US1] Implement test maturity analyzer in packages/cli/src/lib/maturity/test-analyzer.mjs
- [ ] T017 [US1] Implement level calculator (L1-L5) from weighted scores in packages/cli/src/lib/maturity/level-calculator.mjs
- [ ] T018 [US1] Implement assess subcommand in packages/cli/src/commands/maturity/assess.mjs
- [ ] T019 [US1] Add table output formatter to packages/cli/src/lib/maturity/formatters/table.mjs
- [ ] T020 [US1] Add JSON output formatter to packages/cli/src/lib/maturity/formatters/json.mjs
- [ ] T021 [US1] Add Turtle (TTL) output formatter to packages/cli/src/lib/maturity/formatters/ttl.mjs

**Checkpoint**: At this point, `unrdf maturity assess [package]` should work for any of 21 packages

---

## Phase 4: User Story 2 - Synergy Documentation (Priority: P2)

**Goal**: Document all 11 synergy categories (A-K) with packages, capabilities, and emergent values

**Independent Test**: Run `unrdf maturity synergy A` and verify output shows packages, capability, emergent value

### Implementation for User Story 2

- [ ] T022 [P] [US2] Create synergy data definitions in packages/cli/src/lib/maturity/synergies/definitions.mjs
- [ ] T023 [P] [US2] Implement synergy category A-E in packages/cli/src/lib/maturity/synergies/categories-ae.mjs
- [ ] T024 [P] [US2] Implement synergy category F-K in packages/cli/src/lib/maturity/synergies/categories-fk.mjs
- [ ] T025 [US2] Implement package-to-synergy mapper in packages/cli/src/lib/maturity/synergies/mapper.mjs
- [ ] T026 [US2] Implement synergy subcommand in packages/cli/src/commands/maturity/synergy.mjs
- [ ] T027 [US2] Add synergy markdown formatter to packages/cli/src/lib/maturity/formatters/synergy-md.mjs

**Checkpoint**: At this point, `unrdf maturity synergy` lists all 11 categories with packages

---

## Phase 5: User Story 3 - Report Generation (Priority: P3)

**Goal**: Generate comprehensive maturity report for all 21 packages with synergy analysis

**Independent Test**: Run `unrdf maturity report --format md` and verify complete report with all packages

### Implementation for User Story 3

- [ ] T028 [P] [US3] Implement batch package assessor in packages/cli/src/lib/maturity/batch-assessor.mjs
- [ ] T029 [P] [US3] Implement report aggregator in packages/cli/src/lib/maturity/report/aggregator.mjs
- [ ] T030 [P] [US3] Implement level distribution calculator in packages/cli/src/lib/maturity/report/distribution.mjs
- [ ] T031 [US3] Implement report subcommand in packages/cli/src/commands/maturity/report.mjs
- [ ] T032 [US3] Add markdown report formatter to packages/cli/src/lib/maturity/formatters/report-md.mjs
- [ ] T033 [US3] Add HTML report formatter to packages/cli/src/lib/maturity/formatters/report-html.mjs

**Checkpoint**: At this point, `unrdf maturity report` generates full 21-package maturity report

---

## Phase 6: User Story 4 - Package Comparison (Priority: P4)

**Goal**: Compare maturity between 2+ packages across specific criteria

**Independent Test**: Run `unrdf maturity compare core streaming` and verify side-by-side comparison

### Implementation for User Story 4

- [ ] T034 [P] [US4] Implement comparison engine in packages/cli/src/lib/maturity/compare/engine.mjs
- [ ] T035 [P] [US4] Implement criteria selector in packages/cli/src/lib/maturity/compare/criteria.mjs
- [ ] T036 [US4] Implement compare subcommand in packages/cli/src/commands/maturity/compare.mjs
- [ ] T037 [US4] Add comparison table formatter to packages/cli/src/lib/maturity/formatters/compare-table.mjs

**Checkpoint**: At this point, all 4 CLI subcommands are fully functional

---

## Phase 7: Observability & Validation

**Purpose**: OTEL instrumentation for assessment metrics (score ≥80/100 validation)

- [ ] T038 [P] Add OTEL spans to assess command in packages/cli/src/commands/maturity/assess.mjs
- [ ] T039 [P] Add OTEL spans to report command in packages/cli/src/commands/maturity/report.mjs
- [ ] T040 [P] Add OTEL spans to synergy command in packages/cli/src/commands/maturity/synergy.mjs
- [ ] T041 Create OTEL validation test in packages/validation/test/maturity-otel.test.mjs
- [ ] T042 Verify OTEL score ≥80/100 via validation/run-all.mjs

---

## Phase 8: Documentation & Polish

**Purpose**: User-facing documentation and final cleanup

- [ ] T043 [P] Create maturity matrix documentation in docs/maturity-matrix.md
- [ ] T044 [P] Create synergy discovery guide in docs/synergy-guide.md
- [ ] T045 [P] Update packages/cli/README.md with maturity command usage
- [ ] T046 Store sample maturity assessment data in packages/core (RDF triples)
- [ ] T047 Validate against quickstart.md scenarios
- [ ] T048 Run linting verification (0 violations)
- [ ] T049 Run type checking verification (100% JSDoc coverage)
- [ ] T050 Performance verification: assess < 5s per package, report < 30s total

---

## Dependencies & Execution Order

### Phase Dependencies

```
Phase 1 (Setup) → Phase 2 (Foundational) → [US1, US2, US3, US4 can proceed in parallel]
                                          ↓
                                   Phase 7 (Observability)
                                          ↓
                                   Phase 8 (Documentation)
```

### User Story Dependencies

| Story             | Depends On | Can Start After                           |
| ----------------- | ---------- | ----------------------------------------- |
| **US1 (Assess)**  | Phase 2    | Foundational complete                     |
| **US2 (Synergy)** | Phase 2    | Foundational complete (parallel with US1) |
| **US3 (Report)**  | US1        | US1 complete (uses assessor)              |
| **US4 (Compare)** | US1        | US1 complete (uses scorer)                |

### Within Each User Story

- Parallel [P] tasks → Sequential tasks → Subcommand implementation → Formatters
- All formatters can run in parallel once core logic exists

### Parallel Opportunities

**Phase 1 (all parallel):**

```bash
Task T001, T002, T003, T004 - All different files
```

**Phase 2 (partial parallel):**

```bash
Task T005, T006, T007 - Different modules
Task T008, T009 - Sequential (command before export)
```

**Phase 3 - US1 (maximum parallel):**

```bash
Task T010, T011, T012, T013, T014, T015 - All analyzers parallel
Task T016, T017 - Depend on analyzers
Task T018 - Depends on scorer
Task T019, T020, T021 - Formatters parallel after T018
```

**Phase 4 - US2 (partial parallel):**

```bash
Task T022, T023, T024 - Definitions and categories parallel
Task T025 - Depends on categories
Task T026, T027 - Sequential
```

---

## Parallel Example: User Story 1 (Maturity Assessment)

```bash
# Launch all analyzers in parallel (6 agents):
Task("Metadata Reader", "Implement package metadata reader...", "backend-dev")
Task("Coverage Parser", "Implement Vitest coverage parser...", "backend-dev")
Task("API Analyzer", "Implement API stability analyzer...", "code-analyzer")
Task("Doc Checker", "Implement documentation quality checker...", "code-analyzer")
Task("Security Scanner", "Implement security status scanner...", "code-analyzer")
Task("Perf Checker", "Implement performance status checker...", "code-analyzer")

# Then sequential (after analyzers complete):
Task("Test Analyzer", "Implement test maturity analyzer...", "backend-dev")
Task("Level Calculator", "Implement L1-L5 level calculator...", "backend-dev")
Task("Assess Command", "Implement assess subcommand...", "backend-dev")

# Finally formatters in parallel (3 agents):
Task("Table Formatter", "Add table output formatter...", "coder")
Task("JSON Formatter", "Add JSON output formatter...", "coder")
Task("TTL Formatter", "Add Turtle output formatter...", "coder")
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational
3. Complete Phase 3: User Story 1 (Maturity Assessment)
4. **STOP and VALIDATE**: Run `unrdf maturity assess core` - verify output
5. Deploy/demo if ready - MVP achieved!

### Incremental Delivery

| Increment | Stories         | CLI Commands Available                   |
| --------- | --------------- | ---------------------------------------- |
| MVP       | US1             | `assess`                                 |
| v2        | US1 + US2       | `assess`, `synergy`                      |
| v3        | US1 + US2 + US3 | `assess`, `synergy`, `report`            |
| Complete  | All             | `assess`, `synergy`, `report`, `compare` |

### Parallel Team Strategy

With 4+ developers after Phase 2:

- Developer A: User Story 1 (assess)
- Developer B: User Story 2 (synergy)
- Developer C: Phase 7 prep (OTEL setup)
- Developer D: Phase 8 prep (docs skeleton)

---

## Task Summary

| Phase                  | Task Count | Parallel Tasks | Purpose                   |
| ---------------------- | ---------- | -------------- | ------------------------- |
| Phase 1: Setup         | 4          | 3              | Project structure         |
| Phase 2: Foundational  | 5          | 3              | Core infrastructure       |
| Phase 3: US1 Assess    | 12         | 8              | Single package assessment |
| Phase 4: US2 Synergy   | 6          | 3              | Synergy documentation     |
| Phase 5: US3 Report    | 6          | 3              | Full maturity report      |
| Phase 6: US4 Compare   | 4          | 2              | Package comparison        |
| Phase 7: Observability | 5          | 3              | OTEL validation           |
| Phase 8: Polish        | 8          | 3              | Documentation & cleanup   |
| **Total**              | **50**     | **28 (56%)**   |                           |

---

## Notes

- All CLI code in MJS + JSDoc (NO TypeScript in src/)
- RDF storage uses @unrdf/oxigraph (via core package)
- OTEL spans via @unrdf/validation extension
- Performance targets: <5s single, <30s all 21 packages
- Tests optional unless explicitly requested

---

**Generated**: 2025-12-20
**Status**: Ready for Implementation (Phase 2 - Tasks)
**Next**: Begin Phase 1 Setup tasks
