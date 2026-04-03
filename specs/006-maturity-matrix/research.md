# Research: Package Maturity Matrix

**Feature**: 006-maturity-matrix
**Date**: 2025-12-20
**Phase**: 0 - Research

---

## 1. Maturity Framework Standards

### Decision: Hybrid framework combining CNCF Maturity Model + npm Quality Indicators

### Rationale

- **CNCF Maturity Model** provides proven levels (Sandbox → Incubating → Graduated) with clear criteria
- **npm Quality Indicators** (maintenance, popularity, quality) align with JavaScript ecosystem
- Combine both for comprehensive coverage: technical quality + community adoption

### Alternatives Considered

| Framework           | Pros                              | Cons                                  | Decision                      |
| ------------------- | --------------------------------- | ------------------------------------- | ----------------------------- |
| CNCF Maturity Model | Industry standard, clear criteria | Cloud-native focused, not JS-specific | Adopt levels                  |
| npm Quality Score   | JS-native, automated metrics      | Limited to popularity, missing depth  | Adopt metrics                 |
| Semver alone        | Simple, well-understood           | Doesn't capture quality/stability     | Supplement only               |
| Custom 5-level      | Tailored to UNRDF needs           | Not industry standard                 | Implement with CNCF alignment |

### Adopted Framework: 5 Levels

| Level       | CNCF Equivalent | Description                        | UNRDF Criteria                           |
| ----------- | --------------- | ---------------------------------- | ---------------------------------------- |
| L1 (Alpha)  | Pre-Sandbox     | Experimental, API unstable         | <50% coverage, breaking changes expected |
| L2 (Beta)   | Sandbox         | Core features, stabilizing         | 50-70% coverage, basic docs              |
| L3 (RC)     | Incubating      | Feature-complete, stable API       | 70-85% coverage, comprehensive docs      |
| L4 (Stable) | Graduated       | Production-ready, battle-tested    | 85%+ coverage, semver-strict             |
| L5 (LTS)    | Graduated+      | Enterprise-grade, extended support | 95%+ coverage, 24-month support          |

---

## 2. Coverage Collection from Vitest

### Decision: Use `vitest run --coverage --json` with c8/v8 provider

### Rationale

- Vitest already configured in `vitest.config.unified.mjs` with v8 provider
- JSON output enables programmatic parsing
- Per-package coverage available via workspace configuration

### Implementation Pattern

```javascript
// collector.mjs - Extract coverage per package
import { execSync } from 'node:child_process';
import { readFileSync } from 'node:fs';

export async function collectCoverage(packageName) {
  const result = execSync(`pnpm --filter @unrdf/${packageName} test:coverage --reporter=json`, {
    encoding: 'utf8',
  });

  const coverageJson = JSON.parse(
    readFileSync(`packages/${packageName}/coverage/coverage-summary.json`, 'utf8')
  );

  return {
    lines: coverageJson.total.lines.pct,
    branches: coverageJson.total.branches.pct,
    functions: coverageJson.total.functions.pct,
    statements: coverageJson.total.statements.pct,
  };
}
```

### Alternatives Considered

| Approach             | Pros             | Cons                  | Decision |
| -------------------- | ---------------- | --------------------- | -------- |
| Vitest JSON reporter | Native, accurate | Requires test run     | Adopt    |
| Parse lcov.info      | Standard format  | Parsing complexity    | Fallback |
| Istanbul CLI         | Cross-tool       | Additional dependency | Rejected |
| Manual inspection    | No setup         | Not scalable          | Rejected |

---

## 3. OTEL Integration for Maturity Metrics

### Decision: Extend @unrdf/validation with maturity-specific OTEL spans

### Rationale

- @unrdf/validation already has OTEL infrastructure
- Maturity assessment is a validation activity
- Consistent observability across the system

### OTEL Span Schema

```javascript
// Maturity assessment span attributes
{
  'maturity.package': 'core',
  'maturity.level': 4,
  'maturity.score': 87.5,
  'maturity.coverage.lines': 92.3,
  'maturity.coverage.branches': 85.1,
  'maturity.api_stability': 'stable',
  'maturity.doc_quality': 'excellent',
  'maturity.test_maturity': 'unit_integration_e2e',
  'maturity.security_status': 'audit_passed'
}
```

### Integration with Existing Validation

```javascript
// otel-collector.mjs
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('maturity-assessment');

export async function assessWithOtel(packageName, assessment) {
  return tracer.startActiveSpan('maturity.assess', async span => {
    span.setAttributes({
      'maturity.package': packageName,
      'maturity.level': assessment.level,
      'maturity.score': assessment.score,
    });
    // ... assessment logic
    span.end();
    return assessment;
  });
}
```

---

## 4. RDF Ontology for Maturity Data

### Decision: Custom OWL ontology extending schema.org/SoftwareSourceCode

### Rationale

- schema.org provides base `SoftwareSourceCode` class
- Custom extension for maturity-specific properties
- SHACL shapes for validation
- Enables SPARQL queries for maturity analysis

### Ontology Design (Turtle)

```turtle
@prefix mat: <https://unrdf.org/ontology/maturity#> .
@prefix schema: <https://schema.org/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Classes
mat:Package a owl:Class ;
    rdfs:subClassOf schema:SoftwareSourceCode ;
    rdfs:label "UNRDF Package" .

mat:MaturityLevel a owl:Class ;
    rdfs:label "Maturity Level" .

mat:SynergyCategory a owl:Class ;
    rdfs:label "Package Synergy Category" .

# Maturity Level Instances
mat:L1_Alpha a mat:MaturityLevel ; mat:score 1 ; rdfs:label "Experimental (Alpha)" .
mat:L2_Beta a mat:MaturityLevel ; mat:score 2 ; rdfs:label "Development (Beta)" .
mat:L3_RC a mat:MaturityLevel ; mat:score 3 ; rdfs:label "Pre-Production (RC)" .
mat:L4_Stable a mat:MaturityLevel ; mat:score 4 ; rdfs:label "Production-Ready (Stable)" .
mat:L5_LTS a mat:MaturityLevel ; mat:score 5 ; rdfs:label "Enterprise (LTS)" .

# Properties
mat:hasMaturityLevel a owl:ObjectProperty ;
    rdfs:domain mat:Package ;
    rdfs:range mat:MaturityLevel .

mat:coveragePercent a owl:DatatypeProperty ;
    rdfs:domain mat:Package ;
    rdfs:range xsd:decimal .

mat:participatesInSynergy a owl:ObjectProperty ;
    rdfs:domain mat:Package ;
    rdfs:range mat:SynergyCategory .

mat:enablesCapability a owl:DatatypeProperty ;
    rdfs:domain mat:SynergyCategory ;
    rdfs:range xsd:string .
```

### Alternatives Considered

| Approach                        | Pros                           | Cons                   | Decision       |
| ------------------------------- | ------------------------------ | ---------------------- | -------------- |
| Custom OWL + schema.org         | Standards-compliant, queryable | Design effort          | Adopt          |
| JSON-LD only                    | Simple, web-friendly           | Limited reasoning      | Supplement     |
| DOAP (Description of a Project) | Package-focused                | Less maturity-specific | Reference only |
| Custom JSON schema              | Minimal setup                  | Not semantic web       | Rejected       |

---

## 5. CLI Patterns in @unrdf/cli

### Decision: Follow existing citty-based command pattern

### Research: Existing CLI Structure

```bash
# Current CLI commands (from packages/cli/src/index.mjs)
packages/cli/src/
├── commands/
│   ├── graph.mjs      # Graph operations
│   ├── query.mjs      # SPARQL queries
│   ├── validate.mjs   # SHACL validation
│   └── convert.mjs    # Format conversion
└── lib/
    └── utils.mjs      # Shared utilities
```

### New Maturity Command Pattern

```javascript
// packages/cli/src/commands/maturity.mjs
import { defineCommand } from 'citty';

export default defineCommand({
  meta: {
    name: 'maturity',
    description: 'Assess package maturity levels',
  },
  subCommands: {
    assess: () => import('./maturity/assess.mjs'),
    report: () => import('./maturity/report.mjs'),
    synergy: () => import('./maturity/synergy.mjs'),
  },
});

// Subcommands:
// unrdf maturity assess [package]     - Assess single package
// unrdf maturity report               - Full maturity report (all 21)
// unrdf maturity synergy [category]   - Synergy guide for category
```

### Integration Points

- Reuse existing `lib/utils.mjs` for common operations
- Output formats: JSON, TTL, Markdown (like other commands)
- Batch mode for CI/CD integration

---

## Summary: All Clarifications Resolved

| Question                     | Resolution                                                   |
| ---------------------------- | ------------------------------------------------------------ |
| Maturity Framework Standards | Hybrid CNCF + npm with 5 custom levels                       |
| Coverage Collection          | Vitest JSON reporter + coverage-summary.json parsing         |
| OTEL Integration             | Extend @unrdf/validation with maturity spans                 |
| RDF Ontology                 | Custom OWL extending schema.org/SoftwareSourceCode           |
| CLI Patterns                 | citty-based commands following existing graph/query patterns |

---

**Status**: Research Complete
**Next**: Phase 1 - Generate data-model.md, contracts/, quickstart.md
