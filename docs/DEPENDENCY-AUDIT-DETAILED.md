# UNRDF Monorepo Dependency Audit Report - DETAILED FINDINGS

**Generated**: 2025-12-21T04:44:latestZ
**Packages Analyzed**: 21
**Total Unique Dependencies**: 85
**Shared Dependencies**: 15
**Version Conflicts**: 6

## Top 20 Shared Dependencies

| Rank | Dependency | Packages | Versions | Status |
|------|------------|----------|----------|--------|
| 1 | vitest | 15 | ^latest | ✅ Aligned |
| 2 | @unrdf/core | 12 | workspace:* | ✅ Aligned |
| 3 | @types/node | 12 | ^latest | ✅ Aligned |
| 4 | @unrdf/oxigraph | 6 | workspace:* | ✅ Aligned |
| 5 | zod | 6 | ^latest, ^latest | ⚠️ CONFLICT |
| 6 | @unrdf/streaming | 3 | workspace:* | ✅ Aligned |
| 7 | @unrdf/hooks | 3 | workspace:* | ✅ Aligned |
| 8 | citty | 3 | ^latest | ✅ Aligned |
| 9 | yaml | 2 | ^latest, ^latest | ⚠️ CONFLICT |
| 10 | comment-parser | 2 | ^latest | ✅ Aligned |
| 11 | eslint | 2 | ^latest, ^latest | ⚠️ CONFLICT |
| 12 | @rdfjs/data-model | 2 | ^latest, ^latest | ⚠️ CONFLICT |
| 13 | @opentelemetry/api | 2 | ^latest, ^latest | ⚠️ CONFLICT |
| 14 | @playwright/test | 2 | ^latest, ^latest | ⚠️ CONFLICT |
| 15 | typescript | 2 | ^latest | ✅ Aligned |
| 16 | @unrdf/test-utils | 1 | workspace:* | ✅ Aligned |
| 17 | fs-extra | 1 | ^latest | ✅ Aligned |
| 18 | gray-matter | 1 | ^latest | ✅ Aligned |
| 19 | nunjucks | 1 | ^latest | ✅ Aligned |
| 20 | @amiceli/vitest-cucumber | 1 | ^latest | ✅ Aligned |

## All Version Conflicts

### zod
- **Packages affected**: 6
- **Versions**: ^latest | ^latest

  - **^latest**: @unrdf/core, @unrdf/oxigraph, @unrdf/nextra-docs, @unrdf/hooks, @unrdf/federation
  - **^latest**: @unrdf/streaming

### yaml
- **Packages affected**: 2
- **Versions**: ^latest | ^latest

  - **^latest**: @unrdf/kgn
  - **^latest**: @unrdf/cli

### eslint
- **Packages affected**: 2
- **Versions**: ^latest | ^latest

  - **^latest**: @unrdf/kgn
  - **^latest**: docs

### @rdfjs/data-model
- **Packages affected**: 2
- **Versions**: ^latest | ^latest

  - **^latest**: @unrdf/core
  - **^latest**: @unrdf/streaming

### @opentelemetry/api
- **Packages affected**: 2
- **Versions**: ^latest | ^latest

  - **^latest**: @unrdf/atomvm
  - **^latest**: @unrdf/streaming

### @playwright/test
- **Packages affected**: 2
- **Versions**: ^latest | ^latest

  - **^latest**: @unrdf/atomvm
  - **^latest**: docs

## Package Dependency Counts

| Package | Total Deps | Dependencies | DevDependencies |
|---------|------------|--------------|------------------|
| @unrdf/kgn | 13 | 6 | 7 |
| @unrdf/test-utils | 0 | 0 | 0 |
| @unrdf/composables | 5 | 3 | 2 |
| @unrdf/core | 14 | 12 | 2 |
| @unrdf/atomvm | 7 | 2 | 5 |
| @unrdf/dark-matter | 4 | 2 | 2 |
| @unrdf/oxigraph | 5 | 2 | 3 |
| docs | 33 | 21 | 12 |
| @unrdf/cli | 10 | 8 | 2 |
| @unrdf/project-engine | 3 | 1 | 2 |
| types | 0 | 0 | 0 |
| .next | 0 | 0 | 0 |
| @unrdf/nextra-docs | 11 | 7 | 4 |
| @unrdf/streaming | 11 | 8 | 3 |
| @unrdf/engine-gateway | 3 | 2 | 1 |
| @unrdf/hooks | 6 | 4 | 2 |
| @unrdf/federation | 6 | 4 | 2 |
| @unrdf/kgc-4d | 8 | 4 | 4 |
| @unrdf/domain | 0 | 0 | 0 |
| @unrdf/validation | 0 | 0 | 0 |
| @unrdf/knowledge-engine | 5 | 3 | 2 |
