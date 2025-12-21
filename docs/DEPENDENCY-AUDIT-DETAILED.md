# UNRDF Monorepo Dependency Audit Report - DETAILED FINDINGS

**Generated**: 2025-12-21T04:44:44.266Z
**Packages Analyzed**: 21
**Total Unique Dependencies**: 85
**Shared Dependencies**: 15
**Version Conflicts**: 6

## Top 20 Shared Dependencies

| Rank | Dependency | Packages | Versions | Status |
|------|------------|----------|----------|--------|
| 1 | vitest | 15 | ^4.0.15 | ✅ Aligned |
| 2 | @unrdf/core | 12 | workspace:* | ✅ Aligned |
| 3 | @types/node | 12 | ^24.10.1 | ✅ Aligned |
| 4 | @unrdf/oxigraph | 6 | workspace:* | ✅ Aligned |
| 5 | zod | 6 | ^4.1.13, ^3.24.1 | ⚠️ CONFLICT |
| 6 | @unrdf/streaming | 3 | workspace:* | ✅ Aligned |
| 7 | @unrdf/hooks | 3 | workspace:* | ✅ Aligned |
| 8 | citty | 3 | ^0.1.6 | ✅ Aligned |
| 9 | yaml | 2 | ^2.8.1, ^2.8.2 | ⚠️ CONFLICT |
| 10 | comment-parser | 2 | ^1.4.1 | ✅ Aligned |
| 11 | eslint | 2 | ^8.56.0, ^9.39.1 | ⚠️ CONFLICT |
| 12 | @rdfjs/data-model | 2 | ^2.1.1, ^2.0.2 | ⚠️ CONFLICT |
| 13 | @opentelemetry/api | 2 | ^1.8.0, ^1.9.0 | ⚠️ CONFLICT |
| 14 | @playwright/test | 2 | ^1.57.0, ^1.49.1 | ⚠️ CONFLICT |
| 15 | typescript | 2 | ^5.9.3 | ✅ Aligned |
| 16 | @unrdf/test-utils | 1 | workspace:* | ✅ Aligned |
| 17 | fs-extra | 1 | ^11.3.1 | ✅ Aligned |
| 18 | gray-matter | 1 | ^4.0.3 | ✅ Aligned |
| 19 | nunjucks | 1 | ^3.2.4 | ✅ Aligned |
| 20 | @amiceli/vitest-cucumber | 1 | ^4.1.1 | ✅ Aligned |

## All Version Conflicts

### zod
- **Packages affected**: 6
- **Versions**: ^4.1.13 | ^3.24.1

  - **^4.1.13**: @unrdf/core, @unrdf/oxigraph, @unrdf/nextra-docs, @unrdf/hooks, @unrdf/federation
  - **^3.24.1**: @unrdf/streaming

### yaml
- **Packages affected**: 2
- **Versions**: ^2.8.1 | ^2.8.2

  - **^2.8.1**: @unrdf/kgn
  - **^2.8.2**: @unrdf/cli

### eslint
- **Packages affected**: 2
- **Versions**: ^8.56.0 | ^9.39.1

  - **^8.56.0**: @unrdf/kgn
  - **^9.39.1**: docs

### @rdfjs/data-model
- **Packages affected**: 2
- **Versions**: ^2.1.1 | ^2.0.2

  - **^2.1.1**: @unrdf/core
  - **^2.0.2**: @unrdf/streaming

### @opentelemetry/api
- **Packages affected**: 2
- **Versions**: ^1.8.0 | ^1.9.0

  - **^1.8.0**: @unrdf/atomvm
  - **^1.9.0**: @unrdf/streaming

### @playwright/test
- **Packages affected**: 2
- **Versions**: ^1.57.0 | ^1.49.1

  - **^1.57.0**: @unrdf/atomvm
  - **^1.49.1**: docs

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
