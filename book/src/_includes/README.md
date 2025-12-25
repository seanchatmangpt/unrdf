# Shared Content Includes

This directory contains reusable content patterns used across multiple chapters.

## Purpose

**80/20 Consolidation:**
- Extract common patterns (20% of content)
- Eliminate duplication (80% of maintenance burden)
- Single source of truth
- Update once, apply everywhere

## Structure

### `/examples`
Shared code examples used across chapters:
- `ecommerce-base.md` - Product schema and RDF data

### `/patterns`
Reusable implementation patterns:
- `knowledge-hook-base.md` - Standard hook structure
- `testing-pattern.md` - Standard test approach
- `performance-metrics.md` - Benchmarking standards
- `migration-guide-template.md` - Migration process
- `comparison-table-template.md` - Comparison format

## Usage in Chapters

**Reference with link:**
```markdown
See [Shared Testing Pattern](../_includes/patterns/testing-pattern.md) for standard approach.
```

**Extend for specific use case:**
```markdown
This chapter extends the [base e-commerce example](../_includes/examples/ecommerce-base.md) with real-time reactive updates.
```

## Benefits

✅ **Consistency** - Same patterns everywhere
✅ **Maintainability** - Update once, not 3+ times
✅ **Reduced bloat** - 20-30% fewer lines
✅ **Better DX** - Clear structure and navigation

## Line Count Impact

**Before consolidation:** 1,808 lines across 3 chapters
**After consolidation:** ~1,250-1,400 lines (30% reduction)
**Shared patterns:** ~500 lines in `_includes/`

Total: Maintain 1,5,465 lines to deliver same 1,808 lines of value.
