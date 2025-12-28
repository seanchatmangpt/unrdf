# UNRDF Composition Dependency Chains

**Generated**: 2025-12-28T04:09:54.774Z
**Total Composition Chains**: 5

---

## Overview

This document maps composition dependency chains, showing how capabilities build upon each other. Understanding these chains helps:

1. Identify foundational capabilities (most dependencies)
2. Understand composition complexity (chain depth)
3. Plan integration strategies (minimal dependency paths)
4. Detect circular dependencies (should be none)

---

## Chains by Depth

### Depth 2 (2 chains)

#### C1: Oxigraph + Core

**Package**: @unrdf/oxigraph

**Dependency Chain**:

```
@unrdf/oxigraph:C1 → @unrdf/oxigraph → @unrdf/core
```

#### C2: Oxigraph + Hooks

**Package**: @unrdf/oxigraph

**Dependency Chain**:

```
@unrdf/oxigraph:C2 → @unrdf/oxigraph → @unrdf/hooks
```

### Depth 1 (3 chains)

#### C1: Query + Validation

**Package**: @unrdf/core

**Dependency Chain**:

```
@unrdf/core:C1 → @unrdf/core
```

#### C2: Store + Logging

**Package**: @unrdf/core

**Dependency Chain**:

```
@unrdf/core:C2 → @unrdf/core
```

#### C3: Canonicalize + Isomorphism

**Package**: @unrdf/core

**Dependency Chain**:

```
@unrdf/core:C3 → @unrdf/core
```

---

## All Chains (Alphabetical)

- **C1** (@unrdf/oxigraph): @unrdf/oxigraph:C1 → @unrdf/oxigraph → @unrdf/core
- **C1** (@unrdf/core): @unrdf/core:C1 → @unrdf/core
- **C2** (@unrdf/oxigraph): @unrdf/oxigraph:C2 → @unrdf/oxigraph → @unrdf/hooks
- **C2** (@unrdf/core): @unrdf/core:C2 → @unrdf/core
- **C3** (@unrdf/core): @unrdf/core:C3 → @unrdf/core

---

**Last Updated**: 2025-12-28T04:09:54.774Z
**Source**: Automated composition chain analysis
