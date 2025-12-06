# API Reference Overview

Complete API documentation for all UNRDF packages.

## Core Packages

### [@unrdf/core](/docs/reference/api/core)
Foundation RDF store implementation with Oxigraph backend.

**Key Exports**:
- `createStore()` - Create RDF store instance
- `dataFactory` - RDF term creation utilities
- `executeQuery()` - SPARQL query execution

### [@unrdf/hooks](/docs/reference/api/hooks)
40+ React hooks for RDF integration.

**Popular Hooks**:
- `useQuery()` - Execute SPARQL queries
- `useStore()` - Access RDF store context
- `useReasoner()` - Automated reasoning
- `useValidator()` - Data validation with Zod

### [@unrdf/browser](/docs/reference/api/browser)
Browser-optimized RDF with persistence.

**Features**:
- IndexedDB storage
- Web Worker support
- Offline-first architecture

### [@unrdf/kgc-4d](/docs/reference/api/kgc-4d)
Advanced knowledge graph construction.

**Capabilities**:
- Forensic UX patterns
- Multiverse reasoning
- Autonomic coaching
- Real-time visualization

## Package Coverage

- ✅ **19/19 Public API exports documented** (100%)
- ✅ **330/330 Tests passing** (100%)
- ✅ **TypeScript definitions** for all packages
- ✅ **Interactive examples** with live code editors
