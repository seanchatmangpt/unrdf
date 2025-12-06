# @unrdf/test-utils

![Version](https://img.shields.io/badge/version-5.0.0--beta.1-blue) ![Production Ready](https://img.shields.io/badge/production-ready-green)


Testing utilities, fixtures, and helpers for UNRDF development.

**Status:** Internal use only (private package)

## Purpose

This package provides shared testing infrastructure for all UNRDF packages:

- Test fixtures (sample RDF data, SPARQL queries, SHACL shapes)
- Helper functions for creating test stores and data
- Common test setup and configuration
- Mock implementations for testing

## Installation

This is a private package. Use only in test environments:

```bash
pnpm --filter @unrdf/test-utils install
```

## Usage

### Creating a Test Store

```javascript
import { createTestStore } from '@unrdf/test-utils';

const store = await createTestStore();
// Store is ready for testing
```

### Loading Test Fixtures

```javascript
import { loadFixture } from '@unrdf/test-utils';

const sampleGraph = loadFixture('sample-graph.ttl');
const shapes = loadFixture('shapes.ttl');

store.load(sampleGraph, 'text/turtle');
```

### Test Helpers

```javascript
import {
  createTestStore,
  loadFixture,
  assertQueryResults,
  assertValidShape
} from '@unrdf/test-utils';
```

## Contents

```
src/
├── store.mjs           # Test store creation helpers
├── fixtures.mjs        # Fixture loading utilities
├── assertions.mjs      # Custom test assertions
└── index.mjs           # Main exports
```

## See Also

- [TESTING-STRATEGY.md](../../docs/TESTING-STRATEGY.md) - Testing guide
- [LOCAL-DEVELOPMENT.md](../../docs/LOCAL-DEVELOPMENT.md) - Dev setup
- Individual package `test/` directories for usage examples
