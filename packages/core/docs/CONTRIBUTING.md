# Contributing to @unrdf/core

Guide for contributing to the core RDF substrate.

## Getting Started

```bash
cd packages/core
pnpm install
pnpm test
```

## Structure

```
packages/core/
├── src/
│   ├── index.mjs           # Main export
│   ├── rdf/                # RDF operations
│   ├── sparql/             # SPARQL execution
│   ├── types.mjs           # Type definitions
│   └── constants.mjs       # Constants
├── test/
│   └── *.test.mjs          # Test files
├── docs/
│   ├── API.md              # API reference
│   ├── GUIDE.md            # Usage guide
│   └── CONTRIBUTING.md     # This file
├── examples/
│   └── *.mjs               # Example files
└── package.json
```

## Development Workflow

### 1. Create Feature Branch

```bash
git checkout -b feat/add-new-feature
```

### 2. Implement Feature

Edit files in `src/` following patterns:

- **RDF Operations**: Add to `src/rdf/`
- **SPARQL**: Add to `src/sparql/`
- **Types**: Update `src/types.mjs`
- **Constants**: Add to `src/constants.mjs`

### 3. Write Tests

Add tests in `test/` for all new code:

```javascript
// test/my-feature.test.mjs
import { describe, it, expect } from 'vitest'
import { myFeature } from '../src/my-feature.mjs'

describe('myFeature', () => {
  it('should do something', () => {
    expect(myFeature()).toBe('something')
  })
})
```

### 4. Add Examples

Create examples in `examples/` showing usage:

```javascript
// examples/my-feature-example.mjs
import { myFeature } from '@unrdf/core'

const result = myFeature()
console.log(result)
```

### 5. Update Documentation

Update `docs/API.md` with new function signature and example.

### 6. Run Tests

```bash
# Test this package
pnpm test

# Test in watch mode
pnpm test:watch

# Test with coverage
pnpm test -- --coverage
```

### 7. Lint and Format

```bash
# Lint code
pnpm lint

# Fix lint errors
pnpm lint:fix

# Format code
pnpm format

# Check formatting
pnpm format:check
```

### 8. Create Commit

```bash
git add .
git commit -m "feat: add new feature"
```

## Code Standards

### Type Safety
- Use JSDoc for type information
- Validate inputs with zod
- Document all parameters

### Testing
- Minimum 80% coverage
- Test happy path and edge cases
- Use descriptive test names

### Documentation
- Add JSDoc comments to all exports
- Document parameters and return values
- Include usage examples in docs

### Examples
- Keep examples simple and focused
- Include expected output
- Add comments explaining the code

## Pull Request Process

1. Ensure all tests pass: `pnpm test`
2. Ensure code is formatted: `pnpm format:check`
3. Ensure no lint errors: `pnpm lint`
4. Create PR with description of changes
5. Address review feedback
6. Merge when approved

## Important Notes

- **Core Stability**: @unrdf/core is stable substrate. Changes should be backwards compatible.
- **No New Dependencies**: Minimize adding dependencies. Prefer built-in Node APIs.
- **RDF Compliance**: Follow RDF and SPARQL specifications.
- **Performance**: Consider performance implications of changes.

## Questions?

See `docs/API.md` for API reference or open an issue for discussions.
