# Contributing to unrdf

Thank you for your interest in contributing to unrdf! This document provides guidelines and information for contributors.

## Philosophy

unrdf follows a strict philosophy:

- **JSDoc + Zod only, no TypeScript** - TypeScript is an illusion of safety that collapses at runtime
- **Opinionated design** - Single path through the RDF universe
- **Composable architecture** - Each composable owns exactly one concern
- **Runtime validation** - Zod ensures data integrity at execution time
- **Comprehensive error handling** - Every operation includes proper error handling

## Development Setup

### Prerequisites

- Node.js 18+ 
- pnpm (required package manager)
- Git

### Installation

```bash
# Clone the repository
git clone https://github.com/gitvan/unrdf.git
cd unrdf

# Install dependencies
pnpm install

# Build the project
pnpm build
```

### Development Commands

```bash
# Run tests
pnpm test

# Run tests in watch mode
pnpm dev

# Lint code
pnpm lint

# Fix linting issues
pnpm lint:fix

# Build project
pnpm build
```

## Code Style

### JSDoc Documentation

All functions must include comprehensive JSDoc documentation:

```javascript
/**
 * Parse input from a specified format into a Zod-validated object
 * @param {import('zod').ZodSchema} schema - The Zod schema to validate against
 * @param {string} format - The input format (e.g., 'json', 'yaml', 'toml')
 * @param {string} input - The input string to parse
 * @param {Object} [opts] - Optional configuration
 * @returns {Promise<any>} The parsed and validated object
 * 
 * @example
 * const schema = z.object({ name: z.string(), age: z.number() });
 * const result = await parseFrom(schema, 'json', '{"name": "Alice", "age": 30}');
 * console.log(result); // { name: "Alice", age: 30 }
 */
export async function parseFrom(schema, format, input, opts = {}) {
  // implementation
}
```

### Error Handling

All functions must include proper error handling:

```javascript
export function exampleFunction(input) {
  if (typeof input !== "string" || !input.trim()) {
    throw new Error("exampleFunction: non-empty string required");
  }
  
  try {
    // implementation
  } catch (error) {
    throw new Error(`exampleFunction failed: ${error.message}`);
  }
}
```

### File Structure

- **Source files**: Use `.mjs` extension
- **JSDoc only**: No TypeScript type annotations
- **ES modules**: Use `import`/`export` syntax
- **Consistent naming**: Use camelCase for functions and variables

## Adding New Features

### 1. Composables

When adding new composables:

1. Create file in `src/composables/`
2. Follow the established pattern
3. Include comprehensive JSDoc
4. Add proper error handling
5. Export from `src/composables/index.mjs`

```javascript
/**
 * @fileoverview useExample composable - Example functionality
 * 
 * This composable provides example functionality.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { RdfEngine } from "../engines/RdfEngine.mjs";

/**
 * Create an example composable
 * @param {Object} [options] - Example options
 * @returns {Object} Example interface
 */
export function useExample(options = {}) {
  const engine = new RdfEngine();
  
  return {
    /**
     * Example method
     * @param {string} input - Input string
     * @returns {Promise<string>} Processed result
     */
    async process(input) {
      if (typeof input !== "string" || !input.trim()) {
        throw new Error("process: non-empty string required");
      }
      
      try {
        // implementation
        return result;
      } catch (error) {
        throw new Error(`process failed: ${error.message}`);
      }
    }
  };
}
```

### 2. Utilities

When adding new utilities:

1. Create file in `src/utils/`
2. Follow the established pattern
3. Include comprehensive JSDoc
4. Add proper error handling
5. Export from `src/utils/index.mjs`

```javascript
/**
 * @fileoverview Example utility functions
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

/**
 * Example utility function
 * @param {string} input - Input string
 * @returns {string} Processed result
 * 
 * @example
 * const result = exampleFunction('input');
 * console.log(result); // 'processed input'
 */
export function exampleFunction(input) {
  if (typeof input !== "string" || !input.trim()) {
    throw new Error("exampleFunction: non-empty string required");
  }
  
  try {
    // implementation
    return result;
  } catch (error) {
    throw new Error(`exampleFunction failed: ${error.message}`);
  }
}
```

### 3. Engine Extensions

When extending the RdfEngine:

1. Add methods to `src/engines/RdfEngine.mjs`
2. Include comprehensive JSDoc
3. Add proper error handling
4. Update tests

```javascript
/**
 * Example engine method
 * @param {Store} store - Store to process
 * @param {Object} [options] - Processing options
 * @returns {Promise<Store>} Processed store
 */
async exampleMethod(store, options = {}) {
  if (!store || typeof store.size !== 'number') {
    throw new Error("exampleMethod: valid store required");
  }
  
  try {
    // implementation
    return result;
  } catch (error) {
    throw new Error(`exampleMethod failed: ${error.message}`);
  }
}
```

## Testing

### Test Structure

All tests must follow this structure:

```javascript
import { describe, it, expect, beforeEach } from 'vitest';
import { useExample } from 'unrdf';

describe('useExample', () => {
  let example;
  
  beforeEach(() => {
    example = useExample();
  });
  
  it('should create example interface', () => {
    expect(example).toBeDefined();
    expect(typeof example.process).toBe('function');
  });
  
  it('should process input correctly', async () => {
    const result = await example.process('input');
    expect(result).toBe('processed input');
  });
  
  it('should throw error for invalid input', async () => {
    await expect(example.process('')).rejects.toThrow('non-empty string required');
  });
});
```

### Test Categories

- **Unit tests**: Test individual functions
- **Integration tests**: Test composable interactions
- **Edge case tests**: Test error conditions and edge cases
- **Performance tests**: Test with large datasets

### Running Tests

```bash
# Run all tests
pnpm test

# Run specific test file
pnpm test test/composables/useExample.test.mjs

# Run tests in watch mode
pnpm dev

# Run tests with coverage
pnpm test --coverage
```

## Documentation

### Adding Documentation

When adding new features:

1. Update relevant documentation files
2. Add examples to `docs/examples/`
3. Update API reference
4. Include JSDoc comments

### Documentation Structure

- `docs/README.md` - Main documentation
- `docs/getting-started.md` - Quick start guide
- `docs/core-concepts.md` - Framework philosophy
- `docs/api-reference.md` - Complete API documentation
- `docs/composables/` - Composable documentation
- `docs/utilities/` - Utility documentation
- `docs/engines/` - Engine documentation
- `docs/examples/` - Usage examples

## Pull Request Process

### Before Submitting

1. **Fork the repository**
2. **Create a feature branch**: `git checkout -b feature/amazing-feature`
3. **Follow code style guidelines**
4. **Add comprehensive tests**
5. **Update documentation**
6. **Run tests**: `pnpm test`
7. **Run linting**: `pnpm lint`

### Pull Request Guidelines

1. **Clear title**: Describe what the PR does
2. **Detailed description**: Explain changes and motivation
3. **Reference issues**: Link to related issues
4. **Include tests**: All new features must have tests
5. **Update documentation**: Include relevant doc updates
6. **Follow philosophy**: Maintain JSDoc + Zod only approach

### PR Template

```markdown
## Description
Brief description of changes

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Breaking change
- [ ] Documentation update

## Testing
- [ ] Unit tests added/updated
- [ ] Integration tests added/updated
- [ ] Edge case tests added/updated
- [ ] All tests pass

## Documentation
- [ ] JSDoc comments added/updated
- [ ] API reference updated
- [ ] Examples added/updated
- [ ] README updated if needed

## Checklist
- [ ] Code follows style guidelines
- [ ] Self-review completed
- [ ] No TypeScript code added
- [ ] Error handling included
- [ ] Performance considered
```

## Code Review

### Review Checklist

- [ ] **Philosophy compliance**: No TypeScript, JSDoc + Zod only
- [ ] **Code style**: Follows established patterns
- [ ] **Error handling**: Comprehensive error handling
- [ ] **Documentation**: JSDoc comments included
- [ ] **Tests**: Adequate test coverage
- [ ] **Performance**: No performance regressions
- [ ] **Security**: No security vulnerabilities

### Review Process

1. **Automated checks**: CI/CD pipeline runs tests and linting
2. **Code review**: At least one maintainer reviews
3. **Testing**: All tests must pass
4. **Documentation**: Documentation must be updated
5. **Approval**: Maintainer approval required

## Issue Reporting

### Bug Reports

When reporting bugs, include:

1. **Clear title**: Brief description of the issue
2. **Steps to reproduce**: Detailed steps
3. **Expected behavior**: What should happen
4. **Actual behavior**: What actually happens
5. **Environment**: Node.js version, OS, etc.
6. **Code example**: Minimal reproduction case

### Feature Requests

When requesting features:

1. **Clear title**: Brief description of the feature
2. **Use case**: Why is this feature needed?
3. **Proposed solution**: How should it work?
4. **Alternatives**: Other solutions considered
5. **Additional context**: Any other relevant information

## Release Process

### Versioning

We follow [Semantic Versioning](https://semver.org/):

- **MAJOR**: Breaking changes
- **MINOR**: New features (backward compatible)
- **PATCH**: Bug fixes (backward compatible)

### Release Steps

1. **Update version**: Update `package.json` version
2. **Update changelog**: Add changes to CHANGELOG.md
3. **Create release**: Create GitHub release
4. **Publish package**: Publish to npm
5. **Update documentation**: Update docs if needed

## Community Guidelines

### Code of Conduct

- **Be respectful**: Treat everyone with respect
- **Be constructive**: Provide helpful feedback
- **Be patient**: Remember that everyone is learning
- **Be inclusive**: Welcome contributors from all backgrounds

### Getting Help

- **GitHub Issues**: For bugs and feature requests
- **GitHub Discussions**: For questions and discussions
- **Documentation**: Check existing documentation first
- **Examples**: Look at examples for usage patterns

## License

By contributing to unrdf, you agree that your contributions will be licensed under the MIT License.

## Recognition

Contributors will be recognized in:

- **README.md**: Contributor list
- **CHANGELOG.md**: Release notes
- **GitHub**: Contributor statistics

Thank you for contributing to unrdf! 🚀