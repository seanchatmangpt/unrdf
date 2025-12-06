

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Guidelines](#development-guidelines)
- [Pull Request Process](#pull-request-process)
- [Issue Reporting](#issue-reporting)
- [Release Process](#release-process)
- [Architecture Guidelines](#architecture-guidelines)

## Code of Conduct

This project adheres to a code of conduct that ensures a welcoming environment for all contributors. By participating, you agree to uphold this code.

### Our Pledge

We pledge to make participation in our project a harassment-free experience for everyone, regardless of age, body size, disability, ethnicity, gender identity and expression, level of experience, nationality, personal appearance, race, religion, or sexual identity and orientation.

### Expected Behavior

- Use welcoming and inclusive language
- Be respectful of differing viewpoints and experiences
- Gracefully accept constructive criticism
- Focus on what is best for the community
- Show empathy towards other community members

## Getting Started

### Prerequisites

- Node.js 18.0.0 or higher
- pnpm 10.15.0 or higher
- Git

### Development Setup

1. **Fork and Clone**
   ```bash
   git clone https://github.com/your-username/unrdf.git
   cd unrdf
   ```

2. **Install Dependencies**
   ```bash
   pnpm install
   ```

3. **Run Tests**
   ```bash
   pnpm test
   ```

4. **Start Development**
   ```bash
   pnpm dev
   ```

### Project Structure

```
unrdf/
├── src/                    # Source code
│   └── knowledge-engine/   # knowledge engine core
├── test/                   # Test suites
├── docs/                   # Documentation
├── examples/               # Usage examples
└── dist/                   # Built artifacts
```

## Development Guidelines

### Technology Stack

- **Language**: JavaScript (ESM only)
- **Type Safety**: JSDoc + Zod validation
- **Testing**: Vitest
- **Linting**: ESLint + Prettier
- **Build**: obuild
- **Package Manager**: pnpm

### Code Standards

#### 1. Pure ESM + JSDoc Only

- **NO TypeScript**: This project uses pure JavaScript with JSDoc for type safety
- **NO .ts files**: All source files must use `.mjs` extension
- **JSDoc Required**: All public APIs must have comprehensive JSDoc documentation

```javascript
/**
 * Execute a knowledge hook with the given event and options
 * @param {Object} hook - The knowledge hook definition
 * @param {Object} event - The hook event containing payload and context
 * @param {Object} [options] - Optional execution options
 * @param {boolean} [options.strictMode=false] - Enable strict mode validation
 * @param {number} [options.timeout=30000] - Execution timeout in milliseconds
 * @returns {Promise<Object>} Hook execution result
 * @throws {TypeError} When hook definition is invalid
 * @throws {TimeoutError} When execution exceeds timeout
 * @example
 * const result = await executeHook(hook, event, { strictMode: true });
 */
export async function executeHook(hook, event, options = {}) {
  // Implementation
}
```

#### 2. Zod Validation

All public APIs must validate inputs using Zod schemas:

```javascript
import { z } from 'zod';

const HookEventSchema = z.object({
  name: z.string().min(1),
  payload: z.record(z.any()),
  context: z.object({
    graph: z.any(),
    env: z.record(z.any()).optional()
  })
});

export async function executeHook(hook, event, options = {}) {
  const validatedEvent = HookEventSchema.parse(event);
  // Implementation
}
```

#### 3. Error Handling

- Use structured error responses
- Include actionable error messages
- Provide remediation guidance

```javascript
export function validateHook(hook) {
  try {
    return HookSchema.parse(hook);
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: error.issues.map(issue => ({
          path: issue.path.join('.'),
          message: issue.message,
          code: issue.code,
          received: issue.received,
          expected: issue.expected
        }))
      };
    }
    throw error;
  }
}
```

#### 4. Performance Considerations

- Meet KGC PRD performance targets
- Use efficient algorithms and data structures
- Implement caching where appropriate
- Monitor memory usage and prevent leaks

#### 5. Security

- All untrusted code execution must use sandboxing
- Validate all inputs at boundaries
- Use cryptographic hashing for integrity
- Follow principle of least privilege

### Testing Requirements

#### 1. Test Categories

- **Unit Tests**: Individual component testing
- **Property Tests**: Consistency and integrity validation
- **Permutation Tests**: Order-dependent behavior
- **Combination Tests**: Multi-component interactions
- **Stress Tests**: High-load scenarios
- **Adversarial Tests**: Security and resilience
- **Benchmark Tests**: Performance validation

#### 2. Test Structure

```javascript
import { describe, it, expect, beforeAll, afterAll } from 'vitest';

describe('KnowledgeHookManager', () => {
  let manager;
  
  beforeAll(() => {
    manager = new KnowledgeHookManager({
      basePath: './test/fixtures',
      strictMode: true
    });
  });
  
  afterAll(async () => {
    await manager.cleanup();
  });
  
  it('should register and execute hooks', async () => {
    // Test implementation
  });
});
```

#### 3. Coverage Requirements

- **Statements**: ≥ 90%
- **Branches**: ≥ 85%
- **Functions**: ≥ 90%
- **Lines**: ≥ 90%

### Documentation Requirements

#### 1. JSDoc Standards

- All public APIs must have JSDoc
- Include parameter types, return types, and examples
- Document error conditions and exceptions
- Use proper JSDoc tags

#### 2. README Updates

- Update README.md for new features
- Include usage examples
- Update configuration options
- Document breaking changes

#### 3. Architecture Documentation

- Update architecture diagrams
- Document new components
- Explain design decisions
- Provide troubleshooting guides

## Pull Request Process

### 1. Before Submitting

- [ ] Run `pnpm test` and ensure all tests pass
- [ ] Run `pnpm lint` and fix any issues
- [ ] Update documentation as needed
- [ ] Add tests for new functionality
- [ ] Ensure performance targets are met

### 2. Pull Request Template

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
- [ ] Performance tests added/updated
- [ ] All tests pass

## Documentation
- [ ] JSDoc updated
- [ ] README updated
- [ ] Architecture docs updated

## Performance Impact
- [ ] No performance impact
- [ ] Performance improved
- [ ] Performance impact documented

## Security Considerations
- [ ] No security impact
- [ ] Security review completed
- [ ] Security impact documented
```

### 3. Review Process

1. **Automated Checks**: CI/CD pipeline must pass
2. **Code Review**: At least one maintainer approval required
3. **Performance Review**: Performance impact assessment
4. **Security Review**: Security implications evaluation
5. **Documentation Review**: Documentation completeness check

### 4. Merge Requirements

- All CI checks must pass
- At least one maintainer approval
- No unresolved conversations
- Performance targets met
- Security review completed

## Issue Reporting

### Bug Reports

When reporting bugs, please include:

1. **Environment**: Node.js version, OS, package version
2. **Steps to Reproduce**: Clear, minimal reproduction steps
3. **Expected Behavior**: What should happen
4. **Actual Behavior**: What actually happens
5. **Error Messages**: Full error messages and stack traces
6. **Additional Context**: Any other relevant information

### Feature Requests

When requesting features, please include:

1. **Use Case**: Why is this feature needed?
2. **Proposed Solution**: How should it work?
3. **Alternatives**: What alternatives have you considered?
4. **Additional Context**: Any other relevant information

### Issue Labels

- `bug`: Something isn't working
- `enhancement`: New feature or request
- `documentation`: Improvements or additions to documentation
- `good first issue`: Good for newcomers
- `help wanted`: Extra attention is needed
- `performance`: Performance-related issues
- `security`: Security-related issues

## Release Process

### Versioning

We follow [Semantic Versioning](https://semver.org/):

- **MAJOR**: Breaking changes
- **MINOR**: New features (backward compatible)
- **PATCH**: Bug fixes (backward compatible)

### Release Steps

1. **Update Version**: Update version in package.json
2. **Update Changelog**: Add entry to CHANGELOG.md
3. **Create Release**: Create GitHub release with tag
4. **Publish Package**: Publish to npm
5. **Update Documentation**: Update docs for new version

### Release Checklist

- [ ] All tests pass
- [ ] Performance targets met
- [ ] Security review completed
- [ ] Documentation updated
- [ ] Changelog updated
- [ ] Version bumped
- [ ] Release notes prepared
- [ ] Package published
- [ ] Release announced

## Architecture Guidelines

### 1. KGC PRD Compliance

All changes must maintain compliance with the KGC PRD:

- **G1**: Deterministic transaction receipts with dual hash
- **G2**: Policy-pack–driven knowledge hooks
- **G3**: Sandboxed effects with strict time/resources
- **G4**: First-class Zod schemas for deltas/receipts/hooks
- **G5**: Observability (OTel traces/metrics)
- **G6**: Optional multi-agent resolution

### 2. Performance Targets

- **p50 pre-hook pipeline**: ≤ 200 µs
- **p99 pre-hook pipeline**: ≤ 2 ms
- **Receipt write**: ≤ 5 ms median
- **Hook engine**: ≥ 10k exec/min sustained
- **Error isolation**: 100%

### 3. Security Model

- **Sandboxing**: All untrusted code execution
- **Validation**: All inputs validated at boundaries
- **Cryptography**: SHA3/BLAKE3 dual hash
- **Isolation**: VM2/worker thread isolation

### 4. Observability

- **Tracing**: OpenTelemetry distributed tracing
- **Metrics**: Performance and business metrics
- **Logging**: Structured logging with correlation IDs
- **Monitoring**: Health checks and alerting

## Getting Help

### Community

- **GitHub Discussions**: For questions and discussions
- **GitHub Issues**: For bug reports and feature requests
- **Discord**: For real-time chat (if available)

### Documentation

- **README.md**: Getting started guide
- **docs/**: Comprehensive documentation
- **examples/**: Usage examples
- **API Reference**: Generated from JSDoc

### Maintainers

- **@seanchatmangpt**: Project maintainer
- **@gitvan-team**: Development team

## License

By contributing to UNRDF, you agree that your contributions will be licensed under the MIT License.

## Acknowledgments

Thank you to all contributors who help make UNRDF better!

---

**Happy Contributing! 🚀**




