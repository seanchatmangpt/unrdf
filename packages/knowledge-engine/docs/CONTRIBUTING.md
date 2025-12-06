# Contributing to @unrdf/knowledge-engine

Guide for contributing to the @unrdf/knowledge-engine package.

## Getting Started

```bash
cd packages/knowledge-engine
pnpm install
pnpm test
```

## Development Workflow

1. Create a feature branch
2. Implement your feature in `src/`
3. Write tests in `test/`
4. Add examples in `examples/`
5. Update `docs/API.md`
6. Run tests: `pnpm test`
7. Lint code: `pnpm lint:fix`
8. Create commit: `git commit -m "feat: ..."`

## Code Standards

- 80%+ test coverage
- JSDoc comments on all exports
- Examples for all features
- Keep packages focused

See root [CONTRIBUTING.md](../../CONTRIBUTING.md) for details.

---
