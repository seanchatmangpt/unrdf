# Developer Onboarding - Quick Reference

Fast reference guide for new contributors. **Bookmark this page!**

## 5-Minute Setup

```bash
# 1. Fork on GitHub: https://github.com/unrdf/unrdf

# 2. Clone your fork
git clone https://github.com/YOUR-USERNAME/unrdf.git
cd unrdf

# 3. Install dependencies
npm install -g pnpm  # If you don't have pnpm
pnpm install

# 4. Build packages
pnpm run build

# 5. Run tests
pnpm test

# ✅ You're ready to contribute!
```

## Essential Commands

```bash
# Development
pnpm run build         # Build all packages
pnpm test              # Run all tests
pnpm run lint          # Check code style
pnpm run lint:fix      # Fix code style

# Working on specific package
pnpm --filter @unrdf/core test    # Test one package
pnpm --filter @unrdf/core build   # Build one package

# Git workflow
git checkout -b feat/my-feature   # Create branch
git add .                         # Stage changes
git commit -m "feat: description" # Commit
git push origin feat/my-feature   # Push to fork
```

## File Structure

```
unrdf/
├── packages/          # Main code (20+ packages)
│   ├── core/          # ⭐ Start here - main RDF library
│   ├── hooks/         # Autonomous behaviors
│   ├── streaming/     # Large graph processing
│   └── ...
├── docs/              # Documentation
├── examples/          # Usage examples
└── test/              # Cross-package tests
```

## Key Documentation

| Document | When to Use |
|----------|-------------|
| [FIRST-TIME-CONTRIBUTORS.md](FIRST-TIME-CONTRIBUTORS.md) | Never contributed to open source before |
| [ONBOARDING.md](ONBOARDING.md) | Complete step-by-step setup |
| [WALKTHROUGHS.md](WALKTHROUGHS.md) | Learn by doing tutorials |
| [TROUBLESHOOTING.md](TROUBLESHOOTING.md) | Something broke |
| [CONTRIBUTING.md](../CONTRIBUTING.md) | Contribution guidelines |
| [START-HERE.md](START-HERE.md) | Understand what UNRDF is |
| [ARCHITECTURE.md](ARCHITECTURE.md) | How the system works |

## Common Tasks

### Add Documentation
1. Edit `docs/*.md` or `README.md`
2. `git commit -m "docs: description"`
3. Push and create PR

### Fix a Bug
1. Find issue: https://github.com/unrdf/unrdf/labels/bug
2. Create branch: `git checkout -b fix/issue-NUMBER`
3. Make fix + add test
4. `pnpm test` - ensure all pass
5. Push and create PR

### Add a Feature
1. Discuss in issue first
2. Create branch: `git checkout -b feat/feature-name`
3. Implement + add tests + update docs
4. `pnpm test && pnpm run lint`
5. Push and create PR

## Getting Help

1. **Check docs** - Start with [START-HERE.md](START-HERE.md)
2. **Search issues** - https://github.com/unrdf/unrdf/issues
3. **Ask in Discussions** - https://github.com/unrdf/unrdf/discussions
4. **Troubleshooting guide** - [TROUBLESHOOTING.md](TROUBLESHOOTING.md)

## Commit Message Format

```
<type>(<scope>): <description>

Examples:
feat(core): add SPARQL property paths
fix(hooks): resolve race condition
docs(readme): update installation steps
test(streaming): add large graph tests
```

Types: `feat`, `fix`, `docs`, `test`, `refactor`, `perf`, `chore`

## Quick Links

- 🐛 [Report Bug](https://github.com/unrdf/unrdf/issues/new?labels=bug)
- 💡 [Request Feature](https://github.com/unrdf/unrdf/issues/new?labels=enhancement)
- 🙋 [Ask Question](https://github.com/unrdf/unrdf/discussions)
- 📚 [Full Docs](https://github.com/unrdf/unrdf/tree/main/docs)

## Contribution Checklist

Before submitting PR:

- [ ] Tests pass (`pnpm test`)
- [ ] Linting passes (`pnpm run lint`)
- [ ] Build succeeds (`pnpm run build`)
- [ ] Added tests for new code
- [ ] Updated documentation
- [ ] Followed commit message format
- [ ] Responded to review feedback

## Your First PR

**Easiest contributions:**
1. Fix typos in documentation
2. Add examples to docs
3. Improve error messages
4. Add tests for existing features

**Find issues:** https://github.com/unrdf/unrdf/labels/good%20first%20issue

## Community

- **Be respectful** - We're all learning
- **Ask questions** - No question is too basic
- **Help others** - Share what you've learned
- **Be patient** - Reviews take time

---

**Welcome to UNRDF!** Pick a task from the list above and start contributing.

**Need more details?** See the full guides linked above.
