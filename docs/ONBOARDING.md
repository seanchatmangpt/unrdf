# UNRDF Developer Onboarding

Complete onboarding checklist for new contributors. Follow this guide to go from zero to your first merged PR.

**Time to complete:** 30-60 minutes

## Prerequisites Check

Before starting, verify you have:

- [ ] **Node.js 18+** installed
  ```bash
  node --version  # Should show v18.x.x or higher
  ```
  If not: [Download Node.js](https://nodejs.org)

- [ ] **pnpm** installed
  ```bash
  pnpm --version  # Should show 8.x.x or higher
  ```
  If not:
  ```bash
  npm install -g pnpm
  ```

- [ ] **Git** installed
  ```bash
  git --version  # Any recent version works
  ```

- [ ] **GitHub account** with SSH keys set up
  - If not: [GitHub SSH Setup Guide](https://docs.github.com/en/authentication/connecting-to-github-with-ssh)

## Step 1: Get the Code (5 minutes)

### 1.1 Fork the Repository

1. Go to https://github.com/unrdf/unrdf
2. Click "Fork" in the top right
3. Wait for GitHub to create your fork

### 1.2 Clone Your Fork

```bash
# Replace YOUR-USERNAME with your GitHub username
git clone git@github.com:YOUR-USERNAME/unrdf.git
cd unrdf
```

### 1.3 Add Upstream Remote

This lets you sync with the main repository:

```bash
git remote add upstream https://github.com/unrdf/unrdf.git
git remote -v  # Verify you see both 'origin' and 'upstream'
```

Expected output:
```
origin    git@github.com:YOUR-USERNAME/unrdf.git (fetch)
origin    git@github.com:YOUR-USERNAME/unrdf.git (push)
upstream  https://github.com/unrdf/unrdf.git (fetch)
upstream  https://github.com/unrdf/unrdf.git (push)
```

## Step 2: Install Dependencies (3-5 minutes)

```bash
pnpm install
```

This will:
- Read `pnpm-workspace.yaml` to find all packages
- Install dependencies for all 20+ packages
- Link internal packages together
- Take 3-5 minutes depending on internet speed

**Expected output:**
```
Progress: resolved XXX, reused XXX, downloaded XXX, added XXX, done
```

**Verification:**
```bash
# Should list all @unrdf/* packages
pnpm list --depth 0 | grep @unrdf
```

## Step 3: Build All Packages (2-3 minutes)

```bash
pnpm run build
```

This compiles all packages in dependency order.

**Expected output:**
```
> @unrdf/core: Building...
> @unrdf/hooks: Building...
[... more packages ...]
```

**Verification:**
```bash
# Check that dist/ folders exist
ls packages/core/dist/
# Should show compiled files
```

## Step 4: Run Tests (2-5 minutes)

```bash
timeout 30s pnpm test
```

**What to expect:**
- Many tests should pass
- Some tests might fail (that's OK for now)
- You'll see a summary at the end

**Example output:**
```
Test Files  25 passed (25)
     Tests  443 passed (443)
  Start at  XX:XX:XX
  Duration  XX.XXs
```

**If tests fail:**
- Note which packages have failures
- Check if it's a known issue
- Don't worry - you can still contribute!

## Step 5: Read Architecture Docs (10-15 minutes)

Read these in order to understand the codebase:

### 5.1 Start Here (5 min)
```bash
cat docs/START-HERE.md
# Or open in your editor
```

**Key takeaways:**
- UNRDF is a knowledge graph platform
- `@unrdf/core` is the main package (80% of usage)
- `createKnowledgeSubstrateCore()` is the main API
- RDF = triples (subject-predicate-object)
- SPARQL = query language for RDF

### 5.2 Architecture Overview (5 min)
```bash
cat docs/ARCHITECTURE.md | head -150
```

**Key takeaways:**
- Layered architecture (Application → Engine → Foundation)
- N3.Store for RDF storage
- Comunica for SPARQL queries
- Knowledge Hooks for reactive behaviors

### 5.3 Monorepo Structure (5 min)
```bash
cat docs/MONOREPO-QUICK-REFERENCE.md
```

**Key takeaways:**
- 20+ packages in `packages/` directory
- Essential: core, oxigraph, hooks
- Extended: streaming, federation, cli
- Each package has its own tests and docs

## Step 6: Make Your First Change (10 minutes)

Let's make a simple documentation fix to practice the workflow.

### 6.1 Create a Feature Branch

```bash
git checkout -b feat/my-first-contribution
```

### 6.2 Find Something to Fix

Look for:
- Typos in `README.md` or `docs/*.md`
- Missing examples in documentation
- Unclear error messages
- Outdated information

For practice, let's add your name to a contributor list:

```bash
# Create a simple change
echo "- $(git config user.name) - Onboarding complete!" >> docs/ONBOARDING.md
```

### 6.3 Verify Your Change

```bash
git diff  # Review what changed
```

### 6.4 Run Tests

```bash
# Quick test to ensure nothing broke
timeout 10s pnpm test:fast || echo "Tests completed"
```

### 6.5 Commit Your Change

```bash
git add docs/ONBOARDING.md
git commit -m "docs: add contributor to onboarding doc"
```

### 6.6 Push to Your Fork

```bash
git push -u origin feat/my-first-contribution
```

### 6.7 Create a Pull Request

1. Go to https://github.com/YOUR-USERNAME/unrdf
2. Click "Compare & pull request"
3. Fill in the template:
   - Title: `docs: add contributor to onboarding doc`
   - Description: Explain this is a practice PR from onboarding
4. Click "Create pull request"

Congratulations! You've completed the contribution workflow.

## Step 7: Explore the Codebase (15-20 minutes)

### 7.1 Understand Package Structure

```bash
# Look at the core package structure
tree -L 2 packages/core/
```

**Key directories:**
```
packages/core/
├── src/           # Source code (*.mjs files with JSDoc)
├── test/          # Test files (*.test.mjs)
├── dist/          # Compiled output (generated by build)
├── package.json   # Package metadata and scripts
└── README.md      # Package documentation
```

### 7.2 Read Example Code

```bash
# Simple parse and query example
cat examples/01-minimal-parse-query.mjs
```

Run it:
```bash
node examples/01-minimal-parse-query.mjs
```

**Expected output:**
```
Parsed X triples
Query results: [...]
```

### 7.3 Explore More Examples

```bash
# List all examples
ls examples/*.mjs

# Read a more complex one
cat examples/basic-knowledge-hook.mjs
```

### 7.4 Browse Tests

Tests show how to use the code:

```bash
# Look at core tests
cat packages/core/test/parse.test.mjs | head -50
```

**Key pattern:**
```javascript
import { describe, it, expect } from 'vitest';
import { parseTurtle } from '../src/parse.mjs';

describe('parseTurtle', () => {
  it('should parse valid Turtle', async () => {
    const store = await parseTurtle(`...`);
    expect(store.size).toBeGreaterThan(0);
  });
});
```

## Step 8: Development Environment Setup (10 minutes)

### 8.1 VS Code Setup (Optional but Recommended)

If using VS Code, install these extensions:

```bash
# Install from VS Code Extensions Marketplace:
# 1. ESLint
# 2. Prettier - Code formatter
# 3. Vitest
# 4. GitLens
```

Create `.vscode/settings.json`:
```json
{
  "editor.defaultFormatter": "esbenp.prettier-vscode",
  "editor.formatOnSave": true,
  "editor.codeActionsOnSave": {
    "source.fixAll.eslint": true
  },
  "[javascript]": {
    "editor.defaultFormatter": "esbenp.prettier-vscode"
  }
}
```

### 8.2 Git Hooks (Optional)

Set up pre-commit hooks to catch issues early:

```bash
# Create .git/hooks/pre-commit
cat > .git/hooks/pre-commit << 'EOF'
#!/bin/bash
echo "Running pre-commit checks..."
pnpm run lint || exit 1
timeout 30s pnpm test:fast || exit 1
echo "Pre-commit checks passed!"
EOF

chmod +x .git/hooks/pre-commit
```

### 8.3 Shell Aliases (Optional)

Add to `~/.bashrc` or `~/.zshrc`:

```bash
# UNRDF development shortcuts
alias ut='pnpm test'
alias ub='pnpm run build'
alias ul='pnpm run lint'
alias ulf='pnpm run lint:fix'
alias ucore='pnpm --filter @unrdf/core'
```

## Step 9: Complete First Real Contribution (20-30 minutes)

Now that you're set up, find a real issue to work on:

### 9.1 Find a Good First Issue

1. Go to https://github.com/unrdf/unrdf/issues
2. Filter by label: `good first issue`
3. Pick one that interests you
4. Comment: "I'd like to work on this"

### 9.2 Understand the Issue

- Read the issue description carefully
- Check if there are any linked PRs or discussions
- Ask questions if anything is unclear

### 9.3 Create a Feature Branch

```bash
git checkout main
git pull upstream main
git checkout -b fix/issue-NUMBER-description
```

### 9.4 Implement the Fix

- Make your changes
- Follow existing code patterns
- Add JSDoc comments
- Add tests if needed

### 9.5 Test Your Changes

```bash
# Test the specific package
pnpm --filter @unrdf/PACKAGE-NAME test

# Run linting
pnpm run lint:fix

# Build to ensure no errors
pnpm run build
```

### 9.6 Commit and Push

```bash
git add .
git commit -m "fix: description of what you fixed (#ISSUE-NUMBER)"
git push -u origin fix/issue-NUMBER-description
```

### 9.7 Create Pull Request

1. Go to your fork on GitHub
2. Click "Compare & pull request"
3. Fill in the PR template:
   - Reference the issue number
   - Describe your changes
   - Mention any testing you did
4. Submit the PR

### 9.8 Respond to Reviews

- Maintainers will review your PR
- Make requested changes if needed
- Push updates to the same branch
- Be patient - reviews may take a few days

## Step 10: Learn Advanced Topics (Optional)

Once you're comfortable with basics, explore:

### 10.1 Knowledge Hooks
```bash
cat docs/GETTING-STARTED/knowledge-hooks.md
cat examples/basic-knowledge-hook.mjs
```

### 10.2 SPARQL Queries
```bash
cat docs/GETTING-STARTED/sparql.md
```

### 10.3 Streaming Large Graphs
```bash
cat packages/streaming/README.md
```

### 10.4 Performance Optimization
```bash
cat docs/performance-analysis-summary.md
```

## Onboarding Checklist

Track your progress:

### Environment Setup
- [ ] Node.js 18+ installed
- [ ] pnpm installed
- [ ] Git configured
- [ ] GitHub SSH keys set up
- [ ] Repository forked and cloned
- [ ] Dependencies installed (`pnpm install`)
- [ ] Packages built (`pnpm run build`)
- [ ] Tests run successfully

### Knowledge Acquisition
- [ ] Read START-HERE.md
- [ ] Read ARCHITECTURE.md overview
- [ ] Read MONOREPO-QUICK-REFERENCE.md
- [ ] Understand package structure
- [ ] Explored examples directory
- [ ] Reviewed test patterns

### Hands-On Practice
- [ ] Created feature branch
- [ ] Made a change
- [ ] Ran tests locally
- [ ] Committed changes
- [ ] Pushed to fork
- [ ] Created a pull request

### First Contribution
- [ ] Found a good first issue
- [ ] Implemented a fix
- [ ] Added tests (if needed)
- [ ] Updated documentation (if needed)
- [ ] Submitted PR
- [ ] Responded to review feedback
- [ ] PR merged!

## Common Issues and Solutions

### Issue: "pnpm: command not found"
**Solution:**
```bash
npm install -g pnpm
```

### Issue: Build fails with "Permission denied"
**Solution:**
```bash
sudo chown -R $(whoami) node_modules
pnpm run build
```

### Issue: Tests fail after pulling latest changes
**Solution:**
```bash
git checkout main
git pull upstream main
pnpm install  # Install any new dependencies
pnpm run build  # Rebuild with latest code
pnpm test
```

### Issue: Can't push to GitHub
**Solution:**
```bash
# Verify remote URLs
git remote -v

# Should push to YOUR fork, not upstream
git push origin feat/my-branch
```

### Issue: Merge conflicts
**Solution:**
```bash
git checkout main
git pull upstream main
git checkout feat/my-branch
git merge main
# Resolve conflicts in your editor
git add .
git commit -m "chore: resolve merge conflicts"
git push
```

## Getting Help

Stuck? Here's where to go:

1. **Search existing docs**
   - Start with docs/START-HERE.md
   - Check docs/TROUBLESHOOTING.md (if exists)

2. **Search GitHub Issues**
   - Someone may have had the same problem
   - https://github.com/unrdf/unrdf/issues

3. **Ask in Discussions**
   - https://github.com/unrdf/unrdf/discussions
   - Tag with `question`

4. **Open an Issue**
   - If you found a bug or have a question
   - Use the issue template

## Next Steps

Now that you're onboarded:

1. **Complete a real contribution** - Find a `good first issue` and fix it
2. **Learn by example** - Read [docs/WALKTHROUGHS.md](WALKTHROUGHS.md)
3. **Deep dive** - Pick a package and become an expert
4. **Help others** - Answer questions in Discussions
5. **Maintain quality** - Review PRs from other contributors

## Resources

- **Main Docs**: [docs/START-HERE.md](START-HERE.md)
- **API Reference**: [docs/API-REFERENCE.md](API-REFERENCE.md)
- **Contributing Guide**: [docs/CONTRIBUTING.md](CONTRIBUTING.md)
- **Walkthroughs**: [docs/WALKTHROUGHS.md](WALKTHROUGHS.md)
- **Architecture**: [docs/ARCHITECTURE.md](ARCHITECTURE.md)

## Feedback

Help us improve this onboarding guide!

- Found something confusing? Open an issue.
- Have a suggestion? Create a PR.
- Want to share your experience? Post in Discussions.

**Welcome to the UNRDF community!** We're excited to have you here.

---

*Completed onboarding? Add your name below:*
