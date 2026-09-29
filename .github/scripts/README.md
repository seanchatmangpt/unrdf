# GitHub Actions Scripts for UNRDF v6

This directory contains supporting scripts for the v6 CI/CD pipeline.

## Scripts

### pr-comment.mjs

Generates formatted PR comments with validation results.

**Usage:**
```bash
node pr-comment.mjs \
  --validation-report <path-to-validation.json> \
  --coverage <path-to-coverage-summary.json> \
  --output <output-path>
```

**Example:**
```bash
node pr-comment.mjs \
  --validation-report validation-artifacts/v6-validation-report/v6-validation.json \
  --coverage validation-artifacts/v6-coverage-report/coverage-summary.json \
  --output pr-comment.md
```

---

### baseline-metrics.mjs

Compares performance metrics against baseline to detect regressions.

**Usage:**
```bash
node baseline-metrics.mjs compare \
  <baseline.json> \
  <current.json> \
  --threshold <percentage>
```

**Example:**
```bash
node baseline-metrics.mjs compare \
  .baseline/baseline.json \
  .current/baseline.json \
  --threshold 10
```

**Output:**
- Console report with regressions/improvements
- `regression-report.json` file
- Exit code 1 if regressions found

---

### release-notes.mjs

Generates comprehensive release notes with cryptographic receipt.

**Usage:**
```bash
node release-notes.mjs \
  --version <version> \
  --type <alpha|beta|rc|stable> \
  --output <output-path>
```

**Example:**
```bash
node release-notes.mjs \
  --version 6.0.0-alpha.2 \
  --type alpha \
  --output release-notes.md
```

**Output:**
- Full markdown release notes
- Categorized commits (features/fixes/breaking)
- Cryptographic receipt
- Migration guide links

---

## Development

### Testing Scripts Locally

```bash
# Test pr-comment
node pr-comment.mjs --help

# Test baseline-metrics
node baseline-metrics.mjs compare test-baseline.json test-current.json --threshold 5

# Test release-notes
node release-notes.mjs --version 6.0.0-alpha.1 --type alpha --output /tmp/notes.md
```

### Debugging

All scripts output to stderr for errors and stdout for results. Use standard Node.js debugging:

```bash
NODE_OPTIONS='--inspect-brk' node script.mjs [args]
```

---

## Maintenance

- Scripts use Node.js 18+ features (ESM, modern APIs)
- No external dependencies (only Node.js built-ins)
- Executable permissions required (`chmod +x *.mjs`)
- Follow project linting standards

---

## See Also

- [CI/CD Pipeline Documentation](../../docs/v6/CI_CD_PIPELINE.md)
- [v6 Validation Script](../../scripts/v6-validate.mjs)
- [Migration Plan](../../docs/v6/MIGRATION_PLAN.md)
