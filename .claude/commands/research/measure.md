---
description: Measure a specific metric with evidence
arguments:
  - name: metric
    description: The metric to measure (performance, coverage, size, complexity)
    required: true
  - name: target
    description: What to measure (file, directory, system)
    required: false
    default: '.'
---

# Metric Measurement: $metric

## Target: $target

Measure "$metric" for target "$target" with empirical evidence.

## Measurement Protocol

### Step 1: Metric Definition

Define what "$metric" means precisely:

**Performance**: Execution time, throughput, latency
**Coverage**: Test coverage percentage, line coverage
**Size**: Lines of code, file count, bundle size
**Complexity**: Cyclomatic complexity, nesting depth
**Quality**: Lint violations, type errors, test pass rate

**For metric "$metric"**:
- Definition: [Precise definition]
- Unit: [Measurement unit]
- Tool: [Measurement tool/command]
- Baseline: [Expected range or comparison point]

### Step 2: Measurement Execution

Execute measurement command:

```bash
# Performance
timeout 10s time [command]

# Coverage
timeout 15s npm run coverage -- $target

# Size
wc -l $target/**/*.{js,mjs,ts} 2>/dev/null | tail -1

# Complexity
timeout 5s npm run lint -- $target --format json

# Quality
timeout 5s npm test -- $target 2>&1 | grep -E "Tests:|passed|failed"
```

Capture FULL output:

```bash
timeout [timeout]s [measurement-command] 2>&1 | tee /tmp/metric-output.txt
cat /tmp/metric-output.txt
```

### Step 3: Data Extraction

Extract numeric value from output:

```bash
# Example: Extract test pass rate
grep "Tests:" /tmp/metric-output.txt | sed 's/.*(\([0-9.]*\)%).*/\1/'
```

**Measured Value**: [numeric value] [unit]

### Step 4: Analysis

Compare against baseline:

| Metric | Target | Measured | Baseline | Delta | Status |
|--------|--------|----------|----------|-------|--------|
| $metric | $target | [value] | [expected] | [±%] | ✅/❌/⚠️ |

**Interpretation**:
- ✅ **GOOD**: Measured value meets or exceeds baseline
- ❌ **BAD**: Measured value below acceptable threshold
- ⚠️ **WARNING**: Measured value near threshold

### Step 5: Trend Analysis

If previous measurements exist, analyze trend:

```javascript
// Retrieve previous measurements
mcp__claude-flow__memory_usage({
  action: 'retrieve',
  key: 'research/metrics/$metric/$target',
  namespace: 'measurements'
})
```

Calculate trend:
- **Improving**: ↗️ Value increasing (for positive metrics)
- **Degrading**: ↘️ Value decreasing (for positive metrics)
- **Stable**: ➡️ Value within ±5% of previous

### Step 6: Visualization

Present measurement in context:

```
Metric: $metric
Target: $target
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Current:  [value] [unit]
Baseline: [value] [unit]
Delta:    [±value] ([±%])
Trend:    ↗️/↘️/➡️

History (last 5 measurements):
1. [timestamp]: [value]
2. [timestamp]: [value]
3. [timestamp]: [value]
4. [timestamp]: [value]
5. [timestamp]: [value]
```

### Step 7: Storage

Store measurement with metadata:

```javascript
mcp__claude-flow__memory_usage({
  action: 'store',
  key: 'research/metrics/$metric/$target/' + Date.now(),
  namespace: 'measurements',
  value: JSON.stringify({
    metric: "$metric",
    target: "$target",
    value: [measured_value],
    unit: "[unit]",
    baseline: [baseline_value],
    delta_percent: [delta],
    status: "GOOD|BAD|WARNING",
    command: "[measurement command]",
    output: "[full output]",
    timestamp: new Date().toISOString()
  })
})
```

## Metric-Specific Protocols

### Performance Measurement

```bash
# Execution time
timeout 10s time npm test -- $target 2>&1 | grep real

# Memory usage
timeout 5s /usr/bin/time -v node $target 2>&1 | grep "Maximum resident"

# Benchmark
timeout 20s npm run bench -- $target
```

### Coverage Measurement

```bash
# Line coverage
timeout 15s npm run coverage -- $target 2>&1 | grep "Statements"

# Branch coverage
timeout 15s npm run coverage -- $target 2>&1 | grep "Branches"
```

### Size Measurement

```bash
# Lines of code
find $target -name "*.js" -o -name "*.mjs" | xargs wc -l | tail -1

# File count
find $target -type f | wc -l

# Bundle size
timeout 10s npm run build && du -h dist/bundle.js
```

### Complexity Measurement

```bash
# Lint violations
timeout 5s npm run lint -- $target --format json | jq '.[] | length'

# Type errors
timeout 5s npm run typecheck 2>&1 | grep -c "error TS"
```

### Quality Measurement

```bash
# Test pass rate
timeout 10s npm test -- $target 2>&1 | grep -E "[0-9]+ passed" | sed 's/.*\([0-9]*\) passed.*/\1/'

# OTEL validation
timeout 20s node validation/run-all.mjs comprehensive | grep "Score:"
```

## Success Criteria

- [ ] Metric precisely defined with unit
- [ ] Measurement command executed successfully
- [ ] Full output captured and stored
- [ ] Numeric value extracted correctly
- [ ] Baseline comparison performed
- [ ] Trend analysis (if historical data exists)
- [ ] Results stored in coordination memory

## Output Format

Provide measurement report:

```markdown
## Measurement Report: $metric

**Target**: $target
**Measured Value**: [value] [unit]
**Baseline**: [value] [unit]
**Delta**: [±value] ([±%])
**Status**: ✅/❌/⚠️
**Trend**: ↗️/↘️/➡️

### Evidence
- Command: `[measurement command]`
- Output: [link to full output]
- Timestamp: [ISO-8601]

### Analysis
[2-3 sentence interpretation]

### Recommendations
- [Action items based on status]
```

---

**Usage Examples**:

```
/research/measure performance src/core
/research/measure coverage tests/
/research/measure size src/
/research/measure complexity src/utils/validation.mjs
/research/measure quality
```

---

**Adversarial Measurement Checklist**:

- ❓ Did I RUN measurement or estimate?
- ❓ Did I capture FULL output?
- ❓ Is baseline realistic?
- ❓ Can measurement be reproduced?
- ❓ What's measurement precision (±%)?
