# P0 Implementation Testing Guide

## Quick Start Testing

### 1. Start Jaeger (Optional - for trace visualization)

```bash
# Using Docker
docker run -d --name jaeger \
  -e COLLECTOR_OTLP_ENABLED=true \
  -p 16686:16686 \
  -p 4318:4318 \
  jaegertracing/all-in-one:latest

# Access Jaeger UI: http://localhost:16686
```

### 2. Test Hook Evaluation

#### Test SPARQL ASK Hook

**Create test hook:**
```bash
cat > /tmp/test-hook.json <<'EOF'
{
  "meta": {
    "name": "test-sparql-ask",
    "description": "Test SPARQL ASK query"
  },
  "when": {
    "kind": "sparql-ask",
    "query": "ASK { ?s ?p ?o }"
  }
}
EOF
```

**Create test data:**
```bash
cat > /tmp/test-data.ttl <<'EOF'
@prefix ex: <http://example.org/> .

ex:Alice a ex:Person ;
    ex:name "Alice" .
EOF
```

**Run evaluation:**
```bash
OTEL_DEBUG=1 node cli/unrdf.mjs hook eval /tmp/test-hook.json \
  --data=/tmp/test-data.ttl \
  --verbose

# Expected output:
# 🔍 Evaluating hook: /tmp/test-hook.json
# 📊 Loaded 2 triples from /tmp/test-data.ttl
#
# 🔥 Result: ✅ FIRED
#    Type: sparql-ask
#    Duration: 23ms
#
# 🔍 Trace ID: abc123def456...
#    View in Jaeger: http://localhost:16686/trace/abc123...
```

#### Test Threshold Hook

**Create threshold hook:**
```bash
cat > /tmp/threshold-hook.json <<'EOF'
{
  "meta": {
    "name": "test-threshold",
    "description": "Test threshold check"
  },
  "when": {
    "kind": "threshold",
    "query": "SELECT (COUNT(?s) AS ?count) WHERE { ?s ?p ?o }",
    "threshold": 1,
    "operator": "gt"
  }
}
EOF
```

**Run evaluation:**
```bash
node cli/unrdf.mjs hook eval /tmp/threshold-hook.json \
  --data=/tmp/test-data.ttl

# Expected output:
# 🔥 Result: ✅ FIRED
#    Type: threshold
#    Duration: 18ms
#    Value: 2 gt 1
```

---

### 3. Test Policy Validation

#### Create Policy Pack

**Create policy manifest:**
```bash
mkdir -p /tmp/policies/test-policy

cat > /tmp/policies/test-policy/manifest.json <<'EOF'
{
  "id": "test-policy-pack",
  "meta": {
    "name": "test-policy",
    "version": "1.0.0",
    "description": "Test policy pack"
  },
  "config": {
    "enabled": true,
    "priority": 50
  },
  "hooks": [
    {
      "meta": {
        "name": "data-exists",
        "description": "Ensure data exists",
        "severity": "error"
      },
      "when": {
        "kind": "sparql-ask",
        "query": "ASK { ?s ?p ?o }"
      }
    }
  ]
}
EOF
```

**Run validation:**
```bash
node cli/unrdf.mjs graph validate default \
  --policy=test-policy \
  --report=/tmp/validation-report.txt

# Expected output:
# 🔍 Validating graph: default
#    Policy Pack: test-policy
#
# ✅ Validation PASSED
#
# ============================================================
#   POLICY VALIDATION REPORT
# ============================================================
#
# Policy Pack: test-policy
# Status: ✅ PASSED
# Hooks Evaluated: 1
# Violations: 0
# Execution Time: 45ms
#
# 🔍 Trace ID: def456ghi789...
```

---

### 4. Test with OTEL Debugging

```bash
# Enable OTEL debugging
export OTEL_DEBUG=1

# Run any command
node cli/unrdf.mjs hook eval /tmp/test-hook.json --data=/tmp/test-data.ttl

# You should see:
# [OTEL] Tracer initialized
# [OTEL] Exporting to Jaeger: http://localhost:14268/api/traces
# [OTEL] Service: unrdf-cli
# ... (command output)
# [OTEL] Flushing spans and shutting down...
# [OTEL] Shutdown complete
```

---

## Automated Test Suite

### Create Test Runner

```bash
cat > /tmp/test-p0-implementations.sh <<'EOF'
#!/bin/bash

set -e

echo "🧪 P0 Implementation Test Suite"
echo "================================"

# Test 1: Hook Evaluation - SPARQL ASK
echo ""
echo "Test 1: SPARQL ASK Hook"
node cli/unrdf.mjs hook eval /tmp/test-hook.json \
  --data=/tmp/test-data.ttl \
  --format=json > /tmp/result1.json

if grep -q '"fired": true' /tmp/result1.json; then
  echo "✅ PASS: Hook fired correctly"
else
  echo "❌ FAIL: Hook did not fire"
  exit 1
fi

# Test 2: Hook Evaluation - Threshold
echo ""
echo "Test 2: Threshold Hook"
node cli/unrdf.mjs hook eval /tmp/threshold-hook.json \
  --data=/tmp/test-data.ttl \
  --format=json > /tmp/result2.json

if grep -q '"fired": true' /tmp/result2.json; then
  echo "✅ PASS: Threshold hook fired correctly"
else
  echo "❌ FAIL: Threshold hook did not fire"
  exit 1
fi

# Test 3: Policy Validation
echo ""
echo "Test 3: Policy Validation"
node cli/unrdf.mjs graph validate default \
  --policy=test-policy 2>&1 | tee /tmp/result3.txt

if grep -q "Validation PASSED" /tmp/result3.txt; then
  echo "✅ PASS: Validation passed"
else
  echo "❌ FAIL: Validation failed"
  exit 1
fi

# Test 4: OTEL Trace Generation
echo ""
echo "Test 4: OTEL Trace ID Export"
OTEL_DEBUG=1 node cli/unrdf.mjs hook eval /tmp/test-hook.json \
  --data=/tmp/test-data.ttl 2>&1 | tee /tmp/result4.txt

if grep -q "Trace ID:" /tmp/result4.txt; then
  echo "✅ PASS: Trace ID exported to stdout"
else
  echo "❌ FAIL: No trace ID in output"
  exit 1
fi

echo ""
echo "================================"
echo "🎉 All P0 tests PASSED!"
EOF

chmod +x /tmp/test-p0-implementations.sh
```

**Run automated tests:**
```bash
/tmp/test-p0-implementations.sh
```

---

## Integration with npm test

### Run Specific Test Suites

```bash
# Run only hook evaluation tests
npm test -- test/cli/hook-eval.test.mjs

# Run only policy validation tests
npm test -- test/cli/policy-validate.test.mjs

# Run all CLI tests
npm test -- test/cli/*.test.mjs
```

---

## Verifying OTEL Traces in Jaeger

1. **Access Jaeger UI:** http://localhost:16686
2. **Select Service:** `unrdf-cli`
3. **Find Traces:** Click "Find Traces"
4. **Verify Spans:**
   - Look for `hook.eval` or `graph.validate` root spans
   - Check nested spans: `hook.evaluate.sparql-ask`, `policy.validate`
   - Verify attributes: `hook.name`, `hook.fired`, `store.size`

---

## Troubleshooting

### Issue: No traces in Jaeger

**Solution:**
```bash
# Check Jaeger is running
docker ps | grep jaeger

# Check OTEL_DEBUG output
OTEL_DEBUG=1 node cli/unrdf.mjs hook eval /tmp/test-hook.json ...

# Verify exporter URL
echo $OTEL_EXPORTER_OTLP_ENDPOINT  # Should be http://localhost:4318/v1/traces
```

### Issue: Hook evaluation fails

**Solution:**
```bash
# Check hook file syntax
cat /tmp/test-hook.json | jq .

# Verify data file
cat /tmp/test-data.ttl

# Run with verbose mode
node cli/unrdf.mjs hook eval /tmp/test-hook.json \
  --data=/tmp/test-data.ttl \
  --verbose
```

### Issue: Policy validation fails

**Solution:**
```bash
# Check policy pack exists
ls -la /tmp/policies/test-policy/

# Verify manifest syntax
cat /tmp/policies/test-policy/manifest.json | jq .

# Check policy pack loading
node -e "
  import { PolicyPackManager } from './src/knowledge-engine/policy-pack.mjs';
  const mgr = new PolicyPackManager('/tmp/policies');
  const pack = await mgr.loadPolicyPack({ name: 'test-policy' });
  console.log(pack.getStats());
"
```

---

## Performance Benchmarking

### Measure Hook Evaluation Performance

```bash
# Benchmark SPARQL ASK
time node cli/unrdf.mjs hook eval /tmp/test-hook.json \
  --data=/tmp/test-data.ttl

# Benchmark Threshold
time node cli/unrdf.mjs hook eval /tmp/threshold-hook.json \
  --data=/tmp/test-data.ttl

# Benchmark Policy Validation (5 hooks)
time node cli/unrdf.mjs graph validate default \
  --policy=test-policy
```

**Expected Performance:**
- Hook Evaluation: < 100ms
- Policy Validation (5 hooks): < 200ms
- OTEL Overhead: < 10ms

---

## CI/CD Integration

### GitHub Actions Example

```yaml
# .github/workflows/test-p0.yml
name: P0 Implementation Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest

    services:
      jaeger:
        image: jaegertracing/all-in-one:latest
        ports:
          - 16686:16686
          - 4318:4318
        env:
          COLLECTOR_OTLP_ENABLED: true

    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
        with:
          node-version: '20'

      - run: npm ci
      - run: npm test -- test/cli/*.test.mjs

      - name: Run P0 Manual Tests
        run: |
          chmod +x /tmp/test-p0-implementations.sh
          /tmp/test-p0-implementations.sh
```

---

**Testing Complete!** 🎉

All P0 implementations are ready for validation.
