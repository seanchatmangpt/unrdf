# OTEL Trace Export - Verification Steps

## ✅ Step-by-Step Verification Guide

Follow these steps to verify OTEL traces are exported to Jaeger and visible.

---

## Prerequisites

1. **Jaeger Running**
   ```bash
   docker run -d --name jaeger \
     -p 6831:6831/udp \
     -p 6832:6832/udp \
     -p 14268:14268 \
     -p 16686:16686 \
     jaegertracing/all-in-one:latest
   ```

2. **Check Jaeger is Accessible**
   ```bash
   curl http://localhost:16686/api/services
   # Should return: {"data":[], ...}
   ```

---

## Verification Steps

### Step 1: Check Implementation Files Exist

```bash
# Core tracer module
ls -lh /Users/sac/unrdf/src/cli/utils/otel-tracer.mjs
# Expected: ~4.5KB file

# Modified CLI
ls -lh /Users/sac/unrdf/cli/unrdf.mjs
# Expected: ~22KB file

# Modified store commands
ls -lh /Users/sac/unrdf/src/cli/commands/store.mjs
# Expected: ~7KB file
```

**Expected Result**: All files exist and have expected sizes.

---

### Step 2: Syntax Validation

```bash
# Check tracer syntax
node --check /Users/sac/unrdf/src/cli/utils/otel-tracer.mjs
# Expected: ✅ otel-tracer.mjs syntax OK

# Check CLI syntax
node --check /Users/sac/unrdf/cli/unrdf.mjs
# Expected: ✅ unrdf.mjs syntax OK

# Check store commands syntax
node --check /Users/sac/unrdf/src/cli/commands/store.mjs
# Expected: ✅ store.mjs syntax OK
```

**Expected Result**: All syntax checks pass with no errors.

---

### Step 3: Create Test Data

```bash
cat > /tmp/otel-verify.ttl << 'EOF'
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Alice rdf:type ex:Person ;
         ex:name "Alice" ;
         ex:age 30 .

ex:Bob rdf:type ex:Person ;
       ex:name "Bob" ;
       ex:age 25 .
EOF

# Verify file created
cat /tmp/otel-verify.ttl
```

**Expected Result**: Test data file created with 2 people (Alice, Bob).

---

### Step 4: Run CLI Command with OTEL Debug

```bash
# Set environment for debug output
export OTEL_DEBUG=1
export JAEGER_ENDPOINT=http://localhost:14268/api/traces

# Run CLI command
node cli/unrdf.mjs store import /tmp/otel-verify.ttl --graph=otel-verify
```

**Expected Output**:
```
[OTEL] Tracer initialized
[OTEL] Exporting to Jaeger: http://localhost:14268/api/traces
[OTEL] Service: unrdf-cli
✅ Imported 6 triples to graph 'otel-verify'

🔍 Trace ID: 1a2b3c4d5e6f7g8h9i0j
   View in Jaeger: http://localhost:16686/trace/1a2b3c4d5e6f7g8h9i0j
```

**What to Look For**:
- ✅ `[OTEL] Tracer initialized` - OTEL SDK started
- ✅ `[OTEL] Exporting to Jaeger:...` - Endpoint configured
- ✅ `✅ Imported X triples` - Command succeeded
- ✅ `🔍 Trace ID: ...` - Trace ID captured
- ✅ `View in Jaeger: ...` - Jaeger link printed

**If Missing**:
- No `[OTEL]` logs → Check import in `cli/unrdf.mjs`
- No trace ID → Check `printTraceInfo()` call in `store.mjs`

---

### Step 5: Wait for Trace Export

```bash
# Wait 2 seconds for batch export
sleep 2
echo "✅ Export delay complete"
```

**Reason**: OTEL batches spans and exports every ~500ms. Waiting ensures export completes.

---

### Step 6: Query Jaeger API

```bash
# Query Jaeger for traces
curl -s "http://localhost:16686/api/traces?service=unrdf-cli&limit=5" | jq '.'
```

**Expected Response**:
```json
{
  "data": [
    {
      "traceID": "1a2b3c4d5e6f7g8h9i0j",
      "spans": [
        {
          "traceID": "1a2b3c4d5e6f7g8h9i0j",
          "spanID": "def456...",
          "operationName": "store.import",
          "duration": 125000,
          "tags": [
            { "key": "cli.command", "value": "store import" },
            { "key": "file", "value": "/tmp/otel-verify.ttl" },
            { "key": "graph", "value": "otel-verify" },
            { "key": "import.quads", "value": 6 },
            { "key": "import.success", "value": true }
          ]
        }
      ]
    }
  ]
}
```

**What to Verify**:
- ✅ `data` array has at least one trace
- ✅ `traceID` matches console output
- ✅ `operationName` is "store.import"
- ✅ `tags` include `cli.command`, `file`, `graph`, `import.quads`, `import.success`

**If Empty**:
- Check Jaeger is running: `curl http://localhost:16686/api/services`
- Check endpoint: `echo $JAEGER_ENDPOINT`
- Check CLI output for errors
- Try again with longer wait time

---

### Step 7: Verify Specific Trace by ID

```bash
# Extract trace ID from step 4 output
TRACE_ID="1a2b3c4d5e6f7g8h9i0j"  # Replace with actual trace ID

# Query specific trace
curl -s "http://localhost:16686/api/traces/$TRACE_ID" | jq '.data[0].spans[0] | {
  operationName,
  duration,
  tags: .tags | map(select(.key | startswith("cli.") or startswith("import.")))
}'
```

**Expected Output**:
```json
{
  "operationName": "store.import",
  "duration": 125000,
  "tags": [
    { "key": "cli.command", "value": "store import" },
    { "key": "import.quads", "value": 6 },
    { "key": "import.duration_ms", "value": 125 },
    { "key": "import.success", "value": true }
  ]
}
```

**What to Verify**:
- ✅ Trace found by ID
- ✅ Operation name correct
- ✅ Duration recorded
- ✅ All expected tags present

---

### Step 8: Open Jaeger UI

```bash
# Open Jaeger UI in browser
open http://localhost:16686

# Or manually navigate to:
# http://localhost:16686
```

**In Jaeger UI**:
1. **Service Dropdown**: Select `unrdf-cli`
2. **Find Traces Button**: Click to search
3. **Trace List**: Should show recent traces
4. **Click a Trace**: Opens timeline view

**Expected in Timeline View**:
- Span: `store.import`
- Duration: ~125ms
- Tags:
  - `cli.command: store import`
  - `file: /tmp/otel-verify.ttl`
  - `graph: otel-verify`
  - `import.quads: 6`
  - `import.success: true`
- Events:
  - `file.read` (with size)
  - `data.parsed` (with quads)

**If Not Visible**:
- Refresh page
- Change time range to "Last 15 minutes"
- Check service dropdown shows `unrdf-cli`

---

### Step 9: Test Error Handling

```bash
# Try to import non-existent file
OTEL_DEBUG=1 node cli/unrdf.mjs store import /tmp/does-not-exist.ttl --graph=error-test 2>&1 | tee /tmp/error-test.log

# Check for error span
sleep 2
curl -s "http://localhost:16686/api/traces?service=unrdf-cli&limit=1" | jq '.data[0].spans[] | select(.tags[] | select(.key == "import.success" and .value == false))'
```

**Expected**:
- CLI exits with error code
- Trace still exported with `import.success: false`
- Span includes exception details

---

### Step 10: Automated Test Suite

```bash
# Run comprehensive test suite
./hive/backend/otel-trace-export/test-commands.sh
```

**Expected Output**:
```
🧪 OTEL Trace Export Test Suite
================================

1️⃣  Checking Jaeger status...
✅ Jaeger is running

2️⃣  Creating test data file...
✅ Test data created: /tmp/test-otel.ttl

3️⃣  Running CLI command with OTEL tracing...
[OTEL] Tracer initialized
✅ Imported 9 triples to graph 'otel-test'
🔍 Trace ID: ...
✅ Trace ID captured: ...

4️⃣  Waiting for trace export (2 seconds)...
✅ Export delay complete

5️⃣  Querying Jaeger API for traces...
✅ Traces found in Jaeger
   Found 1 trace(s)

6️⃣  Verifying trace by ID...
✅ Trace verified by ID

7️⃣  Checking span attributes...
✅ Found attribute: cli.command
✅ Found attribute: file
✅ Found attribute: graph
✅ Found attribute: import.quads
✅ Found attribute: import.success
✅ All expected attributes present

================================
📋 Test Summary
================================
✅ Jaeger running and accessible
✅ CLI command executed with OTEL
✅ Trace ID captured
✅ Traces visible in Jaeger
✅ Trace correlation verified
✅ All span attributes present

🌐 Open Jaeger UI:
   http://localhost:16686/trace/...

🎉 Test suite complete!
```

---

## ✅ Success Criteria

**All of the following must be true**:

1. ✅ Syntax checks pass for all modified files
2. ✅ CLI runs without errors
3. ✅ Console output includes `[OTEL] Tracer initialized`
4. ✅ Console output includes trace ID
5. ✅ Jaeger API returns traces for `unrdf-cli` service
6. ✅ Traces visible in Jaeger UI
7. ✅ Span attributes include all expected fields
8. ✅ Automated test suite passes all checks

**If ANY fail**: See troubleshooting section in README.md

---

## 🔍 Quick Validation Command

```bash
# One-liner to verify everything
./hive/backend/otel-trace-export/test-commands.sh && echo "🎉 ALL TESTS PASSED"
```

**Expected**: Test suite runs and prints `🎉 ALL TESTS PASSED` at the end.

---

## 📊 Verification Checklist

Use this checklist to track verification progress:

- [ ] Jaeger container running
- [ ] Implementation files exist with correct sizes
- [ ] Syntax validation passes (3/3 files)
- [ ] Test data created
- [ ] CLI command runs with OTEL debug
- [ ] Console shows `[OTEL] Tracer initialized`
- [ ] Console shows trace ID
- [ ] Jaeger API returns traces
- [ ] Specific trace found by ID
- [ ] Jaeger UI shows traces
- [ ] Span attributes verified (5/5 expected)
- [ ] Error handling tested
- [ ] Automated test suite passes

**Status**: ✅ VERIFIED (when all checked)

---

## 🚨 Troubleshooting

### Issue: No `[OTEL]` logs
**Fix**: Check `cli/unrdf.mjs` imports and initialization

### Issue: No trace ID in console
**Fix**: Check `printTraceInfo()` is called in `store.mjs`

### Issue: Empty Jaeger API response
**Fix**:
1. Wait longer (2-5 seconds)
2. Check `JAEGER_ENDPOINT` env var
3. Check Jaeger is running: `curl http://localhost:16686/api/services`

### Issue: Trace ID mismatch
**Fix**: Ensure you're using the exact trace ID from console output

---

**Last Updated**: 2025-10-01
**Status**: Ready for Verification
