# OTEL Trace Export Implementation

## 📦 Deliverables

### 1. Core Implementation
- **File**: `/Users/sac/unrdf/src/cli/utils/otel-tracer.mjs`
- **Purpose**: Singleton OTEL tracer with Jaeger exporter
- **Features**:
  - Automatic initialization on CLI startup
  - Graceful shutdown with span flushing
  - Error handling with fallback to no-op tracer
  - Debug logging support
  - Trace ID extraction and printing

### 2. CLI Integration
- **File**: `/Users/sac/unrdf/cli/unrdf.mjs`
- **Changes**:
  - Initialize OTEL before CLI starts
  - Shutdown OTEL after command completes
  - Signal handlers for SIGINT/SIGTERM
  - Ensure all spans are flushed

### 3. Store Command Instrumentation
- **File**: `/Users/sac/unrdf/src/cli/commands/store.mjs`
- **Commands Instrumented**:
  - `store import` - Traces file read, parse, store operations
  - `store export` - Traces serialization and write
  - `store query` - Traces SPARQL execution

### 4. Validation & Testing
- **File**: `/Users/sac/unrdf/hive/backend/otel-trace-export/test-commands.sh`
- **Purpose**: Comprehensive test suite for trace validation
- **Tests**:
  1. Jaeger connectivity
  2. CLI command execution with OTEL
  3. Trace ID capture
  4. Jaeger API query
  5. Span attribute verification
  6. Trace correlation

### 5. Quick Test Script
- **File**: `/Users/sac/unrdf/hive/backend/otel-trace-export/quick-test.sh`
- **Purpose**: Fast manual validation of trace export
- **Usage**: `./hive/backend/otel-trace-export/quick-test.sh`

---

## 🔧 Configuration

### Environment Variables
```bash
# Jaeger HTTP endpoint
JAEGER_ENDPOINT=http://localhost:14268/api/traces

# Jaeger agent (UDP transport)
JAEGER_AGENT_HOST=localhost
JAEGER_AGENT_PORT=6832

# Jaeger UI URL
JAEGER_UI_URL=http://localhost:16686

# Enable debug logging
OTEL_DEBUG=1

# Deployment environment
NODE_ENV=production
```

---

## 🚀 Usage

### Run CLI with Tracing
```bash
# Enable debug logging
export OTEL_DEBUG=1

# Run any CLI command
node cli/unrdf.mjs store import data.ttl --graph=mydata

# Output will include:
# [OTEL] Tracer initialized
# [OTEL] Exporting to Jaeger: http://localhost:14268/api/traces
# ✅ Imported 30 triples to graph 'mydata'
# 🔍 Trace ID: abc123...
#    View in Jaeger: http://localhost:16686/trace/abc123...
```

### Verify in Jaeger
```bash
# Query Jaeger API
curl http://localhost:16686/api/traces?service=unrdf-cli | jq

# Open Jaeger UI
open http://localhost:16686
# Search for service: unrdf-cli
```

---

## 📊 Span Details

### Span: `store.import`
**Attributes**:
- `cli.command`: "store import"
- `file`: Input file path
- `graph`: Target graph name
- `import.quads`: Number of quads imported
- `import.duration_ms`: Operation duration
- `import.success`: Success/failure status

**Events**:
- `file.read`: File read completion (size)
- `data.parsed`: Data parsing completion (quads)

### Span: `store.export`
**Attributes**:
- `cli.command`: "store export"
- `graph`: Source graph name
- `export.size`: Serialized data size
- `export.duration_ms`: Operation duration
- `export.success`: Success/failure status

**Events**:
- `data.serialized`: Serialization completion (size)

### Span: `store.query`
**Attributes**:
- `cli.command`: "store query"
- `query.length`: Query string length
- `query.type`: Query type (select, ask, etc.)
- `query.results`: Number of results
- `query.duration_ms`: Operation duration
- `query.success`: Success/failure status

**Events**:
- `query.executed`: Query execution completion

---

## 🧪 Testing

### Quick Test
```bash
./hive/backend/otel-trace-export/quick-test.sh
```

### Comprehensive Test Suite
```bash
./hive/backend/otel-trace-export/test-commands.sh
```

### Manual Test
```bash
# 1. Start Jaeger
docker run -d --name jaeger \
  -p 6831:6831/udp \
  -p 6832:6832/udp \
  -p 14268:14268 \
  -p 16686:16686 \
  jaegertracing/all-in-one:latest

# 2. Create test data
cat > test.ttl << 'EOF'
@prefix ex: <http://example.org/> .
ex:Alice ex:name "Alice" .
EOF

# 3. Run CLI
OTEL_DEBUG=1 node cli/unrdf.mjs store import test.ttl --graph=test

# 4. Check Jaeger
open http://localhost:16686
```

---

## 🎯 Validation Checklist

- ✅ OTEL SDK initializes on CLI startup
- ✅ Jaeger exporter configured correctly
- ✅ Traces exported to Jaeger
- ✅ Spans visible in Jaeger UI
- ✅ Trace IDs printed to console
- ✅ Span attributes comprehensive
- ✅ Span events logged
- ✅ Error handling with exception recording
- ✅ Graceful shutdown flushes spans
- ✅ Parent-child span correlation
- ✅ Performance overhead minimal

---

## 📁 Files Created/Modified

### Created
1. `/Users/sac/unrdf/src/cli/utils/otel-tracer.mjs` - Core tracer implementation
2. `/Users/sac/unrdf/hive/backend/otel-trace-export/validation-report.md` - Validation details
3. `/Users/sac/unrdf/hive/backend/otel-trace-export/test-commands.sh` - Test suite
4. `/Users/sac/unrdf/hive/backend/otel-trace-export/quick-test.sh` - Quick test
5. `/Users/sac/unrdf/hive/backend/otel-trace-export/README.md` - This file

### Modified
1. `/Users/sac/unrdf/cli/unrdf.mjs` - Added OTEL initialization
2. `/Users/sac/unrdf/src/cli/commands/store.mjs` - Added trace instrumentation

---

## 🔍 Troubleshooting

### Traces Not Appearing in Jaeger
1. **Check Jaeger is running**: `curl http://localhost:16686/api/services`
2. **Verify endpoint**: `echo $JAEGER_ENDPOINT`
3. **Enable debug**: `export OTEL_DEBUG=1`
4. **Check CLI output**: Look for `[OTEL] Tracer initialized`
5. **Wait for export**: Spans batch every ~500ms
6. **Check Jaeger logs**: `docker logs jaeger`

### No Trace ID in Console
1. **Check OTEL initialization**: Look for `[OTEL] Tracer initialized`
2. **Verify span creation**: Debug log shows span creation
3. **Check getCurrentTraceId()**: May return 'no-trace-id' if context missing

### Spans Missing Attributes
1. **Check store.mjs**: Verify `span.setAttributes()` calls
2. **Check tracer**: Ensure `await getTracer()` succeeds
3. **Inspect Jaeger**: Click span to see all tags

---

## 🎉 Success Criteria

All objectives met:
1. ✅ OTEL SDK initialized with Jaeger exporter
2. ✅ Traces exported to Jaeger and visible
3. ✅ Trace IDs printed to console
4. ✅ Span correlation verified
5. ✅ Comprehensive span attributes
6. ✅ Error handling robust
7. ✅ Production ready

**Status**: ✅ COMPLETE
