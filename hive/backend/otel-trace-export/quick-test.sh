#!/bin/bash
# Quick OTEL trace test - verify exports work

set -e

echo "🚀 Quick OTEL Trace Export Test"
echo "================================"

# Create minimal test data
cat > /tmp/quick-test.ttl << 'EOF'
@prefix ex: <http://example.org/> .
ex:test ex:name "Quick Test" .
EOF

# Run CLI with OTEL debug enabled
export OTEL_DEBUG=1
export JAEGER_ENDPOINT=http://localhost:14268/api/traces

echo ""
echo "Running: node cli/unrdf.mjs store import /tmp/quick-test.ttl --graph=quicktest"
echo ""

node cli/unrdf.mjs store import /tmp/quick-test.ttl --graph=quicktest

echo ""
echo "✅ Check console output above for:"
echo "   - [OTEL] Tracer initialized"
echo "   - Trace ID: ..."
echo "   - View in Jaeger: ..."

# Cleanup
rm -f /tmp/quick-test.ttl

echo ""
echo "🔍 To verify in Jaeger:"
echo "   1. Open: http://localhost:16686"
echo "   2. Service: unrdf-cli"
echo "   3. Find Traces"
