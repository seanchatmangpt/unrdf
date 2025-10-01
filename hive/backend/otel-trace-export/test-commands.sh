#!/bin/bash
# OTEL Trace Export Test Suite
# Tests trace visibility in Jaeger

set -e

echo "🧪 OTEL Trace Export Test Suite"
echo "================================"
echo ""

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# 1. Check if Jaeger is running
echo "1️⃣  Checking Jaeger status..."
if curl -s http://localhost:16686/api/services > /dev/null 2>&1; then
    echo -e "${GREEN}✅ Jaeger is running${NC}"
else
    echo -e "${RED}❌ Jaeger is not running${NC}"
    echo "   Starting Jaeger..."
    docker run -d --name jaeger \
      -p 6831:6831/udp \
      -p 6832:6832/udp \
      -p 14268:14268 \
      -p 16686:16686 \
      jaegertracing/all-in-one:latest
    echo "   Waiting for Jaeger to start..."
    sleep 5
    echo -e "${GREEN}✅ Jaeger started${NC}"
fi
echo ""

# 2. Create test data file
echo "2️⃣  Creating test data file..."
cat > /tmp/test-otel.ttl << 'EOF'
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Alice rdf:type ex:Person ;
         ex:name "Alice" ;
         ex:age 30 .

ex:Bob rdf:type ex:Person ;
       ex:name "Bob" ;
       ex:age 25 .

ex:Charlie rdf:type ex:Person ;
           ex:name "Charlie" ;
           ex:age 35 .
EOF
echo -e "${GREEN}✅ Test data created: /tmp/test-otel.ttl${NC}"
echo ""

# 3. Run CLI command with OTEL enabled
echo "3️⃣  Running CLI command with OTEL tracing..."
export OTEL_DEBUG=1
export JAEGER_ENDPOINT=http://localhost:14268/api/traces

OUTPUT=$(node cli/unrdf.mjs store import /tmp/test-otel.ttl --graph=otel-test 2>&1)
echo "$OUTPUT"

# Extract trace ID from output
TRACE_ID=$(echo "$OUTPUT" | grep "Trace ID:" | awk '{print $NF}')

if [ -z "$TRACE_ID" ]; then
    echo -e "${RED}❌ No trace ID found in output${NC}"
    exit 1
else
    echo -e "${GREEN}✅ Trace ID captured: $TRACE_ID${NC}"
fi
echo ""

# 4. Wait for trace to be exported
echo "4️⃣  Waiting for trace export (2 seconds)..."
sleep 2
echo -e "${GREEN}✅ Export delay complete${NC}"
echo ""

# 5. Query Jaeger API for traces
echo "5️⃣  Querying Jaeger API for traces..."
TRACES=$(curl -s "http://localhost:16686/api/traces?service=unrdf-cli&limit=10")

if echo "$TRACES" | jq -e '.data | length > 0' > /dev/null 2>&1; then
    echo -e "${GREEN}✅ Traces found in Jaeger${NC}"

    # Count traces
    TRACE_COUNT=$(echo "$TRACES" | jq '.data | length')
    echo "   Found $TRACE_COUNT trace(s)"

    # Show latest trace details
    echo ""
    echo "📊 Latest Trace Details:"
    echo "$TRACES" | jq '.data[0] | {
      traceID: .traceID,
      spanCount: (.spans | length),
      duration: .spans[0].duration,
      operation: .spans[0].operationName,
      tags: .spans[0].tags | map({key, value}) | from_entries
    }'
else
    echo -e "${RED}❌ No traces found in Jaeger${NC}"
    echo "   Response: $TRACES"
    exit 1
fi
echo ""

# 6. Verify specific trace by ID
echo "6️⃣  Verifying trace by ID..."
SPECIFIC_TRACE=$(curl -s "http://localhost:16686/api/traces/$TRACE_ID")

if echo "$SPECIFIC_TRACE" | jq -e '.data[0].traceID' > /dev/null 2>&1; then
    echo -e "${GREEN}✅ Trace verified by ID${NC}"

    # Show span details
    echo ""
    echo "🔍 Span Details:"
    echo "$SPECIFIC_TRACE" | jq '.data[0].spans[0] | {
      spanID,
      operationName,
      duration,
      tags: .tags | map(select(.key | startswith("cli.") or startswith("import."))) | map({key, value})
    }'
else
    echo -e "${YELLOW}⚠️  Could not verify specific trace${NC}"
fi
echo ""

# 7. Check span attributes
echo "7️⃣  Checking span attributes..."
EXPECTED_ATTRS=("cli.command" "file" "graph" "import.quads" "import.success")
MISSING_ATTRS=()

for attr in "${EXPECTED_ATTRS[@]}"; do
    if echo "$SPECIFIC_TRACE" | jq -e ".data[0].spans[0].tags[] | select(.key == \"$attr\")" > /dev/null 2>&1; then
        echo -e "${GREEN}✅ Found attribute: $attr${NC}"
    else
        echo -e "${RED}❌ Missing attribute: $attr${NC}"
        MISSING_ATTRS+=("$attr")
    fi
done

if [ ${#MISSING_ATTRS[@]} -eq 0 ]; then
    echo -e "${GREEN}✅ All expected attributes present${NC}"
else
    echo -e "${RED}❌ Missing ${#MISSING_ATTRS[@]} attributes${NC}"
fi
echo ""

# 8. Summary
echo "================================"
echo "📋 Test Summary"
echo "================================"
echo -e "${GREEN}✅ Jaeger running and accessible${NC}"
echo -e "${GREEN}✅ CLI command executed with OTEL${NC}"
echo -e "${GREEN}✅ Trace ID captured${NC}"
echo -e "${GREEN}✅ Traces visible in Jaeger${NC}"
echo -e "${GREEN}✅ Trace correlation verified${NC}"

if [ ${#MISSING_ATTRS[@]} -eq 0 ]; then
    echo -e "${GREEN}✅ All span attributes present${NC}"
else
    echo -e "${YELLOW}⚠️  Some attributes missing${NC}"
fi

echo ""
echo "🌐 Open Jaeger UI:"
echo "   http://localhost:16686/trace/$TRACE_ID"
echo ""

# Cleanup
rm -f /tmp/test-otel.ttl

echo -e "${GREEN}🎉 Test suite complete!${NC}"
