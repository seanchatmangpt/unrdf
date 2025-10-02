#!/bin/bash
set -e

# UNRDF Production Environment Validation Script
# This script validates the production environment configuration
# and ensures all required services and dependencies are available

echo "🔍 Validating UNRDF Production Environment..."
echo "================================================"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Track validation status
VALIDATION_FAILED=0

# ==============================================================================
# Function: Check required environment variable
# ==============================================================================
check_required_var() {
  local var_name="$1"
  if [ -z "${!var_name}" ]; then
    echo -e "${RED}❌ Missing required env var: $var_name${NC}"
    VALIDATION_FAILED=1
  else
    echo -e "${GREEN}✅ $var_name=${!var_name}${NC}"
  fi
}

# ==============================================================================
# Function: Check optional environment variable
# ==============================================================================
check_optional_var() {
  local var_name="$1"
  local default_value="$2"
  if [ -z "${!var_name}" ]; then
    echo -e "${YELLOW}⚠️  Optional var not set: $var_name (using default: $default_value)${NC}"
  else
    echo -e "${GREEN}✅ $var_name=${!var_name}${NC}"
  fi
}

# ==============================================================================
# 1. Check Required Environment Variables
# ==============================================================================
echo ""
echo "📋 Checking Required Environment Variables..."
echo "----------------------------------------------"

required_vars=(
  "OTEL_EXPORTER_JAEGER_ENDPOINT"
  "OTEL_SERVICE_NAME"
  "UNRDF_BASE_IRI"
  "UNRDF_STORE_PATH"
  "KGC_SIDECAR_ENDPOINT"
  "UNRDF_POLICY_DIR"
  "UNRDF_DEFAULT_POLICY"
)

for var in "${required_vars[@]}"; do
  check_required_var "$var"
done

# ==============================================================================
# 2. Check Optional Environment Variables
# ==============================================================================
echo ""
echo "📋 Checking Optional Environment Variables..."
echo "----------------------------------------------"

check_optional_var "REDIS_HOST" "localhost"
check_optional_var "REDIS_PORT" "6379"
check_optional_var "POSTGRES_HOST" "localhost"
check_optional_var "POSTGRES_PORT" "5432"
check_optional_var "CONCURRENT_QUERY_LIMIT" "10"
check_optional_var "MAX_IMPORT_SIZE_MB" "100"

# ==============================================================================
# 3. Validate Jaeger Connectivity
# ==============================================================================
echo ""
echo "🔍 Checking Jaeger Endpoint Connectivity..."
echo "----------------------------------------------"

if [ -n "$OTEL_EXPORTER_JAEGER_ENDPOINT" ]; then
  # Extract host and port from endpoint
  JAEGER_URL="$OTEL_EXPORTER_JAEGER_ENDPOINT"

  if curl -sf --connect-timeout 5 "$JAEGER_URL" > /dev/null 2>&1; then
    echo -e "${GREEN}✅ Jaeger endpoint is reachable at $JAEGER_URL${NC}"
  else
    echo -e "${YELLOW}⚠️  Warning: Cannot connect to Jaeger at $JAEGER_URL${NC}"
    echo -e "${YELLOW}   This is not critical - OTEL will buffer spans${NC}"
  fi
fi

# ==============================================================================
# 4. Validate Redis Connectivity (Optional)
# ==============================================================================
echo ""
echo "🔍 Checking Redis Connectivity (Optional)..."
echo "----------------------------------------------"

if command -v redis-cli &> /dev/null; then
  if redis-cli -h "${REDIS_HOST:-localhost}" -p "${REDIS_PORT:-6379}" ping > /dev/null 2>&1; then
    echo -e "${GREEN}✅ Redis is available at ${REDIS_HOST:-localhost}:${REDIS_PORT:-6379}${NC}"
  else
    echo -e "${YELLOW}⚠️  Redis is not available (cache will be disabled)${NC}"
  fi
else
  echo -e "${YELLOW}⚠️  redis-cli not installed (cannot test Redis connectivity)${NC}"
fi

# ==============================================================================
# 5. Validate PostgreSQL Connectivity (Optional)
# ==============================================================================
echo ""
echo "🔍 Checking PostgreSQL Connectivity (Optional)..."
echo "----------------------------------------------"

if command -v psql &> /dev/null; then
  PGPASSWORD="${POSTGRES_PASSWORD}" psql \
    -h "${POSTGRES_HOST:-localhost}" \
    -p "${POSTGRES_PORT:-5432}" \
    -U "${POSTGRES_USER:-postgres}" \
    -d "${POSTGRES_DB:-unrdf}" \
    -c "SELECT 1;" > /dev/null 2>&1 && \
    echo -e "${GREEN}✅ PostgreSQL is available at ${POSTGRES_HOST:-localhost}:${POSTGRES_PORT:-5432}${NC}" || \
    echo -e "${YELLOW}⚠️  PostgreSQL is not available (lockchain will be disabled)${NC}"
else
  echo -e "${YELLOW}⚠️  psql not installed (cannot test PostgreSQL connectivity)${NC}"
fi

# ==============================================================================
# 6. Validate Directory Structure
# ==============================================================================
echo ""
echo "🔍 Checking Directory Structure..."
echo "----------------------------------------------"

directories=(
  ".unrdf/hooks"
  ".unrdf/policies"
  ".unrdf/cache"
  ".unrdf/logs"
  "${UNRDF_POLICY_DIR:-.unrdf/policies}"
)

for dir in "${directories[@]}"; do
  mkdir -p "$dir"
  if [ -d "$dir" ]; then
    echo -e "${GREEN}✅ Directory exists: $dir${NC}"
  else
    echo -e "${RED}❌ Failed to create directory: $dir${NC}"
    VALIDATION_FAILED=1
  fi
done

# ==============================================================================
# 7. Validate File Permissions
# ==============================================================================
echo ""
echo "🔍 Checking File Permissions..."
echo "----------------------------------------------"

if [ -w ".unrdf" ]; then
  echo -e "${GREEN}✅ .unrdf directory is writable${NC}"
else
  echo -e "${RED}❌ .unrdf directory is not writable${NC}"
  VALIDATION_FAILED=1
fi

# ==============================================================================
# 8. Validate Node.js Version
# ==============================================================================
echo ""
echo "🔍 Checking Node.js Version..."
echo "----------------------------------------------"

NODE_VERSION=$(node -v | cut -d'v' -f2 | cut -d'.' -f1)
REQUIRED_NODE_VERSION=18

if [ "$NODE_VERSION" -ge "$REQUIRED_NODE_VERSION" ]; then
  echo -e "${GREEN}✅ Node.js version $(node -v) is compatible (>= v${REQUIRED_NODE_VERSION})${NC}"
else
  echo -e "${RED}❌ Node.js version $(node -v) is too old (requires >= v${REQUIRED_NODE_VERSION})${NC}"
  VALIDATION_FAILED=1
fi

# ==============================================================================
# 9. Check KGC Sidecar Endpoint
# ==============================================================================
echo ""
echo "🔍 Checking KGC Sidecar Endpoint..."
echo "----------------------------------------------"

if [ -n "$KGC_SIDECAR_ENDPOINT" ]; then
  # Extract host and port
  KGC_HOST=$(echo "$KGC_SIDECAR_ENDPOINT" | cut -d':' -f1)
  KGC_PORT=$(echo "$KGC_SIDECAR_ENDPOINT" | cut -d':' -f2)

  if timeout 2 bash -c "cat < /dev/null > /dev/tcp/${KGC_HOST}/${KGC_PORT}" 2>/dev/null; then
    echo -e "${GREEN}✅ KGC Sidecar is reachable at $KGC_SIDECAR_ENDPOINT${NC}"
  else
    echo -e "${YELLOW}⚠️  Warning: Cannot connect to KGC Sidecar at $KGC_SIDECAR_ENDPOINT${NC}"
    echo -e "${YELLOW}   KGC features will be unavailable${NC}"
  fi
fi

# ==============================================================================
# 10. Validate Configuration Values
# ==============================================================================
echo ""
echo "🔍 Validating Configuration Values..."
echo "----------------------------------------------"

# Validate CONCURRENT_QUERY_LIMIT
if [ -n "$CONCURRENT_QUERY_LIMIT" ] && [ "$CONCURRENT_QUERY_LIMIT" -gt 0 ] 2>/dev/null; then
  echo -e "${GREEN}✅ CONCURRENT_QUERY_LIMIT=$CONCURRENT_QUERY_LIMIT is valid${NC}"
else
  echo -e "${RED}❌ CONCURRENT_QUERY_LIMIT must be a positive integer${NC}"
  VALIDATION_FAILED=1
fi

# Validate MAX_IMPORT_SIZE_MB
if [ -n "$MAX_IMPORT_SIZE_MB" ] && [ "$MAX_IMPORT_SIZE_MB" -gt 0 ] 2>/dev/null; then
  echo -e "${GREEN}✅ MAX_IMPORT_SIZE_MB=$MAX_IMPORT_SIZE_MB is valid${NC}"
else
  echo -e "${RED}❌ MAX_IMPORT_SIZE_MB must be a positive integer${NC}"
  VALIDATION_FAILED=1
fi

# ==============================================================================
# Final Summary
# ==============================================================================
echo ""
echo "================================================"
if [ $VALIDATION_FAILED -eq 0 ]; then
  echo -e "${GREEN}✅ Environment validation complete - ALL CHECKS PASSED${NC}"
  echo ""
  echo "Your UNRDF production environment is ready!"
  exit 0
else
  echo -e "${RED}❌ Environment validation FAILED${NC}"
  echo ""
  echo "Please fix the issues above before proceeding to production."
  exit 1
fi
