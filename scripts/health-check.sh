#!/bin/bash
# Health Check Script for UNRDF v6
# This script performs comprehensive health checks on the deployment

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
BASE_URL="${1:-http://localhost}"
HEALTH_PORT="${HEALTH_CHECK_PORT:-8080}"
API_PORT="${API_PORT:-3000}"
METRICS_PORT="${PROMETHEUS_PORT:-9090}"
MAX_ATTEMPTS="${MAX_ATTEMPTS:-30}"
RETRY_INTERVAL="${RETRY_INTERVAL:-10}"

# Functions
log() {
    echo -e "${BLUE}[$(date +'%Y-%m-%d %H:%M:%S')]${NC} $1"
}

error() {
    echo -e "${RED}[FAIL]${NC} $1"
}

success() {
    echo -e "${GREEN}[PASS]${NC} $1"
}

warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

# Check HTTP endpoint
check_endpoint() {
    local url=$1
    local name=$2
    local max_attempts=${3:-$MAX_ATTEMPTS}
    local interval=${4:-$RETRY_INTERVAL}

    log "Checking $name..."

    for ((attempt=1; attempt<=max_attempts; attempt++)); do
        if curl -f -s -o /dev/null -w "%{http_code}" "$url" | grep -q "^2"; then
            success "$name is healthy (attempt $attempt/$max_attempts)"
            return 0
        fi

        if [[ $attempt -lt $max_attempts ]]; then
            log "Attempt $attempt/$max_attempts failed, retrying in ${interval}s..."
            sleep "$interval"
        fi
    done

    error "$name is not responding after $max_attempts attempts"
    return 1
}

# Check health endpoint
check_health() {
    local url="$BASE_URL:$HEALTH_PORT/health"
    check_endpoint "$url" "Health endpoint" || return 1

    # Parse health response
    response=$(curl -s "$url")
    status=$(echo "$response" | grep -o '"status":"[^"]*"' | cut -d'"' -f4 || echo "unknown")

    if [[ "$status" == "ok" ]] || [[ "$status" == "healthy" ]]; then
        success "Health status: $status"
    else
        warning "Health status: $status"
    fi
}

# Check API endpoint
check_api() {
    local url="$BASE_URL:$API_PORT"
    check_endpoint "$url" "API endpoint" 10 5 || warning "API endpoint check failed"
}

# Check metrics endpoint
check_metrics() {
    local url="$BASE_URL:$METRICS_PORT/metrics"

    if curl -f -s "$url" > /dev/null 2>&1; then
        success "Metrics endpoint is accessible"

        # Check for specific metrics
        metrics=$(curl -s "$url")

        if echo "$metrics" | grep -q "unrdf_"; then
            success "UNRDF metrics are being exported"
        else
            warning "UNRDF metrics not found"
        fi
    else
        warning "Metrics endpoint is not accessible"
    fi
}

# Check Docker containers
check_containers() {
    if ! command -v docker &> /dev/null; then
        warning "Docker not available, skipping container checks"
        return 0
    fi

    log "Checking Docker containers..."

    # Check UNRDF app
    if docker ps | grep -q unrdf-app; then
        success "UNRDF container is running"

        # Check container health
        health=$(docker inspect --format='{{.State.Health.Status}}' unrdf-app 2>/dev/null || echo "none")
        if [[ "$health" == "healthy" ]]; then
            success "Container health status: healthy"
        elif [[ "$health" == "none" ]]; then
            warning "Container has no health check"
        else
            warning "Container health status: $health"
        fi
    else
        error "UNRDF container is not running"
        return 1
    fi

    # Check Prometheus
    if docker ps | grep -q unrdf-prometheus; then
        success "Prometheus container is running"
    else
        warning "Prometheus container is not running"
    fi

    # Check Grafana
    if docker ps | grep -q unrdf-grafana; then
        success "Grafana container is running"
    else
        warning "Grafana container is not running"
    fi
}

# Check system resources
check_resources() {
    log "Checking system resources..."

    # Check disk space
    disk_usage=$(df -h / | awk 'NR==2 {print $5}' | sed 's/%//')
    if [[ $disk_usage -lt 80 ]]; then
        success "Disk usage: ${disk_usage}%"
    else
        warning "Disk usage: ${disk_usage}% (high)"
    fi

    # Check memory
    if command -v free &> /dev/null; then
        mem_usage=$(free | awk 'NR==2 {printf "%.0f", $3/$2 * 100}')
        if [[ $mem_usage -lt 80 ]]; then
            success "Memory usage: ${mem_usage}%"
        else
            warning "Memory usage: ${mem_usage}% (high)"
        fi
    fi

    # Check CPU load
    if command -v uptime &> /dev/null; then
        load=$(uptime | awk -F'load average:' '{print $2}' | awk '{print $1}' | sed 's/,//')
        success "CPU load average (1m): $load"
    fi
}

# Check database
check_database() {
    log "Checking database..."

    # Check if database file exists (for file-based stores)
    if [[ -f "/app/data/unrdf.db" ]]; then
        db_size=$(du -h /app/data/unrdf.db | awk '{print $1}')
        success "Database file exists (size: $db_size)"
    else
        warning "Database file not found (may be using memory store)"
    fi
}

# Check logs for errors
check_logs() {
    if ! command -v docker &> /dev/null; then
        return 0
    fi

    log "Checking recent logs for errors..."

    # Check last 100 lines of logs
    error_count=$(docker logs unrdf-app --tail 100 2>&1 | grep -i "error\|fatal\|exception" | wc -l || echo 0)

    if [[ $error_count -eq 0 ]]; then
        success "No errors found in recent logs"
    else
        warning "Found $error_count error(s) in recent logs"
    fi
}

# Generate report
generate_report() {
    local all_checks_passed=$1

    echo ""
    echo "========================================="
    echo "Health Check Summary"
    echo "========================================="
    echo "Timestamp: $(date)"
    echo "Base URL: $BASE_URL"
    echo "Status: $([ $all_checks_passed -eq 0 ] && echo 'HEALTHY' || echo 'UNHEALTHY')"
    echo "========================================="
}

# Main health check flow
main() {
    log "========================================="
    log "UNRDF v6 Health Check"
    log "========================================="

    checks_passed=0

    # Run all checks
    check_health || checks_passed=1
    check_api || checks_passed=1
    check_metrics || checks_passed=1
    check_containers || checks_passed=1
    check_resources || checks_passed=1
    check_database || checks_passed=1
    check_logs || checks_passed=1

    generate_report $checks_passed

    exit $checks_passed
}

# Run health check
main "$@"
