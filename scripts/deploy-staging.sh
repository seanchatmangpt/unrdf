#!/bin/bash
# Staging Deployment Script for UNRDF v6
# This script deploys the application to staging environment

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
DEPLOYMENT_NAME="unrdf-staging"
DOCKER_COMPOSE_FILE="docker-compose.yml"
ENV_FILE=".env.staging"
LOG_FILE="/var/log/unrdf/staging-deployment-$(date +%Y%m%d-%H%M%S).log"

# Functions
log() {
    echo -e "${BLUE}[$(date +'%Y-%m-%d %H:%M:%S')]${NC} $1" | tee -a "$LOG_FILE"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1" | tee -a "$LOG_FILE"
    exit 1
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1" | tee -a "$LOG_FILE"
}

warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1" | tee -a "$LOG_FILE"
}

# Pre-deployment checks
pre_deployment_checks() {
    log "Running pre-deployment checks..."

    # Check required commands
    for cmd in docker docker-compose git curl; do
        if ! command -v $cmd &> /dev/null; then
            error "$cmd is not installed"
        fi
    done

    # Check environment file
    if [[ ! -f "$ENV_FILE" ]]; then
        error "Environment file $ENV_FILE not found"
    fi

    # Check Docker daemon
    if ! docker info &> /dev/null; then
        error "Docker daemon is not running"
    fi

    success "Pre-deployment checks passed"
}

# Pull latest images
pull_images() {
    log "Pulling latest Docker images..."

    if [[ -n "${IMAGE_TAG:-}" ]]; then
        log "Using image tag: $IMAGE_TAG"
        export IMAGE_TAG
    else
        export IMAGE_TAG="staging-latest"
    fi

    docker-compose -f "$DOCKER_COMPOSE_FILE" pull || error "Failed to pull images"

    success "Images pulled successfully"
}

# Deploy services
deploy_services() {
    log "Deploying services..."

    # Use staging environment
    export ENV_FILE

    # Deploy with docker-compose
    docker-compose -f "$DOCKER_COMPOSE_FILE" --env-file "$ENV_FILE" up -d --force-recreate || error "Failed to deploy services"

    success "Services deployed"
}

# Health check
health_check() {
    log "Running health checks..."

    max_attempts=20
    attempt=0
    health_url="${HEALTH_CHECK_URL:-http://localhost:8080/health}"

    while [[ $attempt -lt $max_attempts ]]; do
        if curl -f -s "$health_url" > /dev/null 2>&1; then
            success "Health check passed"
            return 0
        fi

        attempt=$((attempt + 1))
        log "Health check attempt $attempt/$max_attempts failed, retrying in 5s..."
        sleep 5
    done

    error "Health check failed after $max_attempts attempts"
}

# Run smoke tests
run_smoke_tests() {
    log "Running smoke tests..."

    base_url="${STAGING_URL:-http://localhost:3000}"

    # Test health endpoint
    if ! curl -f -s "$base_url/../health" > /dev/null 2>&1; then
        warning "Health endpoint test failed"
    else
        success "Health endpoint accessible"
    fi

    # Test metrics endpoint
    if ! curl -f -s "$base_url/../metrics" > /dev/null 2>&1; then
        warning "Metrics endpoint test failed"
    else
        success "Metrics endpoint accessible"
    fi

    # Test API endpoint
    if ! curl -f -s "$base_url" > /dev/null 2>&1; then
        warning "API endpoint test failed"
    else
        success "API endpoint accessible"
    fi
}

# Show logs
show_logs() {
    log "Recent container logs:"
    docker-compose -f "$DOCKER_COMPOSE_FILE" logs --tail=50
}

# Main deployment flow
main() {
    log "========================================="
    log "UNRDF v6 Staging Deployment Starting"
    log "========================================="

    pre_deployment_checks
    pull_images
    deploy_services
    health_check
    run_smoke_tests

    log "========================================="
    success "Staging deployment completed!"
    log "========================================="
    log "Log file: $LOG_FILE"
    log ""
    log "To view logs: docker-compose -f $DOCKER_COMPOSE_FILE logs -f"
    log "To check status: docker-compose -f $DOCKER_COMPOSE_FILE ps"
}

# Run deployment
main "$@"
