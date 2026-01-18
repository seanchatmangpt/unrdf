#!/bin/bash
# Production Deployment Script for UNRDF v6
# This script deploys the application to production environment

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
DEPLOYMENT_NAME="unrdf-production"
DOCKER_COMPOSE_FILE="docker-compose.yml"
ENV_FILE=".env.production"
BACKUP_DIR="/var/backups/unrdf"
LOG_FILE="/var/log/unrdf/deployment-$(date +%Y%m%d-%H%M%S).log"

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

    # Check if running as appropriate user
    if [[ $EUID -eq 0 ]]; then
        warning "Running as root is not recommended"
    fi

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

    # Check disk space (require at least 10GB free)
    available_space=$(df -BG / | awk 'NR==2 {print $4}' | sed 's/G//')
    if [[ $available_space -lt 10 ]]; then
        error "Insufficient disk space. Required: 10GB, Available: ${available_space}GB"
    fi

    success "Pre-deployment checks passed"
}

# Create backup
create_backup() {
    log "Creating backup..."

    mkdir -p "$BACKUP_DIR"
    timestamp=$(date +%Y%m%d-%H%M%S)
    backup_name="unrdf-backup-$timestamp"

    # Backup database
    if [[ -d "/app/data" ]]; then
        tar -czf "$BACKUP_DIR/$backup_name-data.tar.gz" /app/data 2>/dev/null || true
    fi

    # Backup configuration
    cp "$ENV_FILE" "$BACKUP_DIR/$backup_name-env" 2>/dev/null || true

    # Save current docker-compose state
    docker-compose -f "$DOCKER_COMPOSE_FILE" config > "$BACKUP_DIR/$backup_name-compose.yml" 2>/dev/null || true

    echo "$timestamp" > "$BACKUP_DIR/latest"

    success "Backup created: $backup_name"
}

# Pull latest images
pull_images() {
    log "Pulling latest Docker images..."

    if [[ -n "${IMAGE_TAG:-}" ]]; then
        log "Using image tag: $IMAGE_TAG"
        export IMAGE_TAG
    fi

    docker-compose -f "$DOCKER_COMPOSE_FILE" pull || error "Failed to pull images"

    success "Images pulled successfully"
}

# Stop services gracefully
stop_services() {
    log "Stopping services gracefully..."

    # Send termination signal
    docker-compose -f "$DOCKER_COMPOSE_FILE" down --timeout 30 || warning "Failed to stop some services"

    # Wait for containers to stop
    sleep 5

    success "Services stopped"
}

# Start services
start_services() {
    log "Starting services..."

    docker-compose -f "$DOCKER_COMPOSE_FILE" up -d || error "Failed to start services"

    success "Services started"
}

# Health check
health_check() {
    log "Running health checks..."

    max_attempts=30
    attempt=0
    health_url="${HEALTH_CHECK_URL:-http://localhost:8080/health}"

    while [[ $attempt -lt $max_attempts ]]; do
        if curl -f -s "$health_url" > /dev/null 2>&1; then
            success "Health check passed"
            return 0
        fi

        attempt=$((attempt + 1))
        log "Health check attempt $attempt/$max_attempts failed, retrying in 10s..."
        sleep 10
    done

    error "Health check failed after $max_attempts attempts"
}

# Verify deployment
verify_deployment() {
    log "Verifying deployment..."

    # Check all containers are running
    failing_containers=$(docker-compose -f "$DOCKER_COMPOSE_FILE" ps | grep -v "Up" | grep -v "NAME" || true)
    if [[ -n "$failing_containers" ]]; then
        error "Some containers are not running:\n$failing_containers"
    fi

    # Check metrics endpoint
    metrics_url="${METRICS_URL:-http://localhost:9090/metrics}"
    if ! curl -f -s "$metrics_url" > /dev/null 2>&1; then
        warning "Metrics endpoint not accessible"
    fi

    # Check API endpoint
    api_url="${API_URL:-http://localhost:3000}"
    if ! curl -f -s "$api_url" > /dev/null 2>&1; then
        warning "API endpoint not accessible"
    fi

    success "Deployment verified"
}

# Post-deployment tasks
post_deployment() {
    log "Running post-deployment tasks..."

    # Tag deployment
    deployment_tag="production-$(date +%Y%m%d-%H%M%S)"
    git tag -a "$deployment_tag" -m "Production deployment" 2>/dev/null || warning "Failed to create git tag"

    # Clean old images
    docker image prune -f > /dev/null 2>&1 || warning "Failed to prune images"

    # Log deployment info
    log "Deployment information:"
    log "  - Timestamp: $(date)"
    log "  - User: $USER"
    log "  - Host: $(hostname)"
    log "  - Image: ${IMAGE_TAG:-latest}"

    success "Post-deployment tasks completed"
}

# Main deployment flow
main() {
    log "========================================="
    log "UNRDF v6 Production Deployment Starting"
    log "========================================="

    pre_deployment_checks
    create_backup
    pull_images
    stop_services
    start_services
    health_check
    verify_deployment
    post_deployment

    log "========================================="
    success "Deployment completed successfully!"
    log "========================================="
    log "Log file: $LOG_FILE"
}

# Run deployment
main "$@"
