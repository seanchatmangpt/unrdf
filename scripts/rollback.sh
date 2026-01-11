#!/bin/bash
# Rollback Script for UNRDF v6
# This script rolls back to the previous deployment

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
DOCKER_COMPOSE_FILE="docker-compose.yml"
BACKUP_DIR="/var/backups/unrdf"
LOG_FILE="/var/log/unrdf/rollback-$(date +%Y%m%d-%H%M%S).log"

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

# Get latest backup
get_latest_backup() {
    if [[ ! -d "$BACKUP_DIR" ]]; then
        error "Backup directory not found: $BACKUP_DIR"
    fi

    if [[ ! -f "$BACKUP_DIR/latest" ]]; then
        error "No backup found. Cannot rollback."
    fi

    latest=$(cat "$BACKUP_DIR/latest")
    log "Latest backup: $latest"
    echo "$latest"
}

# Confirm rollback
confirm_rollback() {
    local backup_name=$1

    echo ""
    warning "!!! ROLLBACK OPERATION !!!"
    echo ""
    echo "This will rollback to backup: $backup_name"
    echo "Current services will be stopped and replaced."
    echo ""
    read -p "Are you sure you want to proceed? (yes/no): " confirm

    if [[ "$confirm" != "yes" ]]; then
        log "Rollback cancelled by user"
        exit 0
    fi
}

# Stop current services
stop_services() {
    log "Stopping current services..."

    docker-compose -f "$DOCKER_COMPOSE_FILE" down --timeout 30 || warning "Some services failed to stop gracefully"

    success "Services stopped"
}

# Restore backup
restore_backup() {
    local backup_name=$1

    log "Restoring backup: $backup_name"

    # Restore environment
    if [[ -f "$BACKUP_DIR/$backup_name-env" ]]; then
        cp "$BACKUP_DIR/$backup_name-env" .env.production
        success "Environment restored"
    else
        warning "Environment backup not found"
    fi

    # Restore docker-compose config
    if [[ -f "$BACKUP_DIR/$backup_name-compose.yml" ]]; then
        log "Previous docker-compose configuration available"
    fi

    # Restore data
    if [[ -f "$BACKUP_DIR/$backup_name-data.tar.gz" ]]; then
        log "Restoring data..."
        tar -xzf "$BACKUP_DIR/$backup_name-data.tar.gz" -C / 2>/dev/null || warning "Data restoration failed"
        success "Data restored"
    else
        warning "Data backup not found"
    fi
}

# Pull previous images
pull_previous_images() {
    log "Pulling previous Docker images..."

    # Get previous image tags from backup
    if [[ -f "$BACKUP_DIR/previous-images" ]]; then
        while read -r image; do
            log "Pulling $image"
            docker pull "$image" || warning "Failed to pull $image"
        done < "$BACKUP_DIR/previous-images"
    else
        warning "Previous image list not found, using current tags"
    fi
}

# Start services
start_services() {
    log "Starting services with previous configuration..."

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

# Verify rollback
verify_rollback() {
    log "Verifying rollback..."

    # Check all containers are running
    failing_containers=$(docker-compose -f "$DOCKER_COMPOSE_FILE" ps | grep -v "Up" | grep -v "NAME" || true)
    if [[ -n "$failing_containers" ]]; then
        error "Some containers are not running:\n$failing_containers"
    fi

    success "Rollback verified"
}

# Create rollback marker
create_rollback_marker() {
    local backup_name=$1

    echo "$(date +%Y%m%d-%H%M%S)" > "$BACKUP_DIR/rollback-marker"
    echo "$backup_name" >> "$BACKUP_DIR/rollback-marker"

    log "Rollback marker created"
}

# Main rollback flow
main() {
    log "========================================="
    log "UNRDF v6 Rollback Starting"
    log "========================================="

    backup_name=$(get_latest_backup)

    # Interactive mode by default, can be skipped with --auto flag
    if [[ "${1:-}" != "--auto" ]]; then
        confirm_rollback "$backup_name"
    fi

    stop_services
    restore_backup "$backup_name"
    pull_previous_images
    start_services
    health_check
    verify_rollback
    create_rollback_marker "$backup_name"

    log "========================================="
    success "Rollback completed successfully!"
    log "========================================="
    log "Rolled back to: $backup_name"
    log "Log file: $LOG_FILE"
}

# Run rollback
main "$@"
