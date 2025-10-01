#!/bin/bash

# Stop E2E test services

set -e

echo "Stopping E2E test services..."

# Stop services
docker-compose -f test/e2e/docker-compose.yml down

echo "Services stopped"
