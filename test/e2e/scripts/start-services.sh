#!/bin/bash

# Start E2E test services using Docker Compose

set -e

echo "Starting E2E test services..."

# Check if Docker is running
if ! docker info > /dev/null 2>&1; then
    echo "Error: Docker is not running. Please start Docker and try again."
    exit 1
fi

# Start services
docker-compose -f test/e2e/docker-compose.yml up -d

# Wait for services to be healthy
echo "Waiting for services to be healthy..."

# Wait for PostgreSQL
echo "Waiting for PostgreSQL..."
until docker-compose -f test/e2e/docker-compose.yml exec -T postgres pg_isready -U testuser -d unrdf_test > /dev/null 2>&1; do
    sleep 1
done
echo "PostgreSQL is ready"

# Wait for Redis
echo "Waiting for Redis..."
until docker-compose -f test/e2e/docker-compose.yml exec -T redis redis-cli ping > /dev/null 2>&1; do
    sleep 1
done
echo "Redis is ready"

# Wait for MinIO
echo "Waiting for MinIO..."
until curl -f http://localhost:9000/minio/health/live > /dev/null 2>&1; do
    sleep 1
done
echo "MinIO is ready"

# Wait for Fuseki
echo "Waiting for Fuseki..."
until curl -f http://localhost:3030/$/ping > /dev/null 2>&1; do
    sleep 1
done
echo "Fuseki is ready"

echo "All services are ready!"
echo ""
echo "Service endpoints:"
echo "  PostgreSQL: postgresql://testuser:testpass@localhost:5432/unrdf_test"
echo "  Redis: redis://localhost:6379"
echo "  MinIO: http://localhost:9000 (admin/minioadmin)"
echo "  MinIO Console: http://localhost:9001"
echo "  Fuseki: http://localhost:3030"
echo "  Fuseki SPARQL: http://localhost:3030/unrdf-test/sparql"
echo ""
echo "To stop services, run: docker-compose -f test/e2e/docker-compose.yml down"




