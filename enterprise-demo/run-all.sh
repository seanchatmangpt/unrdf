#!/usr/bin/env bash
set -euxo pipefail

# Clean lockfile to avoid stale git+ssh deps
rm -f pnpm-lock.yaml

echo "Installing dependencies..."
pnpm install

echo "Running all unit, performance, and telemetry tests..."
pnpm test

echo "All tests passed!"