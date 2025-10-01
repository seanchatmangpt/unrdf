#!/usr/bin/env bash
set -e
cd "$(dirname "$0")/../.."
echo "Installing dependencies..."
pnpm install
echo "Running demo..."
pnpm start:demo
echo "Demo complete. Output:" 
cat out/demo.md