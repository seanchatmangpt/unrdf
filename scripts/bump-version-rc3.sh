#!/bin/bash
# Bump all packages to v6.0.0-rc.3

set -e

NEW_VERSION="6.0.0-rc.3"

echo "Bumping root package to $NEW_VERSION..."
cd /home/user/unrdf
npm version $NEW_VERSION --no-git-tag-version

echo "Bumping all workspace packages to $NEW_VERSION..."

# Essential Tier (7 packages)
PACKAGES=(
  "core"
  "oxigraph"
  "kgc-4d"
  "yawl"
  "hooks"
  "streaming"
  "v6-core"
  # Extended Tier (8 packages)
  "federation"
  "knowledge-engine"
  "cli"
  "kgc-runtime"
  "kgc-substrate"
  "receipts"
  "consensus"
  "v6-compat"
  # Additional packages
  "kgc-claude"
  "kgc-cli"
  "kgc-docs"
  "kgc-multiverse"
  "kgc-probe"
  "kgc-runtime"
  "kgc-substrate"
  "kgc-swarm"
  "kgc-tools"
  "yawl-ai"
  "yawl-api"
  "yawl-durable"
  "yawl-kafka"
  "yawl-langchain"
  "yawl-observability"
  "yawl-queue"
  "yawl-realtime"
  "yawl-viz"
  "test-utils"
  "validation"
  "daemon"
  "event-automation"
  "observability"
  "domain"
  "project-engine"
  "dark-matter"
  "decision-fabric"
  "fusion"
  "ai-ml-innovations"
  "blockchain"
  "zkp"
  "geosparql"
  "semantic-search"
  "graph-analytics"
  "privacy"
  "serverless"
  "caching"
  "codegen"
  "collab"
  "composables"
  "engine-gateway"
  "spatial-kg"
  "temporal-discovery"
  "ml-inference"
  "ml-versioning"
  "atomvm"
  "rdf-graphql"
  "react"
  "self-healing-workflows"
  "diataxis-kit"
  "integration-tests"
  "kgn"
  "docs"
)

for pkg in "${PACKAGES[@]}"; do
  if [ -f "/home/user/unrdf/packages/$pkg/package.json" ]; then
    echo "Updating $pkg..."
    cd "/home/user/unrdf/packages/$pkg"
    npm version $NEW_VERSION --no-git-tag-version --allow-same-version 2>/dev/null || echo "  (already at $NEW_VERSION or no version field)"
  else
    echo "  Skipping $pkg (not found)"
  fi
done

echo ""
echo "✅ Version bump complete!"
echo ""
echo "Next steps:"
echo "1. Update CHANGELOG.md"
echo "2. Update RELEASE_NOTES.md"
echo "3. Run: pnpm install --lockfile-only"
echo "4. Create tag: git tag -a v6.0.0-rc.3 -m 'Release v6.0.0-rc.3'"
