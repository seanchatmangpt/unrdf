import fs from 'fs';
import path from 'path';

const NEW_VERSION = '26.4.23';
const INTERNAL_PREFIX = '@unrdf/';

const files = [
  './test/narrative-state-chain/package.json',
  './playground/hooks-showcase/package.json',
  './playground/smoke-test/package.json',
  './playground/package.json',
  './playground/papers-thesis-cli/package.json',
  './docs/templates/package-template/package.json',
  './exploration/agents/agent-4/package.json',
  './codegen/generated/package.json',
  './package.json',
  './examples/graphql-gateway/package.json',
  './examples/graph-analytics/package.json',
  './examples/blockchain-audit/package.json',
  './examples/package.json',
  './examples/knowledge-rag/package.json',
  './examples/distributed-orchestration/package.json',
  './benchmarks/package.json',
  './scripts/package.json',
  './packages/test-utils/package.json',
  './packages/core/package.json',
  './packages/core/examples/basic-store/package.json',
  './packages/core/examples/sparql-queries/package.json',
  './packages/core/examples/rdf-parsing/package.json',
  './packages/pictl-algorithms/package.json',
  './packages/dark-matter/package.json',
  './packages/otel/package.json',
  './packages/oxigraph/package.json',
  './packages/docs/package.json',
  './packages/cli/package.json',
  './packages/pictl-semantics/package.json',
  './packages/manufacturing/package.json',
  './packages/streaming/package.json',
  './packages/daemon/package.json',
  './packages/hooks/package.json',
  './packages/hooks/examples/hook-chains/package.json',
  './packages/hooks/examples/policy-hooks/package.json',
  './packages/federation/package.json',
  './packages/kgc-4d/playground/package.json',
  './packages/kgc-4d/package.json',
  './packages/v6-core/package.json',
  './sidecar/package.json',
  './apps/docs-site/package.json',
  './src/receipts/package.json',
  './src/package.json',
  './src/projection/package.json'
];

files.forEach(file => {
  const filePath = path.resolve(process.cwd(), file);
  if (!fs.existsSync(filePath)) {
    console.warn(`File not found: ${file}`);
    return;
  }

  const content = fs.readFileSync(filePath, 'utf8');
  let pkg;
  try {
    pkg = JSON.parse(content);
  } catch (e) {
    console.error(`Error parsing ${file}: ${e.message}`);
    return;
  }

  let changed = false;

  if (pkg.version) {
    pkg.version = NEW_VERSION;
    changed = true;
  }

  const updateDeps = (deps) => {
    if (!deps) return;
    Object.keys(deps).forEach(dep => {
      if (dep.startsWith(INTERNAL_PREFIX)) {
        // We want to keep the prefix if it exists (e.g. ^, ~, etc)
        const currentVersion = deps[dep];
        const prefixMatch = currentVersion.match(/^[\^~]/);
        const prefix = prefixMatch ? prefixMatch[0] : '';
        deps[dep] = prefix + NEW_VERSION;
        changed = true;
      }
    });
  };

  updateDeps(pkg.dependencies);
  updateDeps(pkg.devDependencies);
  updateDeps(pkg.peerDependencies);
  updateDeps(pkg.optionalDependencies);

  if (changed) {
    fs.writeFileSync(filePath, JSON.stringify(pkg, null, 2) + '\n');
    console.log(`Updated ${file}`);
  }
});
