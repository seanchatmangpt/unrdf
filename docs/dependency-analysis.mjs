import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const rootDir = path.join(__dirname, '..');

const packages = [
  'atomvm', 'cli', 'composables', 'core', 'dark-matter', 'docs', 'domain',
  'engine-gateway', 'federation', 'hooks', 'kgc-4d', 'kgn', 'knowledge-engine',
  'nextra', 'oxigraph', 'project-engine', 'streaming', 'test-utils', 'validation'
];

const allDeps = {};
const depsByPackage = {};

packages.forEach(pkg => {
  const pkgPath = path.join(rootDir, 'packages', pkg, 'package.json');
  if (!fs.existsSync(pkgPath)) return;

  const data = JSON.parse(fs.readFileSync(pkgPath, 'utf8'));
  const deps = { ...data.dependencies, ...data.devDependencies };

  depsByPackage[pkg] = deps;

  Object.entries(deps).forEach(([name, version]) => {
    if (name.startsWith('@unrdf/') || version === 'workspace:*') return;
    if (!allDeps[name]) allDeps[name] = {};
    if (!allDeps[name][version]) allDeps[name][version] = [];
    allDeps[name][version].push(pkg);
  });
});

// Sort by number of packages using each dependency
const sorted = Object.entries(allDeps)
  .map(([name, versions]) => ({
    name,
    versions: Object.keys(versions).length,
    usedBy: Object.values(versions).flat().length,
    versionMap: versions
  }))
  .sort((a, b) => b.usedBy - a.usedBy)
  .slice(0, 40);

console.log('TOP 40 SHARED DEPENDENCIES:\n');
sorted.forEach(dep => {
  console.log(`${dep.name}: ${dep.usedBy} packages, ${dep.versions} version(s)`);
  Object.entries(dep.versionMap).forEach(([ver, pkgs]) => {
    console.log(`  ${ver} → [${pkgs.join(', ')}]`);
  });
  console.log('');
});

// Find version conflicts
console.log('\n=== VERSION CONFLICTS ===\n');
const conflicts = sorted.filter(d => d.versions > 1);
conflicts.forEach(dep => {
  console.log(`⚠️  ${dep.name} has ${dep.versions} different versions:`);
  Object.entries(dep.versionMap).forEach(([ver, pkgs]) => {
    console.log(`  ${ver} in: ${pkgs.join(', ')}`);
  });
  console.log('');
});

// Create CSV
const csvLines = [
  'Package Name,Dependency,Current Version,Recommended Version,Action'
];

Object.entries(allDeps).forEach(([name, versions]) => {
  const versionList = Object.keys(versions);
  const recommended = versionList.length > 1
    ? versionList.sort((a, b) => b.localeCompare(a, undefined, { numeric: true }))[0]
    : versionList[0];

  Object.entries(versions).forEach(([ver, pkgs]) => {
    pkgs.forEach(pkg => {
      const action = versionList.length > 1 && ver !== recommended
        ? `Update to ${recommended}`
        : 'OK';
      csvLines.push(`${pkg},${name},${ver},${recommended},${action}`);
    });
  });
});

fs.writeFileSync(
  path.join(rootDir, 'docs', 'DEPENDENCY-ALIGNMENT.csv'),
  csvLines.join('\n')
);

console.log(`\n✅ CSV written to docs/DEPENDENCY-ALIGNMENT.csv (${csvLines.length} rows)`);
