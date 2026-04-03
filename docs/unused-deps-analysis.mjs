import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { glob } from 'glob';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const rootDir = path.join(__dirname, '..');

const packages = [
  'atomvm', 'cli', 'composables', 'core', 'dark-matter', 'docs', 'domain',
  'engine-gateway', 'federation', 'hooks', 'kgc-4d', 'kgn', 'knowledge-engine',
  'nextra', 'oxigraph', 'project-engine', 'streaming', 'test-utils', 'validation'
];

const unusedDeps = [];
const importPatterns = [
  /import\s+(?:{[^}]+}|[\w*]+)\s+from\s+['"]([^'"]+)['"]/g,
  /require\s*\(\s*['"]([^'"]+)['"]\s*\)/g,
  /import\s*\(\s*['"]([^'"]+)['"]\s*\)/g
];

async function analyzePackage(pkg) {
  const pkgPath = path.join(rootDir, 'packages', pkg, 'package.json');
  if (!fs.existsSync(pkgPath)) return;

  const pkgData = JSON.parse(fs.readFileSync(pkgPath, 'utf8'));
  const allDeps = {
    ...pkgData.dependencies,
    ...pkgData.devDependencies
  };

  // Skip workspace dependencies
  const externalDeps = Object.keys(allDeps).filter(
    dep => !dep.startsWith('@unrdf/') && allDeps[dep] !== 'workspace:*'
  );

  // Find all source files
  const srcDir = path.join(rootDir, 'packages', pkg, 'src');
  const testDir = path.join(rootDir, 'packages', pkg, 'test');
  const examplesDir = path.join(rootDir, 'packages', pkg, 'examples');

  const sourceFiles = [];
  for (const dir of [srcDir, testDir, examplesDir]) {
    if (fs.existsSync(dir)) {
      const files = await glob(`${dir}/**/*.{js,mjs,ts,mts,jsx,tsx}`, { absolute: true });
      sourceFiles.push(...files);
    }
  }

  // Extract all imports
  const allImports = new Set();
  for (const file of sourceFiles) {
    const content = fs.readFileSync(file, 'utf8');
    for (const pattern of importPatterns) {
      const matches = [...content.matchAll(pattern)];
      matches.forEach(match => {
        const importPath = match[1];
        // Extract package name (handle scoped packages)
        const pkgName = importPath.startsWith('@')
          ? importPath.split('/').slice(0, 2).join('/')
          : importPath.split('/')[0];
        allImports.add(pkgName);
      });
    }
  }

  // Find unused dependencies
  const unused = externalDeps.filter(dep => !allImports.has(dep));

  if (unused.length > 0) {
    unusedDeps.push({
      package: pkg,
      unused,
      total: externalDeps.length,
      scanned: sourceFiles.length
    });
  }

  return {
    package: pkg,
    deps: externalDeps,
    imports: Array.from(allImports),
    unused
  };
}

console.log('Analyzing imports across all packages...\n');

const results = await Promise.all(packages.map(analyzePackage));

console.log('=== UNUSED DEPENDENCIES REPORT ===\n');

if (unusedDeps.length === 0) {
  console.log('✅ No unused dependencies detected!');
} else {
  unusedDeps.forEach(({ package: pkg, unused, total, scanned }) => {
    console.log(`📦 ${pkg} (scanned ${scanned} files):`);
    console.log(`   Declared: ${total} dependencies`);
    console.log(`   Unused: ${unused.length}`);
    unused.forEach(dep => console.log(`     - ${dep}`));
    console.log('');
  });

  // Write detailed report
  const reportLines = [
    '# Unused Dependencies Report',
    '',
    `Generated: ${new Date().toISOString()}`,
    '',
    '## Summary',
    '',
    `- Total packages scanned: ${packages.length}`,
    `- Packages with unused deps: ${unusedDeps.length}`,
    `- Total unused dependencies: ${unusedDeps.reduce((sum, p) => sum + p.unused.length, 0)}`,
    '',
    '## Details',
    ''
  ];

  unusedDeps.forEach(({ package: pkg, unused, total, scanned }) => {
    reportLines.push(`### ${pkg}`);
    reportLines.push('');
    reportLines.push(`- Files scanned: ${scanned}`);
    reportLines.push(`- Total dependencies: ${total}`);
    reportLines.push(`- Unused: ${unused.length}`);
    reportLines.push('');
    reportLines.push('**Unused dependencies:**');
    unused.forEach(dep => reportLines.push(`- \`${dep}\``));
    reportLines.push('');
  });

  reportLines.push('## Recommendations');
  reportLines.push('');
  reportLines.push('1. Review each unused dependency to confirm it\'s truly unused');
  reportLines.push('2. Check if dependencies are used in build configs or scripts');
  reportLines.push('3. Remove confirmed unused dependencies with `pnpm remove <dep>`');
  reportLines.push('4. Update pnpm-lock.yaml with `pnpm install`');

  fs.writeFileSync(
    path.join(rootDir, 'docs', 'UNUSED-DEPENDENCIES.md'),
    reportLines.join('\n')
  );

  console.log(`\n✅ Detailed report written to docs/UNUSED-DEPENDENCIES.md`);
}

// Create summary CSV
const csvLines = ['Package,Dependency,Status,Recommendation'];
results.forEach(({ package: pkg, deps, unused }) => {
  deps.forEach(dep => {
    const status = unused.includes(dep) ? 'UNUSED' : 'USED';
    const rec = unused.includes(dep) ? 'Consider removing' : 'Keep';
    csvLines.push(`${pkg},${dep},${status},${rec}`);
  });
});

fs.writeFileSync(
  path.join(rootDir, 'docs', 'DEPENDENCY-USAGE.csv'),
  csvLines.join('\n')
);

console.log(`✅ Usage CSV written to docs/DEPENDENCY-USAGE.csv (${csvLines.length} rows)`);
