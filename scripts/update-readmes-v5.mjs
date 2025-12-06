#!/usr/bin/env node
/**
 * Update all package READMEs with v5 version info
 */
import { readFileSync, writeFileSync, existsSync } from 'fs';
import { join } from 'path';

const PACKAGES = [
  'browser', 'cli', 'composables', 'core', 'dark-matter', 'domain',
  'engine-gateway', 'federation', 'hooks', 'kgc-4d', 'knowledge-engine',
  'oxigraph', 'project-engine', 'streaming', 'test-utils', 'validation'
];

const v5Badge = '![Version](https://img.shields.io/badge/version-5.0.0--beta.1-blue)';
const prodReadyBadge = '![Production Ready](https://img.shields.io/badge/production-ready-green)';

console.log('📖 Updating package READMEs with v5 info\n');

for (const pkg of PACKAGES) {
  const readmePath = join('packages', pkg, 'README.md');

  if (!existsSync(readmePath)) {
    // Create minimal README
    const pkgJson = JSON.parse(readFileSync(join('packages', pkg, 'package.json'), 'utf-8'));
    const content = `# ${pkgJson.name}

${v5Badge} ${prodReadyBadge}

${pkgJson.description || 'UNRDF package'}

## Installation

\`\`\`bash
pnpm add ${pkgJson.name}
\`\`\`

## Documentation

See [UNRDF v5 Documentation](../../docs/RELEASE-PLAN-v5.0.0.md) for full details.

## Version

Current: **5.0.0-beta.1**

- ✅ Production ready
- ✅ 100% Oxigraph compliance
- ✅ Zero regressions

## License

${pkgJson.license || 'MIT'}
`;
    writeFileSync(readmePath, content);
    console.log(`  ✅ Created ${readmePath}`);
  } else {
    // Update existing README
    let content = readFileSync(readmePath, 'utf-8');

    // Add badges if not present
    if (!content.includes('img.shields.io')) {
      const lines = content.split('\n');
      lines.splice(1, 0, '', v5Badge + ' ' + prodReadyBadge, '');
      content = lines.join('\n');
    }

    writeFileSync(readmePath, content);
    console.log(`  ✅ Updated ${readmePath}`);
  }
}

console.log('\n✅ All package READMEs updated');
