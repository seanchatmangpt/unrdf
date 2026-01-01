#!/usr/bin/env node

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const projectRoot = path.resolve(__dirname, '../..');

// Parse RDF ontology and generate TypeScript types
function parseOntology(filePath) {
  const content = fs.readFileSync(filePath, 'utf-8');
  const packages = [];

  // Extract package definitions
  const lines = content.split('\n');
  let currentPkg = null;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i].trim();

    // Start of package definition
    if (line.includes('a unrdf:Package')) {
      const pkgId = lines[i].split(/\s+/)[0];
      currentPkg = { id: pkgId };
    }

    // Extract properties
    if (currentPkg) {
      if (line.includes('unrdf:packageName')) {
        currentPkg.name = line.match(/"([^"]+)"/)[1];
      }
      if (line.includes('unrdf:version')) {
        currentPkg.version = line.match(/"([^"]+)"/)[1];
      }
      if (line.includes('unrdf:description')) {
        currentPkg.description = line.match(/"([^"]*?)"/)[1];
      }
      if (line.includes('unrdf:tier')) {
        currentPkg.tier = line.includes('EssentialTier') ? 'essential' :
                         line.includes('ExtendedTier') ? 'extended' : 'optional';
      }
      if (line.includes('unrdf:mainExport')) {
        currentPkg.mainExport = line.match(/"([^"]+)"/)[1];
      }
      if (line.includes('unrdf:testCoverage')) {
        currentPkg.testCoverage = parseFloat(line.match(/(\d+\.\d+)/)[1]);
      }
      if (line.includes('rdfs:label')) {
        currentPkg.label = line.match(/"([^"]+)"/)[1];
      }
      if (line.includes('rdfs:comment')) {
        currentPkg.comment = line.match(/"([^"]*?)"/)[1];
      }

      // End of package definition
      if (line === '.') {
        if (currentPkg.name) {
          packages.push(currentPkg);
        }
        currentPkg = null;
      }
    }
  }

  return packages;
}

// Generate TypeScript code
function generateTypeScript(packages) {
  const ts = `/**
 * Auto-generated from ggen ontology
 * Source: schemas/unrdf-packages.ttl
 * Generated: ${new Date().toISOString()}
 */

export interface Package {
  name: string;
  version: string;
  description: string;
  tier: 'essential' | 'extended' | 'optional';
  mainExport: string;
  testCoverage: number;
  label: string;
}

export interface PackageRegistry {
  packages: Package[];
  essential: Package[];
  extended: Package[];
  optional: Package[];
  total: number;
}

export const PACKAGES: Record<string, Package> = {
${packages.map(pkg => `  '${pkg.name}': {
    name: '${pkg.name}',
    version: '${pkg.version}',
    description: '${pkg.description}',
    tier: '${pkg.tier}',
    mainExport: '${pkg.mainExport}',
    testCoverage: ${pkg.testCoverage},
    label: '${pkg.label}'
  }`).join(',\n')}
};

export function getRegistry(): PackageRegistry {
  return {
    packages: Object.values(PACKAGES),
    essential: Object.values(PACKAGES).filter(p => p.tier === 'essential'),
    extended: Object.values(PACKAGES).filter(p => p.tier === 'extended'),
    optional: Object.values(PACKAGES).filter(p => p.tier === 'optional'),
    total: Object.keys(PACKAGES).length
  };
}

export function getPackage(name: string): Package | undefined {
  return PACKAGES[name];
}

export function findByTier(tier: 'essential' | 'extended' | 'optional'): Package[] {
  return Object.values(PACKAGES).filter(p => p.tier === tier);
}
`;

  return ts;
}

// Main
async function main() {
  const ontologyPath = path.join(projectRoot, 'schemas', 'unrdf-packages.ttl');
  const outputPath = path.join(projectRoot, 'ggen', 'generated', 'packages.ts');

  console.log('📝 Parsing ontology...');
  const packages = parseOntology(ontologyPath);
  console.log(`   Found ${packages.length} packages`);

  console.log('🔨 Generating TypeScript...');
  const typeScript = generateTypeScript(packages);

  // Ensure output directory exists
  fs.mkdirSync(path.dirname(outputPath), { recursive: true });

  fs.writeFileSync(outputPath, typeScript);
  console.log(`   Written: ${outputPath}`);

  // Generate ESM version
  const esmPath = path.join(projectRoot, 'ggen', 'generated', 'packages.mjs');
  const esm = typeScript.replace(/export interface/g, 'export').replace(': Package\[]/g', '');
  fs.writeFileSync(esmPath, esm);
  console.log(`   Written: ${esmPath}`);

  console.log(`\n✅ Generated ${packages.length} packages`);
}

main().catch(console.error);
