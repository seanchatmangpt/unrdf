#!/usr/bin/env node
import { readFileSync, readdirSync, statSync, existsSync } from 'fs';
import { join } from 'path';
import { execSync } from 'child_process';

const packagesDir = '/home/user/unrdf/packages';
const packages = readdirSync(packagesDir).filter(name => {
  const pkgPath = join(packagesDir, name);
  return statSync(pkgPath).isDirectory() && existsSync(join(pkgPath, 'package.json'));
}).sort();

const results = [];

for (const pkgName of packages) {
  const pkgPath = join(packagesDir, pkgName);
  const pkgJsonPath = join(pkgPath, 'package.json');

  try {
    const pkgJson = JSON.parse(readFileSync(pkgJsonPath, 'utf8'));

    // Count LOC
    let loc = 0;
    const srcPath = join(pkgPath, 'src');
    if (existsSync(srcPath)) {
      try {
        const output = execSync(
          `find "${srcPath}" -type f \\( -name "*.mjs" -o -name "*.js" -o -name "*.ts" \\) 2>/dev/null | xargs wc -l 2>/dev/null | tail -1`,
          { encoding: 'utf8', timeout: 5000 }
        );
        const match = output.match(/(\d+)\s+total/);
        loc = match ? parseInt(match[1]) : 0;
      } catch (e) {
        loc = 0;
      }
    }

    // Determine entry point
    let entryPoint = pkgJson.main || 'src/index.mjs';
    if (entryPoint.startsWith('./')) entryPoint = entryPoint.slice(2);

    // Count exports
    const exportsObj = pkgJson.exports;
    let exportCount = 0;
    if (exportsObj && typeof exportsObj === 'object') {
      exportCount = Object.keys(exportsObj).length;
    }

    // Extract internal dependencies
    const deps = pkgJson.dependencies || {};
    const internalDeps = Object.keys(deps).filter(d => d.startsWith('@unrdf/')).map(d => d.replace('@unrdf/', ''));

    // Determine runtime
    let runtime = 'Node';
    const keywords = pkgJson.keywords || [];
    const description = pkgJson.description || '';
    const combinedText = [...keywords, description].join(' ').toLowerCase();

    if (combinedText.includes('browser') || combinedText.includes('wasm')) {
      if (combinedText.includes('node')) {
        runtime = 'Node+Browser';
      } else {
        runtime = 'Browser';
      }
    }
    if (combinedText.includes('wasm') || combinedText.includes('webassembly')) {
      runtime = 'WASM';
    }

    results.push({
      name: pkgJson.name || pkgName,
      description: pkgJson.description || '',
      entryPoint,
      exportCount,
      runtime,
      loc,
      internalDeps: internalDeps.join(', '),
      private: pkgJson.private || false,
      version: pkgJson.version || 'N/A'
    });
  } catch (error) {
    console.error(`Error processing ${pkgName}:`, error.message);
  }
}

// Output as JSON
console.log(JSON.stringify(results, null, 2));
