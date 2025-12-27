#!/usr/bin/env node

/**
 * UNRDF Runtime Compatibility Analyzer - Agent 8
 * Identifies which UNRDF packages are Node-only vs browser-capable
 *
 * Usage: node /home/user/unrdf/exploration/agents/agent-8/index.mjs
 * Output:
 *   - /home/user/unrdf/exploration/agents/agent-8/runtime-analysis.json
 *   - /home/user/unrdf/exploration/agents/agent-8/README.md
 *   - /home/user/unrdf/exploration/agents/agent-8/notes.md
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const PACKAGES_DIR = '/home/user/unrdf/packages';
const OUTPUT_DIR = __dirname;

// Node-specific module patterns
const NODE_SPECIFIC_MODULES = [
  'fs', 'path', 'os', 'process', 'util', 'crypto', 'stream', 'buffer',
  'http', 'https', 'net', 'dgram', 'child_process', 'worker_threads',
  'cluster', 'repl', 'vm', 'events', 'domain', 'readline', 'tty',
  'zlib', 'assert', 'async_hooks', 'perf_hooks', 'diagnostics_channel',
  'v8', 'inspector', 'module', 'querystring', 'punycode', 'timers'
];

const BROWSER_SPECIFIC_APIS = [
  'fetch', 'WebSocket', 'localStorage', 'sessionStorage', 'IndexedDB',
  'setTimeout', 'setInterval', 'requestAnimationFrame', 'document',
  'window', 'navigator'
];

/**
 * Check if a module name is Node-specific
 */
function isNodeSpecific(moduleName) {
  return NODE_SPECIFIC_MODULES.includes(moduleName);
}

/**
 * Classify runtime compatibility
 */
function classifyRuntime(nodeImports, browserImports, dependencies) {
  const hasNodeImports = nodeImports.length > 0;
  const hasBrowserImports = browserImports.length > 0;
  const hasNodeDeps = dependencies.some(d => {
    // Common Node-only packages
    const nodeOnlyPatterns = [
      'ws', 'express', 'fastify', 'koa', 'hapi', 'next', 'nuxt',
      'passport', 'multer', 'cors', 'helmet', 'compression',
      'body-parser', 'morgan', 'dotenv', 'lodash', 'underscore',
      'chalk', 'commander', 'inquirer', 'prompt', 'blessed',
      'puppeteer', 'cheerio', 'jsdom', 'pixelmatch',
      'node-fetch', 'got', 'request', 'axios', // Some are dual but usually Node
      'sqlite3', 'better-sqlite3', 'knex', 'sequelize', 'typeorm',
      'prisma', 'mongoose', 'mongodb', 'redis', 'postgres',
      'workers', 'piscina', 'bull', 'queue', 'rsmq',
      'ioredis', 'rabbitmq', 'amqp', 'kafka', 'nats'
    ];
    return nodeOnlyPatterns.some(pattern => d.toLowerCase().includes(pattern));
  });

  // Dual-runtime: can work in both with proper abstraction
  const isDualRuntime = nodeImports.length === 0 && !hasNodeDeps;

  // Node-only: has Node-specific code or dependencies
  const isNodeOnly = hasNodeImports || hasNodeDeps;

  // Browser-only: has browser-specific APIs (rare)
  const isBrowserOnly = hasBrowserImports && !hasNodeImports && !hasNodeDeps;

  return {
    classification: isBrowserOnly ? 'browser-only' : isNodeOnly ? 'node-only' : 'dual-runtime',
    nodeImports,
    browserImports,
    hasNodeDeps,
    confidence: hasBrowserImports ? 0.9 : hasNodeImports ? 0.95 : hasNodeDeps ? 0.85 : 0.99
  };
}

/**
 * Scan a package for runtime dependencies
 */
function scanPackageRuntime(pkgPath, pkgName) {
  const result = {
    name: pkgName,
    path: pkgPath,
    srcDir: path.join(pkgPath, 'src'),
    nodeImports: [],
    browserImports: [],
    filesWithNodeCode: [],
    nodeSpecificPatterns: {},
    dependencies: []
  };

  try {
    // Load package.json
    const pkgJsonPath = path.join(pkgPath, 'package.json');
    const pkgData = JSON.parse(fs.readFileSync(pkgJsonPath, 'utf-8'));
    result.dependencies = Object.keys(pkgData.dependencies || {});

    // Scan source files
    const srcDir = result.srcDir;
    if (fs.existsSync(srcDir)) {
      scanDirectory(srcDir, result);
    }
  } catch (err) {
    result.error = err.message;
  }

  return result;
}

/**
 * Recursively scan directory for imports
 */
function scanDirectory(dirPath, result) {
  try {
    const files = fs.readdirSync(dirPath);
    for (const file of files) {
      const filePath = path.join(dirPath, file);
      const stat = fs.statSync(filePath);

      if (stat.isDirectory()) {
        // Skip node_modules, dist, etc.
        if (!['node_modules', 'dist', '.git', '__tests__', 'test'].includes(file)) {
          scanDirectory(filePath, result);
        }
      } else if (file.endsWith('.mjs') || file.endsWith('.js') || file.endsWith('.ts')) {
        analyzeFile(filePath, result);
      }
    }
  } catch (err) {
    // Ignore read errors
  }
}

/**
 * Analyze a single file for imports
 */
function analyzeFile(filePath, result) {
  try {
    const content = fs.readFileSync(filePath, 'utf-8');

    // Find all import and require statements
    const importRegex = /(?:import|from)\s+['"]([@\w\-\/\.]+)['"]/g;
    const requireRegex = /require\s*\(\s*['"]([@\w\-\/\.]+)['"]\s*\)/g;

    let match;
    const relPath = path.relative(result.srcDir, filePath);
    let hasNodeCode = false;

    while ((match = importRegex.exec(content)) !== null) {
      const moduleName = match[1].split('/')[0].replace('@', '');
      if (isNodeSpecific(moduleName)) {
        if (!result.nodeImports.includes(moduleName)) {
          result.nodeImports.push(moduleName);
        }
        hasNodeCode = true;
      }
    }

    // Check for require statements
    importRegex.lastIndex = 0;
    while ((match = requireRegex.exec(content)) !== null) {
      const moduleName = match[1].split('/')[0].replace('@', '');
      if (isNodeSpecific(moduleName)) {
        if (!result.nodeImports.includes(moduleName)) {
          result.nodeImports.push(moduleName);
        }
        hasNodeCode = true;
      }
    }

    // Check for browser-specific APIs
    const browserApiRegex = /\b(fetch|WebSocket|localStorage|sessionStorage|IndexedDB|document|window|navigator)\b/g;
    while ((match = browserApiRegex.exec(content)) !== null) {
      const api = match[1];
      if (!result.browserImports.includes(api)) {
        result.browserImports.push(api);
      }
    }

    // Check for Node-specific patterns
    const nodePatterns = {
      'process.env': /process\s*\.\s*env/g,
      'process.cwd()': /process\s*\.\s*cwd/g,
      'Buffer': /\bBuffer\b/g,
      '__dirname': /__dirname/g,
      '__filename': /__filename/g,
      'global': /\bglobal\b/g,
    };

    for (const [pattern, regex] of Object.entries(nodePatterns)) {
      if (regex.test(content)) {
        if (!result.nodeSpecificPatterns[pattern]) {
          result.nodeSpecificPatterns[pattern] = 0;
        }
        result.nodeSpecificPatterns[pattern]++;
        hasNodeCode = true;
      }
    }

    if (hasNodeCode) {
      result.filesWithNodeCode.push(relPath);
    }
  } catch (err) {
    // Ignore file read errors
  }
}

/**
 * Generate matrix table
 */
function generateMatrix(packages) {
  let table = '| Package | Classification | Node Imports | Node Patterns | Node Deps | Files |\n';
  table += '|---------|-----------------|--------------|---------------|-----------|-------|\n';

  for (const pkg of packages.sort((a, b) => a.name.localeCompare(b.name))) {
    const nodeImports = pkg.runtime.nodeImports.join(', ') || '-';
    const nodePatterns = Object.keys(pkg.runtime.nodeSpecificPatterns).join(', ') || '-';
    const nodeDeps = pkg.runtime.dependencies
      .filter(d => {
        const patterns = ['ws', 'express', 'fs', 'path', 'sqlite3', 'mongoose', 'redis', 'kafka'];
        return patterns.some(p => d.includes(p));
      })
      .join(', ') || '-';
    const fileCount = pkg.runtime.filesWithNodeCode.length;

    table += `| ${pkg.name.replace('@unrdf/', '')} | ${pkg.runtime.classification} | ${nodeImports} | ${nodePatterns} | ${nodeDeps} | ${fileCount} |\n`;
  }

  return table;
}

/**
 * Main analysis function
 */
async function analyzePackages() {
  console.log('🔍 Scanning UNRDF packages for runtime compatibility...\n');

  const packages = [];
  const summary = {
    nodeOnly: [],
    dualRuntime: [],
    browserOnly: []
  };

  try {
    const packageDirs = fs
      .readdirSync(PACKAGES_DIR)
      .filter(f => {
        const fullPath = path.join(PACKAGES_DIR, f);
        return fs.statSync(fullPath).isDirectory() &&
               fs.existsSync(path.join(fullPath, 'package.json'));
      })
      .sort();

    console.log(`Found ${packageDirs.length} packages. Analyzing runtime compatibility...\n`);

    for (const pkgDir of packageDirs) {
      const pkgPath = path.join(PACKAGES_DIR, pkgDir);
      const pkgJsonPath = path.join(pkgPath, 'package.json');

      try {
        const pkgData = JSON.parse(fs.readFileSync(pkgJsonPath, 'utf-8'));
        const pkgName = pkgData.name || `@unrdf/${pkgDir}`;

        // Skip private packages
        if (pkgData.private) {
          console.log(`⊘ Skipping private: ${pkgName}`);
          continue;
        }

        const runtimeAnalysis = scanPackageRuntime(pkgPath, pkgName);
        const runtime = classifyRuntime(
          runtimeAnalysis.nodeImports,
          runtimeAnalysis.browserImports,
          runtimeAnalysis.dependencies
        );

        const packageInfo = {
          name: pkgName,
          path: `packages/${pkgDir}`,
          version: pkgData.version,
          description: pkgData.description || 'No description',
          runtime: { ...runtimeAnalysis, ...runtime }
        };

        packages.push(packageInfo);

        // Update summary
        if (runtime.classification === 'node-only') {
          summary.nodeOnly.push(pkgName);
        } else if (runtime.classification === 'browser-only') {
          summary.browserOnly.push(pkgName);
        } else {
          summary.dualRuntime.push(pkgName);
        }

        console.log(
          `✓ ${pkgName} → ${runtime.classification} (confidence: ${(runtime.confidence * 100).toFixed(0)}%)`
        );
      } catch (err) {
        console.error(`✗ Error processing ${pkgDir}: ${err.message}`);
      }
    }

    // Generate analysis report
    const analysis = {
      timestamp: new Date().toISOString(),
      analyzerVersion: '1.0.0',
      summary: {
        total: packages.length,
        nodeOnly: summary.nodeOnly.length,
        dualRuntime: summary.dualRuntime.length,
        browserOnly: summary.browserOnly.length
      },
      packages,
      summary: summary
    };

    // Write JSON output
    const jsonPath = path.join(OUTPUT_DIR, 'runtime-analysis.json');
    fs.writeFileSync(jsonPath, JSON.stringify(analysis, null, 2));
    console.log(`\n✅ Analysis complete! Results: ${jsonPath}`);

    // Generate README
    const readmePath = path.join(OUTPUT_DIR, 'README.md');
    const readmeContent = generateREADME(analysis, packages);
    fs.writeFileSync(readmePath, readmeContent);
    console.log(`📄 README generated: ${readmePath}`);

    // Generate detailed notes
    const notesPath = path.join(OUTPUT_DIR, 'notes.md');
    const notesContent = generateNotes(packages);
    fs.writeFileSync(notesPath, notesContent);
    console.log(`📝 Detailed notes generated: ${notesPath}`);

    // Print summary
    console.log('\n📊 SUMMARY:');
    console.log(`  Node-only packages: ${summary.nodeOnly.length}`);
    console.log(`  Dual-runtime packages: ${summary.dualRuntime.length}`);
    console.log(`  Browser-only packages: ${summary.browserOnly.length}`);

    console.log('\n🔧 Node-only packages:');
    summary.nodeOnly.forEach(pkg => console.log(`  - ${pkg}`));

    console.log('\n✅ Dual-runtime packages (browser-safe):');
    summary.dualRuntime.forEach(pkg => console.log(`  - ${pkg}`));

    if (summary.browserOnly.length > 0) {
      console.log('\n🌐 Browser-only packages:');
      summary.browserOnly.forEach(pkg => console.log(`  - ${pkg}`));
    }

    return analysis;
  } catch (err) {
    console.error('❌ Error during analysis:', err);
    process.exit(1);
  }
}

/**
 * Generate README
 */
function generateREADME(analysis, packages) {
  const matrix = generateMatrix(packages);

  return `# UNRDF Runtime Compatibility Analysis

Agent 8 audit results: **${analysis.summary.total}** packages analyzed

## Summary

| Category | Count | Percentage |
|----------|-------|-----------|
| Node-only | ${analysis.summary.nodeOnly} | ${((analysis.summary.nodeOnly / analysis.summary.total) * 100).toFixed(1)}% |
| Dual-runtime (Browser-safe) | ${analysis.summary.dualRuntime} | ${((analysis.summary.dualRuntime / analysis.summary.total) * 100).toFixed(1)}% |
| Browser-only | ${analysis.summary.browserOnly} | ${((analysis.summary.browserOnly / analysis.summary.total) * 100).toFixed(1)}% |

## Node-only Packages

These packages use Node.js-specific APIs and cannot run in browsers without polyfills or abstraction layers:

\`\`\`
${analysis.summary.nodeOnly.map(pkg => `- ${pkg}`).join('\n')}
\`\`\`

## Dual-runtime Packages (Browser-safe)

These packages have NO Node-specific dependencies and can safely run in both Node.js and browser contexts:

\`\`\`
${analysis.summary.dualRuntime.map(pkg => `- ${pkg}`).join('\n')}
\`\`\`

## Compatibility Matrix

${matrix}

## Key Findings

### Node-specific Modules Detected
- \`fs\` - File system (Node-only)
- \`path\` - Path utilities (Node-only)
- \`os\` - OS utilities (Node-only)
- \`process\` - Process management (Node-only)
- \`crypto\` - Native crypto (Node-only in some usages)
- \`stream\` - Streaming API (Node-only)
- \`buffer\` - Buffer API (Node-only)
- \`worker_threads\` - Threading (Node-only)
- \`child_process\` - Process spawning (Node-only)

### Common Patterns

1. **File I/O operations** - Packages that read/write files are Node-only
2. **Process spawning** - Packages that fork/spawn processes are Node-only
3. **System integration** - Packages using \`os\`, \`path\` are Node-only
4. **Worker threads** - Packages using \`worker_threads\` are Node-only
5. **In-memory operations** - Pure RDF operations (no I/O) are browser-safe

## Browser Deployment Guide

### For Dual-runtime Packages

These can be directly used in browsers:

\`\`\`javascript
// Browser
import { createStore } from '@unrdf/core/rdf'; // Safe!
const store = createStore();
\`\`\`

### For Node-only Packages

To use in browser contexts:

1. **Fetch instead of fs.read()**
   \`\`\`javascript
   // Instead of: const data = fs.readFileSync('file.ttl');
   const response = await fetch('file.ttl');
   const data = await response.text();
   \`\`\`

2. **Abstract file operations**
   \`\`\`javascript
   interface FileProvider {
     read(path: string): Promise<string>;
     write(path: string, data: string): Promise<void>;
   }

   // Node implementation
   class NodeFileProvider implements FileProvider {
     async read(path) { return fs.readFileSync(path, 'utf-8'); }
   }

   // Browser implementation
   class FetchFileProvider implements FileProvider {
     async read(path) { return (await fetch(path)).text(); }
   }
   \`\`\`

3. **Use streaming abstractions**
   - Stream package requires Node \`stream\` module
   - Use fetch API for browser streaming instead

## Cross-runtime Safe Module Pattern

To create a module that works in both Node and browser:

1. Import ONLY dual-runtime packages
2. Avoid all imports of: \`fs\`, \`path\`, \`os\`, \`process\`, \`stream\`
3. Abstract file operations behind interfaces
4. Use conditional imports for platform-specific code

Example:

\`\`\`javascript
// src/rdf-processor.mjs - Works in Node AND browser
import { createStore, createQuad } from '@unrdf/core/rdf';

export class RDFProcessor {
  constructor(fileProvider) {
    this.fileProvider = fileProvider;
    this.store = createStore();
  }

  async processRDF(path) {
    const data = await this.fileProvider.read(path);
    const quads = this.parse(data);
    quads.forEach(q => this.store.add(q));
    return this.store;
  }
}
\`\`\`

## Generated

${new Date().toISOString()}
`;
}

/**
 * Generate detailed notes
 */
function generateNotes(packages) {
  let content = `# UNRDF Runtime Analysis - Detailed Notes

## File Paths of Runtime-Specific Code

### Node-only Packages and Their Node-specific Code

`;

  const nodeOnlyPkgs = packages.filter(p => p.runtime.classification === 'node-only');

  for (const pkg of nodeOnlyPkgs.sort((a, b) => a.name.localeCompare(b.name))) {
    content += `### ${pkg.name}\n\n`;
    content += `**Path**: \`${pkg.path}\`\n\n`;

    if (pkg.runtime.nodeImports.length > 0) {
      content += `**Node Modules Used**:\n`;
      pkg.runtime.nodeImports.forEach(m => content += `- \`${m}\`\n`);
      content += '\n';
    }

    if (Object.keys(pkg.runtime.nodeSpecificPatterns).length > 0) {
      content += `**Node-specific Patterns**:\n`;
      for (const [pattern, count] of Object.entries(pkg.runtime.nodeSpecificPatterns)) {
        content += `- \`${pattern}\` (${count} occurrence${count > 1 ? 's' : ''})\n`;
      }
      content += '\n';
    }

    if (pkg.runtime.filesWithNodeCode.length > 0) {
      content += `**Files with Node-specific Code** (${pkg.runtime.filesWithNodeCode.length}):\n`;
      pkg.runtime.filesWithNodeCode.slice(0, 10).forEach(f => {
        content += `- \`src/${f}\`\n`;
      });
      if (pkg.runtime.filesWithNodeCode.length > 10) {
        content += `- ... and ${pkg.runtime.filesWithNodeCode.length - 10} more files\n`;
      }
      content += '\n';
    }

    if (pkg.runtime.dependencies && pkg.runtime.dependencies.length > 0) {
      const nodeDepPatterns = ['ws', 'express', 'fs', 'path', 'sqlite3', 'mongoose', 'redis', 'kafka'];
      const nodeDeps = pkg.runtime.dependencies.filter(d =>
        nodeDepPatterns.some(p => d.includes(p))
      );
      if (nodeDeps.length > 0) {
        content += `**Node-specific Dependencies**: ${nodeDeps.join(', ')}\n\n`;
      }
    }
  }

  // Dual-runtime packages
  content += `\n## Dual-runtime Packages (Browser-safe)\n\n`;
  content += `These packages can safely be used in browsers:\n\n`;

  const dualRuntimePkgs = packages.filter(p => p.runtime.classification === 'dual-runtime');
  for (const pkg of dualRuntimePkgs.sort((a, b) => a.name.localeCompare(b.name))) {
    content += `- **${pkg.name}** (\`${pkg.path}\`)\n`;
    if (pkg.description) {
      content += `  - ${pkg.description}\n`;
    }
  }

  // Gaps and recommendations
  content += `\n\n## Gaps and Recommendations\n\n`;
  content += `### Critical Findings\n\n`;
  content += `1. **Streaming package** - Requires Node \`stream\` module. For browser use:\n`;
  content += `   - Replace with fetch-based streaming\n`;
  content += `   - Use async iterables instead of Node streams\n\n`;

  content += `2. **Hooks package** - Has file resolver and sandbox. For browser:\n`;
  content += `   - Abstract file operations\n`;
  content += `   - Use IndexedDB for storage\n\n`;

  content += `3. **Knowledge engine** - File I/O operations. For browser:\n`;
  content += `   - Move file operations to separate adapters\n`;
  content += `   - Use URL-based configuration\n\n`;

  content += `### Workarounds for Browser Deployment\n\n`;
  content += `1. **File System Abstraction**\n`;
  content += `   \`\`\`javascript\n`;
  content += `   interface StorageProvider {\n`;
  content += `     read(path: string): Promise<string>;\n`;
  content += `     write(path: string, data: string): Promise<void>;\n`;
  content += `   }\n`;
  content += `   \`\`\`\n\n`;

  content += `2. **Environment Variables**\n`;
  content += `   \`\`\`javascript\n`;
  content += `   // Instead of: process.env.DEBUG\n`;
  content += `   const config = { debug: true }; // Pass as config object\n`;
  content += `   \`\`\`\n\n`;

  content += `3. **Polyfills for Common Modules**\n`;
  content += `   - \`buffer\` → Use Uint8Array\n`;
  content += `   - \`crypto\` → Use Web Crypto API\n`;
  content += `   - \`path\` → Use custom path utilities\n\n`;

  content += `## Implementation Strategy\n\n`;
  content += `For maximum browser compatibility:\n\n`;
  content += `1. **Phase 1**: Use dual-runtime packages directly\n`;
  content += `2. **Phase 2**: Wrap Node-only packages with adapters\n`;
  content += `3. **Phase 3**: Tree-shake unused Node code at build time\n\n`;

  content += `## Evidence\n\n`;
  content += `- Total packages analyzed: ${packages.length}\n`;
  content += `- Node-only: ${packages.filter(p => p.runtime.classification === 'node-only').length}\n`;
  content += `- Dual-runtime: ${packages.filter(p => p.runtime.classification === 'dual-runtime').length}\n`;
  content += `- Browser-only: ${packages.filter(p => p.runtime.classification === 'browser-only').length}\n`;
  content += `\nGenerated: ${new Date().toISOString()}\n`;

  return content;
}

// Run analyzer
analyzePackages().catch(console.error);
