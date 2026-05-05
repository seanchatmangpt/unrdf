/**
 * Quality Gates - Zero Tolerance Validation System
 *
 * Eight quality gates that must ALL pass for production readiness.
 * Each gate returns { pass: boolean, evidence: string[], failures: string[] }
 */

import fs from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const AUTONOMIC_ROOT = path.resolve(__dirname, '../..');

/**
 * Run all quality gates sequentially
 * @returns {Promise<{pass: boolean, gates: Array, failures: Array}>}
 */
export async function runQualityGates() {
  const gates = [
    { name: 'Import Resolution', fn: verifyImports },
    { name: 'Circular Dependencies', fn: detectCircularDeps },
    { name: 'JSDoc Coverage', fn: verifyJSDoc },
    { name: 'Prohibited Imports', fn: checkProhibitedImports },
    { name: 'Zod Validation', fn: verifyZodValidation },
    { name: 'Determinism Check', fn: checkDeterminism },
    { name: 'Network Calls', fn: checkNetworkCalls },
    { name: 'File Sizes', fn: checkFileSizes }
  ];

  const results = [];
  const allFailures = [];

  for (const gate of gates) {
    const result = await gate.fn();

    // Validate result structure
    if (typeof result.pass !== 'boolean' || !Array.isArray(result.evidence) || !Array.isArray(result.failures)) {
      throw new Error(`Invalid gate result from ${gate.name}`);
    }

    results.push({
      name: gate.name,
      ...result
    });

    if (!result.pass) {
      allFailures.push(...result.failures.map(f => `[${gate.name}] ${f}`));
    }
  }

  return {
    pass: allFailures.length === 0,
    gates: results,
    failures: allFailures
  };
}

/**
 * Verify import resolution for all agent modules
 * @returns {Promise<{pass: boolean, evidence: string[], failures: string[]}>}
 */
export async function verifyImports() {
  const agentModules = [
    '../agent-2/src/index.mjs',
    '../agent-3/src/index.mjs',
    '../agent-4/src/index.mjs',
    '../agent-5/src/index.mjs',
    '../agent-6/src/index.mjs',
    '../agent-7/src/index.mjs',
    '../agent-8/src/index.mjs',
    '../agent-9/src/index.mjs'
  ];

  const failures = [];
  let successCount = 0;

  for (const modulePath of agentModules) {
    const fullPath = path.resolve(__dirname, modulePath);
    try {
      // Check if file exists first
      await fs.access(fullPath);

      // Try to import
      const imported = await import(fullPath);
      if (!imported || Object.keys(imported).length === 0) {
        failures.push(`${modulePath}: Empty exports`);
      } else {
        successCount++;
      }
    } catch (error) {
      failures.push(`${modulePath}: ${error.message}`);
    }
  }

  return {
    pass: failures.length === 0,
    evidence: [`Checked ${agentModules.length} modules, ${successCount} loaded successfully`],
    failures
  };
}

/**
 * Detect circular dependencies in module graph
 * @returns {Promise<{pass: boolean, evidence: string[], failures: string[]}>}
 */
export async function detectCircularDeps() {
  const { buildDependencyGraph, findCycles } = await import('./dependency-analyzer.mjs');

  try {
    const graph = await buildDependencyGraph();
    const cycles = findCycles(graph);

    return {
      pass: cycles.length === 0,
      evidence: [`Analyzed ${graph.nodeCount} modules`],
      failures: cycles.map(cycle => `Circular: ${cycle.join(' → ')}`)
    };
  } catch (error) {
    return {
      pass: false,
      evidence: ['Failed to analyze dependency graph'],
      failures: [error.message]
    };
  }
}

/**
 * Verify 100% JSDoc coverage on exported functions
 * @returns {Promise<{pass: boolean, evidence: string[], failures: string[]}>}
 */
export async function verifyJSDoc() {
  const agentDirs = ['agent-2', 'agent-3', 'agent-4', 'agent-5', 'agent-6', 'agent-7', 'agent-8', 'agent-9'];
  const failures = [];
  let checkedCount = 0;

  for (const agentDir of agentDirs) {
    const indexPath = path.join(AUTONOMIC_ROOT, agentDir, 'src', 'index.mjs');

    try {
      const content = await fs.readFile(indexPath, 'utf-8');

      // Extract export statements
      const exportRegex = /export\s+(?:async\s+)?function\s+(\w+)/g;
      let match;

      while ((match = exportRegex.exec(content)) !== null) {
        const funcName = match[1];
        checkedCount++;

        // Find JSDoc comment before function
        const funcIndex = match.index;
        const beforeFunc = content.substring(0, funcIndex);
        const lines = beforeFunc.split('\n');

        // Check last few lines for JSDoc
        let hasJSDoc = false;
        for (let i = lines.length - 1; i >= Math.max(0, lines.length - 10); i--) {
          if (lines[i].includes('/**')) {
            hasJSDoc = true;
            break;
          }
        }

        if (!hasJSDoc) {
          failures.push(`${agentDir}::${funcName}: Missing JSDoc`);
        }
      }
    } catch (error) {
      // File doesn't exist - skip silently for now
      if (error.code !== 'ENOENT') {
        failures.push(`${agentDir}: ${error.message}`);
      }
    }
  }

  return {
    pass: failures.length === 0,
    evidence: [`Checked JSDoc for ${checkedCount} exported functions`],
    failures
  };
}

/**
 * Check for prohibited imports (e.g., direct 'n3' usage)
 * @returns {Promise<{pass: boolean, evidence: string[], failures: string[]}>}
 */
export async function checkProhibitedImports() {
  const prohibited = [
    { pattern: /from\s+['"]n3['"]/g, name: 'n3 (use @unrdf/oxigraph)' },
    { pattern: /console\.(log|debug|info|warn)\(/g, name: 'console.* (use proper logging)' }
  ];

  const failures = [];
  let filesScanned = 0;

  const agentDirs = ['agent-2', 'agent-3', 'agent-4', 'agent-5', 'agent-6', 'agent-7', 'agent-8', 'agent-9', 'agent-10'];

  for (const agentDir of agentDirs) {
    const srcDir = path.join(AUTONOMIC_ROOT, agentDir, 'src');

    try {
      const files = await fs.readdir(srcDir);

      for (const file of files) {
        if (!file.endsWith('.mjs')) continue;

        const filePath = path.join(srcDir, file);
        const content = await fs.readFile(filePath, 'utf-8');
        filesScanned++;

        for (const { pattern, name } of prohibited) {
          const matches = content.match(pattern);
          if (matches) {
            failures.push(`${agentDir}/${file}: Prohibited usage: ${name} (${matches.length} occurrence${matches.length > 1 ? 's' : ''})`);
          }
        }
      }
    } catch (error) {
      // Directory doesn't exist - skip
      if (error.code !== 'ENOENT') {
        failures.push(`${agentDir}: ${error.message}`);
      }
    }
  }

  return {
    pass: failures.length === 0,
    evidence: [`Scanned ${filesScanned} .mjs files`],
    failures
  };
}

/**
 * Verify Zod validation on all public API functions
 * @returns {Promise<{pass: boolean, evidence: string[], failures: string[]}>}
 */
export async function verifyZodValidation() {
  const agentDirs = ['agent-2', 'agent-3', 'agent-4', 'agent-5', 'agent-6', 'agent-7', 'agent-8', 'agent-9'];
  const failures = [];
  let checkedModules = 0;

  for (const agentDir of agentDirs) {
    const indexPath = path.join(AUTONOMIC_ROOT, agentDir, 'src', 'index.mjs');

    try {
      const content = await fs.readFile(indexPath, 'utf-8');
      checkedModules++;

      // Check for Zod import
      if (!content.includes("from 'zod'") && !content.includes('from "zod"')) {
        failures.push(`${agentDir}: No Zod import found in index.mjs`);
      }

      // Extract exported functions and check for validation
      const exportRegex = /export\s+(?:async\s+)?function\s+(\w+)\s*\([^)]*\)\s*\{([^}]+(?:\{[^}]*\}[^}]*)*)\}/gs;
      let match;

      while ((match = exportRegex.exec(content)) !== null) {
        const funcName = match[1];
        const funcBody = match[2];

        // Check for .parse( or .safeParse( in function body
        if (!funcBody.includes('.parse(') && !funcBody.includes('.safeParse(')) {
          failures.push(`${agentDir}::${funcName}: No Zod validation detected`);
        }
      }
    } catch (error) {
      // File doesn't exist - skip
      if (error.code !== 'ENOENT') {
        failures.push(`${agentDir}: ${error.message}`);
      }
    }
  }

  return {
    pass: failures.length === 0,
    evidence: [`Verified Zod usage in ${checkedModules} agent modules`],
    failures
  };
}

/**
 * Check for unseeded randomness (Math.random, etc.)
 * @returns {Promise<{pass: boolean, evidence: string[], failures: string[]}>}
 */
export async function checkDeterminism() {
  const randomPatterns = [
    { pattern: /Math\.random\(\)/g, name: 'Math.random()' },
    { pattern: /crypto\.randomBytes\(/g, name: 'crypto.randomBytes()' },
    { pattern: /Date\.now\(\)/g, name: 'Date.now()' },
    { pattern: /new\s+Date\(\)/g, name: 'new Date()' }
  ];

  const failures = [];
  let filesScanned = 0;

  const agentDirs = ['agent-2', 'agent-3', 'agent-4', 'agent-5', 'agent-6', 'agent-7', 'agent-8', 'agent-9', 'agent-10'];

  for (const agentDir of agentDirs) {
    const srcDir = path.join(AUTONOMIC_ROOT, agentDir, 'src');

    try {
      const files = await fs.readdir(srcDir);

      for (const file of files) {
        if (!file.endsWith('.mjs')) continue;

        const filePath = path.join(srcDir, file);
        const content = await fs.readFile(filePath, 'utf-8');
        filesScanned++;

        const lines = content.split('\n');

        for (const { pattern, name } of randomPatterns) {
          pattern.lastIndex = 0; // Reset regex
          let match;

          while ((match = pattern.exec(content)) !== null) {
            // Find line number
            let lineNum = 0;
            let charCount = 0;
            for (let i = 0; i < lines.length; i++) {
              charCount += lines[i].length + 1; // +1 for newline
              if (charCount > match.index) {
                lineNum = i + 1;
                break;
              }
            }

            // Check for seed/deterministic comment nearby
            const startLine = Math.max(0, lineNum - 3);
            const endLine = Math.min(lines.length, lineNum + 2);
            const context = lines.slice(startLine, endLine).join('\n');

            if (!/seed|deterministic|controlled|test|mock/i.test(context)) {
              failures.push(`${agentDir}/${file}:${lineNum}: Unseeded randomness: ${name}`);
            }
          }
        }
      }
    } catch (error) {
      if (error.code !== 'ENOENT') {
        failures.push(`${agentDir}: ${error.message}`);
      }
    }
  }

  return {
    pass: failures.length === 0,
    evidence: [`Scanned ${filesScanned} files for randomness patterns`],
    failures
  };
}

/**
 * Check for external network calls
 * @returns {Promise<{pass: boolean, evidence: string[], failures: string[]}>}
 */
export async function checkNetworkCalls() {
  const networkPatterns = [
    { pattern: /\bfetch\s*\(/g, name: 'fetch()' },
    { pattern: /\baxios\./g, name: 'axios.*' },
    { pattern: /\bhttp\.request/g, name: 'http.request' },
    { pattern: /\bhttps\.request/g, name: 'https.request' },
    { pattern: /['"]ws:\/\//g, name: 'WebSocket (ws://)' },
    { pattern: /['"]wss:\/\//g, name: 'WebSocket (wss://)' }
  ];

  const failures = [];
  let filesScanned = 0;

  const agentDirs = ['agent-2', 'agent-3', 'agent-4', 'agent-5', 'agent-6', 'agent-7', 'agent-8', 'agent-9', 'agent-10'];

  for (const agentDir of agentDirs) {
    const srcDir = path.join(AUTONOMIC_ROOT, agentDir, 'src');

    try {
      const files = await fs.readdir(srcDir);

      for (const file of files) {
        if (!file.endsWith('.mjs')) continue;

        const filePath = path.join(srcDir, file);
        const content = await fs.readFile(filePath, 'utf-8');
        filesScanned++;

        // Skip self (quality-gates.mjs contains the patterns as strings)
        if (file === 'quality-gates.mjs') {
          continue;
        }

        for (const { pattern, name } of networkPatterns) {
          if (pattern.test(content)) {
            failures.push(`${agentDir}/${file}: Network call detected: ${name}`);
          }
        }
      }
    } catch (error) {
      if (error.code !== 'ENOENT') {
        failures.push(`${agentDir}: ${error.message}`);
      }
    }
  }

  return {
    pass: failures.length === 0,
    evidence: [`Scanned ${filesScanned} files for network patterns`],
    failures
  };
}

/**
 * Check file size constraints (<500 lines)
 * @returns {Promise<{pass: boolean, evidence: string[], failures: string[]}>}
 */
export async function checkFileSizes() {
  const MAX_LINES = 500;
  const failures = [];
  let filesChecked = 0;

  const agentDirs = ['agent-2', 'agent-3', 'agent-4', 'agent-5', 'agent-6', 'agent-7', 'agent-8', 'agent-9', 'agent-10'];

  for (const agentDir of agentDirs) {
    const srcDir = path.join(AUTONOMIC_ROOT, agentDir, 'src');

    try {
      const files = await fs.readdir(srcDir);

      for (const file of files) {
        if (!file.endsWith('.mjs')) continue;

        const filePath = path.join(srcDir, file);
        const content = await fs.readFile(filePath, 'utf-8');
        const lines = content.split('\n').length;
        filesChecked++;

        if (lines > MAX_LINES) {
          failures.push(`${agentDir}/${file}: ${lines} lines (max ${MAX_LINES})`);
        }
      }
    } catch (error) {
      if (error.code !== 'ENOENT') {
        failures.push(`${agentDir}: ${error.message}`);
      }
    }
  }

  return {
    pass: failures.length === 0,
    evidence: [`Checked ${filesChecked} files for size constraints`],
    failures
  };
}
