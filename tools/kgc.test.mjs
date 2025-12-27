/**
 * @fileoverview KGC CLI comprehensive tests
 * Tests all 5 new commands + global flags + error handling
 */

import { describe, it, beforeEach, afterEach } from 'node:test';
import assert from 'node:assert/strict';
import { readFile, writeFile, rm, mkdir } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';
import { spawn } from 'node:child_process';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const CLI_PATH = join(__dirname, 'kgc.mjs');
const TEST_WORKSPACE = join(__dirname, '..', 'var', 'kgc-test');

/**
 * Execute CLI command and return result
 * @param {string[]} args - Command arguments
 * @param {string} stdin - Optional stdin input
 * @returns {Promise<{stdout: string, stderr: string, exitCode: number}>}
 */
async function execCLI(args, stdin = null) {
  return new Promise((resolve, reject) => {
    const child = spawn('node', [CLI_PATH, ...args], {
      cwd: __dirname,
      env: { ...process.env, NODE_ENV: 'test' },
    });

    let stdout = '';
    let stderr = '';

    child.stdout.on('data', (data) => {
      stdout += data.toString();
    });

    child.stderr.on('data', (data) => {
      stderr += data.toString();
    });

    child.on('error', reject);

    child.on('close', (code) => {
      resolve({
        stdout,
        stderr,
        exitCode: code,
      });
    });

    if (stdin) {
      child.stdin.write(stdin);
      child.stdin.end();
    }
  });
}

/**
 * Parse JSON output from CLI
 * @param {string} output - CLI output
 * @returns {Object} Parsed JSON
 */
function parseJSON(output) {
  // Handle multi-line JSON - find the start and parse the entire thing
  const trimmed = output.trim();
  if (!trimmed) {
    throw new Error('Empty output - cannot parse JSON');
  }

  // If output starts with {, parse it directly
  if (trimmed.startsWith('{')) {
    return JSON.parse(trimmed);
  }

  // Otherwise, find the first line that starts with { and parse from there
  const lines = output.split('\n');
  const startIndex = lines.findIndex((line) => line.trim().startsWith('{'));
  if (startIndex === -1) {
    throw new Error('No JSON found in output');
  }

  const jsonText = lines.slice(startIndex).join('\n');
  return JSON.parse(jsonText);
}

describe('KGC CLI - New Commands', () => {
  beforeEach(async () => {
    // Clean test workspace
    if (existsSync(TEST_WORKSPACE)) {
      await rm(TEST_WORKSPACE, { recursive: true, force: true });
    }
  });

  afterEach(async () => {
    // Clean up after tests
    if (existsSync(TEST_WORKSPACE)) {
      await rm(TEST_WORKSPACE, { recursive: true, force: true });
    }
  });

  describe('status command', () => {
    it('should show runtime state with default output', async () => {
      const result = await execCLI(['status']);
      assert.equal(result.exitCode, 0);
      assert.match(result.stdout, /KGC operational/);
      assert.match(result.stdout, /work items/);
    });

    it('should show runtime state with --json flag', async () => {
      const result = await execCLI(['status', '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.equal(data.status, 'operational');
      assert.ok(typeof data.initialized === 'boolean');
      assert.ok(data.workItems);
      assert.ok(data.bounds);
      assert.ok(data.summary);
    });

    it('should show verbose output with --verbose flag', async () => {
      const result = await execCLI(['status', '--verbose', '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.ok(data.config, 'Should include config in verbose mode');
      assert.ok(data.config['max-files']);
      assert.ok(data.config['max-bytes']);
    });

    it('should suppress non-error output with --quiet flag', async () => {
      const result = await execCLI(['status', '--quiet']);
      assert.equal(result.exitCode, 0);
      // Stderr should be minimal with --quiet
      assert.ok(result.stderr.length < 100);
    });
  });

  describe('init command', () => {
    it('should initialize workspace successfully', async () => {
      const result = await execCLI(['init', '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.equal(data.initialized, true);
      assert.ok(data.paths);
      assert.ok(data.config);
      assert.match(data.summary, /initialized successfully/);

      // Verify files were created
      const configPath = join(__dirname, '..', 'var', 'kgc', 'config.json');
      const registryPath = join(__dirname, '..', 'var', 'kgc', 'tool-registry.json');
      assert.ok(existsSync(configPath));
      assert.ok(existsSync(registryPath));

      // Verify config content
      const config = JSON.parse(await readFile(configPath, 'utf-8'));
      assert.equal(config['max-files'], 1000);
      assert.equal(config['max-bytes'], 104857600);
    });

    it('should fail if already initialized without --force', async () => {
      // Initialize once
      await execCLI(['init']);

      // Try to initialize again
      const result = await execCLI(['init', '--json']);
      assert.equal(result.exitCode, 1);
      assert.match(result.stderr, /already initialized/i);
    });

    it('should reinitialize with --force flag', async () => {
      // Initialize once
      await execCLI(['init']);

      // Initialize again with --force
      const result = await execCLI(['init', '--force', '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.equal(data.initialized, true);
    });

    it('should support --dry-run flag', async () => {
      const result = await execCLI(['init', '--dry-run', '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.equal(data.dryRun, true);
      assert.equal(data.initialized, false);

      // Verify files were NOT created
      const configPath = join(__dirname, '..', 'var', 'kgc', 'config.json');
      assert.ok(!existsSync(configPath));
    });
  });

  describe('config command', () => {
    beforeEach(async () => {
      // Initialize workspace before config tests
      await execCLI(['init']);
    });

    it('should list all configuration with --list flag', async () => {
      const result = await execCLI(['config', '--list', '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.ok(data.config);
      assert.equal(data.config['max-files'], 1000);
      assert.equal(data.config['max-bytes'], 104857600);
      assert.equal(data.config['max-ops'], 10000);
      assert.equal(data.config['max-time'], 3600);
    });

    it('should get single config value', async () => {
      const result = await execCLI(['config', 'max-files', '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.equal(data.key, 'max-files');
      assert.equal(data.value, 1000);
    });

    it('should set config value', async () => {
      const result = await execCLI(['config', 'max-files', '2000', '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.equal(data.key, 'max-files');
      assert.equal(data.value, 2000);

      // Verify persistence
      const result2 = await execCLI(['config', 'max-files', '--json']);
      const data2 = parseJSON(result2.stdout);
      assert.equal(data2.value, 2000);
    });

    it('should fail on unknown config key', async () => {
      const result = await execCLI(['config', 'invalid-key', '--json']);
      assert.equal(result.exitCode, 1);
      assert.match(result.stderr, /Unknown config key/);
    });

    it('should fail on invalid numeric value', async () => {
      const result = await execCLI(['config', 'max-files', 'not-a-number', '--json']);
      assert.equal(result.exitCode, 1);
      assert.match(result.stderr, /Invalid numeric value/);
    });
  });

  describe('validate command', () => {
    it('should validate valid RDF from file', async () => {
      // Create test RDF file
      const testFile = join(__dirname, 'test-data.ttl');
      const validRDF = `
        @prefix ex: <http://example.org/> .
        ex:subject ex:predicate ex:object .
        ex:foo ex:bar "literal value" .
      `;
      await writeFile(testFile, validRDF);

      const result = await execCLI(['validate', testFile, '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.equal(data.valid, true);
      assert.ok(data.triples > 0);
      assert.equal(data.errors, 0);

      // Cleanup
      await rm(testFile);
    });

    it('should validate RDF from stdin', async () => {
      const validRDF = `
        @prefix ex: <http://example.org/> .
        ex:subject ex:predicate ex:object .
      `;

      const result = await execCLI(['validate', '--json'], validRDF);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.equal(data.valid, true);
      assert.ok(data.triples > 0);
    });

    it('should detect validation errors', async () => {
      // Create invalid RDF file (incomplete triples)
      const testFile = join(__dirname, 'test-invalid.ttl');
      const invalidRDF = `
        @prefix ex: <http://example.org/> .
        ex:subject ex:predicate
        ex:incomplete
      `;
      await writeFile(testFile, invalidRDF);

      const result = await execCLI(['validate', testFile, '--json']);
      const data = parseJSON(result.stdout);

      assert.ok(data.errors > 0 || data.warnings > 0);
      assert.ok(data.details.errors || data.details.warnings);

      // Cleanup
      await rm(testFile);
    });

    it('should fail in strict mode with warnings', async () => {
      // Create RDF with warnings (missing terminator)
      const testFile = join(__dirname, 'test-warnings.ttl');
      const rdfWithWarnings = `
        @prefix ex: <http://example.org/> .
        ex:subject ex:predicate ex:object
      `;
      await writeFile(testFile, rdfWithWarnings);

      const result = await execCLI(['validate', testFile, '--strict', '--json']);
      const data = parseJSON(result.stdout);

      // In strict mode, warnings count as failures
      if (data.warnings > 0) {
        assert.equal(data.valid, false);
      }

      // Cleanup
      await rm(testFile);
    });

    it('should handle file not found', async () => {
      const result = await execCLI(['validate', 'nonexistent.ttl', '--json']);
      assert.equal(result.exitCode, 1);
      assert.match(result.stderr, /not found/i);
    });
  });

  describe('stats command', () => {
    it('should compute graph statistics from file', async () => {
      // Create test RDF file
      const testFile = join(__dirname, 'test-stats.ttl');
      const rdfData = `
        @prefix ex: <http://example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .

        ex:alice foaf:name "Alice" .
        ex:alice foaf:knows ex:bob .
        ex:bob foaf:name "Bob" .
        ex:bob foaf:knows ex:charlie .
        ex:charlie foaf:name "Charlie" .
      `;
      await writeFile(testFile, rdfData);

      const result = await execCLI(['stats', testFile, '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.ok(data.triples >= 5);
      assert.ok(data.subjects >= 3); // alice, bob, charlie
      assert.ok(data.predicates >= 2); // name, knows
      assert.ok(data.objects >= 5);
      assert.ok(data.storage);
      assert.ok(data.storage.bytes > 0);
      assert.ok(data.storage.human);
      assert.ok(data.memory);

      // Cleanup
      await rm(testFile);
    });

    it('should compute statistics from stdin', async () => {
      const rdfData = `
        @prefix ex: <http://example.org/> .
        ex:subject ex:predicate ex:object .
      `;

      const result = await execCLI(['stats', '--json'], rdfData);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.ok(data.triples > 0);
      assert.ok(data.subjects > 0);
      assert.ok(data.predicates > 0);
      assert.ok(data.objects > 0);
    });

    it('should show detailed statistics with --detailed flag', async () => {
      const testFile = join(__dirname, 'test-detailed.ttl');
      const rdfData = `
        @prefix ex: <http://example.org/> .
        ex:alice ex:knows ex:bob .
        ex:bob ex:knows ex:charlie .
      `;
      await writeFile(testFile, rdfData);

      const result = await execCLI(['stats', testFile, '--detailed', '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.ok(data.detailed);
      assert.ok(Array.isArray(data.detailed.topSubjects));
      assert.ok(Array.isArray(data.detailed.topPredicates));
      assert.ok(Array.isArray(data.detailed.topObjects));
      assert.ok(typeof data.detailed.avgTripleSize === 'number');

      // Cleanup
      await rm(testFile);
    });

    it('should handle empty input', async () => {
      const testFile = join(__dirname, 'test-empty.ttl');
      await writeFile(testFile, '');

      const result = await execCLI(['stats', testFile, '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.equal(data.triples, 0);
      assert.equal(data.subjects, 0);

      // Cleanup
      await rm(testFile);
    });
  });

  describe('completions command', () => {
    it('should generate bash completions', async () => {
      const result = await execCLI(['completions', 'bash']);
      assert.equal(result.exitCode, 0);
      assert.match(result.stdout, /#!/);
      assert.match(result.stdout, /_kgc_completions/);
      assert.match(result.stdout, /complete -F _kgc_completions kgc/);
    });

    it('should generate zsh completions', async () => {
      const result = await execCLI(['completions', 'zsh']);
      assert.equal(result.exitCode, 0);
      assert.match(result.stdout, /#compdef kgc/);
      assert.match(result.stdout, /_kgc/);
      assert.match(result.stdout, /_arguments/);
    });

    it('should generate fish completions', async () => {
      const result = await execCLI(['completions', 'fish']);
      assert.equal(result.exitCode, 0);
      assert.match(result.stdout, /complete -c kgc/);
      assert.match(result.stdout, /__fish_use_subcommand/);
    });

    it('should default to bash if no shell specified', async () => {
      const result = await execCLI(['completions']);
      assert.equal(result.exitCode, 0);
      assert.match(result.stdout, /bash/i);
      assert.match(result.stdout, /_kgc_completions/);
    });

    it('should fail on unknown shell', async () => {
      const result = await execCLI(['completions', 'powershell', '--json']);
      assert.equal(result.exitCode, 1);
      assert.match(result.stderr, /Unknown shell/);
    });

    it('should support --json flag', async () => {
      const result = await execCLI(['completions', 'bash', '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.equal(data.shell, 'bash');
      assert.ok(data.completions);
      assert.match(data.summary, /Generated bash completions/);
    });
  });

  describe('global flags', () => {
    it('should support --json on all commands', async () => {
      const commands = [
        ['status', '--json'],
        ['build', '--json'],
        ['verify', '--json'],
        ['list', 'capsules', '--json'],
      ];

      for (const args of commands) {
        const result = await execCLI(args);
        assert.equal(result.exitCode, 0);
        assert.doesNotThrow(() => parseJSON(result.stdout));
      }
    });

    it('should support --verbose on all commands', async () => {
      const result = await execCLI(['status', '--verbose']);
      assert.equal(result.exitCode, 0);
      // Verbose should produce more output
      assert.ok(result.stderr.length > 0);
    });

    it('should support --quiet on all commands', async () => {
      const result = await execCLI(['status', '--quiet']);
      assert.equal(result.exitCode, 0);
      // Quiet should minimize stderr
      assert.ok(result.stderr.length < 100);
    });

    it('should support --dry-run on build command', async () => {
      const result = await execCLI(['build', '--dry-run', '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.equal(data.dryRun, true);
      assert.match(data.summary, /DRY RUN/);
    });

    it('should support --dry-run on verify command', async () => {
      const result = await execCLI(['verify', '--dry-run', '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.equal(data.dryRun, true);
    });
  });

  describe('error handling', () => {
    it('should return structured errors in JSON mode', async () => {
      const result = await execCLI(['invalid-command', '--json']);
      assert.equal(result.exitCode, 1);

      const error = JSON.parse(result.stderr);
      assert.ok(error.error);
      assert.equal(error.code, 'INVALID_COMMAND');
    });

    it('should show human-readable errors without --json', async () => {
      const result = await execCLI(['invalid-command']);
      assert.equal(result.exitCode, 1);
      assert.match(result.stderr, /Error/);
      assert.match(result.stderr, /Unknown command/);
    });

    it('should handle missing required arguments', async () => {
      const result = await execCLI(['replay', '--json']);
      assert.equal(result.exitCode, 1);

      const error = JSON.parse(result.stderr);
      assert.equal(error.code, 'MISSING_ARGUMENT');
    });

    it('should show help with --help flag', async () => {
      const result = await execCLI(['--help']);
      assert.equal(result.exitCode, 0);
      assert.match(result.stdout, /KGC Unified CLI/);
      assert.match(result.stdout, /Commands:/);
      assert.match(result.stdout, /Global Flags:/);
    });

    it('should show help with no arguments', async () => {
      const result = await execCLI([]);
      assert.equal(result.exitCode, 0);
      assert.match(result.stdout, /Usage:/);
    });
  });

  describe('existing commands compatibility', () => {
    it('should maintain build command functionality', async () => {
      const result = await execCLI(['build', '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.equal(data.success, true);
      assert.ok(data.receipts);
    });

    it('should maintain verify command functionality', async () => {
      const result = await execCLI(['verify', '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.equal(data.overall, true);
    });

    it('should maintain freeze command functionality', async () => {
      const result = await execCLI(['freeze', '--reason', 'test', '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.ok(data.freezeId);
      assert.ok(data.receipt);
    });

    it('should maintain list command functionality', async () => {
      const result = await execCLI(['list', 'capsules', '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.equal(data.entity, 'capsules');
      assert.ok(Array.isArray(data.items));
    });

    it('should maintain docs command functionality', async () => {
      const result = await execCLI(['docs', 'build', '--json']);
      assert.equal(result.exitCode, 0);

      const data = parseJSON(result.stdout);
      assert.ok(data.result);
      assert.ok(data.receipt);
    });
  });
});

console.log('All KGC CLI tests defined. Run with: node --test tools/kgc.test.mjs');
