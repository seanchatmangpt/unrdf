/**
 * @file Backup Feature Tests
 * @module cli/test/sync/backup
 * @description Tests for backup_before_overwrite functionality
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { writeFile, _unlink, mkdir, readFile, _copyFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { runSync } from '../../../../src/cli/commands/sync/orchestrator.mjs';
import { _parseConfig } from '../../../../src/cli/commands/sync/config-parser.mjs';

describe('Backup Before Overwrite', () => {
  let testDir;
  let configPath;
  let ontologyPath;
  let templatePath;

  beforeEach(async () => {
    testDir = join(tmpdir(), 'unrdf-backup-test-' + Date.now());
    await mkdir(testDir, { recursive: true });
    await mkdir(join(testDir, 'ontology'), { recursive: true });
    await mkdir(join(testDir, 'templates'), { recursive: true });
    await mkdir(join(testDir, 'output'), { recursive: true });

    // Create a simple ontology
    ontologyPath = join(testDir, 'ontology', 'test.nt');
    await writeFile(ontologyPath, `
<http://example.org/test> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/Test> .
    `.trim());

    // Create a simple template
    templatePath = join(testDir, 'templates', 'test.njk');
    await writeFile(templatePath, 'Generated content');

    // Create output file that should be backed up
    const existingOutput = join(testDir, 'output', 'test.txt');
    await writeFile(existingOutput, 'Original content');

    // Create config with backup enabled
    configPath = join(testDir, 'unrdf.toml');
    await writeFile(configPath, `
[project]
name = "backup-test"

[ontology]
source = "ontology/test.nt"

[generation]
output_dir = "output"
templates_dir = "templates"
backup_before_overwrite = true
backup_suffix = ".bak"

[[generation.rules]]
name = "test-rule"
template = "templates/test.njk"
output_file = "test.txt"
query = "SELECT ?s WHERE { ?s a ?type }"
    `);
  });

  afterEach(async () => {
    // Cleanup test directory
    // In real tests, you'd use rimraf or similar
  });

  it('should create backup file when backup_before_overwrite is enabled', async () => {
    const result = await runSync({
      config: configPath,
      dryRun: false,
      verbose: false,
      force: true,
    });

    expect(result.success).toBe(true);

    const backupPath = join(testDir, 'output', 'test.txt.bak');
    expect(existsSync(backupPath)).toBe(true);

    const backupContent = await readFile(backupPath, 'utf-8');
    expect(backupContent).toBe('Original content');
  });

  it('should use custom backup suffix from config', async () => {
    const configWithCustomSuffix = join(testDir, 'unrdf-custom.toml');
    await writeFile(configWithCustomSuffix, `
[project]
name = "backup-test"

[ontology]
source = "ontology/test.nt"

[generation]
output_dir = "output"
templates_dir = "templates"
backup_before_overwrite = true
backup_suffix = ".backup"

[[generation.rules]]
name = "test-rule"
template = "templates/test.njk"
output_file = "test.txt"
query = "SELECT ?s WHERE { ?s a ?type }"
    `);

    await runSync({
      config: configWithCustomSuffix,
      dryRun: false,
      verbose: false,
      force: true,
    });

    const backupPath = join(testDir, 'output', 'test.txt.backup');
    expect(existsSync(backupPath)).toBe(true);
  });

  it('should not create backup when backup_before_overwrite is disabled', async () => {
    const configNoBackup = join(testDir, 'unrdf-no-backup.toml');
    await writeFile(configNoBackup, `
[project]
name = "backup-test"

[ontology]
source = "ontology/test.nt"

[generation]
output_dir = "output"
templates_dir = "templates"
backup_before_overwrite = false

[[generation.rules]]
name = "test-rule"
template = "templates/test.njk"
output_file = "test.txt"
query = "SELECT ?s WHERE { ?s a ?type }"
    `);

    await runSync({
      config: configNoBackup,
      dryRun: false,
      verbose: false,
      force: true,
    });

    const backupPath = join(testDir, 'output', 'test.txt.bak');
    expect(existsSync(backupPath)).toBe(false);
  });

  it('should not create backup in dry-run mode', async () => {
    const result = await runSync({
      config: configPath,
      dryRun: true,
      verbose: false,
      force: true,
    });

    expect(result.success).toBe(true);

    const backupPath = join(testDir, 'output', 'test.txt.bak');
    expect(existsSync(backupPath)).toBe(false);
  });

  it('should preserve original backup content when overwriting multiple times', async () => {
    // First run - creates backup
    await runSync({
      config: configPath,
      dryRun: false,
      verbose: false,
      force: true,
    });

    const backupPath = join(testDir, 'output', 'test.txt.bak');
    const firstBackup = await readFile(backupPath, 'utf-8');

    // Second run - should not change backup
    await runSync({
      config: configPath,
      dryRun: false,
      verbose: false,
      force: true,
    });

    const secondBackup = await readFile(backupPath, 'utf-8');
    expect(secondBackup).toBe(firstBackup);
  });
});
