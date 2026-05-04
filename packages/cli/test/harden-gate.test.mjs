import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { runSync } from '../src/cli/commands/sync/orchestrator.mjs';
import { existsSync, writeFileSync, mkdirSync, rmSync } from 'node:fs';
import { resolve, join } from 'node:path';
import { tmpdir } from 'node:os';

describe('OStar-Unrdf Hardening Gates', () => {
  let tempDir;
  let configPath;
  let ontologyPath;

  beforeEach(() => {
    tempDir = resolve(tmpdir(), `unrdf-harden-test-${Date.now()}`);
    mkdirSync(tempDir, { recursive: true });
    
    ontologyPath = join(tempDir, 'ontology.ttl');
    configPath = join(tempDir, 'unrdf.toml');
    
    writeFileSync(configPath, `
[project]
name = "harden-test"

[ontology]
source = "${ontologyPath}"

[generation]
output_dir = "${join(tempDir, 'lib')}"

[[generation.rules]]
name = "test-rule"
query = "SELECT * WHERE { ?s ?p ?o }"
template = "template.njk"
output_file = "output.txt"
    `);
    
    writeFileSync(join(tempDir, 'template.njk'), `
---
to: output.txt
---
Hello {{ name }}
    `);
  });

  afterEach(() => {
    if (existsSync(tempDir)) {
      rmSync(tempDir, { recursive: true, force: true });
    }
  });

  it('should block projection if --harden is enabled and ontology lacks SpecKit receipt', async () => {
    // Ontology without shacl:conforms
    writeFileSync(ontologyPath, `
@prefix ex: <http://example.org/> .
ex:Subject ex:predicate "Object" .
    `);

    // We expect the command to exit or throw if we use the CLI wrapper, 
    // but here we test the gate logic.
    // Since runSync is called after the gate in main/sync.mjs, we need to test the gate itself.
    
    // Mocking console.error to capture gate failure
    const consoleSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
    const exitSpy = vi.spyOn(process, 'exit').mockImplementation(() => { throw new Error('process.exit'); });

    // Re-importing sync command to use the gate logic
    const { syncCommand } = await import('../src/cli/commands/sync.mjs');
    
    await expect(syncCommand.run({ 
      args: { 
        config: configPath, 
        harden: true,
        'dry-run': false,
        verbose: false,
        force: false,
        output: 'text'
      } 
    })).rejects.toThrow('process.exit');

    expect(consoleSpy).toHaveBeenCalledWith(expect.stringContaining('[Gate 1] Constitutional Integrity Failure'));
    
    consoleSpy.mockRestore();
    exitSpy.mockRestore();
  });

  it('should allow projection if --harden is enabled and ontology has SpecKit receipt', async () => {
    // Ontology WITH shacl:conforms
    writeFileSync(ontologyPath, `
@prefix ex: <http://example.org/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
ex:Subject ex:predicate "Object" ;
    sh:conforms true .
    `);

    const result = await runSync({
      config: configPath,
      harden: true,
      dryRun: false,
      verbose: false,
      force: true,
      output: 'text'
    });

    expect(result.success).toBe(true);
  });

  it('should pass harden context variable to templates', async () => {
    writeFileSync(ontologyPath, `
@prefix ex: <http://example.org/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
ex:Subject ex:predicate "Object" ;
    sh:conforms true .
    `);

    writeFileSync(join(tempDir, 'template.njk'), `
---
to: output.txt
---
Harden is {{ harden }}
    `);

    await runSync({
      config: configPath,
      harden: true,
      dryRun: false,
      verbose: false,
      force: true,
      output: 'text'
    });

    const outputPath = join(tempDir, 'lib', 'output.txt');
    expect(existsSync(outputPath)).toBe(true);
    const content = await import('fs/promises').then(fs => fs.readFile(outputPath, 'utf-8'));
    expect(content).toContain('Harden is true');
  });
});
