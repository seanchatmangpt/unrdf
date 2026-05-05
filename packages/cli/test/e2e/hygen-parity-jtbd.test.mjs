/**
 * @file Hygen Parity JTBD E2E Tests
 * @module cli/test/e2e/hygen-parity-jtbd
 * @description End-to-end Job-To-Be-Done simulations for Hygen parity in sync
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { writeFile, mkdir, rm, readFile, stat } from 'fs/promises';
import { existsSync } from 'fs';
import { join } from 'path';
import { tmpdir } from 'os';
import { runSync } from '../../src/cli/commands/sync/orchestrator.mjs';

const ONTOLOGY = `
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix ex: <http://example.org/schema#> .

ex:User a owl:Class ; rdfs:label "User" .
ex:Post a owl:Class ; rdfs:label "Post" .
ex:Comment a owl:Class ; rdfs:label "Comment" .
`;

describe('Hygen Parity JTBD E2E', () => {
  let testDir, ontologyPath, configPath, outputDir;

  beforeEach(async () => {
    testDir = join(tmpdir(), `hygen-jtbd-${Date.now()}-${Math.random().toString(36).slice(2)}`);
    await mkdir(testDir, { recursive: true });
    ontologyPath = join(testDir, 'schema.ttl');
    configPath = join(testDir, 'unrdf.toml');
    outputDir = join(testDir, 'src');
    await mkdir(outputDir, { recursive: true });
    await writeFile(ontologyPath, ONTOLOGY);
  });

  afterEach(async () => {
    await rm(testDir, { recursive: true, force: true });
  });

  /**
   * JTBD 1: Generate a new file per ontology class
   */
  it('JTBD 1: should scaffold a new service file for each class found in ontology', async () => {
    const templatePath = join(testDir, 'service.njk');
    await writeFile(templatePath, `---
to: services/{{ label | pascalCase }}Service.ts
---
export class {{ label | pascalCase }}Service {
  // Logic for {{ label }}
}
`);

    const config = `
[project]
name = "jtbd-1"
[ontology]
source = "${ontologyPath}"
[generation]
output_dir = "${outputDir}"
[[generation.rules]]
name = "scaffold"
query = "SELECT ?entity ?label WHERE { ?entity a owl:Class . ?entity rdfs:label ?label }"
template = "${templatePath}"
output_file = "services/{{ label | pascalCase }}Service.ts"
`;
    await writeFile(configPath, config);

    const result = await runSync({ config: configPath });
    expect(result.success).toBe(true);
    expect(result.metrics.filesGenerated).toBe(3);
    
    expect(existsSync(join(outputDir, 'services/UserService.ts'))).toBe(true);
    expect(existsSync(join(outputDir, 'services/PostService.ts'))).toBe(true);
    expect(existsSync(join(outputDir, 'services/CommentService.ts'))).toBe(true);

    const content = await readFile(join(outputDir, 'services/UserService.ts'), 'utf-8');
    expect(content).toContain('export class UserService');
  });

  /**
   * JTBD 2: Inject code into an entry point without duplicates
   */
  it('JTBD 2: should register services in index.ts without overwriting or duplicating', async () => {
    const indexPath = join(outputDir, 'services/index.ts');
    await mkdir(join(outputDir, 'services'), { recursive: true });
    await writeFile(indexPath, `// Manual exports\nexport const version = '[VERSION]';\n\n// AUTO-GENERATED EXPORTS`);

    const templatePath = join(testDir, 'register.njk');
    await writeFile(templatePath, `---
to: services/index.ts
inject: true
after: "// AUTO-GENERATED EXPORTS"
skip_if: "/export \\\\* from '\\\\.\\\\/{{ label | pascalCase }}Service'/"
---
export * from './{{ label | pascalCase }}Service';`);

    const config = `
[project]
name = "jtbd-2"
[ontology]
source = "${ontologyPath}"
[generation]
output_dir = "${outputDir}"
[[generation.rules]]
name = "register"
query = "SELECT ?entity ?label WHERE { ?entity a owl:Class . ?entity rdfs:label ?label }"
template = "${templatePath}"
output_file = "services/index.ts"
`;
    await writeFile(configPath, config);

    // First run - should inject 3 lines
    const result1 = await runSync({ config: configPath });
    expect(result1.success).toBe(true);
    const content1 = await readFile(indexPath, 'utf-8');
    expect(content1).toContain("export * from './UserService'");
    expect(content1).toContain("export * from './PostService'");
    expect(content1).toContain("export * from './CommentService'");
    expect(content1.split('\n').filter(l => l.includes('Service')).length).toBe(3);

    // Second run - should skip all (due to skip_if)
    const result2 = await runSync({ config: configPath });
    expect(result2.metrics.filesSkipped).toBe(3);
    const content2 = await readFile(indexPath, 'utf-8');
    expect(content2.split('\n').filter(l => l.includes('Service')).length).toBe(3);
  });

  /**
   * JTBD 3: Generate executable shell script
   */
  it('JTBD 3: should generate an executable CLI tool for a class with correct permissions', async () => {
    const templatePath = join(testDir, 'cli.njk');
    await writeFile(templatePath, `---
to: bin/{{ label | kebabCase }}.sh
chmod: "755"
---
#!/bin/bash
echo "Managing {{ label }}s..."`);

    const config = `
[project]
name = "jtbd-3"
[ontology]
source = "${ontologyPath}"
[generation]
output_dir = "${outputDir}"
[[generation.rules]]
name = "cli"
query = "SELECT ?label WHERE { <http://example.org/schema#User> rdfs:label ?label }"
template = "${templatePath}"
output_file = "bin/{{ label | kebabCase }}.sh"
`;
    await writeFile(configPath, config);

    const result = await runSync({ config: configPath });
    expect(result.success).toBe(true);
    
    const cliPath = join(outputDir, 'bin/user.sh');
    expect(existsSync(cliPath)).toBe(true);
    
    const stats = await stat(cliPath);
    // 0o755 = 493 decimal
    expect(stats.mode & 0o777).toBe(0o755);
  });

  /**
   * JTBD 4: One-time generation (unless_exists)
   */
  it('JTBD 4: should generate a config file only if it does not already exist', async () => {
    const templatePath = join(testDir, 'config.njk');
    await writeFile(templatePath, `---
to: config.json
unless_exists: true
---
{ "generated": true }`);

    const config = `
[project]
name = "jtbd-4"
[ontology]
source = "${ontologyPath}"
[generation]
output_dir = "${outputDir}"
[[generation.rules]]
name = "config"
query = "SELECT ?label WHERE { <http://example.org/schema#User> rdfs:label ?label }"
template = "${templatePath}"
output_file = "config.json"
`;
    await writeFile(configPath, config);

    // Initial run - generates
    await runSync({ config: configPath });
    const content1 = await readFile(join(outputDir, 'config.json'), 'utf-8');
    expect(JSON.parse(content1).generated).toBe(true);

    // User modifies it
    await writeFile(join(outputDir, 'config.json'), '{ "custom": true }');

    // Second run - should skip
    const result2 = await runSync({ config: configPath });
    expect(result2.metrics.filesSkipped).toBe(1);
    const content2 = await readFile(join(outputDir, 'config.json'), 'utf-8');
    expect(JSON.parse(content2).custom).toBe(true);
  });

  /**
   * JTBD 5: Multi-positional injection (before/after/lineAt)
   */
  it('JTBD 5: should perform complex multi-positional injections in a single file', async () => {
    const targetPath = join(outputDir, 'complex.txt');
    await writeFile(targetPath, 'Section 1\nSection 2\nSection 3');

    // Template 1: After Section 1
    const t1Path = join(testDir, 't1.njk');
    await writeFile(t1Path, '---\nto: complex.txt\ninject: true\nafter: "Section 1"\n---\nSub 1.1');

    // Template 2: Before Section 3
    const t2Path = join(testDir, 't2.njk');
    await writeFile(t2Path, '---\nto: complex.txt\ninject: true\nbefore: "Section 3"\n---\nSub 2.9');

    // Template 3: At line 0
    const t3Path = join(testDir, 't3.njk');
    await writeFile(t3Path, '---\nto: complex.txt\ninject: true\nat_line: 0\n---\nHEADER');

    const config = `
[project]
name = "jtbd-5"
[ontology]
source = "${ontologyPath}"
[generation]
output_dir = "${outputDir}"

[[generation.rules]]
name = "after"
query = "SELECT ?label WHERE { <http://example.org/schema#User> rdfs:label ?label }"
template = "${t1Path}"
output_file = "complex.txt"

[[generation.rules]]
name = "before"
query = "SELECT ?label WHERE { <http://example.org/schema#User> rdfs:label ?label }"
template = "${t2Path}"
output_file = "complex.txt"

[[generation.rules]]
name = "atLine"
query = "SELECT ?label WHERE { <http://example.org/schema#User> rdfs:label ?label }"
template = "${t3Path}"
output_file = "complex.txt"
`;
    await writeFile(configPath, config);

    const result = await runSync({ config: configPath });
    expect(result.success).toBe(true);

    const finalContent = await readFile(targetPath, 'utf-8');
    const lines = finalContent.split('\n');
    
    expect(lines[0]).toBe('HEADER');
    expect(lines[1]).toBe('Section 1');
    expect(lines[2]).toBe('Sub 1.1');
    expect(lines[3]).toBe('Section 2');
    expect(lines[4]).toBe('Sub 2.9');
    expect(lines[5]).toBe('Section 3');
  });

  /**
   * JTBD 6: Respect mode: skip_existing from config
   */
  it('JTBD 6: should respect mode: skip_existing from unrdf.toml even if not in template', async () => {
    const targetPath = join(outputDir, 'existing.txt');
    await writeFile(targetPath, 'Old Content');

    const templatePath = join(testDir, 'simple.njk');
    await writeFile(templatePath, '---\nto: existing.txt\n---\nNew Content');

    const config = `
[project]
name = "jtbd-6"
[ontology]
source = "${ontologyPath}"
[generation]
output_dir = "${outputDir}"
[[generation.rules]]
name = "skip"
query = "SELECT ?label WHERE { <http://example.org/schema#User> rdfs:label ?label }"
template = "${templatePath}"
output_file = "existing.txt"
mode = "skip_existing"
`;
    await writeFile(configPath, config);

    const result = await runSync({ config: configPath });
    expect(result.metrics.filesSkipped).toBe(1);
    const content = await readFile(targetPath, 'utf-8');
    expect(content).toBe('Old Content');
  });

  /**
   * JTBD 7: CLI --force overrides everything
   */
  it('JTBD 7: should overwrite file when --force is passed even if mode is skip_existing', async () => {
    const targetPath = join(outputDir, 'forced.txt');
    await writeFile(targetPath, 'Old Content');

    const templatePath = join(testDir, 'simple.njk');
    await writeFile(templatePath, '---\nto: forced.txt\n---\nNew Content');

    const config = `
[project]
name = "jtbd-7"
[ontology]
source = "${ontologyPath}"
[generation]
output_dir = "${outputDir}"
[[generation.rules]]
name = "force"
query = "SELECT ?label WHERE { <http://example.org/schema#User> rdfs:label ?label }"
template = "${templatePath}"
output_file = "forced.txt"
mode = "skip_existing"
`;
    await writeFile(configPath, config);

    // Run with force: true
    const result = await runSync({ config: configPath, force: true });
    expect(result.metrics.filesGenerated).toBe(1);
    const content = await readFile(targetPath, 'utf-8');
    expect(content).toBe('New Content');
  });
});
