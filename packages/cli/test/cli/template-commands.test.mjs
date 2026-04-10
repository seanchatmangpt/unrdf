/**
 * Integration tests for template CLI commands (generate, list, query, extract)
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { mkdirSync, rmSync, readFileSync, existsSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { dirname } from 'node:path';

import { templateCommand } from '../../src/cli/commands/template.mjs';
import { convertCommand } from '../../src/cli/commands/convert.mjs';

const FIXTURES = join(dirname(new URL(import.meta.url).pathname), '../fixtures');

describe('Template CLI commands', () => {
  let testDir;

  beforeEach(() => {
    testDir = join(tmpdir(), 'unrdf-template-cli-' + Date.now());
    mkdirSync(testDir, { recursive: true });
  });

  afterEach(() => {
    if (existsSync(testDir)) rmSync(testDir, { recursive: true, force: true });
  });

  it('should generate HTML from RDF template and list templates as JSON', async () => {
    const personTtl = join(FIXTURES, 'person.ttl');
    const personCard = join(FIXTURES, 'person-card.njk');
    const outDir = join(testDir, 'out');
    mkdirSync(outDir, { recursive: true });

    const sparql = `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?name ?title ?mbox WHERE {
  ?subject foaf:name ?name ; foaf:title ?title ; foaf:mbox ?mbox .
  FILTER(?subject = <http://example.org/alice>)
}`;

    // Generate
    await templateCommand.subCommands.generate.run({
      args: { file: personTtl, template: personCard, outputDir: outDir, sparql, dryRun: false, force: true, batch: false },
    });
    const htmlPath = join(outDir, 'src', 'alice-smith.html');
    expect(existsSync(htmlPath)).toBe(true);
    const html = readFileSync(htmlPath, 'utf-8');
    expect(html).toContain('Alice Smith');

    // List templates
    const logs = [];
    const orig = console.log;
    console.log = (...a) => logs.push(a.join(' '));
    try {
      await templateCommand.subCommands.list.run({ args: { format: 'json' } });
    } finally {
      console.log = orig;
    }
    const parsed = JSON.parse(logs[0]);
    expect(Array.isArray(parsed)).toBe(true);
    expect(parsed.length).toBeGreaterThan(0);
  });

  it('should support dry-run mode and convert + generate pipeline', async () => {
    const personTtl = join(FIXTURES, 'person.ttl');
    const personCard = join(FIXTURES, 'person-card.njk');
    const outDir = join(testDir, 'dry');
    mkdirSync(outDir, { recursive: true });

    const sparql = `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?name WHERE { ?subject foaf:name ?name ; foaf:title ?title ; foaf:mbox ?mbox .
  FILTER(?subject = <http://example.org/alice>)
}`;

    // Dry-run should not write
    await templateCommand.subCommands.generate.run({
      args: { file: personTtl, template: personCard, outputDir: outDir, sparql, dryRun: true, force: true, batch: false },
    });
    expect(existsSync(join(outDir, 'src', 'alice-smith.html'))).toBe(false);

    // Convert + generate pipeline
    const ntPath = join(testDir, 'data.nt');
    await convertCommand.run({ args: { input: personTtl, output: ntPath } });
    expect(existsSync(ntPath)).toBe(true);

    const outNtDir = join(testDir, 'out-nt');
    mkdirSync(outNtDir, { recursive: true });
    await templateCommand.subCommands.generate.run({
      args: { file: ntPath, template: personCard, outputDir: outNtDir, sparql, dryRun: false, force: true, batch: false },
    });
    expect(existsSync(join(outNtDir, 'src', 'alice-smith.html'))).toBe(true);
  });

  it('should run SPARQL query and extract context for a subject', async () => {
    const personTtl = join(FIXTURES, 'person.ttl');
    const logs = [];
    const orig = console.log;
    console.log = (...a) => logs.push(a.join(' '));
    try {
      // Query
      await templateCommand.subCommands.query.run({
        args: { file: personTtl, sparql: 'SELECT ?s WHERE { ?s a <http://xmlns.com/foaf/0.1/Person> }', format: 'table' },
      });
      expect(logs.some(l => l.includes('alice') || l.includes('http'))).toBe(true);

      // Extract
      logs.length = 0;
      await templateCommand.subCommands.extract.run({ args: { file: personTtl, subject: 'http://example.org/alice' } });
      const obj = JSON.parse(logs[0]);
      expect(obj).toBeDefined();
    } finally {
      console.log = orig;
    }
  });
});
