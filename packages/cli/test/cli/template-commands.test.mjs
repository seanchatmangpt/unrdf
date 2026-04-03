/**
 * Integration tests for template CLI commands (generate, list, query, extract)
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { mkdirSync, rmSync, readFileSync, existsSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';

import { templateCommand } from '../../src/cli/commands/template.mjs';
import { convertCommand } from '../../src/cli/commands/convert.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const FIXTURES = join(__dirname, '../fixtures');

describe('Template CLI commands', () => {
  let testDir;

  beforeEach(() => {
    testDir = join(tmpdir(), 'unrdf-template-cli-' + Date.now());
    mkdirSync(testDir, { recursive: true });
  });

  afterEach(() => {
    if (existsSync(testDir)) {
      rmSync(testDir, { recursive: true, force: true });
    }
  });

  describe('generate', () => {
    it('writes rendered HTML from RDF + template', async () => {
      const personTtl = join(FIXTURES, 'person.ttl');
      const personCard = join(FIXTURES, 'person-card.njk');
      const outDir = join(testDir, 'out');
      mkdirSync(outDir, { recursive: true });

      const sparql = `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?name ?title ?mbox WHERE {
  ?subject foaf:name ?name ;
           foaf:title ?title ;
           foaf:mbox ?mbox .
  FILTER(?subject = <http://example.org/alice>)
}`;

      await templateCommand.subCommands.generate.run({
        args: {
          file: personTtl,
          template: personCard,
          outputDir: outDir,
          sparql,
          dryRun: false,
          force: true,
          batch: false,
        },
      });

      const htmlPath = join(outDir, 'generated', 'alice-smith.html');
      expect(existsSync(htmlPath)).toBe(true);
      const html = readFileSync(htmlPath, 'utf-8');
      expect(html).toContain('person-card');
      expect(html).toContain('Alice Smith');
    });

    it('dry-run does not write files', async () => {
      const personTtl = join(FIXTURES, 'person.ttl');
      const personCard = join(FIXTURES, 'person-card.njk');
      const outDir = join(testDir, 'dry');
      mkdirSync(outDir, { recursive: true });

      const sparql = `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?name ?title ?mbox WHERE {
  ?subject foaf:name ?name ;
           foaf:title ?title ;
           foaf:mbox ?mbox .
  FILTER(?subject = <http://example.org/alice>)
}`;

      await templateCommand.subCommands.generate.run({
        args: {
          file: personTtl,
          template: personCard,
          outputDir: outDir,
          sparql,
          dryRun: true,
          force: true,
          batch: false,
        },
      });

      const htmlPath = join(outDir, 'generated', 'alice-smith.html');
      expect(existsSync(htmlPath)).toBe(false);
    });
  });

  describe('pipeline: N-Triples after convert', () => {
    it('loads .nt from convert and generates output', async () => {
      const personTtl = join(FIXTURES, 'person.ttl');
      const ntPath = join(testDir, 'data.nt');
      await convertCommand.run({
        args: {
          input: personTtl,
          output: ntPath,
        },
      });
      expect(existsSync(ntPath)).toBe(true);

      const personCard = join(FIXTURES, 'person-card.njk');
      const outDir = join(testDir, 'out-nt');
      mkdirSync(outDir, { recursive: true });

      const sparql = `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?name ?title ?mbox WHERE {
  ?subject foaf:name ?name ;
           foaf:title ?title ;
           foaf:mbox ?mbox .
  FILTER(?subject = <http://example.org/alice>)
}`;

      await templateCommand.subCommands.generate.run({
        args: {
          file: ntPath,
          template: personCard,
          outputDir: outDir,
          sparql,
          dryRun: false,
          force: true,
          batch: false,
        },
      });

      const htmlPath = join(outDir, 'generated', 'alice-smith.html');
      expect(existsSync(htmlPath)).toBe(true);
    });
  });

  describe('list', () => {
    it('discovers templates as JSON', async () => {
      const logs = [];
      const orig = console.log;
      console.log = (...a) => {
        logs.push(a.join(' '));
      };
      try {
        await templateCommand.subCommands.list.run({
          args: {
            format: 'json',
          },
        });
      } finally {
        console.log = orig;
      }
      const parsed = JSON.parse(logs[0]);
      expect(Array.isArray(parsed)).toBe(true);
      expect(parsed.length).toBeGreaterThan(0);
    });
  });

  describe('query', () => {
    it('runs SELECT and prints table', async () => {
      const personTtl = join(FIXTURES, 'person.ttl');
      const logs = [];
      const orig = console.log;
      console.log = (...a) => {
        logs.push(a.join(' '));
      };
      try {
        await templateCommand.subCommands.query.run({
          args: {
            file: personTtl,
            sparql: 'SELECT ?s WHERE { ?s a <http://xmlns.com/foaf/0.1/Person> }',
            format: 'table',
          },
        });
      } finally {
        console.log = orig;
      }
      expect(logs.some(l => l.includes('alice') || l.includes('http'))).toBe(true);
    });
  });

  describe('extract', () => {
    it('prints JSON context for subject', async () => {
      const personTtl = join(FIXTURES, 'person.ttl');
      const logs = [];
      const orig = console.log;
      console.log = (...a) => {
        logs.push(a.join(' '));
      };
      try {
        await templateCommand.subCommands.extract.run({
          args: {
            file: personTtl,
            subject: 'http://example.org/alice',
          },
        });
      } finally {
        console.log = orig;
      }
      const obj = JSON.parse(logs[0]);
      expect(obj).toBeDefined();
    });
  });
});
