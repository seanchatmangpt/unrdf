#!/usr/bin/env node
/**
 * Example: RDF → convert → template generate (same Nunjucks + Hygen stack as `unrdf sync`).
 *
 * Run from repo: `node packages/cli/examples/template-pipeline.mjs`
 */

import { mkdirSync, readFileSync, rmSync, existsSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';

import { convertCommand } from '../src/cli/commands/convert.mjs';
import { templateCommand } from '../src/cli/commands/template.mjs';

const __dirname = dirname(fileURLToPath(import.meta.url));
const root = join(__dirname, '..');
const fixtures = join(root, 'test/fixtures');
const tmp = join(root, '.example-template-pipeline');

const ttl = join(fixtures, 'person.ttl');
const njk = join(fixtures, 'person-card.njk');
const ntOut = join(tmp, 'person.nt');
const genOut = join(tmp, 'generated-html');

const sparql = `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?name ?title ?mbox WHERE {
  ?subject foaf:name ?name ;
           foaf:title ?title ;
           foaf:mbox ?mbox .
  FILTER(?subject = <http://example.org/alice>)
}`;

async function main() {
  if (existsSync(tmp)) rmSync(tmp, { recursive: true, force: true });
  mkdirSync(tmp, { recursive: true });

  await convertCommand.run({ args: { input: ttl, output: ntOut } });

  await templateCommand.subCommands.generate.run({
    args: {
      file: ntOut,
      template: njk,
      outputDir: genOut,
      sparql,
      dryRun: false,
      force: true,
      batch: false,
    },
  });

  const html = join(genOut, 'src', 'alice-smith.html');
  const snippet = existsSync(html)
    ? readFileSync(html, 'utf8').slice(0, 120).replace(/\n/g, ' ')
    : '(missing)';
  console.log('OK:', html, snippet);
}

main().catch(e => {
  console.error(e);
  process.exit(1);
});
