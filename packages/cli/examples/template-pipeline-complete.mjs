#!/usr/bin/env node
/**
 * @file Complete Template Pipeline Showcase
 * @description Demonstrates all @unrdf/cli generation features: SPARQL context,
 * Hygen directives, batch generation, and custom filters.
 *
 * Run: `node packages/cli/examples/template-pipeline-complete.mjs`
 */

import { mkdirSync, readFileSync, rmSync, existsSync, writeFileSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

import { templateCommand } from '../src/cli/commands/template.mjs';

const __dirname = dirname(fileURLToPath(import.meta.url));
const root = join(__dirname, '..');
const tmp = join(root, '.example-complete-pipeline');

// 1. Setup sample RDF data
const ontologyTtl = `
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix ex: <http://example.org/schema#> .

ex:User a owl:Class ; rdfs:label "User" .
ex:Post a owl:Class ; rdfs:label "Post" .
ex:Comment a owl:Class ; rdfs:label "Comment" .

ex:User ex:hasProperty ex:username, ex:email .
ex:username rdfs:label "Username" ; ex:datatype "string" .
ex:email rdfs:label "Email" ; ex:datatype "string" .
`;

// 2. Setup templates
const scaffoldNjk = `---
to: services/{{ label | pascalCase }}Service.ts
---
export class {{ label | pascalCase }}Service {
  /** Generated service for {{ label }} */
  constructor() {}
}
`;

const registryNjk = `---
to: index.ts
inject: true
after: "// AUTO-GENERATED EXPORTS"
skip_if: 'export \* from "./services/{{ label | pascalCase }}Service"'
---
export * from './services/{{ label | pascalCase }}Service';
`;

async function main() {
  console.log('--- Starting Complete Template Pipeline Showcase ---');

  if (existsSync(tmp)) rmSync(tmp, { recursive: true, force: true });
  mkdirSync(tmp, { recursive: true });
  mkdirSync(join(tmp, 'templates'), { recursive: true });

  const dataPath = join(tmp, 'ontology.ttl');
  const scaffoldPath = join(tmp, 'templates/scaffold.njk');
  const registryPath = join(tmp, 'templates/registry.njk');
  const indexPath = join(tmp, 'src/index.ts');

  writeFileSync(dataPath, ontologyTtl);
  writeFileSync(scaffoldPath, scaffoldNjk);
  writeFileSync(registryPath, registryNjk);
  
  mkdirSync(join(tmp, 'src'), { recursive: true });
  writeFileSync(indexPath, '// Manual Exports\nexport const version = "[VERSION]";\n\n// AUTO-GENERATED EXPORTS\n');

  // 3. Batch generate services (One file per Class)
  console.log('\n1. Batch generating services...');
  await templateCommand.subCommands.generate.run({
    args: {
      file: dataPath,
      template: scaffoldPath,
      outputDir: join(tmp, 'src'),
      batch: true,
      classUri: 'http://www.w3.org/2002/07/owl#Class',
      sparql: 'SELECT DISTINCT ?label WHERE { ?subject a <http://www.w3.org/2002/07/owl#Class> . ?subject <http://www.w3.org/2000/01/rdf-schema#label> ?label }',
      force: true
    }
  });

  // 4. Inject into entry point (Standard rendering per entity)
  console.log('\n2. Injecting into index.ts...');
  await templateCommand.subCommands.generate.run({
    args: {
      file: dataPath,
      template: registryPath,
      outputDir: join(tmp, 'src'),
      sparql: 'SELECT DISTINCT ?label WHERE { ?s a <http://www.w3.org/2002/07/owl#Class> . ?s <http://www.w3.org/2000/01/rdf-schema#label> ?label }',
      // No batch needed here because orchestrator handles inject:true automatically per row
    }
  });

  // 5. Verify results
  console.log('\\n--- Verification ---');
  const generatedFiles = [
    'services/UserService.ts',
    'services/PostService.ts',
    'services/CommentService.ts',
    'index.ts'
  ];

  for (const file of generatedFiles) {
    const fullPath = join(tmp, 'src', file);
    if (existsSync(fullPath)) {
      console.log(`✅ Found: ${file}`);
      if (file === 'index.ts') {
        const content = readFileSync(fullPath, 'utf8');
        const exportCount = (content.match(/export \* from/g) || []).length;
        console.log(`   - index.ts export count: ${exportCount} (Expected: 3)`);
      }
    } else {
      console.log(`❌ Missing: ${file}`);
    }
  }

  console.log('\\nShowcase complete! Check output in:', join(tmp, 'src'));
}

main().catch(e => {
  console.error('\\nERROR:', e.message);
  process.exit(1);
});
