/**
 * @file Sync Command E2E Integration Tests
 * @module cli/test/e2e/sync-e2e
 * @description End-to-end integration tests for the sync command pipeline
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { writeFile, mkdir, rm, readFile, access, stat } from 'fs/promises';
import { existsSync } from 'fs';
import { join } from 'path';
import { tmpdir } from 'os';
import { runSync } from '../../src/cli/commands/sync/orchestrator.mjs';

const TEST_ONTOLOGY = `
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix ex: <http://example.org/schema#> .
ex:User a owl:Class ; rdfs:label "User" ; rdfs:comment "A user" .
ex:username a owl:DatatypeProperty ; rdfs:domain ex:User ; rdfs:range xsd:string .
ex:email a owl:DatatypeProperty ; rdfs:domain ex:User ; rdfs:range xsd:string .
ex:createdAt a owl:DatatypeProperty ; rdfs:domain ex:User ; rdfs:range xsd:dateTime .
ex:Post a owl:Class ; rdfs:label "Post" ; rdfs:comment "A blog post" .
ex:title a owl:DatatypeProperty ; rdfs:domain ex:Post ; rdfs:range xsd:string .
ex:content a owl:DatatypeProperty ; rdfs:domain ex:Post ; rdfs:range xsd:string .
ex:author a owl:ObjectProperty ; rdfs:domain ex:Post ; rdfs:range ex:User .
`;

const TEST_TEMPLATE = `---
to: entities.mjs
description: Generated entity summary
---
/**
 * @file Entity Summary
 * @generated {{ now | date("YYYY-MM-DD") }}
 */
// Entity count: {{ sparql_results | length }}
{% for row in sparql_results %}
/** {{ row.label | default("No desc") }} */
export const {{ row.label | upper }} = '{{ row.entity }}';
{% endfor %}
export const ALL_ENTITIES = [
{% for row in sparql_results %}
  {{ row.label | upper }},
{% endfor %}
];
`;

const CLASSES_QUERY = `
SELECT ?entity ?label ?comment WHERE {
  ?entity a owl:Class .
  OPTIONAL { ?entity rdfs:label ?label }
  OPTIONAL { ?entity rdfs:comment ?comment }
} ORDER BY ?label
`;

const PROPERTIES_TEMPLATE = `---
to: properties.mjs
description: Generated property definitions
---
{% for row in sparql_results %}
export const PROP_{{ row.propLabel | upper }} = { uri: '{{ row.prop }}', domain: '{{ row.domain }}', range: '{{ row.range }}', label: '{{ row.propLabel }}' };
{% endfor %}
`;

const PROPERTIES_QUERY = `
SELECT ?prop ?propLabel ?domain ?range WHERE {
  ?prop a owl:DatatypeProperty .
  OPTIONAL { ?prop rdfs:label ?propLabel }
  OPTIONAL { ?prop rdfs:domain ?domain }
  OPTIONAL { ?prop rdfs:range ?range }
} ORDER BY ?propLabel
`;

function generateConfig(ontologyPath, templatePath, outputDir, extraRules = '') {
  return `
[project]
name = "e2e-test"
version = "1.0.0"
[ontology]
source = "${ontologyPath}"
format = "turtle"
[generation]
output_dir = "${outputDir}"
[[generation.rules]]
name = "entities"
query = """${CLASSES_QUERY}"""
template = "${templatePath}"
output_file = "entities.mjs"
enabled = true
${extraRules}
`;
}

describe('Sync Command E2E', () => {
  let testDir, ontologyPath, templatePath, configPath, outputDir;

  beforeEach(async () => {
    testDir = join(tmpdir(), `sync-e2e-${Date.now()}-${Math.random().toString(36).slice(2)}`);
    await mkdir(testDir, { recursive: true });
    ontologyPath = join(testDir, 'schema.ttl');
    templatePath = join(testDir, 'templates', 'entities.njk');
    configPath = join(testDir, 'unrdf.toml');
    outputDir = join(testDir, 'src');
    await mkdir(join(testDir, 'templates'), { recursive: true });
    await mkdir(outputDir, { recursive: true });
    await writeFile(ontologyPath, TEST_ONTOLOGY);
    await writeFile(templatePath, TEST_TEMPLATE);
  });

  afterEach(async () => {
    await rm(testDir, { recursive: true, force: true });
  });

  it('should process full pipeline, handle multiple rules, dry-run, and rule filtering', async () => {
    // Full pipeline
    const config = generateConfig(ontologyPath, templatePath, outputDir);
    await writeFile(configPath, config);
    const result = await runSync({ config: configPath, dryRun: false, verbose: false });
    expect(result.success).toBe(true);
    expect(result.metrics.rulesProcessed).toBe(1);
    expect(result.metrics.filesGenerated).toBe(1);
    const content = await readFile(join(outputDir, 'entities.mjs'), 'utf-8');
    expect(content).toContain('export const USER');
    expect(content).toContain('export const POST');

    // Multiple rules
    const propsPath = join(testDir, 'templates', 'properties.njk');
    await writeFile(propsPath, PROPERTIES_TEMPLATE);
    const multiConfig = generateConfig(ontologyPath, templatePath, outputDir,
      `\n[[generation.rules]]\nname = "properties"\nquery = """${PROPERTIES_QUERY}"""\ntemplate = "${propsPath}"\noutput_file = "properties.mjs"\nenabled = true`);
    await writeFile(configPath, multiConfig);
    const multiResult = await runSync({ config: configPath, dryRun: false, verbose: false });
    expect(multiResult.metrics.rulesProcessed).toBe(2);
    expect(multiResult.metrics.filesGenerated).toBe(2);
    expect(existsSync(join(outputDir, 'properties.mjs'))).toBe(true);

    // Dry-run
    const dryConfig = generateConfig(ontologyPath, templatePath, outputDir);
    await writeFile(configPath, dryConfig);
    const dryResult = await runSync({ config: configPath, dryRun: true, verbose: false });
    expect(dryResult.metrics.filesGenerated).toBe(0);
    expect(dryResult.metrics.filesSkipped).toBe(1);
    expect(existsSync(join(outputDir, 'entities-dry.mjs'))).toBe(false);

    // Rule filtering
    await writeFile(configPath, multiConfig);
    const filterResult = await runSync({ config: configPath, rule: 'entities', dryRun: false });
    expect(filterResult.metrics.rulesProcessed).toBe(1);
    expect(filterResult.metrics.filesGenerated).toBe(1);
  });

  it('should fail for missing ontology, missing template, invalid SPARQL, and missing config', async () => {
    // Missing ontology
    const badConfig = `
[project]
name = "bad"
[ontology]
source = "nonexistent.ttl"
format = "turtle"
[generation]
output_dir = "${outputDir}"
[[generation.rules]]
name = "test"
query = "SELECT * WHERE { ?s ?p ?o }"
template = "${templatePath}"
output_file = "out.mjs"`;
    await writeFile(configPath, badConfig);
    const badResult = await runSync({ config: configPath, dryRun: false });
    expect(badResult.success).toBe(false);
    expect(badResult.error).toContain('not found');

    // Missing template
    const badTplConfig = `
[project]
name = "bad-tpl"
[ontology]
source = "${ontologyPath}"
format = "turtle"
[generation]
output_dir = "${outputDir}"
[[generation.rules]]
name = "test"
query = "SELECT * WHERE { ?s a owl:Class }"
template = "${join(testDir, 'nonexistent.njk')}"
output_file = "out.mjs"`;
    await writeFile(configPath, badTplConfig);
    const tplResult = await runSync({ config: configPath, dryRun: false });
    expect(tplResult.success).toBe(false);
    expect(tplResult.metrics.errors).toBeGreaterThan(0);

    // Invalid SPARQL
    const badSparqlConfig = `
[project]
name = "bad-sparql"
[ontology]
source = "${ontologyPath}"
format = "turtle"
[generation]
output_dir = "${outputDir}"
[[generation.rules]]
name = "test"
query = "THIS IS NOT VALID SPARQL"
template = "${templatePath}"
output_file = "out.mjs"`;
    await writeFile(configPath, badSparqlConfig);
    const sparqlResult = await runSync({ config: configPath, dryRun: false });
    expect(sparqlResult.metrics.errors).toBeGreaterThan(0);
    expect(sparqlResult.results[0].status).toBe('error');

    // Missing config
    const missingResult = await runSync({ config: join(testDir, 'nonexistent.toml'), dryRun: false });
    expect(missingResult.success).toBe(false);
    expect(missingResult.error).toBeDefined();
  });

  it('should render custom filters, provide SPARQL results to template, and report accurate metrics', async () => {
    // Custom filters
    const filterTemplatePath = join(testDir, 'templates', 'filters.njk');
    await writeFile(filterTemplatePath, `---
to: filtered.mjs
---
// camelCase: {{ "hello-world" | camelCase }}
// pascalCase: {{ "hello-world" | pascalCase }}
// localName: {{ "http://example.org/schema#User" | localName }}`);
    const filterConfig = `
[project]
name = "filter-test"
[ontology]
source = "${ontologyPath}"
format = "turtle"
[generation]
output_dir = "${outputDir}"
[[generation.rules]]
name = "filters"
query = "SELECT * WHERE { ?s a owl:Class } LIMIT 1"
template = "${filterTemplatePath}"
output_file = "filtered.mjs"`;
    await writeFile(configPath, filterConfig);
    const filterResult = await runSync({ config: configPath, dryRun: false });
    expect(filterResult.success).toBe(true);
    const filterContent = await readFile(join(outputDir, 'filtered.mjs'), 'utf-8');
    expect(filterContent).toContain('camelCase: helloWorld');
    expect(filterContent).toContain('localName: User');

    // SPARQL results in template
    const iterTemplatePath = join(testDir, 'templates', 'iter.njk');
    await writeFile(iterTemplatePath, `---
to: count.mjs
---
Total classes found: {{ sparql_results | length }}
{% for row in sparql_results %}
// Class {{ loop.index }}: {{ row.entity | localName }}
{% endfor %}`);
    const iterConfig = `
[project]
name = "iter-test"
[ontology]
source = "${ontologyPath}"
format = "turtle"
[generation]
output_dir = "${outputDir}"
[[generation.rules]]
name = "iterate"
query = """${CLASSES_QUERY}"""
template = "${iterTemplatePath}"
output_file = "count.mjs"`;
    await writeFile(configPath, iterConfig);
    const iterResult = await runSync({ config: configPath, dryRun: false });
    expect(iterResult.success).toBe(true);
    const iterContent = await readFile(join(outputDir, 'count.mjs'), 'utf-8');
    expect(iterContent).toContain('Total classes found: 2');

    // Accurate metrics
    const metricsConfig = generateConfig(ontologyPath, templatePath, outputDir);
    await writeFile(configPath, metricsConfig);
    const metricsResult = await runSync({ config: configPath, dryRun: false });
    expect(metricsResult.metrics.totalBytes).toBeGreaterThan(0);
    expect(metricsResult.results[0].bytes).toBeGreaterThan(0);
    const fileStat = await stat(join(outputDir, 'entities.mjs'));
    expect(fileStat.size).toBe(metricsResult.results[0].bytes);
  });
});
