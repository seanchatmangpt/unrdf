/**
 * @file Sync Command E2E Integration Tests
 * @module cli/test/e2e/sync-e2e
 * @description End-to-end integration tests for the sync command pipeline
 *
 * Tests the full pipeline: config -> ontology -> SPARQL -> template -> output
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { writeFile, mkdir, rm, readFile, access, stat } from 'fs/promises';
import { join } from 'path';
import { tmpdir } from 'os';
import { runSync } from '../../src/cli/commands/sync/orchestrator.mjs';

// =============================================================================
// Test Data: Inline ontology, template, and config
// =============================================================================

/**
 * Minimal test ontology in Turtle format
 * Defines a simple domain with User and Post entities
 */
const TEST_ONTOLOGY = `
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix ex: <http://example.org/schema#> .

# User entity
ex:User a owl:Class ;
    rdfs:label "User" ;
    rdfs:comment "A user in the system" .

ex:username a owl:DatatypeProperty ;
    rdfs:domain ex:User ;
    rdfs:range xsd:string ;
    rdfs:label "username" ;
    rdfs:comment "The user's unique username" .

ex:email a owl:DatatypeProperty ;
    rdfs:domain ex:User ;
    rdfs:range xsd:string ;
    rdfs:label "email" ;
    rdfs:comment "The user's email address" .

ex:createdAt a owl:DatatypeProperty ;
    rdfs:domain ex:User ;
    rdfs:range xsd:dateTime ;
    rdfs:label "createdAt" ;
    rdfs:comment "When the user was created" .

# Post entity
ex:Post a owl:Class ;
    rdfs:label "Post" ;
    rdfs:comment "A blog post" .

ex:title a owl:DatatypeProperty ;
    rdfs:domain ex:Post ;
    rdfs:range xsd:string ;
    rdfs:label "title" ;
    rdfs:comment "The post title" .

ex:content a owl:DatatypeProperty ;
    rdfs:domain ex:Post ;
    rdfs:range xsd:string ;
    rdfs:label "content" ;
    rdfs:comment "The post content" .

ex:author a owl:ObjectProperty ;
    rdfs:domain ex:Post ;
    rdfs:range ex:User ;
    rdfs:label "author" ;
    rdfs:comment "The post author" .
`;

/**
 * Test template that generates a simple entity summary
 * Uses Nunjucks with YAML frontmatter
 */
const TEST_TEMPLATE = `---
to: entities.mjs
description: Generated entity summary from RDF ontology
---
/**
 * @file Entity Summary
 * @generated {{ now | date("YYYY-MM-DD") }}
 * @description Auto-generated from RDF ontology
 */

// Entity count: {{ sparql_results | length }}

{% for row in sparql_results %}
/** {{ row["?label"] }} - {{ row["?comment"] | default("No description") }} */
export const {{ row["?label"] | upper }} = '{{ row["?entity"] }}';
{% endfor %}

export const ALL_ENTITIES = [
{% for row in sparql_results %}
  {{ row["?label"] | upper }},
{% endfor %}
];
`;

/**
 * SPARQL query to extract classes from ontology
 */
const CLASSES_QUERY = `
SELECT ?entity ?label ?comment
WHERE {
  ?entity a owl:Class .
  OPTIONAL { ?entity rdfs:label ?label }
  OPTIONAL { ?entity rdfs:comment ?comment }
}
ORDER BY ?label
`;

/**
 * Generate ggen.toml configuration content
 * @param {string} ontologyPath - Path to ontology file
 * @param {string} templatePath - Path to template file
 * @param {string} outputDir - Output directory
 * @returns {string} TOML configuration content
 */
function generateConfig(ontologyPath, templatePath, outputDir) {
  return `
[project]
name = "e2e-test-project"
version = "1.0.0"
description = "E2E test project for sync command"

[ontology]
source = "${ontologyPath}"
format = "turtle"

[generation]
output_dir = "${outputDir}"

[[generation.rules]]
name = "entities"
description = "Generate entity constants"
query = """${CLASSES_QUERY}"""
template = "${templatePath}"
output_file = "entities.mjs"
enabled = true
`;
}

/**
 * Template for testing properties extraction
 */
const PROPERTIES_TEMPLATE = `---
to: properties.mjs
description: Generated property definitions
---
/**
 * @file Property Definitions
 * @generated {{ now | date("YYYY-MM-DD") }}
 */

{% for row in sparql_results %}
export const PROP_{{ row["?propLabel"] | upper }} = {
  uri: '{{ row["?prop"] }}',
  domain: '{{ row["?domain"] }}',
  range: '{{ row["?range"] }}',
  label: '{{ row["?propLabel"] }}',
};
{% endfor %}
`;

const PROPERTIES_QUERY = `
SELECT ?prop ?propLabel ?domain ?range
WHERE {
  ?prop a owl:DatatypeProperty .
  OPTIONAL { ?prop rdfs:label ?propLabel }
  OPTIONAL { ?prop rdfs:domain ?domain }
  OPTIONAL { ?prop rdfs:range ?range }
}
ORDER BY ?propLabel
`;

// =============================================================================
// Test Suite
// =============================================================================

describe('Sync Command E2E', () => {
  let testDir;
  let ontologyPath;
  let templatePath;
  let configPath;
  let outputDir;

  beforeEach(async () => {
    // Create unique temp directory for each test
    testDir = join(tmpdir(), `sync-e2e-${Date.now()}-${Math.random().toString(36).slice(2)}`);
    await mkdir(testDir, { recursive: true });

    // Set up paths
    ontologyPath = join(testDir, 'schema.ttl');
    templatePath = join(testDir, 'templates', 'entities.njk');
    configPath = join(testDir, 'ggen.toml');
    outputDir = join(testDir, 'generated');

    // Create directory structure
    await mkdir(join(testDir, 'templates'), { recursive: true });
    await mkdir(outputDir, { recursive: true });

    // Write test files
    await writeFile(ontologyPath, TEST_ONTOLOGY);
    await writeFile(templatePath, TEST_TEMPLATE);
  });

  afterEach(async () => {
    // Clean up temp directory
    await rm(testDir, { recursive: true, force: true });
  });

  describe('Full Pipeline Integration', () => {
    it('should process config, ontology, SPARQL query, and generate output', async () => {
      // Arrange
      const config = generateConfig(ontologyPath, templatePath, outputDir);
      await writeFile(configPath, config);

      // Act
      const result = await runSync({
        config: configPath,
        dryRun: false,
        verbose: false,
      });

      // Assert - verify success
      expect(result.success).toBe(true);
      expect(result.metrics.rulesProcessed).toBe(1);
      expect(result.metrics.filesGenerated).toBe(1);
      expect(result.metrics.errors).toBe(0);

      // Assert - verify output file exists
      const outputPath = join(outputDir, 'entities.mjs');
      await expect(access(outputPath)).resolves.toBeUndefined();

      // Assert - verify content patterns
      const content = await readFile(outputPath, 'utf-8');
      expect(content).toContain('@file Entity Summary');
      expect(content).toContain('@generated');
      expect(content).toContain('// Entity count:');
      expect(content).toContain('export const USER');
      expect(content).toContain('export const POST');
      expect(content).toContain('export const ALL_ENTITIES');
      expect(content).toContain('http://example.org/schema#User');
      expect(content).toContain('http://example.org/schema#Post');
    });

    it('should handle multiple generation rules', async () => {
      // Arrange - add second template and rule
      const propertiesTemplatePath = join(testDir, 'templates', 'properties.njk');
      await writeFile(propertiesTemplatePath, PROPERTIES_TEMPLATE);

      const multiRuleConfig = `
[project]
name = "multi-rule-test"
version = "1.0.0"

[ontology]
source = "${ontologyPath}"
format = "turtle"

[generation]
output_dir = "${outputDir}"

[[generation.rules]]
name = "entities"
description = "Generate entity constants"
query = """${CLASSES_QUERY}"""
template = "${templatePath}"
output_file = "entities.mjs"
enabled = true

[[generation.rules]]
name = "properties"
description = "Generate property definitions"
query = """${PROPERTIES_QUERY}"""
template = "${propertiesTemplatePath}"
output_file = "properties.mjs"
enabled = true
`;
      await writeFile(configPath, multiRuleConfig);

      // Act
      const result = await runSync({
        config: configPath,
        dryRun: false,
        verbose: false,
      });

      // Assert
      expect(result.success).toBe(true);
      expect(result.metrics.rulesProcessed).toBe(2);
      expect(result.metrics.filesGenerated).toBe(2);

      // Verify both files exist
      const entitiesPath = join(outputDir, 'entities.mjs');
      const propertiesPath = join(outputDir, 'properties.mjs');
      await expect(access(entitiesPath)).resolves.toBeUndefined();
      await expect(access(propertiesPath)).resolves.toBeUndefined();

      // Verify properties content
      const propsContent = await readFile(propertiesPath, 'utf-8');
      expect(propsContent).toContain('PROP_USERNAME');
      expect(propsContent).toContain('PROP_EMAIL');
      expect(propsContent).toContain('PROP_TITLE');
    });

    it('should support dry-run mode without writing files', async () => {
      // Arrange
      const config = generateConfig(ontologyPath, templatePath, outputDir);
      await writeFile(configPath, config);

      // Act
      const result = await runSync({
        config: configPath,
        dryRun: true,
        verbose: false,
      });

      // Assert
      expect(result.success).toBe(true);
      expect(result.metrics.rulesProcessed).toBe(1);
      expect(result.metrics.filesSkipped).toBe(1);
      expect(result.metrics.filesGenerated).toBe(0);
      expect(result.results[0].status).toBe('dry-run');

      // Verify output file was NOT created
      const outputPath = join(outputDir, 'entities.mjs');
      await expect(access(outputPath)).rejects.toThrow();
    });

    it('should filter rules by name', async () => {
      // Arrange - config with multiple rules
      const propertiesTemplatePath = join(testDir, 'templates', 'properties.njk');
      await writeFile(propertiesTemplatePath, PROPERTIES_TEMPLATE);

      const multiRuleConfig = `
[project]
name = "filter-test"
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

[[generation.rules]]
name = "properties"
query = """${PROPERTIES_QUERY}"""
template = "${propertiesTemplatePath}"
output_file = "properties.mjs"
`;
      await writeFile(configPath, multiRuleConfig);

      // Act - run only the "entities" rule
      const result = await runSync({
        config: configPath,
        rule: 'entities',
        dryRun: false,
      });

      // Assert
      expect(result.success).toBe(true);
      expect(result.metrics.rulesProcessed).toBe(1);
      expect(result.metrics.filesGenerated).toBe(1);

      // Only entities.mjs should exist
      const entitiesPath = join(outputDir, 'entities.mjs');
      const propertiesPath = join(outputDir, 'properties.mjs');
      await expect(access(entitiesPath)).resolves.toBeUndefined();
      await expect(access(propertiesPath)).rejects.toThrow();
    });
  });

  describe('Error Handling', () => {
    it('should fail gracefully for missing ontology file', async () => {
      // Arrange - config pointing to non-existent ontology
      const badConfig = `
[project]
name = "bad-ontology"
version = "1.0.0"

[ontology]
source = "nonexistent.ttl"
format = "turtle"

[generation]
output_dir = "${outputDir}"

[[generation.rules]]
name = "test"
query = "SELECT * WHERE { ?s ?p ?o }"
template = "${templatePath}"
output_file = "out.mjs"
`;
      await writeFile(configPath, badConfig);

      // Act
      const result = await runSync({
        config: configPath,
        dryRun: false,
      });

      // Assert
      expect(result.success).toBe(false);
      expect(result.error).toContain('not found');
    });

    it('should fail gracefully for missing template file', async () => {
      // Arrange
      const config = `
[project]
name = "bad-template"
version = "1.0.0"

[ontology]
source = "${ontologyPath}"
format = "turtle"

[generation]
output_dir = "${outputDir}"

[[generation.rules]]
name = "test"
query = "SELECT * WHERE { ?s a owl:Class }"
template = "${join(testDir, 'nonexistent.njk')}"
output_file = "out.mjs"
`;
      await writeFile(configPath, config);

      // Act
      const result = await runSync({
        config: configPath,
        dryRun: false,
      });

      // Assert
      expect(result.success).toBe(false);
      expect(result.metrics.errors).toBeGreaterThan(0);
    });

    it('should fail gracefully for invalid SPARQL query', async () => {
      // Arrange
      const config = `
[project]
name = "bad-sparql"
version = "1.0.0"

[ontology]
source = "${ontologyPath}"
format = "turtle"

[generation]
output_dir = "${outputDir}"

[[generation.rules]]
name = "test"
query = "THIS IS NOT VALID SPARQL"
template = "${templatePath}"
output_file = "out.mjs"
`;
      await writeFile(configPath, config);

      // Act
      const result = await runSync({
        config: configPath,
        dryRun: false,
      });

      // Assert - should have error for the rule
      expect(result.metrics.errors).toBeGreaterThan(0);
      expect(result.results[0].status).toBe('error');
    });

    it('should fail for missing config file', async () => {
      // Act
      const result = await runSync({
        config: join(testDir, 'nonexistent.toml'),
        dryRun: false,
      });

      // Assert
      expect(result.success).toBe(false);
      expect(result.error).toBeDefined();
    });
  });

  describe('Template Rendering', () => {
    it('should apply custom filters correctly', async () => {
      // Arrange - template using various filters
      const filterTemplate = `---
to: filtered.mjs
---
// camelCase: {{ "hello-world" | camelCase }}
// pascalCase: {{ "hello-world" | pascalCase }}
// snakeCase: {{ "helloWorld" | snakeCase }}
// kebabCase: {{ "HelloWorld" | kebabCase }}
// localName: {{ "http://example.org/schema#User" | localName }}
`;
      const filterTemplatePath = join(testDir, 'templates', 'filters.njk');
      await writeFile(filterTemplatePath, filterTemplate);

      const config = `
[project]
name = "filter-test"
version = "1.0.0"

[ontology]
source = "${ontologyPath}"
format = "turtle"

[generation]
output_dir = "${outputDir}"

[[generation.rules]]
name = "filters"
query = "SELECT * WHERE { ?s a owl:Class } LIMIT 1"
template = "${filterTemplatePath}"
output_file = "filtered.mjs"
`;
      await writeFile(configPath, config);

      // Act
      const result = await runSync({
        config: configPath,
        dryRun: false,
      });

      // Assert
      expect(result.success).toBe(true);

      const content = await readFile(join(outputDir, 'filtered.mjs'), 'utf-8');
      expect(content).toContain('camelCase: helloWorld');
      expect(content).toContain('pascalCase: HelloWorld');
      expect(content).toContain('snakeCase: hello_world');
      expect(content).toContain('kebabCase: hello-world');
      expect(content).toContain('localName: User');
    });

    it('should provide SPARQL results to template context', async () => {
      // Arrange - template that iterates over results
      const iterTemplate = `---
to: count.mjs
---
// Total classes found: {{ sparql_results | length }}
{% for row in sparql_results %}
// Class {{ loop.index }}: {{ row["?entity"] | localName }}
{% endfor %}
`;
      const iterTemplatePath = join(testDir, 'templates', 'iter.njk');
      await writeFile(iterTemplatePath, iterTemplate);

      const config = `
[project]
name = "iter-test"
version = "1.0.0"

[ontology]
source = "${ontologyPath}"
format = "turtle"

[generation]
output_dir = "${outputDir}"

[[generation.rules]]
name = "iterate"
query = """${CLASSES_QUERY}"""
template = "${iterTemplatePath}"
output_file = "count.mjs"
`;
      await writeFile(configPath, config);

      // Act
      const result = await runSync({
        config: configPath,
        dryRun: false,
      });

      // Assert
      expect(result.success).toBe(true);

      const content = await readFile(join(outputDir, 'count.mjs'), 'utf-8');
      expect(content).toContain('Total classes found: 2');
      expect(content).toContain('Class 1:');
      expect(content).toContain('Class 2:');
    });
  });

  describe('Output Metrics', () => {
    it('should report accurate file sizes in bytes', async () => {
      // Arrange
      const config = generateConfig(ontologyPath, templatePath, outputDir);
      await writeFile(configPath, config);

      // Act
      const result = await runSync({
        config: configPath,
        dryRun: false,
      });

      // Assert
      expect(result.success).toBe(true);
      expect(result.metrics.totalBytes).toBeGreaterThan(0);
      expect(result.results[0].bytes).toBeGreaterThan(0);

      // Verify actual file size matches reported bytes
      const outputPath = join(outputDir, 'entities.mjs');
      const fileStat = await stat(outputPath);
      expect(fileStat.size).toBe(result.results[0].bytes);
    });

    it('should report accurate processing duration', async () => {
      // Arrange
      const config = generateConfig(ontologyPath, templatePath, outputDir);
      await writeFile(configPath, config);

      // Act
      const result = await runSync({
        config: configPath,
        dryRun: false,
      });

      // Assert
      expect(result.totalDuration).toBeGreaterThan(0);
      expect(result.totalDuration).toBeLessThan(5000); // Should complete under 5s
      expect(result.results[0].duration).toBeGreaterThan(0);
    });
  });

  describe('Disabled Rules', () => {
    it('should skip disabled rules', async () => {
      // Arrange - create templates without frontmatter 'to:' so output_file takes effect
      const enabledTemplatePath = join(testDir, 'templates', 'enabled.njk');
      const disabledTemplatePath = join(testDir, 'templates', 'disabled.njk');

      const simpleTemplate = `---
description: Simple template without output path
---
// Generated content
export const GENERATED = true;
`;
      await writeFile(enabledTemplatePath, simpleTemplate);
      await writeFile(disabledTemplatePath, simpleTemplate);

      const config = `
[project]
name = "disabled-test"
version = "1.0.0"

[ontology]
source = "${ontologyPath}"
format = "turtle"

[generation]
output_dir = "${outputDir}"

[[generation.rules]]
name = "enabled-rule"
query = """${CLASSES_QUERY}"""
template = "${enabledTemplatePath}"
output_file = "enabled.mjs"
enabled = true

[[generation.rules]]
name = "disabled-rule"
query = """${CLASSES_QUERY}"""
template = "${disabledTemplatePath}"
output_file = "disabled.mjs"
enabled = false
`;
      await writeFile(configPath, config);

      // Act
      const result = await runSync({
        config: configPath,
        dryRun: false,
      });

      // Assert
      expect(result.success).toBe(true);
      expect(result.metrics.rulesProcessed).toBe(1);
      expect(result.metrics.filesGenerated).toBe(1);

      // Only enabled rule should generate output
      await expect(access(join(outputDir, 'enabled.mjs'))).resolves.toBeUndefined();
      await expect(access(join(outputDir, 'disabled.mjs'))).rejects.toThrow();
    });
  });

  describe('JSON Output Mode', () => {
    it('should support JSON output format', async () => {
      // Arrange
      const config = generateConfig(ontologyPath, templatePath, outputDir);
      await writeFile(configPath, config);

      // Capture console.log output
      const logs = [];
      const originalLog = console.log;
      console.log = (...args) => logs.push(args.join(' '));

      try {
        // Act
        const result = await runSync({
          config: configPath,
          dryRun: false,
          output: 'json',
        });

        // Assert
        expect(result.success).toBe(true);

        // Find JSON output in logs
        const jsonLog = logs.find((log) => log.includes('"success"'));
        expect(jsonLog).toBeDefined();

        const parsed = JSON.parse(jsonLog);
        expect(parsed.success).toBe(true);
        expect(parsed.metrics).toBeDefined();
        expect(parsed.results).toBeInstanceOf(Array);
      } finally {
        console.log = originalLog;
      }
    });
  });
});

describe('Sync Command Edge Cases', () => {
  let testDir;

  beforeEach(async () => {
    testDir = join(tmpdir(), `sync-edge-${Date.now()}-${Math.random().toString(36).slice(2)}`);
    await mkdir(testDir, { recursive: true });
  });

  afterEach(async () => {
    await rm(testDir, { recursive: true, force: true });
  });

  it('should handle empty ontology (no triples)', async () => {
    // Arrange
    const emptyOntology = `
@prefix ex: <http://example.org/> .
# Empty ontology with just prefixes
`;
    const ontologyPath = join(testDir, 'empty.ttl');
    await writeFile(ontologyPath, emptyOntology);

    const template = `---
to: empty-result.mjs
---
// Results: {{ sparql_results | length }}
export const ITEMS = [];
`;
    const templatePath = join(testDir, 'empty.njk');
    await writeFile(templatePath, template);

    const config = `
[project]
name = "empty-test"
version = "1.0.0"

[ontology]
source = "${ontologyPath}"
format = "turtle"

[generation]
output_dir = "${testDir}/out"

[[generation.rules]]
name = "empty"
query = "SELECT * WHERE { ?s a owl:Class }"
template = "${templatePath}"
output_file = "empty-result.mjs"
`;
    const configPath = join(testDir, 'ggen.toml');
    await writeFile(configPath, config);

    // Act
    const result = await runSync({
      config: configPath,
      dryRun: false,
    });

    // Assert
    expect(result.success).toBe(true);
    const content = await readFile(join(testDir, 'out', 'empty-result.mjs'), 'utf-8');
    expect(content).toContain('Results: 0');
  });

  it('should handle special characters in template output', async () => {
    // Arrange
    const ontologyPath = join(testDir, 'special.ttl');
    await writeFile(
      ontologyPath,
      `
@prefix ex: <http://example.org/> .
ex:Test a <http://www.w3.org/2002/07/owl#Class> ;
    <http://www.w3.org/2000/01/rdf-schema#label> "Test's \\"quoted\\" <entity>" .
`
    );

    const template = `---
to: special.mjs
---
// Special chars test
export const LABEL = {{ '"Test label"' | quote }};
`;
    const templatePath = join(testDir, 'special.njk');
    await writeFile(templatePath, template);

    const config = `
[project]
name = "special-test"
version = "1.0.0"

[ontology]
source = "${ontologyPath}"
format = "turtle"

[generation]
output_dir = "${testDir}/out"

[[generation.rules]]
name = "special"
query = "SELECT * WHERE { ?s a <http://www.w3.org/2002/07/owl#Class> } LIMIT 1"
template = "${templatePath}"
output_file = "special.mjs"
`;
    const configPath = join(testDir, 'ggen.toml');
    await writeFile(configPath, config);

    // Act
    const result = await runSync({
      config: configPath,
      dryRun: false,
    });

    // Assert
    expect(result.success).toBe(true);
    const content = await readFile(join(testDir, 'out', 'special.mjs'), 'utf-8');
    expect(content).toContain('LABEL');
  });

  it('should create nested output directories automatically', async () => {
    // Arrange
    const ontologyPath = join(testDir, 'schema.ttl');
    await writeFile(ontologyPath, TEST_ONTOLOGY);

    const template = `---
to: deep/nested/path/result.mjs
---
export const GENERATED = true;
`;
    const templatePath = join(testDir, 'nested.njk');
    await writeFile(templatePath, template);

    const config = `
[project]
name = "nested-test"
version = "1.0.0"

[ontology]
source = "${ontologyPath}"
format = "turtle"

[generation]
output_dir = "${testDir}/out"

[[generation.rules]]
name = "nested"
query = "SELECT * WHERE { ?s a owl:Class } LIMIT 1"
template = "${templatePath}"
output_file = "deep/nested/path/result.mjs"
`;
    const configPath = join(testDir, 'ggen.toml');
    await writeFile(configPath, config);

    // Act
    const result = await runSync({
      config: configPath,
      dryRun: false,
    });

    // Assert
    expect(result.success).toBe(true);
    const outputPath = join(testDir, 'out', 'deep', 'nested', 'path', 'result.mjs');
    await expect(access(outputPath)).resolves.toBeUndefined();
  });
});

// Inline test ontology constant exported for reuse
const TEST_ONTOLOGY_EXPORT = TEST_ONTOLOGY;
export { TEST_ONTOLOGY_EXPORT as TEST_ONTOLOGY };
