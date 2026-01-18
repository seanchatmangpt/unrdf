/**
 * @file Integration Tests
 * @description End-to-end integration tests for tera-engine
 */

import { describe, it, expect } from 'vitest';
import { createRdfTeraEngine, quickRender } from '../src/api.mjs';

describe('Integration Tests', () => {
  describe('createRdfTeraEngine', () => {
    it('should create engine with all filters', () => {
      const engine = createRdfTeraEngine();
      expect(engine.engine).toBeDefined();
    });

    it('should render with standard filters', async () => {
      const engine = createRdfTeraEngine();
      const result = await engine.render('{{ name | upper }}', {
        context: { name: 'hello' },
      });
      expect(result).toBe('HELLO');
    });

    it('should render with RDF context', async () => {
      const engine = createRdfTeraEngine();
      const result = await engine.render(
        'Triples: {{ rdf.triples | length }}',
        {
          rdf: {
            content: '@prefix ex: <http://example.org/> . ex:s ex:p "o" .',
            format: 'turtle',
          },
        }
      );
      expect(result).toBe('Triples: 1');
    });

    it('should render with TOML context', async () => {
      const engine = createRdfTeraEngine();
      const result = await engine.render(
        'Title: {{ title }}',
        {
          toml: {
            content: 'title = "Test Document"',
          },
        }
      );
      expect(result).toBe('Title: Test Document');
    });

    it('should render with merged TOML and RDF', async () => {
      const engine = createRdfTeraEngine();
      const template = `
Title: {{ title }}
Triples: {{ rdf.triples | length }}
      `.trim();

      const result = await engine.render(template, {
        toml: {
          content: 'title = "Knowledge Graph"',
        },
        rdf: {
          content: `
            @prefix ex: <http://example.org/> .
            ex:Alice ex:knows ex:Bob .
            ex:Bob ex:knows ex:Charlie .
          `,
          format: 'turtle',
        },
      });

      expect(result).toContain('Title: Knowledge Graph');
      expect(result).toContain('Triples: 2');
    });

    it('should register custom filters', async () => {
      const engine = createRdfTeraEngine();
      engine.registerFilter('double', (n) => n * 2);

      const result = await engine.render('{{ count | double }}', {
        context: { count: 21 },
      });
      expect(result).toBe('42');
    });

    it('should use RDF filters on triples', async () => {
      const engine = createRdfTeraEngine();
      const template = `
{% for uri in rdf.subjects() %}
- {{ uri | localname }}
{% endfor %}
      `.trim();

      const result = await engine.render(template, {
        rdf: {
          content: `
            @prefix ex: <http://example.org/> .
            ex:Alice ex:knows ex:Bob .
            ex:Charlie ex:knows ex:Alice .
          `,
          format: 'turtle',
        },
      });

      expect(result).toContain('Alice');
      expect(result).toContain('Charlie');
    });
  });

  describe('quickRender', () => {
    it('should render simple template', async () => {
      const result = await quickRender({
        template: 'Hello {{ name }}!',
        context: { name: 'World' },
      });
      expect(result).toBe('Hello World!');
    });

    it('should render with TOML data', async () => {
      const result = await quickRender({
        template: '{{ app.name }} v{{ app.version }}',
        toml: {
          content: `
            [app]
            name = "MyApp"
            version = "1.0.0"
          `,
        },
      });
      expect(result).toBe('MyApp v1.0.0');
    });

    it('should render with RDF data', async () => {
      const result = await quickRender({
        template: 'Found {{ rdf.triples | length }} triples',
        rdf: {
          content: `
            @prefix ex: <http://example.org/> .
            ex:s1 ex:p1 "o1" .
            ex:s2 ex:p2 "o2" .
          `,
          format: 'turtle',
        },
      });
      expect(result).toBe('Found 2 triples');
    });
  });

  describe('Complete Workflow', () => {
    it('should generate RDF documentation from template', async () => {
      const engine = createRdfTeraEngine();

      const template = `
# {{ project.name }}

Version: {{ project.version }}
Author: {{ project.author }}

## RDF Data

Total Triples: {{ rdf.triples | length }}

### Subjects
{% for subject in rdf.subjects() %}
- {{ subject | localname }}
{% endfor %}

### Predicates
{% for pred in rdf.predicates() %}
- {{ pred | localname }}
{% endfor %}
      `.trim();

      const result = await engine.render(template, {
        toml: {
          content: `
            [project]
            name = "Knowledge Graph Project"
            version = "1.0.0"
            author = "RDF Team"
          `,
        },
        rdf: {
          content: `
            @prefix ex: <http://example.org/> .
            @prefix foaf: <http://xmlns.com/foaf/0.1/> .

            ex:Alice foaf:name "Alice" .
            ex:Alice foaf:knows ex:Bob .
            ex:Bob foaf:name "Bob" .
          `,
          format: 'turtle',
        },
      });

      expect(result).toContain('# Knowledge Graph Project');
      expect(result).toContain('Version: 1.0.0');
      expect(result).toContain('Author: RDF Team');
      expect(result).toContain('Total Triples: 3');
      expect(result).toContain('- Alice');
      expect(result).toContain('- Bob');
      expect(result).toContain('- name');
      expect(result).toContain('- knows');
    });

    it('should process conditional logic with RDF data', async () => {
      const engine = createRdfTeraEngine();

      const template = `
{% if rdf.triples | length %}
Found {{ rdf.triples | length }} triples
{% else %}
No data available
{% endif %}
      `.trim();

      const withData = await engine.render(template, {
        rdf: {
          content: '@prefix ex: <http://example.org/> . ex:s ex:p "o" .',
          format: 'turtle',
        },
      });

      const withoutData = await engine.render(template, {
        rdf: {
          content: '',
          format: 'turtle',
        },
      });

      expect(withData).toContain('Found 1 triples');
      expect(withoutData).toContain('No data available');
    });

    it('should iterate over RDF triples', async () => {
      const engine = createRdfTeraEngine();

      const template = `
{% for triple in rdf.triples %}
{{ triple.subject.value | localname }} {{ triple.predicate.value | localname }} {{ triple.object.value }}
{% endfor %}
      `.trim();

      const result = await engine.render(template, {
        rdf: {
          content: `
            @prefix ex: <http://example.org/> .
            ex:Alice ex:age "30" .
            ex:Bob ex:age "25" .
          `,
          format: 'turtle',
        },
      });

      expect(result).toContain('Alice age 30');
      expect(result).toContain('Bob age 25');
    });

    it('should combine filters in complex expressions', async () => {
      const engine = createRdfTeraEngine();

      const template = `
{{ items | length }} items
First: {{ items | first | upper }}
Last: {{ items | last | upper }}
Joined: {{ items | join(" - ") | upper }}
      `.trim();

      const result = await engine.render(template, {
        context: {
          items: ['alpha', 'beta', 'gamma'],
        },
      });

      expect(result).toContain('3 items');
      expect(result).toContain('First: ALPHA');
      expect(result).toContain('Last: GAMMA');
      expect(result).toContain('Joined: ALPHA - BETA - GAMMA');
    });
  });

  describe('Error Handling', () => {
    it('should handle missing TOML gracefully', async () => {
      const engine = createRdfTeraEngine();
      const result = await engine.render('{{ missing }}', {});
      expect(result).toBe('');
    });

    it('should handle invalid TOML', async () => {
      const engine = createRdfTeraEngine();
      await expect(
        engine.render('test', {
          toml: { content: 'invalid [[' },
        })
      ).rejects.toThrow('Failed to parse TOML');
    });

    it('should handle invalid RDF', async () => {
      const engine = createRdfTeraEngine();
      await expect(
        engine.render('test', {
          rdf: { content: 'invalid @@@', format: 'turtle' },
        })
      ).rejects.toThrow('Failed to load RDF');
    });

    it('should handle strict variable mode', async () => {
      const engine = createRdfTeraEngine();
      await expect(
        engine.render('{{ missing }}', { context: {} }, { strictVariables: true })
      ).rejects.toThrow('Undefined variable');
    });
  });

  describe('Performance', () => {
    it('should handle large TOML files', async () => {
      const engine = createRdfTeraEngine();

      const tomlContent = Array.from({ length: 100 }, (_, i) => `
        [item${i}]
        name = "Item ${i}"
        value = ${i}
      `).join('\n');

      const result = await engine.render('{{ item50.name }}', {
        toml: { content: tomlContent },
      });

      expect(result).toBe('Item 50');
    });

    it('should handle large RDF graphs', async () => {
      const engine = createRdfTeraEngine();

      const triples = Array.from(
        { length: 100 },
        (_, i) => `ex:subject${i} ex:predicate "object${i}" .`
      ).join('\n');

      const rdfContent = `@prefix ex: <http://example.org/> .\n${triples}`;

      const result = await engine.render('{{ rdf.triples | length }}', {
        rdf: { content: rdfContent, format: 'turtle' },
      });

      expect(result).toBe('100');
    });
  });
});
