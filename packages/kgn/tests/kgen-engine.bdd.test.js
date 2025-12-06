// BDD Tests for KGEN Native Template Engine
import { describe, it, expect, beforeEach } from 'vitest';
import { KGenTemplateEngine } from '../src/core/kgen-engine.js';
import fs from 'fs/promises';
import path from 'path';

// BDD helpers
const feature = (name, fn) => describe(`Feature: ${name}`, fn);
const scenario = (name, fn) => it(`Scenario: ${name}`, fn);
const given = (description, fn) => fn(); // Execute setup immediately
const when = async (description, fn) => await fn(); // Execute action
const then = (description, assertion) => assertion(); // Execute assertion

feature('KGEN Native Template Engine', () => {
  let engine;
  let result;
  let template;
  let context;

  beforeEach(() => {
    engine = new KGenTemplateEngine({
      deterministicMode: true,
      strictMode: true,
      enableAttestation: true
    });
    result = null;
    template = '';
    context = {};
  });

  feature('Pipeline: Plan → Render → Post → Attest', () => {
    scenario('Complete pipeline execution', async () => {
      await given('I have a template with variables and filters', () => {
        template = 'Hello {{ name | upper }}! Generated on {{ date | formatDate }}.';
        context = { name: 'kgen', date: '2024-01-01' };
      });

      await when('I execute the complete pipeline', async () => {
        result = await engine.execute(template, context);
      });

      then('the result should be successful', () => {
        expect(result.success).toBe(true);
      });

      then('the result should contain processed content', () => {
        expect(result.content).toContain('Hello KGEN!');
      });

      then('the result should include attestation', () => {
        expect(result.attestation).toBeDefined();
        expect(result.attestation.attested).toBe(true);
      });

      then('the result should have pipeline metadata', () => {
        expect(result.metadata.phase).toBe('attest');
        expect(result.metadata.attestation).toBeDefined();
      });
    });

    scenario('Individual pipeline phases', async () => {
      await given('I have a simple template', () => {
        template = 'Count: {{ items | count }}';
        context = { items: [1, 2, 3] };
      });

      let plan, renderResult, postResult, attestResult;

      await when('I execute the plan phase', async () => {
        plan = await engine.plan(template, context);
      });

      then('the plan should be successful', () => {
        expect(plan.success).toBe(true);
        expect(plan.variables).toContain('items');
      });

      await when('I execute the render phase', async () => {
        renderResult = await engine.render(plan, context);
      });

      then('the render should be successful', () => {
        expect(renderResult.success).toBe(true);
        expect(renderResult.content).toContain('Count: 3');
      });

      await when('I execute the post phase', async () => {
        postResult = await engine.post(renderResult);
      });

      then('the post processing should be successful', () => {
        expect(postResult.success).toBe(true);
        expect(postResult.metadata.post).toBeDefined();
      });

      await when('I execute the attest phase', async () => {
        attestResult = await engine.attest(postResult);
      });

      then('the attestation should be successful', () => {
        expect(attestResult.attested).toBe(true);
        expect(attestResult.attestation.contentHash).toBeDefined();
      });
    });
  });

  feature('Text Filters', () => {
    scenario('Upper case filter', async () => {
      await given('I have a template with upper filter', () => {
        template = '{{ text | upper }}';
        context = { text: 'hello world' };
      });

      await when('I render the template', async () => {
        result = await engine.renderTemplate(template, context);
      });

      then('the text should be uppercase', () => {
        expect(result).toBe('HELLO WORLD');
      });
    });

    scenario('Lower case filter', async () => {
      await given('I have a template with lower filter', () => {
        template = '{{ text | lower }}';
        context = { text: 'HELLO WORLD' };
      });

      await when('I render the template', async () => {
        result = await engine.renderTemplate(template, context);
      });

      then('the text should be lowercase', () => {
        expect(result).toBe('hello world');
      });
    });

    scenario('Trim filter', async () => {
      await given('I have a template with trim filter', () => {
        template = '{{ text | trim }}';
        context = { text: '  hello world  ' };
      });

      await when('I render the template', async () => {
        result = await engine.renderTemplate(template, context);
      });

      then('the text should be trimmed', () => {
        expect(result).toBe('hello world');
      });
    });

    scenario('Replace filter', async () => {
      await given('I have a template with replace filter', () => {
        template = '{{ text | replace("world", "kgen") }}';
        context = { text: 'hello world' };
      });

      await when('I render the template', async () => {
        result = await engine.renderTemplate(template, context);
      });

      then('the text should have replacements', () => {
        expect(result).toBe('hello kgen');
      });
    });

    scenario('Split and join filters', async () => {
      await given('I have a template with split and join filters', () => {
        template = '{{ text | split(" ") | join("-") }}';
        context = { text: 'hello world kgen' };
      });

      await when('I render the template', async () => {
        result = await engine.renderTemplate(template, context);
      });

      then('the text should be split and joined', () => {
        expect(result).toBe('hello-world-kgen');
      });
    });

    scenario('Slice filter', async () => {
      await given('I have a template with slice filter', () => {
        template = '{{ text | slice 0 5 }}';
        context = { text: 'hello world' };
      });

      await when('I render the template', async () => {
        result = await engine.renderTemplate(template, context);
      });

      then('the text should be sliced', () => {
        expect(result).toBe('hello');
      });
    });
  });

  feature('Data Filters', () => {
    scenario('Default filter', async () => {
      await given('I have a template with default filter', () => {
        template = '{{ missing | default("fallback") }}';
        context = {};
      });

      await when('I render the template', async () => {
        result = await engine.renderTemplate(template, context);
      });

      then('the default value should be used', () => {
        expect(result).toBe('fallback');
      });
    });

    scenario('Unique filter', async () => {
      await given('I have a template with unique filter', () => {
        template = '{{ items | unique | join(",") }}';
        context = { items: [1, 2, 2, 3, 1] };
      });

      await when('I render the template', async () => {
        result = await engine.renderTemplate(template, context);
      });

      then('duplicates should be removed', () => {
        expect(result).toBe('1,2,3');
      });
    });

    scenario('Sort filter', async () => {
      await given('I have a template with sort filter', () => {
        template = '{{ items | sort | join(",") }}';
        context = { items: [3, 1, 2] };
      });

      await when('I render the template', async () => {
        result = await engine.renderTemplate(template, context);
      });

      then('items should be sorted', () => {
        expect(result).toBe('1,2,3');
      });
    });

    scenario('GroupBy filter', async () => {
      await given('I have a template with groupby filter', () => {
        template = '{% for entry in (products | groupBy("category") | items) %}{{ entry[0] }}: {{ entry[1] | length }}{% endfor %}';
        context = {
          products: [
            { name: 'Apple', category: 'fruit' },
            { name: 'Banana', category: 'fruit' },
            { name: 'Carrot', category: 'vegetable' }
          ]
        };
      });

      await when('I render the template', async () => {
        result = await engine.renderTemplate(template, context);
      });

      then('items should be grouped by category', () => {
        expect(result).toContain('fruit: 2');
        expect(result).toContain('vegetable: 1');
      });
    });

    scenario('Map filter', async () => {
      await given('I have a template with map filter', () => {
        template = '{{ products | map("name") | join(", ") }}';
        context = {
          products: [
            { name: 'Apple', price: 1.00 },
            { name: 'Banana', price: 0.50 }
          ]
        };
      });

      await when('I render the template', async () => {
        result = await engine.renderTemplate(template, context);
      });

      then('property values should be extracted', () => {
        expect(result).toBe('Apple, Banana');
      });
    });

    scenario('Sum filter', async () => {
      await given('I have a template with sum filter', () => {
        template = 'Total: {{ products | sum("price") }}';
        context = {
          products: [
            { name: 'Apple', price: 1.00 },
            { name: 'Banana', price: 0.50 }
          ]
        };
      });

      await when('I render the template', async () => {
        result = await engine.renderTemplate(template, context);
      });

      then('values should be summed', () => {
        expect(result).toBe('Total: 1.5');
      });
    });

    scenario('Count filter', async () => {
      await given('I have a template with count filter', () => {
        template = 'Items: {{ products | count }}';
        context = {
          products: [
            { name: 'Apple' },
            { name: 'Banana' }
          ]
        };
      });

      await when('I render the template', async () => {
        result = await engine.renderTemplate(template, context);
      });

      then('items should be counted', () => {
        expect(result).toBe('Items: 2');
      });
    });
  });

  feature('Format Filters', () => {
    scenario('JSON filter', async () => {
      await given('I have a template with json filter', () => {
        template = '{{ data | json }}';
        context = { data: { name: 'test', value: 123 } };
      });

      await when('I render the template', async () => {
        result = await engine.renderTemplate(template, context);
      });

      then('object should be JSON formatted', () => {
        expect(JSON.parse(result)).toEqual({ name: 'test', value: 123 });
      });
    });

    scenario('Markdown filter', async () => {
      await given('I have a template with md filter', () => {
        template = '{{ text | md }}';
        context = { text: 'Hello *world*' };
      });

      await when('I render the template', async () => {
        result = await engine.renderTemplate(template, context);
      });

      then('markdown characters should be escaped', () => {
        expect(result).toBe('Hello \\*world\\*');
      });
    });

    scenario('CSV filter', async () => {
      await given('I have a template with csv filter', () => {
        template = '{{ items | csv }}';
        context = { items: ['apple', 'banana', 'cherry'] };
      });

      await when('I render the template', async () => {
        result = await engine.renderTemplate(template, context);
      });

      then('array should be CSV formatted', () => {
        expect(result).toBe('apple,banana,cherry');
      });
    });
  });

  feature('Deterministic Behavior', () => {
    scenario('Multiple renders produce identical output', async () => {
      await given('I have a template with deterministic content', () => {
        template = 'Hello {{ name }}! Hash: {{ content | casDigest }}';
        context = { name: 'kgen', content: 'test content' };
      });

      let results = [];

      await when('I render the template multiple times', async () => {
        for (let i = 0; i < 3; i++) {
          const renderResult = await engine.execute(template, context);
          results.push(renderResult.content);
        }
      });

      then('all outputs should be identical', () => {
        expect(results[0]).toBe(results[1]);
        expect(results[1]).toBe(results[2]);
        expect(results[0]).toContain('Hello kgen!');
      });
    });

    scenario('Determinism verification', async () => {
      await given('I have a deterministic template', () => {
        template = 'Value: {{ value }}';
        context = { value: 42 };
      });

      await when('I verify determinism', async () => {
        result = await engine.verifyDeterminism(template, context, 5);
      });

      then('the template should be verified as deterministic', () => {
        expect(result.isDeterministic).toBe(true);
        expect(result.successfulRuns).toBe(5);
        expect(result.uniqueOutputs).toBe(1);
      });
    });
  });

  feature('Error Handling', () => {
    scenario('Invalid filter in strict mode', async () => {
      await given('I have a template with invalid filter', () => {
        template = '{{ text | invalidFilter }}';
        context = { text: 'hello' };
      });

      await when('I try to render the template', async () => {
        try {
          result = await engine.renderTemplate(template, context);
        } catch (error) {
          result = { error: error.message };
        }
      });

      then('an error should be thrown', () => {
        expect(result.error).toContain('Unknown filter: invalidFilter');
      });
    });

    scenario('Missing variable in strict mode', async () => {
      await given('I have a template with missing variable', () => {
        template = '{{ missingVar }}';
        context = {};
      });

      await when('I try to plan the template', async () => {
        result = await engine.plan(template, context);
      });

      then('an error should be reported', () => {
        expect(result.success).toBe(false);
        expect(result.error).toContain('Missing required variables');
      });
    });
  });

  feature('Template Expressions', () => {
    scenario('Conditional rendering', async () => {
      await given('I have a template with conditionals', () => {
        template = '{% if showGreeting %}Hello {{ name }}!{% else %}Goodbye!{% endif %}';
        context = { showGreeting: true, name: 'KGEN' };
      });

      await when('I render the template', async () => {
        result = await engine.renderTemplate(template, context);
      });

      then('the correct conditional branch should render', () => {
        expect(result).toBe('Hello KGEN!');
      });
    });

    scenario('Loop rendering', async () => {
      await given('I have a template with loops', () => {
        template = '{% for item in items %}{{ item }}{% if not loop.last %}, {% endif %}{% endfor %}';
        context = { items: ['a', 'b', 'c'] };
      });

      await when('I render the template', async () => {
        result = await engine.renderTemplate(template, context);
      });

      then('all items should be rendered with separators', () => {
        expect(result).toBe('a, b, c');
      });
    });
  });
});