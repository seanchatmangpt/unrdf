#!/usr/bin/env node

/**
 * @file generate-docs.mjs
 * @description Generate Diataxis documentation from TOML + Tera templates
 * @module chatman-equation/generate-docs
 */

import { readFileSync, writeFileSync, mkdirSync } from 'node:fs';
import { resolve, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = dirname(fileURLToPath(import.meta.url));

/**
 * Minimal TOML parser (supports subset needed for our configs)
 */
function parseTOML(content) {
  const result = {};
  const lines = content.split('\n');
  let currentSection = result;
  let currentArray = null;
  let currentArrayItem = null;
  let inMultilineString = false;
  let multilineValue = '';
  let multilineKey = '';

  for (let line of lines) {
    line = line.trim();

    // Skip comments and empty lines (unless in multiline string)
    if (!inMultilineString && (line.startsWith('#') || line === '')) continue;

    // Handle multiline strings
    if (inMultilineString) {
      if (line.endsWith('"""')) {
        multilineValue += line.slice(0, -3);
        setNestedValue(currentArrayItem || currentSection, multilineKey, multilineValue.trim());
        inMultilineString = false;
        multilineValue = '';
        multilineKey = '';
      } else {
        multilineValue += line + '\n';
      }
      continue;
    }

    // Section headers [section]
    if (line.startsWith('[') && line.endsWith(']')) {
      const sectionName = line.slice(1, -1);

      // Array of tables [[array]]
      if (sectionName.startsWith('[') && sectionName.endsWith(']')) {
        const arrayName = sectionName.slice(1, -1);
        if (!result[arrayName]) {
          result[arrayName] = [];
        }
        currentArrayItem = {};
        result[arrayName].push(currentArrayItem);
        currentSection = currentArrayItem;
        currentArray = arrayName;
      } else {
        // Regular table [table]
        const parts = sectionName.split('.');
        currentSection = result;
        for (const part of parts) {
          if (!currentSection[part]) {
            currentSection[part] = {};
          }
          currentSection = currentSection[part];
        }
        currentArray = null;
        currentArrayItem = null;
      }
      continue;
    }

    // Key-value pairs
    const equalIndex = line.indexOf('=');
    if (equalIndex > 0) {
      const key = line.slice(0, equalIndex).trim();
      let value = line.slice(equalIndex + 1).trim();

      // Start of multiline string
      if (value.startsWith('"""')) {
        if (value.endsWith('"""') && value.length > 6) {
          // Single-line triple-quoted string
          value = value.slice(3, -3).trim();
          setNestedValue(currentArrayItem || currentSection, key, value);
        } else {
          // Multi-line string starts
          inMultilineString = true;
          multilineKey = key;
          multilineValue = value.slice(3);
          if (multilineValue.endsWith('\n')) {
            multilineValue += '\n';
          }
        }
        continue;
      }

      // Parse value
      const parsedValue = parseValue(value);
      setNestedValue(currentArrayItem || currentSection, key, parsedValue);
    }
  }

  return result;
}

function parseValue(value) {
  // String
  if (value.startsWith('"') && value.endsWith('"')) {
    return value.slice(1, -1).replace(/\\n/g, '\n').replace(/\\t/g, '\t').replace(/\\"/g, '"');
  }

  if (value.startsWith("'") && value.endsWith("'")) {
    return value.slice(1, -1);
  }

  // Array
  if (value.startsWith('[') && value.endsWith(']')) {
    const items = value
      .slice(1, -1)
      .split(',')
      .map((item) => parseValue(item.trim()))
      .filter((item) => item !== '');
    return items;
  }

  // Boolean
  if (value === 'true') return true;
  if (value === 'false') return false;

  // Number
  if (/^-?\d+(\.\d+)?$/.test(value)) {
    return parseFloat(value);
  }

  // Default: return as string
  return value;
}

function setNestedValue(obj, key, value) {
  const parts = key.split('.');
  let current = obj;

  for (let i = 0; i < parts.length - 1; i++) {
    const part = parts[i];
    if (!current[part]) {
      current[part] = {};
    }
    current = current[part];
  }

  current[parts[parts.length - 1]] = value;
}

// Minimal Tera-like template engine (simplified)
class SimpleTemplateEngine {
  constructor(templateDir) {
    this.templateDir = templateDir;
    this.templates = new Map();
  }

  loadTemplate(name) {
    if (this.templates.has(name)) {
      return this.templates.get(name);
    }

    const templatePath = resolve(this.templateDir, name);
    const content = readFileSync(templatePath, 'utf-8');
    this.templates.set(name, content);
    return content;
  }

  render(templateName, context) {
    let template = this.loadTemplate(templateName);

    // Replace variables: {{ variable }}
    template = template.replace(/\{\{\s*([^}]+?)\s*\}\}/g, (match, path) => {
      const value = this.getNestedValue(context, path.trim());
      return value !== undefined ? String(value) : '';
    });

    // Handle for loops: {% for item in items %}...{% endfor %}
    template = template.replace(
      /\{%\s*for\s+(\w+)\s+in\s+(\w+(?:\.\w+)*)\s*%\}([\s\S]*?)\{%\s*endfor\s*%\}/g,
      (match, itemVar, arrayPath, loopContent) => {
        const array = this.getNestedValue(context, arrayPath);
        if (!Array.isArray(array)) return '';

        return array
          .map((item, index) => {
            const loopContext = {
              ...context,
              [itemVar]: item,
              loop: {
                index: index,
                first: index === 0,
                last: index === array.length - 1,
              },
            };
            return this.renderString(loopContent, loopContext);
          })
          .join('');
      }
    );

    // Handle conditionals: {% if condition %}...{% endif %}
    template = template.replace(
      /\{%\s*if\s+([^%]+?)\s*%\}([\s\S]*?)(?:\{%\s*else\s*%\}([\s\S]*?))?\{%\s*endif\s*%\}/g,
      (match, condition, ifContent, elseContent = '') => {
        const conditionValue = this.evaluateCondition(condition, context);
        return conditionValue ? this.renderString(ifContent, context) : this.renderString(elseContent, context);
      }
    );

    return template;
  }

  renderString(template, context) {
    // Re-apply variable substitution for nested content
    return template.replace(/\{\{\s*([^}]+?)\s*\}\}/g, (match, path) => {
      const value = this.getNestedValue(context, path.trim());
      return value !== undefined ? String(value) : '';
    });
  }

  getNestedValue(obj, path) {
    if (path === 'now') {
      return new Date().toISOString();
    }

    const parts = path.split('.');
    let value = obj;

    for (const part of parts) {
      if (value && typeof value === 'object' && part in value) {
        value = value[part];
      } else {
        return undefined;
      }
    }

    return value;
  }

  evaluateCondition(condition, context) {
    // Simple evaluation: just check if variable exists and is truthy
    const trimmed = condition.trim();

    // Handle negation
    if (trimmed.startsWith('not ')) {
      const variable = trimmed.substring(4).trim();
      const value = this.getNestedValue(context, variable);
      return !value;
    }

    // Handle simple variable check
    const value = this.getNestedValue(context, trimmed);
    return Boolean(value);
  }
}

/**
 * Generate documentation from TOML config
 */
async function generateDocs() {
  console.log('🚀 Generating Chatman Equation documentation...\n');

  const configDir = resolve(__dirname, 'docs-config');
  const templateDir = resolve(__dirname, 'templates');
  const outputBaseDir = resolve(__dirname, '../../docs/diataxis/chatman-equation');

  const engine = new SimpleTemplateEngine(templateDir);

  // Categories to process
  const categories = ['tutorials', 'how-to', 'reference', 'explanation'];

  for (const category of categories) {
    console.log(`📚 Processing ${category}...`);

    const configPath = resolve(configDir, `${category}.toml`);
    let config;

    try {
      const configContent = readFileSync(configPath, 'utf-8');
      config = parseTOML(configContent);
    } catch (error) {
      console.error(`❌ Failed to parse ${category}.toml:`, error.message);
      continue;
    }

    const { metadata } = config;
    const outputDir = resolve(outputBaseDir, category);

    // Create output directory
    mkdirSync(outputDir, { recursive: true });

    // Determine the array key for this category
    let items;
    if (category === 'tutorials') {
      items = config.tutorial || [];
    } else if (category === 'how-to') {
      items = config.guide || [];
    } else if (category === 'reference') {
      items = config.api || [];
    } else if (category === 'explanation') {
      items = config.explanation || [];
    }

    if (!Array.isArray(items)) {
      console.warn(`⚠️  No items found in ${category}.toml`);
      continue;
    }

    console.log(`  Found ${items.length} item(s)`);

    // Generate each document
    for (const item of items) {
      const id = item.id;
      const outputPath = resolve(outputDir, `${id}.md`);

      try {
        const markdown = engine.render(`${metadata.template}`, item);
        writeFileSync(outputPath, markdown, 'utf-8');
        console.log(`  ✓ Generated ${id}.md`);
      } catch (error) {
        console.error(`  ❌ Failed to generate ${id}.md:`, error.message);
      }
    }

    console.log();
  }

  // Generate index/README files
  console.log('📝 Generating index files...');

  const categories_info = {
    tutorials: {
      title: 'Tutorials',
      description: 'Step-by-step learning guides for mastering the Chatman Equation',
    },
    'how-to': {
      title: 'How-To Guides',
      description: 'Task-oriented guides for specific operations',
    },
    reference: {
      title: 'API Reference',
      description: 'Technical reference documentation',
    },
    explanation: {
      title: 'Explanation',
      description: 'Conceptual deep-dives and theoretical foundations',
    },
  };

  for (const [category, info] of Object.entries(categories_info)) {
    const readmePath = resolve(outputBaseDir, category, 'README.md');
    const configPath = resolve(configDir, `${category}.toml`);

    let config;
    try {
      const configContent = readFileSync(configPath, 'utf-8');
      config = parseTOML(configContent);
    } catch {
      continue;
    }

    let items;
    if (category === 'tutorials') {
      items = config.tutorial || [];
    } else if (category === 'how-to') {
      items = config.guide || [];
    } else if (category === 'reference') {
      items = config.api || [];
    } else if (category === 'explanation') {
      items = config.explanation || [];
    }

    const readme = `# ${info.title}

${info.description}

## Available ${info.title}

${items
  .map((item) => {
    const title = item.title;
    const id = item.id;
    const duration = item.duration || item.reading_time || '';
    const difficulty = item.difficulty || item.audience || '';

    return `### [${title}](./${id}.md)

${duration ? `**Duration**: ${duration}` : ''}
${difficulty ? `**Level**: ${difficulty}` : ''}

`;
  })
  .join('\n')}

---

**Last updated**: ${new Date().toISOString().split('T')[0]}
`;

    writeFileSync(readmePath, readme, 'utf-8');
    console.log(`  ✓ Generated ${category}/README.md`);
  }

  // Generate main README
  const mainReadmePath = resolve(outputBaseDir, 'README.md');
  const mainReadme = `# Chatman Equation Documentation

Complete Diataxis documentation suite for the Chatman Equation: **S(t) = ⟨O, t_ns, V, G⟩**

## Documentation Structure

This documentation follows the [Diataxis framework](https://diataxis.fr/), organizing content into four categories:

### 📚 [Tutorials](./tutorials/)

Learning-oriented guides that take you from beginner to expert:
- [Understanding the Chatman Equation](./tutorials/01-understanding-chatman-equation.md)
- [Implementing 4D State in Your Application](./tutorials/02-implementing-4d-state.md)
- [Advanced Time-Travel Queries](./tutorials/03-time-travel-queries.md)

### 🛠️ [How-To Guides](./how-to/)

Task-oriented guides for specific operations:
- [Freeze and Verify State](./how-to/freeze-and-verify.md)
- [Reconstruct State from Git](./how-to/reconstruct-state.md)
- [Resolve Merge Conflicts](./how-to/resolve-conflicts.md)
- [Optimize Time-Travel Performance](./how-to/optimize-time-travel.md)
- [Implement Temporal Audit Trails](./how-to/temporal-audit-trails.md)
- [Set Up Distributed KGC Nodes](./how-to/distributed-setup.md)

### 📖 [Reference](./reference/)

Technical API documentation:
- [KGCStore API](./reference/kgc-store-api.md)
- [Receipt Schema](./reference/receipt-schema.md)
- [Vector Clock API](./reference/vector-clock-api.md)
- [State4D Type](./reference/state-4d-type.md)

### 💡 [Explanation](./explanation/)

Conceptual deep-dives and theory:
- [Theoretical Foundations](./explanation/theoretical-foundations.md)
- [Performance and Scaling](./explanation/performance-scaling.md)
- [Security Model](./explanation/security-model.md)

## The Chatman Equation

\`\`\`math
S(t) = \\langle O, t_{ns}, \\vec{V}, G \\rangle
\`\`\`

Where:
- **O**: Observable RDF state
- **t_ns**: Nanosecond timestamp
- **V**: Vector clock (causality)
- **G**: Git commit SHA (cryptographic proof)

## Quick Start

1. **New to KGC-4D?** Start with [Understanding the Chatman Equation](./tutorials/01-understanding-chatman-equation.md)
2. **Ready to build?** Follow [Implementing 4D State](./tutorials/02-implementing-4d-state.md)
3. **Need specific task?** Check [How-To Guides](./how-to/)
4. **API reference?** See [Reference](./reference/)
5. **Deep theory?** Read [Explanations](./explanation/)

---

**Generated from**: TOML configs + Tera templates
**Last updated**: ${new Date().toISOString().split('T')[0]}
`;

  writeFileSync(mainReadmePath, mainReadme, 'utf-8');
  console.log(`  ✓ Generated main README.md\n`);

  console.log('✅ Documentation generation complete!');
  console.log(`📁 Output directory: ${outputBaseDir}`);
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  generateDocs().catch((error) => {
    console.error('❌ Documentation generation failed:', error);
    process.exit(1);
  });
}

export { generateDocs };
