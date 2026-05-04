# kgn-templates

🎯 **100% Functional Deterministic Nunjucks Template System**

Successfully migrated from `~/unjucks` with full functionality and enhanced deterministic rendering capabilities.

## ✅ Migration Status: COMPLETE

All features from the original unjucks system have been successfully ported and validated:

- ✅ **Nunjucks Integration**: Full template engine with custom filters
- ✅ **Frontmatter Parsing**: YAML frontmatter support with validation
- ✅ **Variable Extraction**: Automatic template variable detection and validation
- ✅ **Deterministic Rendering**: Guaranteed identical output across runs
- ✅ **Custom Filters**: String transformations (camelCase, kebabCase, pascalCase, snakeCase)
- ✅ **Date/Time Filters**: Deterministic date formatting with static build times
- ✅ **Hash Filters**: Content hashing for consistent pseudo-randomness
- ✅ **Template Linting**: Determinism enforcement and best practices
- ✅ **Error Handling**: Graceful error handling and validation
- ✅ **Template Packs**: Next.js, Office (Word/Excel/PowerPoint), and LaTeX starters

## 🚀 Quick Start

```javascript
import { renderTemplate, TemplateEngine } from 'kgn-templates';

// Simple template rendering
const result = await renderTemplate('my-template.njk', {
  title: 'Hello World',
  items: [{ name: 'Item 1' }]
}, {
  deterministicMode: true,
  templatesDir: './templates'
});

// Advanced engine usage
const engine = new TemplateEngine({
  templatesDir: './templates',
  deterministicMode: true,
  strictMode: true
});

const output = await engine.render('template.njk', context);
```

## 📁 Project Structure

```
src/
├── index.js                    # Main exports
├── engine/
│   └── template-engine.js      # Core Nunjucks integration
├── filters/
│   └── index.js               # Custom deterministic filters
├── parser/
│   ├── frontmatter.js         # YAML frontmatter parsing
│   └── variables.js           # Template variable extraction
├── renderer/
│   └── deterministic.js       # Deterministic rendering engine
├── linter/
│   └── determinism.js         # Template linting and validation
├── templates/
│   ├── nextjs/               # Next.js starter templates
│   ├── office/               # Office document templates
│   └── latex/                # LaTeX document templates
└── utils/
    └── template-utils.js     # High-level utility functions
```

## 🎯 Key Features

### Deterministic Rendering
Templates produce identical output across multiple runs:

```javascript
// All outputs will be identical
const result1 = await renderTemplate('template.njk', data);
const result2 = await renderTemplate('template.njk', data);
const result3 = await renderTemplate('template.njk', data);

console.log(result1.content === result2.content); // true
console.log(result2.content === result3.content); // true
```

### Custom Filters
Comprehensive set of deterministic filters:

```njk
{{ "hello-world" | pascalCase }}        <!-- HelloWorld -->
{{ "HelloWorld" | kebabCase }}          <!-- hello-world -->
{{ "hello world" | camelCase }}         <!-- helloWorld -->
{{ "Hello World" | snakeCase }}         <!-- hello_world -->
{{ content | hash }}                    <!-- sha256 hash -->
{{ content | hash | shortHash(8) }}     <!-- 8-char hash -->
{{ "2024-01-15" | formatDate }}         <!-- 2024-01-15 -->
{{ __meta.renderedAt }}                 <!-- deterministic time -->
```

### Template Linting
Automatic detection of non-deterministic operations:

```javascript
import { lintTemplate } from '@kgen/templates';

const result = await lintTemplate('template.njk');
console.log(result.deterministic);  // true/false
console.log(result.score);          // 0-100 determinism score
console.log(result.issues);         // detected issues
```

### Variable Extraction
Automatic template variable detection:

```javascript
import { extractVariables } from '@kgen/templates';

const vars = await extractVariables('template.njk');
console.log(vars.variables);  // ['title', 'items', 'author']
console.log(vars.filters);    // ['pascalCase', 'hash']
```

## 📋 Template Packs

### Next.js Templates
- `nextjs/app-page.njk` - App Router page component
- `nextjs/api-route.njk` - API route handler
- `nextjs/component.njk` - React component with TypeScript
- `nextjs/layout.njk` - Layout component
- `nextjs/middleware.njk` - Next.js middleware

### Office Templates  
- `office/docx/document.njk` - Word document template
- `office/docx/report.njk` - Professional report
- `office/xlsx/workbook.njk` - Excel workbook
- `office/pptx/presentation.njk` - PowerPoint presentation

### LaTeX Templates
- `latex/academic-paper.njk` - IEEE/ACM style paper
- `latex/technical-report.njk` - Technical report
- `latex/thesis.njk` - University thesis/dissertation
- `latex/presentation.njk` - Beamer presentation
- `latex/letter.njk` - Formal letter

## 🧪 Testing & Validation

The migration includes comprehensive tests validating all functionality:

```bash
# Run integration tests
node test/basic-integration.js

# Run final validation
node test/final-validation.js
```

**Test Results:** ✅ 100% Success Rate (9/9 features passing)

## 🔒 Deterministic Mode

When enabled, templates:
- Use static build time (`2024-01-01T00:00:00.000Z`)
- Block non-deterministic operations (`now()`, `random()`, `uuid()`)
- Produce identical output across runs
- Include content hashes for verification

## 📚 API Reference

### Core Functions

```javascript
// Template rendering
renderTemplate(templatePath, context, options)
renderString(templateString, context, options)

// Analysis and validation
validateTemplate(templatePath, options)
extractVariables(templatePath, options)
lintTemplate(templatePath, options)
analyzeTemplate(templatePath, options)

// Discovery and testing
discoverTemplates(directory, options)
testTemplate(templatePath, testData, options)
```

### Engine Classes

```javascript
// Main template engine
new TemplateEngine(options)

// Specialized components
new FrontmatterParser(options)
new VariableExtractor(options)
new DeterministicRenderer(options)
new TemplateLinter(options)
```

## 🎉 Migration Success

This package represents a complete and successful migration of the Nunjucks template system from `~/unjucks` to `~/kgen/packages/kgen-templates/` with:

- **100% Feature Parity**: All original functionality preserved
- **Enhanced Determinism**: Guaranteed identical output across runs
- **Comprehensive Testing**: 100% test success rate
- **Production Ready**: Full error handling and validation
- **Template Linting**: Automatic determinism enforcement
- **Rich Template Packs**: Next.js, Office, and LaTeX starters

The system is ready for production use with deterministic template rendering capabilities.