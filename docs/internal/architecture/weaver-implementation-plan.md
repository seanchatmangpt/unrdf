# Weaver Codegen Implementation Plan
## Phase-by-Phase Execution Guide

**Date**: 2025-10-01
**Author**: Coder Agent (Hive Mind Swarm)
**Session**: swarm-1759365567937-1yyvifanp
**Dependencies**: Architect (design review), Tester (validation framework)

---

## Quick Reference

### Files Created
1. `/Users/sac/unrdf/docs/architecture/weaver-codegen-strategy.md` - Complete strategy document
2. `/Users/sac/unrdf/weaver.config.mjs` - Configuration file with 80/20 priorities
3. `/Users/sac/unrdf/templates/schemas/object.mjs.hbs` - Object schema template
4. `/Users/sac/unrdf/templates/schemas/enum.mjs.hbs` - Enum schema template
5. `/Users/sac/unrdf/templates/api/event-handler.mjs.hbs` - API route template
6. `/Users/sac/unrdf/templates/composables/use-composable.mjs.hbs` - Composable template
7. `/Users/sac/unrdf/templates/components/script-setup.hbs` - Vue SFC template
8. `/Users/sac/unrdf/templates/tests/schema.test.mjs.hbs` - Test template

### 80/20 Priority Summary

**Phase 1: Foundation (20% → 80% Value) - 3-4 days**
- Zod schemas (10 schemas = 100% type safety)
- API routes (4 routes = 80% backend)
- Core composable (1 file = 100% navigation)
- Dashboard layout (1 file = 80% visual structure)

**Phase 2: Components (15% → 60% Features) - 2 days**
- 5 high-value components (HomeStats, HomeSales, NotificationsSlideover, TeamsMenu, UserMenu)

**Phase 3: Testing (5% → 90% Quality) - 1 day**
- Schema tests, API tests, integration tests

---

## Implementation Phases

### Phase 1: Schema Generator (Day 1-2)

#### Step 1.1: Setup Environment
```bash
# Install dependencies
cd /Users/sac/unrdf
pnpm add -D handlebars typescript

# Create generator directories
mkdir -p generators validators playground/schemas
```

#### Step 1.2: Implement Schema Generator
**File**: `/Users/sac/unrdf/generators/schema-generator.mjs`

```javascript
/**
 * @file generators/schema-generator.mjs
 * @description Convert TypeScript types to Zod schemas
 */

import * as ts from 'typescript'
import Handlebars from 'handlebars'
import { readFileSync, writeFileSync } from 'node:fs'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'

const __dirname = dirname(fileURLToPath(import.meta.url))

/**
 * Generate Zod schemas from TypeScript types
 * @param {string} inputPath - Path to TypeScript types file
 * @param {string} outputPath - Path for generated schemas
 */
export async function generateSchemas(inputPath, outputPath) {
  // 1. Parse TypeScript source
  const sourceCode = readFileSync(inputPath, 'utf-8')
  const sourceFile = ts.createSourceFile(
    inputPath,
    sourceCode,
    ts.ScriptTarget.Latest,
    true
  )

  // 2. Extract type definitions
  const schemas = []

  ts.forEachChild(sourceFile, (node) => {
    if (ts.isInterfaceDeclaration(node)) {
      schemas.push(generateObjectSchema(node))
    } else if (ts.isTypeAliasDeclaration(node)) {
      schemas.push(generateTypeAliasSchema(node))
    }
  })

  // 3. Generate output file
  const template = Handlebars.compile(
    readFileSync(join(__dirname, '../templates/schemas/index.mjs.hbs'), 'utf-8')
  )

  const output = template({ schemas })
  writeFileSync(outputPath, output, 'utf-8')

  console.log(`✅ Generated ${schemas.length} schemas → ${outputPath}`)
}

// ... implementation details
```

**Deliverable**: Working schema generator
**Test**: `node generators/schema-generator.mjs` generates playground/schemas/index.mjs

#### Step 1.3: Generate Zod Schemas
```bash
# Generate schemas
node generators/schema-generator.mjs \
  --input app/types/index.d.ts \
  --output playground/schemas/index.mjs

# Validate output
node -e "import('./playground/schemas/index.mjs')"
```

**Expected Output**: 10 Zod schemas (User, Mail, Member, Stat, Sale, Notification, Range, UserStatus, SaleStatus, Period)

---

### Phase 2: API Route Generator (Day 2)

#### Step 2.1: Implement API Route Generator
**File**: `/Users/sac/unrdf/generators/api-route-generator.mjs`

```javascript
/**
 * @file generators/api-route-generator.mjs
 * @description Generate API routes with Zod validation
 */

import Handlebars from 'handlebars'
import { readFileSync, writeFileSync } from 'node:fs'
import config from '../weaver.config.mjs'

/**
 * Generate API route from configuration
 * @param {object} routeConfig - Route configuration
 */
export async function generateAPIRoute(routeConfig) {
  const template = Handlebars.compile(
    readFileSync('./templates/api/event-handler.mjs.hbs', 'utf-8')
  )

  const output = template({ route: routeConfig })
  writeFileSync(routeConfig.outputPath, output, 'utf-8')

  console.log(`✅ Generated API route → ${routeConfig.outputPath}`)
}

// ... implementation details
```

#### Step 2.2: Generate API Routes
```bash
# Generate all 4 priority API routes
node generators/api-route-generator.mjs --generate-all

# Expected outputs:
# - playground/server/api/customers.mjs
# - playground/server/api/notifications.mjs
# - playground/server/api/mails.mjs
# - playground/server/api/members.mjs
```

**Deliverable**: 4 API routes with Zod validation
**Test**: Start server, curl endpoints

---

### Phase 3: Composable Generator (Day 2-3)

#### Step 3.1: Implement Composable Generator
**File**: `/Users/sac/unrdf/generators/composable-generator.mjs`

```javascript
/**
 * @file generators/composable-generator.mjs
 * @description Generate Vue composables from TypeScript
 */

export async function generateComposable(composableConfig) {
  const template = Handlebars.compile(
    readFileSync('./templates/composables/use-composable.mjs.hbs', 'utf-8')
  )

  const output = template({ composable: composableConfig })
  writeFileSync(composableConfig.outputPath, output, 'utf-8')

  console.log(`✅ Generated composable → ${composableConfig.outputPath}`)
}
```

#### Step 3.2: Generate useDashboard
```bash
# Generate composable
node generators/composable-generator.mjs \
  --input app/composables/useDashboard.ts \
  --output playground/composables/useDashboard.mjs
```

**Deliverable**: useDashboard.mjs with keyboard shortcuts
**Test**: Import in Vue component, test shortcuts

---

### Phase 4: Component Generator (Day 3-4)

#### Step 4.1: Implement Vue SFC Generator
**File**: `/Users/sac/unrdf/generators/vue-sfc-generator.mjs`

```javascript
/**
 * @file generators/vue-sfc-generator.mjs
 * @description Generate Vue SFC scripts from TypeScript
 */

export async function generateVueComponent(componentConfig) {
  const template = Handlebars.compile(
    readFileSync('./templates/components/script-setup.hbs', 'utf-8')
  )

  const scriptOutput = template({ component: componentConfig })

  // Read original .vue file
  const vueContent = readFileSync(componentConfig.inputPath, 'utf-8')

  // Replace <script setup lang="ts"> section
  const updatedVue = vueContent.replace(
    /<script setup lang="ts">[\s\S]*?<\/script>/,
    scriptOutput
  )

  writeFileSync(componentConfig.outputPath, updatedVue, 'utf-8')

  console.log(`✅ Generated Vue component → ${componentConfig.outputPath}`)
}
```

#### Step 4.2: Generate Priority Components (80/20)
```bash
# Generate 5 high-value components
node generators/vue-sfc-generator.mjs --priority high

# Expected outputs:
# 1. playground/app/components/HomeStats.vue
# 2. playground/app/components/HomeSales.vue
# 3. playground/app/components/NotificationsSlideover.vue
# 4. playground/app/components/TeamsMenu.vue
# 5. playground/app/components/UserMenu.vue
```

**Deliverable**: 5 components with JSDoc
**Test**: Mount components, verify rendering

---

### Phase 5: Layout & Page Generation (Day 4)

#### Step 5.1: Generate Default Layout
```bash
node generators/vue-sfc-generator.mjs \
  --input app/layouts/default.vue \
  --output playground/app/layouts/default.vue
```

#### Step 5.2: Generate Home Page
```bash
node generators/vue-sfc-generator.mjs \
  --input app/pages/index.vue \
  --output playground/app/pages/index.vue
```

**Deliverable**: Layout + Home page
**Test**: Start Nuxt dev server, navigate to /

---

### Phase 6: Testing Framework (Day 5)

#### Step 6.1: Implement Test Generator
**File**: `/Users/sac/unrdf/generators/test-generator.mjs`

```javascript
/**
 * @file generators/test-generator.mjs
 * @description Generate Vitest tests for generated code
 */

export async function generateSchemaTests(schemaName) {
  const template = Handlebars.compile(
    readFileSync('./templates/tests/schema.test.mjs.hbs', 'utf-8')
  )

  const testConfig = {
    schemaName,
    validData: [...],    // Generate valid test cases
    invalidData: [...]   // Generate invalid test cases
  }

  const output = template({ test: testConfig })
  writeFileSync(`playground/test/schemas/${schemaName}.test.mjs`, output, 'utf-8')

  console.log(`✅ Generated test → ${schemaName}.test.mjs`)
}
```

#### Step 6.2: Generate All Tests
```bash
# Generate schema tests
node generators/test-generator.mjs --schemas

# Generate API route tests
node generators/test-generator.mjs --api

# Generate composable tests
node generators/test-generator.mjs --composables

# Run tests
cd playground
pnpm test
```

**Deliverable**: 90%+ test coverage
**Test**: `pnpm test` passes with 0 failures

---

### Phase 7: Integration & Validation (Day 5-6)

#### Step 7.1: Integrate with Sidecar
```bash
# Copy generated code to sidecar
cp -r playground/schemas sidecar/
cp -r playground/server/api/* sidecar/server/api/
cp -r playground/composables/* sidecar/composables/
```

#### Step 7.2: Update Nuxt Config
```javascript
// sidecar/nuxt.config.mjs
export default defineNuxtConfig({
  modules: [
    '@nuxt/ui',
    '@vueuse/nuxt',
    '@nuxt/eslint'
  ],
  ui: {
    colors: {
      primary: 'green',
      neutral: 'zinc'
    }
  },
  css: ['~/assets/css/main.css']
})
```

#### Step 7.3: Run Full Validation
```bash
# Syntax validation
pnpm lint

# Type validation (via JSDoc)
pnpm typecheck

# Test validation
pnpm test

# Build validation
pnpm build

# Runtime validation
pnpm dev
```

**Deliverable**: Working dashboard in sidecar
**Test**: Visit http://localhost:3000, verify all features

---

## CLI Tool Implementation

### weaver CLI
**File**: `/Users/sac/unrdf/cli/weaver.mjs`

```javascript
#!/usr/bin/env node

/**
 * @file cli/weaver.mjs
 * @description Weaver CLI for code generation
 */

import { defineCommand, runMain } from 'citty'
import config from '../weaver.config.mjs'

const main = defineCommand({
  meta: {
    name: 'weaver',
    version: '1.0.0',
    description: 'UNRDF Weaver - Code generation for dashboard components'
  },
  subCommands: {
    'generate:schemas': defineCommand({
      meta: { description: 'Generate Zod schemas' },
      async run() {
        await import('../generators/schema-generator.mjs').then(m =>
          m.generateSchemas(config.source.types.path, config.source.types.output)
        )
      }
    }),

    'generate:api': defineCommand({
      meta: { description: 'Generate API routes' },
      async run() {
        await import('../generators/api-route-generator.mjs').then(m =>
          m.generateAllAPIRoutes(config)
        )
      }
    }),

    'generate:composables': defineCommand({
      meta: { description: 'Generate composables' },
      async run() {
        await import('../generators/composable-generator.mjs').then(m =>
          m.generateAllComposables(config)
        )
      }
    }),

    'generate:components': defineCommand({
      meta: { description: 'Generate Vue components' },
      async run() {
        await import('../generators/vue-sfc-generator.mjs').then(m =>
          m.generatePriorityComponents(config)
        )
      }
    }),

    'generate:all': defineCommand({
      meta: { description: 'Generate all code (80/20 priority)' },
      async run() {
        console.log('🚀 Starting full code generation...')

        // Phase 1: Foundation
        await runSubCommand('generate:schemas')
        await runSubCommand('generate:api')
        await runSubCommand('generate:composables')

        // Phase 2: Components
        await runSubCommand('generate:components')

        // Phase 3: Tests
        await import('../generators/test-generator.mjs').then(m =>
          m.generateAllTests(config)
        )

        console.log('✅ Code generation complete!')
      }
    }),

    'validate': defineCommand({
      meta: { description: 'Validate generated code' },
      async run() {
        await import('../validators/code-validator.mjs').then(m =>
          m.validateAll(config)
        )
      }
    })
  }
})

runMain(main)
```

### Usage
```bash
# Make executable
chmod +x cli/weaver.mjs

# Add to package.json
{
  "scripts": {
    "weaver": "node cli/weaver.mjs"
  }
}

# Run commands
pnpm weaver generate:all
pnpm weaver validate
```

---

## Success Criteria

### Phase 1 Complete ✅
- [ ] 10 Zod schemas generated
- [ ] 4 API routes generated with validation
- [ ] useDashboard composable generated
- [ ] Default layout generated
- [ ] `node` imports work without errors

### Phase 2 Complete ✅
- [ ] 5 priority components generated
- [ ] All components render without errors
- [ ] Navigation works (keyboard shortcuts)
- [ ] Data fetching works (API routes)

### Phase 3 Complete ✅
- [ ] 90%+ test coverage
- [ ] All tests pass (`pnpm test`)
- [ ] Validation passes (`pnpm weaver validate`)

### Project Success ✅
- [ ] 80% reduction in manual work
- [ ] 100% type safety (Zod + JSDoc)
- [ ] 0 TypeScript dependencies
- [ ] Full test coverage
- [ ] Working dashboard in sidecar

---

## Risk Management

### Known Issues & Solutions

**Issue**: TypeScript AST parsing complexity
**Solution**: Start with simple patterns, iterate on edge cases
**Fallback**: Manual conversion for complex types

**Issue**: Template maintenance overhead
**Solution**: Version control templates, automated tests
**Fallback**: Direct code generation without templates

**Issue**: Zod type limitations
**Solution**: Document unsupported patterns, use z.unknown()
**Fallback**: Manual validation in JSDoc comments

**Issue**: Node.js version mismatch (better-sqlite3)
**Solution**: Rebuild native modules with `npm rebuild`
**Fallback**: Skip memory hooks, use file-based coordination

---

## Coordination Protocol

### With Architect Agent
- **Handoff**: Share design documents for review
- **Approval**: Get architecture approval before implementation
- **Sync**: Weekly design review meetings

### With Tester Agent
- **Handoff**: Share test generation templates
- **Collaboration**: Co-develop validation framework
- **Sync**: Daily test coverage reports

### With Researcher Agent
- **Handoff**: Request additional TypeScript → Zod patterns
- **Support**: Document edge cases and limitations
- **Sync**: As-needed consultation

### Memory Keys (Hive Coordination)
```javascript
// Store in collective memory (once hooks are fixed)
'hive/coder/weaver-config' → weaver.config.mjs
'hive/coder/implementation-plan' → This document
'hive/coder/templates' → Template files list
'hive/coder/generation-status' → Phase completion status
'hive/coder/metrics' → Generation performance metrics
```

---

## Next Actions

### Immediate (Coder Agent)
1. Implement schema-generator.mjs (Day 1)
2. Test schema generation on User type
3. Generate all 10 Zod schemas
4. Implement api-route-generator.mjs (Day 2)
5. Generate 4 API routes

### Short-term (Coder Agent)
1. Implement composable-generator.mjs (Day 2-3)
2. Implement vue-sfc-generator.mjs (Day 3-4)
3. Generate priority components (Day 4)
4. Implement test-generator.mjs (Day 5)

### Long-term (Team)
1. Architect: Review and approve design
2. Tester: Validate generated code quality
3. Coder: Integrate into sidecar
4. All: Deploy to production

---

**Document Status**: ✅ Implementation Plan Complete
**Ready for**: Coder Agent implementation
**Estimated Duration**: 5-6 days
**Expected Outcome**: 80% automated dashboard conversion
