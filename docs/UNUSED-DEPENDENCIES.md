# Unused Dependencies Report

Generated: 2025-12-21T05:28:03.989Z

## Summary

- Total packages scanned: 19
- Packages with unused deps: 15
- Total unused dependencies: 73

## Details

### docs

- Files scanned: 2
- Total dependencies: 33
- Unused: 32

**Unused dependencies:**
- `@ai-sdk/gateway`
- `@ai-sdk/vue`
- `@electric-sql/pglite`
- `@iconify-json/logos`
- `@iconify-json/lucide`
- `@iconify-json/simple-icons`
- `@iconify-json/vscode-icons`
- `@nuxt/content`
- `@nuxt/image`
- `@nuxt/ui`
- `@nuxtjs/mdc`
- `ai`
- `better-sqlite3`
- `date-fns`
- `drizzle-orm`
- `nuxt`
- `nuxt-auth-utils`
- `nuxt-charts`
- `nuxt-llms`
- `nuxt-og-image`
- `shiki-stream`
- `@nuxt/eslint`
- `@playwright/test`
- `@types/node`
- `@vitest/ui`
- `@vue/test-utils`
- `drizzle-kit`
- `eslint`
- `happy-dom`
- `msw`
- `typescript`
- `vue-tsc`

### nextra

- Files scanned: 2
- Total dependencies: 11
- Unused: 11

**Unused dependencies:**
- `katex`
- `next`
- `nextra`
- `nextra-theme-docs`
- `react`
- `react-dom`
- `zod`
- `@types/node`
- `@types/react`
- `@types/react-dom`
- `typescript`

### oxigraph

- Files scanned: 8
- Total dependencies: 4
- Unused: 2

**Unused dependencies:**
- `zod`
- `@types/node`

### kgn

- Files scanned: 64
- Total dependencies: 11
- Unused: 4

**Unused dependencies:**
- `fs-extra`
- `@amiceli/vitest-cucumber`
- `eslint`
- `nodemon`

### atomvm

- Files scanned: 25
- Total dependencies: 7
- Unused: 3

**Unused dependencies:**
- `vite`
- `jsdom`
- `@vitest/browser`

### composables

- Files scanned: 26
- Total dependencies: 3
- Unused: 1

**Unused dependencies:**
- `@types/node`

### dark-matter

- Files scanned: 17
- Total dependencies: 3
- Unused: 2

**Unused dependencies:**
- `typhonjs-escomplex`
- `@types/node`

### project-engine

- Files scanned: 40
- Total dependencies: 2
- Unused: 1

**Unused dependencies:**
- `@types/node`

### knowledge-engine

- Files scanned: 65
- Total dependencies: 3
- Unused: 1

**Unused dependencies:**
- `@types/node`

### kgc-4d

- Files scanned: 54
- Total dependencies: 6
- Unused: 2

**Unused dependencies:**
- `comment-parser`
- `simple-statistics`

### federation

- Files scanned: 69
- Total dependencies: 4
- Unused: 1

**Unused dependencies:**
- `@types/node`

### streaming

- Files scanned: 61
- Total dependencies: 8
- Unused: 2

**Unused dependencies:**
- `ws`
- `@types/node`

### core

- Files scanned: 80
- Total dependencies: 13
- Unused: 8

**Unused dependencies:**
- `@rdfjs/data-model`
- `@rdfjs/serializer-jsonld`
- `@rdfjs/serializer-turtle`
- `@rdfjs/to-ntriples`
- `jsonld`
- `rdf-ext`
- `rdf-validate-shacl`
- `@types/node`

### cli

- Files scanned: 87
- Total dependencies: 5
- Unused: 2

**Unused dependencies:**
- `yaml`
- `@types/node`

### hooks

- Files scanned: 105
- Total dependencies: 4
- Unused: 1

**Unused dependencies:**
- `@types/node`

## Recommendations

1. Review each unused dependency to confirm it's truly unused
2. Check if dependencies are used in build configs or scripts
3. Remove confirmed unused dependencies with `pnpm remove <dep>`
4. Update pnpm-lock.yaml with `pnpm install`