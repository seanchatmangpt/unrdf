---
title: "Enterprise Demo: unrdf + kgen Fusion"
theme: default
class: lead
paginate: true
---

# Enterprise Knowledge Platform Demo

_Opinionated RDF substrate (unrdf) + deterministic artifact generator (kgen) with full OpenTelemetry observability_

---

## 📋 Agenda

- Monorepo & Workspace
- Core Composables API
- CLI & Pipeline DSL
- Observability (OpenTelemetry)
- London BDD Testing
- Running the Demo

---

## 🗂 Monorepo Layout

```
enterprise-demo/
├── packages/
│   ├── unrdf/
│   │   └── src/index.mjs      # parseTurtle(), select()
│   └── kgen/
   │       └── src/
   │           ├── index.mjs       # generate()
   │           └── cli.mjs         # kgen render CLI
├── telemetry/               # OTEL setup & tracer
├── examples/demo/           # graph.ttl + template + out.md
├── slides/presentation.md   # this deck
├── package.json             # workspace scripts
└── pnpm-workspaces.yaml
```

---

## 🔧 Core Composables API (unrdf)

- **parseTurtle(ttl)** → N3.Store
- **select(store, query)** → Array of `{s,p,o}`

```js
import { parseTurtle, select } from 'unrdf'
const store = parseTurtle(`@prefix ex: <...> . ex:a ex:b ex:c .`)
const data = await select(store, 'SELECT ?s WHERE { ?s ?p ?o }')
```

---

## 🚀 CLI & Pipeline DSL (kgen)

- **generate(graph, tpl, out)**  
- **kgen render <graph> <tpl> <out>** CLI

```bash
$ kgen render examples/demo/graph.ttl examples/demo/template.hbs out/demo.md
Generated out/demo.md
```

---

## 📊 Observability (OpenTelemetry)

- **otel-setup.mjs**: Initialize NodeSDK + JaegerExporter  
- **tracer.mjs**: `traced(name, fn)` & `tracedSync(name, fn)` wrappers  
- Spans for every core function, FS I/O, CLI command

---

## 🧪 London BDD Testing

Using `citty-test-utils`:

```js
given('a Turtle string', ctx => { ctx.ttl = '@prefix ex:...'; })
when('we parse it', ctx => { ctx.store = parseTurtle(ctx.ttl) })
then('store has 1 quad', ctx => {
  expect(ctx.store.getQuads()).toHaveLength(1)
})
```

CLI test:

```js
await execa('node', ['packages/kgen/src/cli.mjs','render',g,t,out])
expect(fs.existsSync(out)).toBe(true)
```

---

## 📁 Running the Demo

```bash
cd enterprise-demo
pnpm install
pnpm start:demo
# === Output: ===
cat examples/demo/out.md
# Knowledge Graph Export
... (2 triples)
```

---

# 🎉 Thank You!

_Ready for Fortune-5 POC & YC demo_