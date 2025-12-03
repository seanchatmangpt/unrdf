# UNRDF

**RDF knowledge graphs in 11 lines of code.**

```bash
npm install unrdf
```

---

## Quick Start

```javascript
import { initStore, useGraph, useTurtle } from 'unrdf';

await initStore();
const turtle = useTurtle();
const graph = useGraph();

const store = turtle.parse(`
  @prefix ex: <http://example.org/> .
  ex:Alice ex:knows ex:Bob .
`);

const results = graph.select(`SELECT ?person WHERE { ?person ?p ?o }`);
console.log(results); // [{ person: 'ex:Alice' }, { person: 'ex:Bob' }]
```

**That's it.** You have a working RDF system.

---

## The 10 Essential Functions

| Function | Purpose |
|----------|---------|
| `initStore()` | Initialize context (call once) |
| `useGraph()` | Graph operations (select, add, remove) |
| `useTurtle()` | Parse/serialize Turtle |
| `useTerms()` | Create RDF terms (namedNode, literal) |
| `useValidator()` | SHACL validation |
| `useReasoner()` | N3 reasoning |
| `parseTurtle(ttl)` | Direct parse (no context) |
| `query(store, sparql)` | Direct query (no context) |
| `validateShacl(store, shapes)` | Direct validation |
| `reason(store, rules)` | Direct reasoning |

**Use composables** (`useGraph`, `useTurtle`, etc.) for most cases. Use direct functions only for multi-store scenarios.

---

## Next Steps (Only If Needed)

Most users stop here. Only add complexity when you have a specific need:

| Need | Solution | Guide |
|------|----------|-------|
| Validate data | SHACL validation | [docs/how-to/define-shacl-constraints.md](docs/how-to/define-shacl-constraints.md) |
| React integration | React hooks | [docs/how-to/use-react-hooks.md](docs/how-to/use-react-hooks.md) |
| Autonomous behaviors | Knowledge Hooks | [docs/tutorials/02-first-knowledge-hook.md](docs/tutorials/02-first-knowledge-hook.md) |
| Audit trails | Lockchain | [docs/how-to/implement-audit-trails.md](docs/how-to/implement-audit-trails.md) |
| Performance tuning | Dark Matter | [docs/how-to/optimize-query-performance.md](docs/how-to/optimize-query-performance.md) |

**Not sure if you need these?** You probably don't. Start with the 11-line example above.

---

## Documentation

| Type | Purpose | Link |
|------|---------|------|
| **Start Here** | The simplest path | [docs/START-HERE.md](docs/START-HERE.md) |
| **Tutorials** | Step-by-step learning | [docs/tutorials/](docs/tutorials/README.md) |
| **How-To Guides** | Task-oriented recipes | [docs/how-to/](docs/how-to/README.md) |
| **Reference** | API documentation | [docs/reference/](docs/reference/README.md) |
| **Which Features?** | Decision trees | [docs/WHICH-FEATURES.md](docs/WHICH-FEATURES.md) |

---

## Examples

```bash
# Run the minimal example
node examples/01-minimal-parse-query.mjs

# See all examples
cat examples/README.md
```

---

## Requirements

- Node.js 18+
- ESM modules

---

## Advanced Features (Skip Unless You Need Them)

<details>
<summary>Knowledge Hooks (autonomous behaviors)</summary>

```javascript
import { defineHook, registerHook, TransactionManager } from 'unrdf';

const validationHook = defineHook({
  meta: { name: 'validator' },
  before(event) { /* validate */ },
  run(event) { /* execute */ },
  after(result) { /* cleanup */ }
});

registerHook(validationHook);
const txManager = new TransactionManager();
txManager.addHook(validationHook);
await txManager.apply(store, delta);
```

[Learn more →](docs/tutorials/02-first-knowledge-hook.md)
</details>

<details>
<summary>Lockchain (audit trails)</summary>

```javascript
import { LockchainWriter } from 'unrdf/knowledge-engine';

const lockchain = new LockchainWriter({ gitNotesRef: 'audit' });
await lockchain.writeReceipt(transaction);
```

[Learn more →](docs/how-to/implement-audit-trails.md)
</details>

<details>
<summary>Dark Matter (performance optimization)</summary>

```javascript
import { createKnowledgeSubstrateCore } from 'unrdf/knowledge-engine';

const substrate = createKnowledgeSubstrateCore({
  core: ['TransactionManager', 'KnowledgeHookManager'],
  optional: ['PolicyPackManager']
});
```

[Learn more →](docs/how-to/optimize-query-performance.md)
</details>

<details>
<summary>React Hooks</summary>

```javascript
import { useRDFStore, useRDFQuery } from 'unrdf/react-hooks';

function MyComponent() {
  const store = useRDFStore();
  const results = useRDFQuery('SELECT ...');
  return <div>{/* render */}</div>;
}
```

[Learn more →](docs/how-to/use-react-hooks.md)
</details>

<details>
<summary>CLI</summary>

```bash
unrdf parse data.ttl
unrdf query "SELECT ?s WHERE { ?s ?p ?o }" data.ttl
unrdf validate shapes.ttl data.ttl
```

[Learn more →](docs/reference/cli.md)
</details>

---

## License

MIT

---

## Links

- [GitHub](https://github.com/seanchatmangpt/unrdf)
- [npm](https://www.npmjs.com/package/unrdf)
- [Documentation](docs/)
