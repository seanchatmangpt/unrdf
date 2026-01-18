# How-To: Use Watch Mode for Rapid Development

**Objective:** Set up continuous code regeneration during development.

**Prerequisites:** Working `ggen.toml`, ontology file, and at least one template.

**Estimated Time:** 5 minutes

---

## Problem

Running `unrdf sync` manually after each ontology or template change breaks your workflow.

---

## Solution

Use the `--watch` flag to automatically regenerate code when files change.

---

## Step-by-Step Guide

### 1. Start Watch Mode

```bash
unrdf sync --watch --verbose
```

**Output:**

```
UNRDF Sync
Phase 1: Loading configuration...
Phase 2: Loading ontology...
   Loaded: 47 triples
Phase 3: Processing rules...
   OK lib/schemas/entities.mjs (1247 bytes)

Sync complete!
   Duration: 23.4ms

Watching:
  /project/ggen.toml
  /project/ontology/schema.ttl
  /project/templates/entities.njk

Watching for changes... (Ctrl+C to stop)
```

### 2. Edit Ontology File

Save changes to your ontology. The watcher detects and regenerates:

```
[change] schema.ttl

──────────────────────────────────────────────────

UNRDF Sync
...
   Loaded: 48 triples
   OK lib/schemas/entities.mjs (1312 bytes)

Watching for changes... (Ctrl+C to stop)
```

### 3. Edit Template File

Modify a template. Same automatic regeneration occurs.

### 4. Stop Watch Mode

Press `Ctrl+C`:

```
^C
Stopping watch mode...
```

---

## Performance Tips

### Debouncing

Watch mode uses **300ms debounce**. Rapid saves within this window trigger only one regeneration.

### Selective Rules

For faster regeneration in large projects:

```bash
unrdf sync --watch --rule zod-entities
```

### Dry Run Testing

Test changes without writing files:

```bash
unrdf sync --watch --dry-run --verbose
```

---

## Integration with Dev Tools

**npm scripts** - Add to `package.json`:
```json
{ "scripts": { "codegen:watch": "unrdf sync --watch --verbose" } }
```

**Concurrent servers** - Run alongside vite/webpack:
```json
{ "scripts": { "dev": "concurrently \"unrdf sync --watch\" \"vite\"" } }
```

**nodemon** - For custom file patterns:
```bash
nodemon --watch ontology --watch templates -e ttl,njk --exec "unrdf sync"
```

---

## Watched Files

| File Type | Source |
|-----------|--------|
| Config | `--config` argument or `ggen.toml` |
| Ontology | `[ontology].source` |
| Templates | `[[generation.rules]].template` |

---

## Troubleshooting

| Issue | Solution |
|-------|----------|
| Changes not detected | Use `--verbose` to verify watched paths |
| Too many regenerations | 300ms debounce handles most cases |
| Watcher stops | Check for config syntax errors |

---

## Related

- [sync-command.md](/home/user/unrdf/packages/cli/docs/sync-command.md) - Full reference
- [generate-rdf-from-templates.md](./generate-rdf-from-templates.md) - Template guide
