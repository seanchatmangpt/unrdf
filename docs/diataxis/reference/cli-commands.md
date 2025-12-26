# CLI Commands Reference

**Purpose:** Complete reference for UNRDF command-line interface.

**Audience:** CLI users and automation scripts

**Version:** 5.0.1

---

## Overview

UNRDF CLI provides commands for:
- RDF parsing and validation
- SPARQL query execution
- Hook management
- Universe operations
- Workflow orchestration

---

## Installation

**[Placeholder - Content to be filled]**

```bash
pnpm add -g @unrdf/cli
# or
npx @unrdf/cli [command]
```

**Evidence:** CLI package at `/home/user/unrdf/packages/cli/`

---

## Global Options

**[Placeholder - Global flags]**

```bash
unrdf [command] [options]

Global Options:
  --verbose, -v        Verbose output
  --quiet, -q          Suppress non-error output
  --config, -c <file>  Config file path
  --help, -h           Show help
  --version            Show version
```

**Evidence:** Global options at `/home/user/unrdf/packages/cli/src/options.mjs`

---

## Commands

### `unrdf parse <file>`

**[Placeholder - Parse command details]**

**Evidence:** Parse command at `/home/user/unrdf/packages/cli/src/commands/parse.mjs`

---

### `unrdf query <query-file>`

**[Placeholder - Query command details]**

**Evidence:** Query command at `/home/user/unrdf/packages/cli/src/commands/query.mjs`

---

### `unrdf validate <file>`

**[Placeholder - Validate command details]**

**Evidence:** Validate command at `/home/user/unrdf/packages/cli/src/commands/validate.mjs`

---

### `unrdf freeze <universe>`

**[Placeholder - Freeze command details]**

**Evidence:** Freeze command at `/home/user/unrdf/packages/cli/src/commands/freeze.mjs`

---

### `unrdf hook <subcommand>`

**[Placeholder - Hook management commands]**

**Evidence:** Hook commands at `/home/user/unrdf/packages/cli/src/commands/hook/`

---

### `unrdf workflow <subcommand>`

**[Placeholder - Workflow commands]**

**Evidence:** Workflow commands at `/home/user/unrdf/packages/cli/src/commands/workflow/`

---

## Configuration File

**[Placeholder - Config file format]**

```yaml
# unrdf.config.yml
store:
  type: oxigraph
  path: ./data/store

hooks:
  enabled: true
  directory: ./hooks

observability:
  otel:
    enabled: true
    endpoint: http://localhost:4318
```

**Evidence:** Config schema at `/home/user/unrdf/packages/cli/src/config-schema.mjs`

---

## Examples

**[Placeholder - CLI examples]**

**Evidence:** Examples at `/home/user/unrdf/examples/cli/`

---

## Exit Codes

**[Placeholder - Exit code reference]**

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | General error |
| 2 | Invalid arguments |
| 3 | Validation failed |
| ... | ... |

**Evidence:** Exit codes at `/home/user/unrdf/packages/cli/src/exit-codes.mjs`

---

## Environment Variables

**[Placeholder - Environment variables]**

**Evidence:** Env vars at `/home/user/unrdf/packages/cli/src/env.mjs`

---

## Related References

- **[Package Exports Reference](./package-exports.md)** - CLI package exports
- **[Hook API Reference](./hook-api.md)** - Hook CLI integration

---

**Questions?** Check [TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
