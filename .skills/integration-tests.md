# Skill: Integration Test Setup

Set up prerequisites for integration tests that depend on external tools or API keys.

## Open-Ontologies Integration

Binary: `~/.local/bin/open-ontologies` — must be installed for MCP tools to work.

Tests that depend on this will gracefully skip when the binary is missing.

## Groq LLM Integration

Requires `GROQ_API_KEY` environment variable.

23 integration tests (12 MCP + 11 ensemble) — all skip when key is not set.

## OTEL Initialization Warning

Daemon tests emit a non-blocking warning during OTEL SDK initialization:
```
__vite_ssr_import_1__.default.default is not a function
```
This is expected — tests pass, tracing is disabled. Ignore it.
