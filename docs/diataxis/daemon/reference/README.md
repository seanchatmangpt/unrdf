# Reference: @unrdf/daemon

Reference documentation is **information-oriented**. Use it to look up exact API signatures, tool names, and environment variable names. It does not teach — it describes.

## Reference Documents

| Document                                            | Contains                                                                                          |
| --------------------------------------------------- | ------------------------------------------------------------------------------------------------- |
| [Daemon API](./daemon-api.md)                       | `Daemon` class constructor, lifecycle methods, operation methods, status methods, events, schemas |
| [MCP Tools](./mcp-tools.md)                         | All 36 MCP tools grouped by category with descriptions and OTel span names                        |
| [Environment Variables](./environment-variables.md) | All env vars: `OTEL_ENABLED`, `UNRDF_API_KEY`, OTLP settings, sampling, batching                  |

## Source Locations

| Topic                | Source file                                        |
| -------------------- | -------------------------------------------------- |
| Daemon class         | `packages/daemon/src/daemon.mjs`                   |
| MCP server           | `packages/daemon/src/mcp/index.mjs`                |
| OTel instrumentation | `packages/daemon/src/mcp/otel-instrumentation.mjs` |
| OTel SDK setup       | `packages/daemon/src/integrations/otel-sdk.mjs`    |
| Zod schemas          | `packages/daemon/src/schemas.mjs`                  |
| Trigger evaluator    | `packages/daemon/src/trigger-evaluator.mjs`        |
