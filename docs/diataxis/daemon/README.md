# @unrdf/daemon Documentation

Background daemon for managed scheduled tasks, event-driven operations, and MCP server integration with OpenTelemetry tracing and security hardening.

## Documentation Structure

This documentation follows the [Diataxis framework](https://diataxis.fr/), organizing content into four categories:

### Tutorials

Learning-oriented guides that take you step by step through building with the daemon:

- [Your First Daemon](./tutorials/01-first-daemon.md)

### How-To Guides

Task-oriented guides for specific operations:

- [Schedule Operations](./how-to/01-schedule-operations.md)
- [Enable OTel Tracing](./how-to/02-enable-otel-tracing.md)
- [Configure Security](./how-to/03-configure-security.md)

### Reference

Technical API documentation:

- [Daemon API](./reference/daemon-api.md)
- [MCP Tools](./reference/mcp-tools.md)
- [Environment Variables](./reference/environment-variables.md)

### Explanation

Conceptual deep-dives and architecture:

- [Daemon Architecture](./explanation/01-daemon-architecture.md)

## What is @unrdf/daemon?

The daemon is a background service that:

- Executes operations based on **cron expressions**, **intervals**, and **events**
- Exposes a **JSON-RPC MCP server** with 36+ instrumented tools
- Provides **distributed tracing** via OpenTelemetry (controlled by `OTEL_ENABLED`)
- Enforces **API key authentication** and injection detection for all operations
- Coordinates **multi-node clusters** with Raft consensus

```javascript
import { Daemon } from '@unrdf/daemon';

const daemon = new Daemon({ id: 'my-daemon' });
await daemon.start();
daemon.schedule({ id: 'task', handler: async () => ({ done: true }) });
const result = await daemon.execute('task');
await daemon.stop();
```

## Quick Navigation

1. New to the daemon? Start with [Your First Daemon](./tutorials/01-first-daemon.md)
2. Need a specific task done? Check the [How-To Guides](./how-to/)
3. Looking up an API? See the [Reference](./reference/)
4. Want to understand the design? Read the [Explanation](./explanation/)
