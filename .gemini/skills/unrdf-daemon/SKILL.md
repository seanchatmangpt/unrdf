---
name: unrdf-daemon
description: Manages the lifecycle, health, and semantic sidecar (Open Ontologies) of the UNRDF Daemon. Use for starting/stopping/monitoring the daemon or diagnosing sidecar state.
---
# UNRDF Daemon Management

## Overview
The UNRDF Daemon is the central hub of autonomic operations.

## Procedures

### 1. Daemon Lifecycle
- **Start**: `daemon.start()` manages the sidecar binary automatically.
- **Stop**: `daemon.stop()` ensures graceful shutdown and persistence flush.

### 2. Sidecar Health
- Check `packages/daemon/src/mcp/semantic-sidecar.mjs` for logging status.
- If the sidecar is unresponsive, check the binary path in `process.env.OPEN_ONTOLOGIES_PATH`.

## Debugging
- Use `tail -f logs/daemon.log` to monitor the sidecar handshake process.
