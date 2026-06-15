# Chapter 6 Research: Resilient Orchestration and the Semantic Sidecar

## Empirical Evidence: Erlang/BEAM Actor Semantics

In Node.js, heavy synchronous tasks (like OWL inference or SHACL validation) block the event loop, breaking the nanosecond scale of the autonomic engine. The UNRDF architecture solves this utilizing the "Semantic Sidecar" pattern, inspired by Erlang/BEAM supervisor trees.

The core evidence is located in `packages/daemon/src/mcp/semantic-sidecar.mjs`.

### 1. Process Decoupling and Isolation

The `SemanticSidecarManager` spawns the high-performance Rust binary (`open-ontologies`) completely detached from the Node event loop.

```javascript
// From: packages/daemon/src/mcp/semantic-sidecar.mjs
this.process = spawn(this.binaryPath, ['serve'], {
  stdio: ['pipe', 'pipe', 'pipe'],
  detached: true, // Isolation
  env: { ...process.env, RUST_LOG: 'info' },
});
this.process.unref(); // Prevent sidecar from keeping Node alive
```

### 2. The "Let It Crash" Watchdog (Chaos Resilience)

Emulating the BEAM architecture, the manager does not attempt to catch internal memory errors from the semantic layer. Instead, it utilizes a passive heartbeat watchdog. If a chaos engineering routine (like `unrdf telco phreak`) explicitly sends a `SIGKILL` to the sidecar, the daemon instantly recognizes the failure and respawns it, restoring state from the immutable KGC-4D event log.

```javascript
// From: packages/daemon/src/mcp/semantic-sidecar.mjs
this.watchdog = setInterval(() => {
  if (!this.process) {
    this.logger.warn('⚠️ Sidecar crashed. Restarting...');
    this._spawn();
  }
}, 5000);
```

## Architectural Synthesis for Thesis

Chapter 6 will utilize this sidecar implementation to prove that UNRDF achieves L4/L5 resilience. By offloading heavy validation to a supervised Rust binary and allowing it to safely crash and restart, the main Node.js routing layer (and the `wasm4pm` engine) remains perfectly stable and responsive, fulfilling the requirements for "Autonomic" self-healing.
