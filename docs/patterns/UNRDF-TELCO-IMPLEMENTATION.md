# UNRDF Telco Pattern: CLI Implementation Guide

This document outlines the technical implementation strategy for the `unrdf telco` CLI command suite, mapping the telecommunications metaphors directly to the underlying `@unrdf` packages and APIs.

## 1. `unrdf telco tap` (The Wiretap)
**Metaphor:** Hooking alligator clips onto a line to listen to traffic.
**Implementation:**
- **Package:** `@unrdf/kgc-4d`, `@unrdf/streaming`
- **Core Logic:** Connects to the Daemon's Server-Sent Events (SSE) stream or directly tails the KGC-4D event log.
- **API Flow:**
  ```javascript
  import { EventStreamClient } from '@unrdf/kgc-4d/client';
  
  // Connect to the live delta stream
  const client = new EventStreamClient('http://localhost:8080/api/tether');
  client.on('delta', (delta) => {
      // Format as a 'tcpdump' style output
      console.log(`[${delta.timestamp}] ${delta.type} <${delta.subject}> <${delta.predicate}> "${delta.object}"`);
  });
  ```

## 2. `unrdf telco tone <hz>` (The Blue Box)
**Metaphor:** Injecting a 2600 Hz tone to seize control of the trunk.
**Implementation:**
- **Package:** `@unrdf/hooks`, `@unrdf/core`
- **Core Logic:** Injects an RDF-star (quoted triple) with a specific annotation that triggers a highly privileged `semantic-inference` hook, bypassing standard queueing.
- **API Flow:**
  ```javascript
  import { dataFactory, createStore } from '@unrdf/core';
  
  // Construct the "tone" (a quoted triple acting as a control signal)
  const toneTriple = dataFactory.quad(
      dataFactory.namedNode('unrdf:sys:ControlPlane'),
      dataFactory.namedNode('unrdf:sys:emitTone'),
      dataFactory.literal(hz) // e.g., '2600'
  );
  
  // Wrap it in an admin assertion (RDF-star)
  const assertion = dataFactory.quad(
      toneTriple, // The subject is the triple itself
      dataFactory.namedNode('unrdf:sys:requiresElevation'),
      dataFactory.literal('true')
  );
  
  await daemon.injectControlSignal(assertion);
  ```

## 3. `unrdf telco route <instance-id> <node>` (The Operator)
**Metaphor:** Manually plugging patch cords to connect endpoints.
**Implementation:**
- **Package:** `@unrdf/wasm4pm`
- **Core Logic:** Interacts with the PoWL v2 engine to manually override a process transition or force a token replay to a different state.
- **API Flow:**
  ```javascript
  import { getKernel } from '@unrdf/wasm4pm';
  
  const kernel = await getKernel();
  // Fetch current PoWL model and execution trace
  const instance = await kernel.getInstance(instanceId);
  
  // Force a manual transition bypass
  await kernel.forceTransition({
      instanceId,
      targetNode: node,
      overrideReason: 'Manual operator route'
  });
  ```

## 4. `unrdf telco trace <uri>` (The Line Trace)
**Metaphor:** Tracing a call origin across the global network.
**Implementation:**
- **Package:** `@unrdf/kgc-4d`, `@unrdf/validation` (OTEL)
- **Core Logic:** Executes a backward-chaining temporal query through the Git-backed event log and correlates it with OpenTelemetry spans to build a complete lineage of an entity.
- **API Flow:**
  ```javascript
  import { GitBackbone } from '@unrdf/kgc-4d';
  import { ValidationRunner } from '@unrdf/validation';
  
  const git = new GitBackbone('.');
  // Walk the git history to find all deltas affecting the URI
  const history = await git.traceEntityHistory(uri);
  
  // Correlate with OTEL traces to find the originating Agent/Hook
  const otelTraces = await ValidationRunner.fetchSpansForEntity(uri);
  
  renderTraceTree(history, otelTraces);
  ```

## 5. `unrdf telco splice <fork-a> <fork-b>` (The Lineman)
**Metaphor:** Stripping and twisting two copper lines together.
**Implementation:**
- **Package:** `@unrdf/kgc-multiverse`, `@unrdf/core`
- **Core Logic:** Invokes the `morphism` engine to perform a semantic merge of two divergent Universe forks using CRDT conflict resolution.
- **API Flow:**
  ```javascript
  import { applyMorphism } from '@unrdf/kgc-multiverse/src/morphism.mjs';
  import { getUniverse } from '@unrdf/kgc-multiverse/src/universe-manager.mjs';
  
  const forkA = await getUniverse(forkA_Id);
  const forkB = await getUniverse(forkB_Id);
  
  // Splice them together using the semantic algebra
  const splicedUniverse = await applyMorphism(forkA, forkB, {
      strategy: 'crdt-vector-clock-merge'
  });
  ```

## 6. `unrdf telco phreak` (The Explorer)
**Metaphor:** Poking the edges of the SS7 network to test resilience.
**Implementation:**
- **Package:** `@unrdf/daemon` (Chaos Engineering Suite)
- **Core Logic:** Injects simulated latency, drops connections to the Open Ontologies sidecar, or inserts malformed shapes to test the Van der Aalst autonomic recovery loop.
- **API Flow:**
  ```javascript
  import { ChaosEngine } from '@unrdf/daemon/test/chaos-utils.mjs';
  
  const chaos = new ChaosEngine(daemon);
  // Simulate a sidecar crash
  await chaos.simulateSidecarCrash('SIGKILL');
  // Or inject malformed data
  await chaos.injectMalformedDeltas({ count: 100 });
  
  // Verify the autonomic loop heals the system
  await chaos.verifyAutonomicRecovery();
  ```

## 7. `unrdf telco pbx` (Private Branch Exchange)
**Metaphor:** Setting up a localized, isolated telephone network.
**Implementation:**
- **Package:** `@unrdf/daemon`
- **Core Logic:** Spins up an ephemeral, in-memory instance of the UNRDF Daemon and Open Ontologies sidecar on an isolated port, disconnected from the federation.
- **API Flow:**
  ```javascript
  import { SemanticSidecarManager } from '@unrdf/daemon';
  
  // Spin up an isolated PBX
  const pbx = new SemanticSidecarManager({
      mode: 'ephemeral',
      storage: 'memory',
      port: 0, // OS assigns random port
      federation: false
  });
  await pbx.start();
  console.log(`Private Branch Exchange ringing on port ${pbx.port}...`);
  ```

## 8. `unrdf telco hangup` (The Teardown)
**Metaphor:** Dropping the receiver to forcefully terminate the connection.
**Implementation:**
- **Package:** `@unrdf/daemon`
- **Core Logic:** An emergency kill switch that bypasses graceful shutdown to halt runaway processes, freeze the KGC-4D event log, and sever the sidecar connection immediately.
- **API Flow:**
  ```javascript
  import { getActiveDaemon } from '@unrdf/daemon';
  
  const daemon = await getActiveDaemon();
  
  // 1. Freeze the universe (prevent further KGC-4D commits)
  await daemon.universe.freeze();
  
  // 2. Send SIGKILL (not SIGTERM) to the sidecar
  daemon.sidecar.kill('SIGKILL');
  
  // 3. Drop all active SSE client connections
  daemon.tether.terminateAll();
  ```
