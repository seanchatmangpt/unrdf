## 5. Reference Implementation

### 5.1 System Architecture

The KGC reference implementation consists of 7 core components following the Dark Matter 80/20 principle:

#### 5.1.1 Component Breakdown

| Component | Value % | Lines | Description |
|-----------|---------|-------|-------------|
| **Transaction Manager** | 25% | 695 | Atomic operations with dual hash (SHA3/BLAKE3) |
| **Knowledge Hook Manager** | 20% | 457 | Hook orchestration with file-based execution |
| **Effect Sandbox** | 15% | 378 | VM2/worker thread isolation |
| **Zod Schemas** | 15% | 964 | Runtime type safety and validation |
| **Observability** | 10% | 506 | OpenTelemetry integration |
| **Performance Optimizer** | 10% | 675 | Fast path and caching strategies |
| **Lockchain Writer** | 5% | 460 | Git-anchored audit trails |

**Total Core**: 4,135 LOC delivering 80% of system value

### 5.2 Transaction Manager Implementation

**Key Algorithms**:

```javascript
async function apply(store, delta, options) {
  const txId = generateUUID();
  const span = observability.startTransactionSpan(txId);

  try {
    // 1. Pre-hook execution with veto semantics
    const preHooks = await executeHooks(hooks.pre, store, delta);
    if (preHooks.some(r => r.veto)) {
      span.setAttribute('kgc.veto', true);
      return { committed: false, veto: true, receipt: preHooks };
    }

    // 2. Apply delta atomically
    store.removeQuads(delta.removals);
    store.addQuads(delta.additions);

    // 3. Compute dual hashes (SHA3 + BLAKE3)
    const afterHash = await hashStore(store, options);

    // 4. Post-hook execution
    const postHooks = await executeHooks(hooks.post, store, delta);

    // 5. Generate cryptographic receipt
    const receipt = {
      transactionId: txId,
      delta: { additions: delta.additions.length, removals: delta.removals.length },
      hashes: { before: beforeHash, after: afterHash },
      hooks: { pre: preHooks, post: postHooks },
      timestamp: new Date().toISOString(),
      committed: true
    };

    // 6. Write to lockchain if enabled
    if (options.enableLockchain) {
      await lockchainWriter.append(receipt);
    }

    span.setStatus({ code: 'OK' });
    return { committed: true, receipt };
  } catch (error) {
    span.recordException(error);
    throw error;
  } finally {
    span.end();
  }
}
```

**Dual Hash Implementation**:

```javascript
async function hashStore(store, options) {
  if (options.afterHashOnly) {
    // Fast path: Simple content hash (no canonicalization)
    const quads = store.getQuads();
    const content = quads.map(q =>
      `${q.subject.value} ${q.predicate.value} ${q.object.value} ${q.graph.value}`
    ).join('\n');

    const bytes = utf8ToBytes(content);
    return {
      sha3: bytesToHex(sha3_256(bytes)),
      blake3: bytesToHex(blake3(bytes))
    };
  }

  // Canonical path: URDNA2015 canonicalization
  const canonical = await canonicalize(store);
  const bytes = utf8ToBytes(canonical);

  return {
    sha3: bytesToHex(sha3_256(bytes)),
    blake3: bytesToHex(blake3(bytes))
  };
}
```

### 5.3 Knowledge Hook Manager

**Hook Execution Pipeline**:

```javascript
async function evaluateHook(hook, options) {
  const startTime = performance.now();

  // 1. Execute SPARQL query
  const queryStart = performance.now();
  const bindings = await executeQuery(hook.select);
  const queryDuration = performance.now() - queryStart;

  // 2. Evaluate predicates
  const predicateStart = performance.now();
  const predicateResults = await Promise.all(
    hook.predicates.map(p => evaluatePredicate(p, bindings))
  );
  const predicateDuration = performance.now() - predicateStart;

  // 3. Apply combinator function
  const fired = hook.combine === 'AND'
    ? predicateResults.every(r => r.ok)
    : predicateResults.some(r => r.ok);

  // 4. Generate cryptographic receipt
  const canonStart = performance.now();
  const provenance = await generateProvenance(hook, bindings);
  const canonDuration = performance.now() - canonStart;

  return {
    id: hook.id,
    fired,
    predicates: predicateResults,
    durations: {
      totalMs: performance.now() - startTime,
      queryMs: queryDuration,
      predicateMs: predicateDuration,
      canonicalizationMs: canonDuration
    },
    provenance,
    timestamp: new Date().toISOString(),
    input: {
      bindingsCount: bindings.length,
      variables: Object.keys(bindings[0] || {})
    }
  };
}
```

**Predicate Evaluation Dispatcher**:

```javascript
async function evaluatePredicate(predicate, bindings) {
  const startTime = performance.now();

  try {
    let result;
    switch (predicate.kind) {
      case 'ASK':
        result = await evaluateAskPredicate(predicate.spec, bindings);
        break;
      case 'SHACL':
        result = await evaluateShaclPredicate(predicate.spec, bindings);
        break;
      case 'DELTA':
        result = await evaluateDeltaPredicate(predicate.spec, bindings);
        break;
      case 'THRESHOLD':
        result = await evaluateThresholdPredicate(predicate.spec, bindings);
        break;
      case 'COUNT':
        result = await evaluateCountPredicate(predicate.spec, bindings);
        break;
      case 'WINDOW':
        result = await evaluateWindowPredicate(predicate.spec, bindings);
        break;
      default:
        throw new Error(`Unknown predicate kind: ${predicate.kind}`);
    }

    return {
      kind: predicate.kind,
      ok: result,
      duration: performance.now() - startTime
    };
  } catch (error) {
    return {
      kind: predicate.kind,
      ok: false,
      error: error.message,
      duration: performance.now() - startTime
    };
  }
}
```

### 5.4 Effect Sandbox

**Secure Execution Environment**:

```javascript
class EffectSandbox {
  constructor(options = {}) {
    this.type = options.type || 'worker';
    this.timeout = options.timeout || 30000;
    this.memoryLimit = options.memoryLimit || 64 * 1024 * 1024; // 64MB
    this.enableNetwork = options.enableNetwork || false;
    this.enableFileSystem = options.enableFileSystem || false;
  }

  async execute(code, context) {
    if (this.type === 'worker') {
      return await this.executeInWorker(code, context);
    } else if (this.type === 'vm2') {
      return await this.executeInVM2(code, context);
    }
  }

  async executeInWorker(code, context) {
    const worker = new Worker(code, {
      workerData: context,
      resourceLimits: {
        maxOldGenerationSizeMb: this.memoryLimit / (1024 * 1024),
        maxYoungGenerationSizeMb: this.memoryLimit / (2 * 1024 * 1024)
      }
    });

    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        worker.terminate();
        reject(new Error('Effect execution timeout'));
      }, this.timeout);

      worker.on('message', (result) => {
        clearTimeout(timer);
        worker.terminate();
        resolve(result);
      });

      worker.on('error', (error) => {
        clearTimeout(timer);
        worker.terminate();
        reject(error);
      });
    });
  }
}
```

### 5.5 Performance Optimizer

**Fast Path Optimization**:

```javascript
class PerformanceOptimizer {
  constructor(options) {
    this.enableProfiling = options.enableProfiling;
    this.fastPathEnabled = true;
    this.cache = new LRUCache({ max: 10000, ttl: 60000 });
  }

  async optimizeTransaction(store, delta, options) {
    // Decide on fast path vs canonical path
    const useFastPath = this.shouldUseFastPath(delta, options);

    if (useFastPath) {
      return await this.executeFastPath(store, delta);
    } else {
      return await this.executeCanonicalPath(store, delta);
    }
  }

  shouldUseFastPath(delta, options) {
    // Fast path criteria:
    // 1. Small delta size
    // 2. No canonicalization required
    // 3. Performance mode enabled
    return options.afterHashOnly &&
           (delta.additions.length + delta.removals.length) < 100;
  }

  async executeFastPath(store, delta) {
    // Optimized hash computation without canonicalization
    const hash = await quickHash(store);
    return { hash, method: 'fast' };
  }

  async executeCanonicalPath(store, delta) {
    // Full URDNA2015 canonicalization
    const canonical = await canonicalize(store);
    const hash = sha3_256(canonical);
    return { hash, canonical, method: 'canonical' };
  }
}
```

### 5.6 Lockchain Writer

**Git-Anchored Audit Trail**:

```javascript
class LockchainWriter {
  constructor(options) {
    this.gitRepo = options.gitRepo || process.cwd();
    this.refName = options.refName || 'refs/notes/lockchain';
    this.batchSize = options.batchSize || 10;
    this.receiptQueue = [];
  }

  async append(receipt) {
    this.receiptQueue.push(receipt);

    if (this.receiptQueue.length >= this.batchSize) {
      await this.flush();
    }
  }

  async flush() {
    if (this.receiptQueue.length === 0) return;

    const batch = this.receiptQueue.splice(0);
    const merkleRoot = this.computeMerkleRoot(batch);

    // Write to Git notes
    const currentCommit = await this.getCurrentCommit();
    const noteContent = JSON.stringify({
      receipts: batch,
      merkleRoot,
      timestamp: new Date().toISOString()
    });

    await this.gitNotesAdd(currentCommit, noteContent, this.refName);
  }

  computeMerkleRoot(receipts) {
    const leaves = receipts.map(r => sha3_256(JSON.stringify(r)));
    return this.buildMerkleTree(leaves);
  }

  buildMerkleTree(leaves) {
    if (leaves.length === 1) return leaves[0];

    const nextLevel = [];
    for (let i = 0; i < leaves.length; i += 2) {
      const left = leaves[i];
      const right = leaves[i + 1] || left;
      nextLevel.push(sha3_256(left + right));
    }

    return this.buildMerkleTree(nextLevel);
  }

  async verify(receiptHash) {
    // Verify receipt exists in lockchain
    const notes = await this.gitNotesList(this.refName);

    for (const note of notes) {
      const batch = JSON.parse(note.content);
      const receipt = batch.receipts.find(r => r.provenance.receiptHash === receiptHash);

      if (receipt) {
        // Verify merkle proof
        const proof = this.getMerkleProof(batch.receipts, receipt);
        return this.verifyMerkleProof(proof, batch.merkleRoot, receiptHash);
      }
    }

    return false;
  }
}
```

---

