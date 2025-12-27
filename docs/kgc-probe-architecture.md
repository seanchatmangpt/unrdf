# KGC Probe Architecture & Design Decisions

**Purpose**: Visual and conceptual guide for implementing KGC Probe CLI

**Audience**: Architects, lead developers, system designers

---

## 1. System Architecture

### 1.1 High-Level Data Flow

```
┌─────────────────────────────────────────────────────────────────────┐
│                         kgc probe <command>                         │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌──────────────────┐                                              │
│  │  COMMAND LAYER   │  (kgc-cli via citty)                         │
│  │  (Input Parser)  │  - Argument parsing                          │
│  └────────┬─────────┘  - Option validation                         │
│           │            - Envelope formatting                       │
│           ▼                                                        │
│  ┌──────────────────┐                                              │
│  │ ORCHESTRATION    │  Delegates to:                               │
│  │ LAYER            │  1. ProbeScanner                             │
│  │ (Command Router) │  2. ProbeMerger                              │
│  └────────┬─────────┘  3. ProbeDiffer                              │
│           │            4. ProbeReporter                            │
│           │            5. ProbeVerifier                            │
│           ▼                                                        │
│  ┌──────────────────┐                                              │
│  │ VALIDATION       │  - Zod schema validation                     │
│  │ LAYER            │  - Guard enforcement (timeouts)              │
│  └────────┬─────────┘  - Conflict detection                        │
│           │            - Hash verification                         │
│           ▼                                                        │
│  ┌──────────────────┐                                              │
│  │ PERSISTENCE      │  - Read/write shards                         │
│  │ LAYER            │  - Filesystem operations                     │
│  │ (I/O)            │  - RDF graph serialization                   │
│  └────────┬─────────┘  - PDF generation                            │
│           │                                                        │
│           ▼                                                        │
│  Results + Receipts (Hash Chain + Merkle Tree)                    │
│                                                                    │
└─────────────────────────────────────────────────────────────────────┘
```

### 1.2 Component Interaction Diagram

```
┌─────────────────────────────────────────────────────────────────────┐
│                         @unrdf/kgc-probe                            │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌─────────────────────────────────────────────────────────────┐  │
│  │ /src                                                        │  │
│  │                                                             │  │
│  │  ├─ scanner.mjs ────────────────────┐                      │  │
│  │  │  runProbeScan()                  │ ┌──────────────────┐ │  │
│  │  │  - Execute agents in parallel    │→│ Agent Registry   │ │  │
│  │  │  - Collect shards               │ │ (10 agents)      │ │  │
│  │  │  - Generate hash chains         │ └──────────────────┘ │  │
│  │  │  - Return: {shards, receipts}   │                      │  │
│  │  │                                  │ ┌──────────────────┐ │  │
│  │  ├─ merger.mjs ────────────────────┤→│ RDF Serializer   │ │  │
│  │  │  mergeShards()                   │ │ (Oxigraph)       │ │  │
│  │  │  - Detect conflicts             │ └──────────────────┘ │  │
│  │  │  - Merge deterministically      │                      │  │
│  │  │  - Return: {artifact, deltas}   │ ┌──────────────────┐ │  │
│  │  │                                  │→│ Config Loader    │ │  │
│  │  ├─ differ.mjs ─────────────────────┤ │ (.kgc-probe.json)│ │  │
│  │  │  diffArtifacts()                 │ └──────────────────┘ │  │
│  │  │  - Compare claims                │                      │  │
│  │  │  - Compute delta (added/removed) │ ┌──────────────────┐ │  │
│  │  │  - Return: {delta, summary}      │→│ Hash Functions   │ │  │
│  │  │                                  │ │ (SHA256)         │ │  │
│  │  ├─ reporter.mjs ──────────────────┤ └──────────────────┘ │  │
│  │  │  generateReport()                │                      │  │
│  │  │  - Build Diataxis structure      │ ┌──────────────────┐ │  │
│  │  │  - Generate Markdown/JSON/TTL    │→│ PDF Generator    │ │  │
│  │  │  - Return: {report, path}        │ │ (Pandoc/Chrome)  │ │  │
│  │  │                                  │ └──────────────────┘ │  │
│  │  ├─ verifier.mjs ──────────────────┐                       │  │
│  │  │  verifyArtifact()                │                       │  │
│  │  │  - Verify hash chains           │ ┌──────────────────┐  │  │
│  │  │  - Check merkle proofs          │→│ Crypto Functions │  │  │
│  │  │  - Validate schemas             │ │ (RSA/ECDSA)      │  │  │
│  │  │  - Return: {valid, details}     │ └──────────────────┘  │  │
│  │  │                                  │                       │  │
│  │  └──────────────────────────────────┘                       │  │
│  │                                                             │  │
│  └─────────────────────────────────────────────────────────────┘  │
│                                                                    │
│  ┌─────────────────────────────────────────────────────────────┐  │
│  │ /src/lib                                                    │  │
│  │                                                             │  │
│  │  ├─ hash.mjs          - SHA256 hashing                     │  │
│  │  ├─ merkle.mjs        - Merkle tree construction/proofs    │  │
│  │  ├─ rdf-converter.mjs - JSON → Turtle RDF                  │  │
│  │  ├─ conflict-resolver.mjs - Merge conflict detection       │  │
│  │  ├─ report-gen.mjs    - Markdown/PDF generation            │  │
│  │  ├─ config-loader.mjs - Load .kgc-probe.json               │  │
│  │  └─ fs-utils.mjs      - Directory/file operations          │  │
│  │                                                             │  │
│  └─────────────────────────────────────────────────────────────┘  │
│                                                                    │
└─────────────────────────────────────────────────────────────────────┘

EXPORTS to @unrdf/kgc-cli:
  - runProbeScan(args)
  - mergeShards(shardDir, args)
  - diffArtifacts(old, new, args)
  - generateReport(artifactPath, args)
  - verifyArtifact(artifactPath, args)
```

---

## 2. Command Flow Diagrams

### 2.1 kgc probe scan Flow

```
START
  ↓
[Load Config] (.kgc-probe.json)
  ├─ Validate schema
  ├─ Check allowlist
  └─ Load guards + merge rules
  ↓
[Generate Run ID] (YYYYMMDD-HHMMSS-xxxxxx)
  ↓
[Create Output Directories]
  ├─ probe/out/<run-id>/shards/
  ├─ probe/out/<run-id>/merged/
  ├─ probe/out/<run-id>/receipts/
  ├─ probe/out/<run-id>/meta/
  └─ probe/out/<run-id>/diff/ (if comparing)
  ↓
[Load Agent Registry] (10 agents)
  ├─ Check allowlist
  └─ Sort topologically (deterministic)
  ↓
[Execute Agents in Parallel]
  │
  ├─→ Agent 1 (orchestrator)
  │   ├─ Run with timeout + memory guard
  │   ├─ Capture output (JSON/RDF)
  │   ├─ Compute SHA256 hash
  │   └─ Write to shards/agent-1.json
  │
  ├─→ Agent 2 (runtime)
  │   └─ (same as Agent 1)
  │
  └─→ ... Agent 10
  ↓
[Await All Results]
  ├─ Collect successful shards
  ├─ Track failed agents
  └─ Log timing metrics
  ↓
[Generate Hash Chain] (receipts/chain.json)
  ├─ For each shard in order:
  │   ├─ Link to previous hash
  │   ├─ Compute entry hash = SHA256(shard||prev)
  │   └─ Optionally sign with private key
  │
  ├─ Result: Linear chain of hashes
  └─ Write to receipts/chain.json
  ↓
[Generate Merkle Tree] (optional, receipts/merkle.json)
  ├─ Build binary merkle tree from shards
  ├─ Compute membership proofs (O(log n))
  ├─ Verify root hash
  └─ Write to receipts/merkle.json
  ↓
[Validate Shards] (if --validate)
  ├─ FOR EACH shard:
  │   ├─ Parse JSON
  │   ├─ Validate against config.schema (Zod)
  │   └─ On error: Log, exclude from merge
  │
  └─ Continue even if some fail
  ↓
[Merge Shards Deterministically]
  ├─ Detect conflicts (DetectConflicts algorithm)
  ├─ Apply merge rules from config
  ├─ Build merged artifact (MergeShardsDeterm algorithm)
  └─ Compute statistics
  ↓
[Generate Reports] (if --format)
  ├─ IF ttl|all:
  │   ├─ Convert artifact to RDF
  │   └─ Write merged/world.ttl
  │
  ├─ IF json|all:
  │   └─ Write merged/index.json
  │
  └─ IF md|all:
      ├─ Generate Diataxis structure
      └─ Write merged/report.md
  ↓
[Compute Diff] (if --compare)
  ├─ Load previous artifact
  ├─ Normalize claims
  ├─ Compute delta (added/removed/modified)
  └─ Write diff/delta.json
  ↓
[Write Metadata]
  ├─ meta/config.json (runtime config)
  ├─ meta/manifest.json (agent versions)
  └─ meta/metrics.json (timing/sizes)
  ↓
[Return Result Envelope]
  ├─ success: true|false
  ├─ runId: "20250101-120000-abc123"
  ├─ outputDir: "/path/to/probe/out/..."
  ├─ shardCount: 10
  ├─ mergedArtifact: {...}
  ├─ receipts: {hashChainPath, merklePath}
  └─ metrics: {duration_ms, agentCount}
  ↓
END
```

### 2.2 kgc probe merge Flow

```
START
  ↓
[Validate Inputs]
  ├─ Check shard-dir exists
  ├─ Count .json files
  └─ Error if none found
  ↓
[Load Config] (.kgc-probe.json)
  ├─ Merge rules
  └─ Schema for validation
  ↓
[Load and Parse Shards]
  ├─ FOR EACH *.json in shardDir:
  │   ├─ Read file
  │   ├─ Parse JSON
  │   └─ On error:
  │       ├─ Log error
  │       ├─ If --on-conflict=fail: RETURN error
  │       └─ Else: Continue
  │
  └─ Result: Array of valid shards
  ↓
[Detect Conflicts] (DetectConflicts algorithm)
  ├─ Build claim map (claimId → [candidates])
  ├─ Find claims with multiple values
  └─ Result: Array of conflicts
  ↓
  IF conflicts.length > 0:
    ↓
    [Handle Conflicts] based on --on-conflict:
    │
    ├─ IF fail:
    │   ├─ Format error with conflict details
    │   └─ RETURN error("MERGE_CONFLICT", ...)
    │
    ├─ IF merge:
    │   ├─ FOR EACH conflict:
    │   │   ├─ Select winner by most recent timestamp
    │   │   ├─ Log warning
    │   │   └─ Continue
    │   │
    │   └─ Continue merge
    │
    └─ IF list (default):
        ├─ Log conflicts
        └─ Continue merge
  ↓
[Merge Shards Deterministically] (MergeShardsDeterm algorithm)
  ├─ Sort shards by name (alphabetical)
  ├─ FOR EACH shard:
  │   ├─ FOR EACH claim in shard:
  │   │   ├─ IF claim.id already exists:
  │   │   │   ├─ Apply merge rule
  │   │   │   └─ (keep-first|keep-last|merge-both|fail)
  │   │   │
  │   │   └─ ELSE:
  │   │       └─ Add claim to result
  │   │
  │   └─ Update agent list + statistics
  │
  └─ Result: Merged artifact
  ↓
[Generate Output]
  ├─ Create output directory (new run ID)
  │
  └─ Generate requested formats:
      ├─ IF ttl|all:
      │   └─ world.ttl
      │
      ├─ IF json|all:
      │   └─ index.json
      │
      └─ IF md|all:
          └─ report.md
  ↓
[Return Result Envelope]
  ├─ success: true (or false if conflicts + fail mode)
  ├─ mergedArtifact: {...}
  ├─ outputDir: "..."
  ├─ shardCount: N
  ├─ conflicts: [...]
  └─ warnings: [...]
  ↓
END
```

### 2.3 kgc probe diff Flow

```
START
  ↓
[Load Artifacts]
  ├─ Load oldArtifact from <artifact-old>
  │   (supports .ttl or .json)
  │
  └─ Load newArtifact from <artifact-new>
  ↓
[Normalize Claims]
  ├─ Strip metadata (timestamps, hashes)
  ├─ Standardize claim structure
  ├─ Generate canonical ID for each claim
  └─ Build normalized index maps
  ↓
[Compute Delta]
  │
  ├─→ FOR EACH claim IN newArtifact:
  │   ├─ Find matching claim in oldArtifact by ID
  │   │
  │   ├─ IF NOT found:
  │   │   └─ ADD to "added" list
  │   │
  │   ├─ IF found but value differs:
  │   │   └─ IF NOT (--ignore-timestamps AND only timestamp differs):
  │   │       └─ ADD to "modified" list
  │   │
  │   └─ IF found and value same:
  │       └─ No change
  │
  ├─→ FOR EACH claim IN oldArtifact:
  │   ├─ IF claim.id NOT in newArtifact:
  │   │   └─ ADD to "removed" list
  │   │
  │   └─ ELSE: Already processed above
  │
  └─ Result: {added, removed, modified}
  ↓
[Compute Summary Statistics]
  ├─ addedCount: length of added array
  ├─ removedCount: length of removed array
  ├─ modifiedCount: length of modified array
  ├─ changesetSize: sum of above three
  ├─ oldSize: oldArtifact.claims.length
  ├─ newSize: newArtifact.claims.length
  └─ timestamp: current ISO timestamp
  ↓
[Format Output] based on --format:
  │
  ├─ IF json (default):
  │   ├─ Create JSON object with delta + summary
  │   └─ Pretty-print (2-space indent)
  │
  └─ IF md:
      ├─ Create Markdown report
      ├─ Include old/new artifact info
      ├─ Section for each change type
      └─ Agent-grouped list of changes
  ↓
[Output Result]
  ├─ IF --output <path>:
  │   └─ Write to file
  │
  └─ ELSE:
      └─ Print to stdout
  ↓
[Return Delta Structure]
  ├─ delta: {added, removed, modified}
  └─ summary: {...}
  ↓
END
```

### 2.4 kgc probe report Flow

```
START
  ↓
[Load Artifact]
  ├─ Load merged/index.json or merged/world.ttl
  └─ Parse claims and metadata
  ↓
[Build Report Structure] based on --format:
  │
  ├─ IF md (default):
  │   │
  │   ├─→ [Generate Tutorial Section]
  │   │   ├─ Step-by-step guide for new users
  │   │   ├─ How to use the discovered capabilities
  │   │   └─ Example workflows
  │   │
  │   ├─→ [Generate How-To Guide Section]
  │   │   ├─ Common patterns
  │   │   ├─ Usage examples per agent
  │   │   └─ Best practices
  │   │
  │   ├─→ [Generate Reference Section]
  │   │   ├─ Group claims by agent
  │   │   ├─ FOR EACH agent:
  │   │   │   ├─ ## Agent Name
  │   │   │   └─ FOR EACH claim:
  │   │   │       ├─ **Name**: description
  │   │   │       └─ Usage: `example`
  │   │   │
  │   │   └─ Limit nesting depth (--max-depth)
  │   │
  │   ├─→ [Generate Explanation Section]
  │   │   ├─ Design rationale
  │   │   ├─ Architecture insights
  │   │   └─ Deep dives on key concepts
  │   │
  │   ├─→ [Generate Statistics Table]
  │   │   ├─ Total claims
  │   │   ├─ Agent count
  │   │   ├─ Coverage percentage
  │   │   └─ Last updated
  │   │
  │   └─ Combine all sections → Markdown report
  │
  ├─ IF json:
  │   │
  │   ├─ Flatten artifact structure
  │   ├─ Group claims by agent
  │   ├─ Build capability index
  │   ├─ Include metadata
  │   └─ JSON.stringify with 2-space indent
  │
  ├─ IF ttl:
  │   │
  │   ├─ Build RDF graph
  │   ├─ Use ontology prefixes (config.ontology)
  │   ├─ Create KGC Report resource
  │   ├─ Link to agents and capabilities
  │   ├─ Include optional provenance (--include-provenance)
  │   └─ Serialize to Turtle format
  │
  └─ IF pdf:
      │
      ├─ Generate Markdown report (as above)
      ├─ Convert Markdown → HTML/PDF
      │   (via Pandoc or headless Chrome)
      └─ Apply styling (--style: technical|executive|audit)
  ↓
[Write Output]
  ├─ Resolve output path (--output or default)
  ├─ Create parent directories
  └─ Write report file
  ↓
[Return Result Envelope]
  ├─ success: true
  ├─ format: <format used>
  ├─ outputPath: "..."
  ├─ sections: [section names]
  └─ stats: {claimCount, agentCount, coverage}
  ↓
END
```

### 2.5 kgc probe verify Flow

```
START
  ↓
[Locate Artifact and Receipts]
  ├─ IF <path> is directory:
  │   ├─ Look for merged/index.json or merged/world.ttl
  │   └─ Look for receipts/ subdirectory
  │
  └─ IF <path> is file:
      ├─ Use as artifact
      └─ Look for receipts/ in parent or --receipt-dir
  ↓
[Load Artifact and Receipts]
  ├─ Load merged artifact (for schema validation)
  ├─ Load receipts/chain.json (hash chain)
  ├─ Load receipts/merkle.json (if --check-merkle)
  └─ Load config for validation schema
  ↓
[Initialize Checks]
  ├─ checks: {hashChainValid: false, merkleValid: false, ...}
  ├─ mismatches: []
  └─ details: []
  ↓
[IF --check-merkle OR hash chain exists]
  │
  ├─→ [Verify Hash Chain Integrity] (O(n))
  │   │
  │   └─ FOR EACH entry IN chain:
  │       ├─ Check: previousHash = last entry's hash
  │       │   └─ If mismatch:
  │       │       ├─ Add to mismatches
  │       │       └─ Set chainValid = false
  │       │
  │       ├─ Check: entry.hash = SHA256(entry without hash field)
  │       │   └─ If mismatch:
  │       │       ├─ Add to mismatches
  │       │       └─ Set chainValid = false
  │       │
  │       └─ previousHash ← entry.hash
  │
  ├─ checks.hashChainValid ← chainValid
  └─ Detail: "Hash chain verified: N entries, root=..."
  ↓
[IF --check-merkle AND merkle.json exists]
  │
  ├─→ [Verify Merkle Tree] (O(n log n))
  │   │
  │   ├─ Compute merkle root from leaf hashes
  │   │   └─ Compare with merkleTree.root
  │   │
  │   └─ FOR EACH proof IN merkle proofs:
  │       ├─ Verify membership proof (O(log n))
  │       │   ├─ Start with leaf hash
  │       │   ├─ FOR EACH step in path:
  │       │   │   ├─ Combine with sibling hash
  │       │   │   └─ Compute parent hash
  │       │   │
  │       │   └─ Compare final hash with merkle.root
  │       │
  │       └─ If mismatch:
  │           ├─ Add to mismatches
  │           └─ merkleValid = false
  │
  ├─ checks.merkleValid ← merkleValid
  └─ Detail: "Merkle tree verified: N proofs OK"
  ↓
[IF --check-schema]
  │
  ├─→ [Validate Shards Against Schema] (O(n))
  │   │
  │   └─ FOR EACH entry IN chain:
  │       ├─ Load shard file (shards/agentName.json)
  │       ├─ Validate via Zod schema (config.schema)
  │       └─ If error:
  │           ├─ Add to mismatches
  │           └─ schemaValid = false
  │
  ├─ checks.schemaValid ← schemaValid
  └─ Detail: "Schema validation: N shards OK" or "N errors"
  ↓
[IF --check-crypto AND keys.json exists]
  │
  ├─→ [Verify Cryptographic Signatures]
  │   │
  │   └─ Load keys.json
  │   │
  │   └─ FOR EACH entry IN chain:
  │       ├─ Get signature from entry.signature
  │       ├─ Get public key for agent from keys.json
  │       ├─ Verify: VerifySignature(entry.hash, signature, pubkey)
  │       └─ If verification fails:
  │           ├─ Add to mismatches
  │           └─ cryptoValid = false
  │
  ├─ checks.cryptoValid ← cryptoValid
  └─ Detail: "Crypto verification: VALID" or "INVALID"
  ↓
  IF --check-crypto AND keys.json NOT exists:
  │
  └─ Detail: "Crypto verification: SKIPPED (no keys.json)"
  ↓
[Determine Overall Validity]
  ├─ allValid ← mismatches.length = 0
  ↓
  IF NOT allValid AND --strict:
    ├─ RETURN error("VERIFICATION_FAILED", mismatches)
    ↓
  ELSE:
    ├─ Continue (allow partial failures)
    ↓
[Return Verification Result]
  ├─ valid: boolean
  ├─ checks: {hashChainValid, merkleValid, schemaValid, cryptoValid}
  ├─ mismatches: [{check, expected, actual, ...}, ...]
  ├─ details: [human-readable messages]
  └─ timestamp: ISO datetime
  ↓
END
```

---

## 3. Decision Trees

### 3.1 Merge Conflict Resolution Tree

```
┌─────────────────────────────────────────────────────────┐
│ Merge Conflict Detected (claim ID appears multiple times)│
└────────────────────┬────────────────────────────────────┘
                     │
             ┌───────┴────────┐
             │                │
      Did user specify    NO
      --on-conflict?      ┌─→ Default: use "list" mode
             │            │   - Log conflict
             │            └─→ Continue merge
             │                (user reviews in output)
             │
        YES │
             ├────────────────┬────────────────┬──────────────┐
             │                │                │              │
        [fail]          [merge]          [list]       (invalid)
             │                │                │
             │                │                │
         ┌───┴──┐         ┌────┴────┐      ┌──┴──┐
         │      │         │         │      │     │
    RETURN  FORMAT    SELECT BY  LOG      CONTINUE
    ERROR   as       TIMESTAMP  & LIST    MERGE
    with    JSON     (most      (user    (with
    conflict with    recent)    reviews  warnings)
    details intent  AUTO-       via
            to help MERGE      output
            user    (⚠️ risky)  files
            resolve
            manually

Merge Rules (from config.mergeRules):
┌──────────────────────────────────────────┐
│ claimId → rule                           │
│                                          │
│ capability.version → "keep-last"         │
│ metadata.timestamp → "keep-last"         │
│ claims.#schema → "merge-both"            │
│ default → "keep-first"                   │
└──────────────────────────────────────────┘
```

### 3.2 Error Handling Decision Tree

```
┌─────────────────────────────────────────────────────────┐
│                 Error Detected                           │
└────────────────────┬────────────────────────────────────┘
                     │
        ┌────────────┼────────────┬─────────────────┐
        │            │            │                 │
    INPUT        EXECUTION    MERGE            VERIFICATION
    ERROR        ERROR        ERROR             ERROR
        │            │            │                 │
        ├─→          ├─→          ├─→              ├─→
        │            │            │                │
    [Codes]     [Codes]      [Codes]         [Codes]
    ─────────    ─────────    ──────────      ──────────────
    INVALID_    SWARM_      MERGE_        VERIFICATION_
    FORMAT     TIMEOUT     CONFLICT      FAILED

    INVALID_   AGENT_       DUPLICATE_    HASH_MISMATCH
    CONFIG    FAILED       CLAIMS

    SHARD_DIR_ SWARM_       NO_SHARDS    MERKLE_INVALID
    NOT_FOUND COMPLETE_
             FAILURE

    NO_SHARDS

    ARTIFACT_
    NOT_FOUND

    PERMISSION_
    DENIED

    DISK_FULL
        │
        │
    ┌───┴──────────────────────────────────────────────┐
    │ Format Response Envelope:                        │
    │                                                 │
    │ {                                               │
    │   "success": false,                             │
    │   "code": "ERROR_CODE",                         │
    │   "message": "Human-readable message",          │
    │   "details": {                                  │
    │     "context": "additional info",               │
    │     "suggestion": "how to fix"                  │
    │   },                                            │
    │   "timestamp": "2025-01-01T12:00:00Z"           │
    │ }                                               │
    └───┴──────────────────────────────────────────────┘
        │
        └─→ Return to CLI layer for pretty-print
```

### 3.3 Output Format Selection Tree

```
┌─────────────────────────────────────────┐
│ kgc probe <cmd> --format <fmt>          │
└────────────────┬────────────────────────┘
                 │
        ┌────────┼─────────┬────────────┐
        │        │         │            │
      ttl      json       md          all
        │        │         │            │
    ┌───┴──┐ ┌───┴──┐ ┌───┴──┐   ┌────┴──┐
    │      │ │      │ │      │   │       │
 (RDF)  (JSON) (MD)    │       │   │
  Turtle Mach- Diataxis│       │   │
  Graph  Readable      │       │   │
  (12MB) Index         │       │   │
         (8.5MB)       │       │   │
                  Human-Readable
                  Markdown
                  Report
                  (2.3MB)
                       │       │
                       └───┬───┘
                           │
                       [ALL 3]
                     Generate:
                     - world.ttl
                     - index.json
                     - report.md
```

### 3.4 Report Style Selection Tree

```
┌──────────────────────────────────────┐
│ kgc probe report --style <name>      │
└────────────┬───────────────────────┐
             │                       │
        ┌────┴────┐             ┌────┴─────┐
        │          │             │          │
    technical   executive     audit    (invalid)
        │          │             │
        │          │             │
    [High-detail][Summary]  [Provenance]
        │          │             │
    ├─→ Complete  ├─→ High-level ├─→ Include
    │  capability │   summary    │  agent/source
    │  index      │              │  metadata
    │             │ Only key     │
    │ Per-agent   │ capabilities │ Hash chain
    │ breakdown   │              │ verification
    │             │ Examples     │ timeline
    │ Examples    │ omitted      │
    │ for each    │              │ Compliance
    │ capability  │ ~5 pages     │ checklist
    │             │              │
    │ ~20 pages   │              │ ~10 pages
    │             │              │
    └────────────┴──────────────┘
```

---

## 4. Data Structure Ownership

### 4.1 Responsibility Matrix

```
┌──────────────────┬──────────────┬────────────────────┐
│ Structure        │ Owner        │ Read-Only Access   │
├──────────────────┼──────────────┼────────────────────┤
│ Shards/          │ Scanner      │ Merger, Differ,    │
│ *.json           │              │ Verifier           │
├──────────────────┼──────────────┼────────────────────┤
│ Merged/          │ Merger       │ Differ, Reporter,  │
│ index.json       │              │ Verifier           │
├──────────────────┼──────────────┼────────────────────┤
│ Merged/          │ Merger       │ Reporter, Differ   │
│ world.ttl        │ (RDF Conv.)  │ (reads only)       │
├──────────────────┼──────────────┼────────────────────┤
│ Receipts/        │ Scanner      │ Verifier, User     │
│ chain.json       │              │ (read-only)        │
├──────────────────┼──────────────┼────────────────────┤
│ Receipts/        │ Scanner      │ Verifier, User     │
│ merkle.json      │              │ (read-only)        │
├──────────────────┼──────────────┼────────────────────┤
│ Meta/            │ Scanner      │ Reporter, Auditor  │
│ config.json      │              │                    │
├──────────────────┼──────────────┼────────────────────┤
│ Meta/            │ Scanner      │ Auditor            │
│ metrics.json     │              │                    │
├──────────────────┼──────────────┼────────────────────┤
│ .kgc-probe.      │ User/        │ All commands       │
│ json             │ DevOps       │ (read-only)        │
└──────────────────┴──────────────┴────────────────────┘
```

### 4.2 Shard Immutability Guarantee

```
Once written:
  shards/agent-01.json
  └─ Cannot be modified
  └─ Hash is cryptographically signed
  └─ Referenced in receipts/chain.json

Integrity verification:
  1. Recompute SHA256(shard_content)
  2. Compare with hash_chain[i].shardHash
  3. Verify hash_chain[i].hash = SHA256(hash_chain[i] without hash field)
  4. Verify hash_chain[i].previousHash = hash_chain[i-1].hash

If ANY verification fails:
  └─ Shard has been tampered with
  └─ RETURN error("HASH_MISMATCH")
  └─ User cannot continue merge until resolved
```

---

## 5. Cryptographic Proof Layers

### 5.1 Proof Verification Stack

```
Layer 4: Merkle Tree (O(log n) proofs)
         ┌────────────────────────────┐
         │ Each shard has membership   │
         │ proof: O(log n) length     │
         │                            │
         │ User can verify any shard  │
         │ belongs to the report      │
         └────────────────────────────┘

Layer 3: Hash Chain (Sequential linking)
         ┌────────────────────────────┐
         │ Each shard hash links to   │
         │ previous (linear proof)    │
         │                            │
         │ Proves execution order     │
         │ Prevents reordering        │
         └────────────────────────────┘

Layer 2: Shard Hashes (Integrity)
         ┌────────────────────────────┐
         │ SHA256(shard_content)      │
         │                            │
         │ Proves shard not tampered  │
         │ Detection of corruption    │
         └────────────────────────────┘

Layer 1: Schema Validation (Correctness)
         ┌────────────────────────────┐
         │ Zod schema validation      │
         │ Rules from config          │
         │                            │
         │ Proves shard is valid      │
         │ Guards against injection   │
         └────────────────────────────┘

Verification order: 1 → 2 → 3 → 4
                    (schema → hash → chain → merkle)
```

### 5.2 Trust Model

```
┌────────────────────────────────────────────────────────┐
│ How to Trust a Probe Report                            │
├────────────────────────────────────────────────────────┤
│                                                        │
│ User Trust Question          Verification Method      │
│ ─────────────────────────────────────────────────────│
│ "Is this report authentic?"  1. Load artifact        │
│ "Can I verify it?"           2. Load receipts/       │
│                              3. Run: kgc probe       │
│                                 verify <path>        │
│                                                      │
│ "Did agent data change?"     4. Check hash chain     │
│                              5. Compare shard hashes  │
│                                 with chain.json       │
│                                                      │
│ "Is agent order preserved?"  6. Verify hash chain    │
│                                 links (sequential)    │
│                                                      │
│ "Can external parties        7. Share merkle.json    │
│  verify one shard?"          8. They compute proof   │
│                              9. Verify against root   │
│                                                      │
│ "Can we prove non-           10. Hash chain is      │
│  repudiation?"                  immutable proof      │
│                              11. Optional: RSA sig   │
│                                 on each entry        │
│                                                      │
└────────────────────────────────────────────────────────┘
```

---

## 6. Performance Characteristics

### 6.1 Execution Timeline (10 agents)

```
Phase              Duration    Bottleneck
─────────────────────────────────────────────
Load Config        ~50ms       File I/O
Load Agents        ~100ms      Registry scan
Execute Agents     ~30s        Agent work (parallel)
  (10 agents                   Wall time (--parallel=10)
   @ 3s each)

Collect Results    ~500ms      Shard aggregation
Validate Shards    ~1s         JSON parsing + Zod
Detect Conflicts   ~200ms      O(n*m) comparison
Merge Shards       ~500ms      Claim consolidation
Generate Hashes    ~100ms      SHA256 computation
Build Merkle       ~50ms       Binary tree ops
Generate Reports   ~2s         Template rendering
Write Files        ~500ms      Disk I/O

TOTAL              ~35-40s     Agent execution
                               (with 10s variance)
```

### 6.2 Space Complexity (10 agents)

```
Input:
  - Agent outputs: ~10MB total (1MB each)

Processing:
  - Shards in memory: ~15MB
  - Claim index: ~5MB
  - Merkle tree: ~1MB
  - Reports (TTL, JSON, MD): ~25MB total

Output (probe/out/<run-id>/):
  - shards/: ~10MB
  - merged/: ~25MB (TTL + JSON + MD)
  - receipts/: ~200KB
  - meta/: ~50KB

TOTAL on disk: ~35-45MB per run
Peak memory: ~50MB
```

### 6.3 Scaling Behavior

```
For N agents:
  - Scan time: O(1) via --parallel (wall time constant)
  - Merge time: O(N*C) where C = claims per agent
  - Diff time: O(C) = O(1) for fixed C
  - Report gen: O(N*C) (linear in data size)
  - Verification: O(N log N) for merkle proofs

Recommended:
  - Max agents: 50 (without optimization)
  - Max claims per agent: 1000
  - Max shards in memory: 100MB

If exceeding:
  - Implement shard streaming (process one at a time)
  - Use claim pagination in reports
  - Split merkle tree into sub-trees
```

---

## 7. Security Considerations

### 7.1 Attack Surface

```
Threat Model:
┌──────────────────────────────────────────────────────┐
│ Attack                  Mitigation                   │
├──────────────────────────────────────────────────────┤
│ Shard tampering         → SHA256 hash check          │
│ Shard reordering        → Hash chain (sequential)    │
│ Claim injection         → Zod schema validation      │
│ Fake receipts           → Merkle tree proofs         │
│ DoS via large shard     → Memory limit guards        │
│ Timeout bypass          → --timeout parameter       │
│ Unauthorized merge      → File permissions (OS)      │
│ Report forgery          → Sign chain.json (crypto)   │
│                           (optional)                 │
└──────────────────────────────────────────────────────┘
```

### 7.2 Guard Enforcement

```
Per-Agent Guards (from config):
┌─────────────────────────────────────────────────┐
│ Guard                Default        Override    │
├─────────────────────────────────────────────────┤
│ timeLimit           30s            per config   │
│ memoryLimit         512MB          per config   │
│ refusals            []             per config   │
│                                                │
│ Example (orchestrator):                         │
│ {                                              │
│   "orchestrator": {                            │
│     "timeLimit": 30000,      ← 30 seconds      │
│     "memoryLimit": "512MB",                    │
│     "refusals": [                              │
│       "external-network",                      │
│       "filesystem-write"                       │
│     ]                                          │
│   }                                            │
│ }                                              │
└─────────────────────────────────────────────────┘
```

---

## 8. Testing Strategy

### 8.1 Test Coverage Plan

```
Unit Tests (70% of effort):
├─ Hash functions (SHA256, merkle)
├─ Conflict detection algorithm
├─ Merge determinism (same input → same output)
├─ Config loading and validation
├─ Report generation (all formats)
└─ Error handling (all error codes)

Integration Tests (20% of effort):
├─ scan → merge → report pipeline
├─ Artifact verification (chain + merkle)
├─ Diff computation (accuracy)
├─ Round-trip: scan → verify → merge
└─ Large dataset (100+ claims)

E2E Tests (10% of effort):
├─ Full CLI commands (all 5)
├─ Exit codes (success/failure)
├─ Output file structure
└─ Parallel agent execution (timing)
```

### 8.2 Test Fixtures

```
Fixture: minimal-probe/
├─ shards/
│   ├── agent-01.json         (1 claim)
│   └── agent-02.json         (1 claim, no conflict)
├─ receipts/
│   ├── chain.json            (2 entries)
│   └── merkle.json           (height 2)
└─ config.json                (minimal config)

Fixture: conflict-probe/
├─ shards/
│   ├── agent-01.json         (3 claims)
│   └── agent-02.json         (cap-2 conflicts)
├─ receipts/
│   └── chain.json
└─ expected-conflicts.json    (for validation)

Fixture: large-probe/
├─ shards/
│   ├── agent-01.json         (100 claims)
│   ├── agent-02.json         (100 claims)
│   └── ... agent-10.json     (100 claims each)
├─ receipts/
│   └── merkle.json           (height 4, 10 leaves)
└─ meta/metrics.json          (performance baseline)
```

---

## Conclusion

This architecture provides:

1. **Modularity**: 5 independent commands, clear separation of concerns
2. **Determinism**: Alphabetical sorting, fixed hashing, reproducible merges
3. **Transparency**: Hash chains + merkle trees provide cryptographic proof
4. **Scalability**: O(n log n) verification, parallel agent execution
5. **Security**: Schema validation, timeout guards, cryptographic signatures
6. **Testability**: Clear unit/integration/E2E test boundaries

Key design decisions:
- **Hash chains** (not just merkle) for sequential integrity
- **Deterministic merge** (alphabetical ordering) for reproducibility
- **Diataxis structure** for reports (tutorial + reference pattern)
- **Zod schemas** for input validation (prevent injection)
- **Config-driven rules** for merge conflicts (user control)

