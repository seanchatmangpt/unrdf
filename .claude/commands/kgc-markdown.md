---
name: kgc-markdown
description: Dynamic KGC Markdown plugin - receipt-driven documentation with proof guarantees
version: 1.0.0
agents:
  - atlas-agent
  - proof-agent
  - diataxis-agent
  - frontier-agent
  - release-agent
requires:
  - '@unrdf/oxigraph'
  - 'glob'
  - 'node:crypto'
  - 'node:fs'
hooks:
  pre_commit: verify-receipts
  post_command: verify-determinism
  on_error: emit-denial-receipt
---

# KGC Markdown Plugin

**Dynamic Knowledge Graph Curation with Receipt-Driven Proof Guarantees**

## 🎯 Core Principle: Trust But Verify

Every claim requires a receipt. Every receipt must validate. Every validation must be reproducible.

**Adversarial PM Questions**:

- Did the scan ACTUALLY find all files? (Prove with count)
- Did proof validation RUN or just check syntax? (Show output)
- Are receipts current or stale? (Timestamp + content hash)
- Can frontier be reproduced from manifest? (Deterministic sort)

---

## 📋 Available Commands

### `/kgc:scan [scope]`

**Purpose**: Build complete atlas of packages, APIs, documentation coverage with receipt manifest.

**Input**:

- `scope` (optional): Package name (e.g., `@unrdf/core`) or glob pattern (e.g., `docs/**/*.md`)
- If omitted: scans entire workspace

**Output**:

- JSON manifest at `.kgc/atlas-manifest.json`
- Coverage report with surface counts
- Receipt index for all discovered documents

**Implementation**:

```javascript
// Subagent: atlas-agent
// Tools: Glob, Read, Write
// Timeout: 10s (justified: large repos 1000+ files)

async function scanScope(scope = '**/*.{md,mjs,js}') {
  // 1. Discover all files matching scope
  const files = await glob(scope);

  // 2. For each file, extract:
  //    - Package affiliation (from path or package.json)
  //    - Surface type (API, doc, test, example)
  //    - Receipt pointers (if present)
  //    - Content hash (SHA-256 of normalized content)

  // 3. Build manifest with deterministic sort
  const manifest = {
    generated: new Date().toISOString(),
    scope: scope,
    total_files: files.length,
    surfaces: files.map(extractSurface).sort(byPackageThenPath),
    receipt_index: buildReceiptIndex(files),
    coverage: computeCoverage(files),
  };

  // 4. Write manifest + verification receipt
  await writeManifest('.kgc/atlas-manifest.json', manifest);

  return {
    files_scanned: files.length,
    packages_found: new Set(manifest.surfaces.map(s => s.package)).size,
    receipts_found: manifest.receipt_index.length,
    coverage_pct: manifest.coverage.documented_pct,
  };
}
```

**Example**:

```bash
# Scan specific package
/kgc:scan @unrdf/oxigraph

# Output:
# ✅ Scanned 47 files in @unrdf/oxigraph
# 📦 Surfaces: 12 API, 8 docs, 18 tests, 9 examples
# 🧾 Receipts: 31/47 files (66% coverage)
# 📊 Manifest: .kgc/atlas-manifest.json (15.2 KB)
# ⏱️  Duration: 2.3s

# Scan all documentation
/kgc:scan "docs/**/*.md"

# Scan entire workspace
/kgc:scan
```

**Adversarial Verification**:

```bash
# Did scan find ALL files?
timeout 5s find docs -name "*.md" | wc -l  # Compare to manifest.total_files

# Are receipts valid?
grep -c "receipt_id" .kgc/atlas-manifest.json  # Must match receipts_found

# Is manifest deterministic?
cp .kgc/atlas-manifest.json /tmp/atlas-1.json
/kgc:scan  # Re-run
diff .kgc/atlas-manifest.json /tmp/atlas-1.json  # Must be identical (except timestamp)
```

**Error Handling**:

- **No files found**: WARN (empty scope is valid), return empty manifest
- **Receipt parse error**: WARN per file, continue scan, flag in manifest
- **Permission denied**: ERROR, abort scan, suggest `sudo` or path correction
- **Timeout exceeded**: ERROR, suggest narrower scope or `--no-timeout` flag

---

### `/kgc:prove <doc>`

**Purpose**: Verify every embedded receipt, regenerate proof appendix with cryptographic guarantees.

**Input**:

- `doc`: Path to `.kgcmd` or `.md` file containing receipt blocks

**Output**:

- Verification report (pass/fail per receipt)
- Updated proof appendix in document
- Exit code 0 (all valid) or 1 (failures detected)

**Implementation**:

```javascript
// Subagent: proof-agent
// Tools: Read, Write, Bash (for executing code blocks)
// Timeout: 15s (justified: code execution + cryptographic hashing)

async function proveDocument(docPath) {
  // 1. Parse document, extract receipt blocks
  const doc = await readFile(docPath);
  const receipts = extractReceiptBlocks(doc);

  // 2. For each receipt:
  //    a. Locate referenced artifact (file, code output, API response)
  //    b. Compute current content hash
  //    c. Compare to receipt.expected_hash
  //    d. If mismatch: flag as STALE or INVALID

  const results = await Promise.all(
    receipts.map(async receipt => {
      const artifact = await resolveArtifact(receipt.pointer);
      const currentHash = computeSHA256(artifact);
      const valid = currentHash === receipt.expected_hash;

      return {
        receipt_id: receipt.id,
        pointer: receipt.pointer,
        expected: receipt.expected_hash,
        actual: currentHash,
        valid: valid,
        timestamp: new Date().toISOString(),
      };
    })
  );

  // 3. Generate proof appendix
  const proofAppendix = generateProofAppendix(results);

  // 4. Update document with proof section
  const updatedDoc = replaceProofSection(doc, proofAppendix);
  await writeFile(docPath, updatedDoc);

  // 5. Return verification report
  const passed = results.filter(r => r.valid).length;
  const failed = results.length - passed;

  return {
    total_receipts: results.length,
    passed: passed,
    failed: failed,
    success_rate: ((passed / results.length) * 100).toFixed(1) + '%',
    details: results.filter(r => !r.valid), // Only show failures
  };
}
```

**Example**:

```bash
# Prove single document
/kgc:prove docs/api/oxigraph-store.kgcmd

# Output:
# 🔍 Verifying 12 receipts in oxigraph-store.kgcmd
# ✅ 11/12 valid (91.7%)
# ❌ 1 STALE: receipt#5 (createStore output changed)
#    Expected: a3f2e1...
#    Actual:   b4c3d2...
#    Fix: Run `/kgc:refresh docs/api/oxigraph-store.kgcmd`
# 📝 Updated proof appendix (14 lines)
# ⏱️  Duration: 3.8s

# Prove all docs in directory
find docs -name "*.kgcmd" -exec /kgc:prove {} \;
```

**Adversarial Verification**:

```bash
# Did proof actually RUN code blocks?
grep "executed_at" docs/api/oxigraph-store.kgcmd  # Must have recent timestamp

# Are hashes cryptographically sound?
grep "sha256:" docs/api/oxigraph-store.kgcmd | wc -l  # Must match receipt count

# Can proof be reproduced?
/kgc:prove docs/api/oxigraph-store.kgcmd  # Run twice
git diff docs/api/oxigraph-store.kgcmd  # Proof section must be identical
```

**Error Handling**:

- **Receipt missing pointer**: ERROR, suggest adding `<!-- receipt:pointer=/path/to/artifact -->`
- **Artifact not found**: ERROR, emit denial receipt with remediation
- **Hash mismatch**: WARN (not error - expected for stale docs), suggest `/kgc:refresh`
- **Execution timeout**: ERROR, suggest increasing timeout or splitting complex blocks
- **Parse error**: ERROR, show line number, suggest syntax fix

---

### `/kgc:refresh <doc>`

**Purpose**: Re-run all executable blocks deterministically, update output hashes and receipts.

**Input**:

- `doc`: Path to `.kgcmd` file with executable code blocks

**Output**:

- Regenerated `.md` file with updated outputs
- Updated `o_hash` (output hash) for each block
- Updated receipts with new timestamps

**Implementation**:

```javascript
// Subagent: proof-agent (reuses execution logic)
// Tools: Read, Write, Bash
// Timeout: 20s (justified: multiple code block executions)

async function refreshDocument(docPath) {
  // 1. Parse .kgcmd, extract executable blocks
  const source = await readFile(docPath);
  const blocks = extractCodeBlocks(source);

  // 2. For each executable block:
  //    a. Execute in isolated context (deterministic env)
  //    b. Capture stdout/stderr
  //    c. Compute output hash (SHA-256)
  //    d. Update block metadata

  const executedBlocks = await Promise.all(
    blocks.map(async block => {
      if (!block.executable) return block;

      // Deterministic execution environment
      const env = {
        NODE_ENV: 'test',
        TZ: 'UTC',
        LANG: 'en_US.UTF-8',
      };

      const result = await executeBlock(block.code, env);
      const outputHash = computeSHA256(normalizeOutput(result.stdout));

      return {
        ...block,
        output: result.stdout,
        o_hash: outputHash,
        executed_at: new Date().toISOString(),
        duration_ms: result.duration,
        exit_code: result.exitCode,
      };
    })
  );

  // 3. Generate updated .md with receipts
  const outputPath = docPath.replace('.kgcmd', '.md');
  const updatedDoc = renderMarkdown(executedBlocks);
  await writeFile(outputPath, updatedDoc);

  // 4. Generate receipt manifest
  const receiptManifest = {
    source: docPath,
    output: outputPath,
    blocks: executedBlocks.length,
    executed: executedBlocks.filter(b => b.executable).length,
    total_hash: computeSHA256(updatedDoc),
    generated_at: new Date().toISOString(),
  };

  await writeFile(outputPath + '.receipt.json', receiptManifest);

  return {
    blocks_executed: receiptManifest.executed,
    output_file: outputPath,
    receipt_file: outputPath + '.receipt.json',
    total_hash: receiptManifest.total_hash,
  };
}
```

**Example**:

```bash
# Refresh single document
/kgc:refresh docs/tutorial/getting-started.kgcmd

# Output:
# ⚙️  Executing 8 code blocks in getting-started.kgcmd
# ✅ Block 1: npm install @unrdf/oxigraph (2.1s, exit=0)
# ✅ Block 2: node examples/create-store.mjs (0.8s, exit=0)
# ✅ Block 3: node examples/query.mjs (1.2s, exit=0)
# ... (5 more blocks)
# 📝 Generated: docs/tutorial/getting-started.md (4.2 KB)
# 🧾 Receipt: docs/tutorial/getting-started.md.receipt.json
# 🔒 Total hash: c5d7e9f2a1b3...
# ⏱️  Duration: 8.4s

# Refresh all tutorials
find docs/tutorial -name "*.kgcmd" -exec /kgc:refresh {} \;
```

**Adversarial Verification**:

```bash
# Did blocks actually execute?
cat docs/tutorial/getting-started.md.receipt.json | jq '.blocks'  # Check executed count

# Are outputs deterministic?
/kgc:refresh docs/tutorial/getting-started.kgcmd  # Run twice
diff docs/tutorial/getting-started.md /tmp/getting-started-1.md  # Must be identical

# Do receipts validate?
/kgc:prove docs/tutorial/getting-started.md  # Must show 100% valid
```

**Error Handling**:

- **Code block fails**: WARN, capture error output, mark block with `exit_code != 0`
- **Non-deterministic output**: ERROR, suggest adding normalization (sort, env vars)
- **Timeout**: ERROR, show which block, suggest splitting or increasing timeout
- **File write error**: ERROR, check permissions, suggest target directory
- **Missing dependencies**: ERROR, show npm/pnpm install command

---

### `/kgc:diataxis <source.kgcmd>`

**Purpose**: Generate 4 Diátaxis views (tutorial/how-to/reference/explanation) from single canonical source.

**Input**:

- `source.kgcmd`: Canonical source file with tagged sections

**Output**:

- `tutorial/<name>.md`: Learning-oriented, step-by-step
- `how-to/<name>.md`: Task-oriented, problem-solving
- `reference/<name>.md`: Information-oriented, complete API
- `explanation/<name>.md`: Understanding-oriented, context

**Implementation**:

```javascript
// Subagent: diataxis-agent
// Tools: Read, Write
// Timeout: 10s (justified: 4 file writes + transformations)

async function generateDiataxis(sourcePath) {
  // 1. Parse source, extract tagged sections
  const source = await readFile(sourcePath);
  const sections = parseTaggedSections(source);

  // Diataxis section tags:
  // <!-- @tutorial --> - Include in tutorial view
  // <!-- @howto --> - Include in how-to view
  // <!-- @reference --> - Include in reference view
  // <!-- @explanation --> - Include in explanation view

  // 2. Build each view by filtering sections
  const views = {
    tutorial: filterSections(sections, 'tutorial'),
    howto: filterSections(sections, 'howto'),
    reference: filterSections(sections, 'reference'),
    explanation: filterSections(sections, 'explanation'),
  };

  // 3. Apply view-specific transformations
  const renderedViews = {
    tutorial: renderTutorial(views.tutorial), // Add step numbers, prerequisites
    howto: renderHowTo(views.howto), // Focus on tasks, outcomes
    reference: renderReference(views.reference), // Complete API tables, signatures
    explanation: renderExplanation(views.explanation), // Add diagrams, context
  };

  // 4. Write to appropriate directories
  const baseName = path.basename(sourcePath, '.kgcmd');
  const outputs = {};

  for (const [view, content] of Object.entries(renderedViews)) {
    const outputPath = `docs/${view}/${baseName}.md`;
    await writeFile(outputPath, content);
    outputs[view] = outputPath;

    // Generate receipt for each view
    const receipt = {
      source: sourcePath,
      view: view,
      sections_included: views[view].length,
      content_hash: computeSHA256(content),
      generated_at: new Date().toISOString(),
    };
    await writeFile(`${outputPath}.receipt.json`, receipt);
  }

  return {
    source: sourcePath,
    views_generated: Object.keys(outputs).length,
    outputs: outputs,
    total_sections: sections.length,
  };
}
```

**Example**:

```bash
# Generate all 4 views from canonical source
/kgc:diataxis docs/source/oxigraph-api.kgcmd

# Output:
# 🔄 Generating Diátaxis views from oxigraph-api.kgcmd
# 📚 Tutorial: docs/tutorial/oxigraph-api.md (18 sections, 3.2 KB)
# 🛠️  How-To: docs/how-to/oxigraph-api.md (12 sections, 2.1 KB)
# 📖 Reference: docs/reference/oxigraph-api.md (34 sections, 8.4 KB)
# 💡 Explanation: docs/explanation/oxigraph-api.md (9 sections, 4.7 KB)
# 🧾 Receipts: 4 files generated
# ⏱️  Duration: 2.7s

# Verify view consistency
find docs -name "oxigraph-api.md.receipt.json" -exec cat {} \; | jq '.source'
# All must point to docs/source/oxigraph-api.kgcmd
```

**Adversarial Verification**:

```bash
# Are all 4 views generated?
ls -1 docs/{tutorial,howto,reference,explanation}/oxigraph-api.md | wc -l  # Must be 4

# Do receipts validate?
find docs -name "oxigraph-api.md" -exec /kgc:prove {} \;  # 100% valid

# Are views deterministic?
/kgc:diataxis docs/source/oxigraph-api.kgcmd  # Run twice
git diff docs/tutorial/oxigraph-api.md  # Must be identical (except timestamp)

# Section counts add up?
cat docs/{tutorial,howto,reference,explanation}/oxigraph-api.md.receipt.json | \
  jq -s 'map(.sections_included) | add'  # Check against source
```

**Error Handling**:

- **Missing view tags**: WARN, generate with all sections (default to reference)
- **Duplicate tags**: WARN, section appears in multiple views (valid use case)
- **Empty view**: WARN, suggest adding tags to source
- **Output directory missing**: CREATE directories automatically
- **Source parse error**: ERROR, show line number, suggest tag syntax

---

### `/kgc:frontier`

**Purpose**: Compute composition frontier - dominance-pruned capability graph across packages.

**Input**:

- `filter` (optional): Package name to focus analysis

**Output**:

- Frontier graph at `.kgc/frontier-graph.json`
- Dominance report (which capabilities supersede others)
- Composition recommendations

**Implementation**:

```javascript
// Subagent: frontier-agent
// Tools: Read, Glob, Write
// Timeout: 15s (justified: graph analysis O(n²) complexity)

async function computeFrontier(filter = null) {
  // 1. Load atlas manifest
  const atlas = await readManifest('.kgc/atlas-manifest.json');

  // 2. Extract capabilities from each package
  const capabilities = atlas.surfaces
    .filter(s => !filter || s.package === filter)
    .flatMap(extractCapabilities);

  // Capability structure:
  // {
  //   id: "oxigraph:createStore",
  //   package: "@unrdf/oxigraph",
  //   version: "1.0.0",
  //   dominates: ["n3:N3Store"],  // Supersedes these
  //   requires: [],                 // Dependencies
  //   evidence: "src/store.mjs#L23" // File:line pointer
  // }

  // 3. Build dominance graph
  const graph = buildDominanceGraph(capabilities);

  // 4. Prune dominated capabilities (Pareto frontier)
  const frontier = pruneDominated(graph);

  // 5. Compute composition recommendations
  const recommendations = frontier.map(cap => ({
    capability: cap.id,
    use_instead_of: cap.dominates,
    reason: explainDominance(cap),
    migration_path: suggestMigration(cap),
  }));

  // 6. Write frontier manifest
  const manifest = {
    generated: new Date().toISOString(),
    filter: filter,
    total_capabilities: capabilities.length,
    frontier_size: frontier.length,
    pruned: capabilities.length - frontier.length,
    frontier: frontier.sort((a, b) => a.id.localeCompare(b.id)), // Deterministic
    recommendations: recommendations,
  };

  await writeManifest('.kgc/frontier-graph.json', manifest);

  return {
    total: capabilities.length,
    frontier: frontier.length,
    pruned: manifest.pruned,
    efficiency: ((1 - frontier.length / capabilities.length) * 100).toFixed(1) + '%',
  };
}
```

**Example**:

```bash
# Compute frontier for all packages
/kgc:frontier

# Output:
# 🔍 Analyzing 127 capabilities across 8 packages
# 🎯 Frontier: 31 capabilities (24% of total)
# ✂️  Pruned: 96 dominated capabilities (76% efficiency)
# 📊 Manifest: .kgc/frontier-graph.json (42.1 KB)
# 💡 Top recommendation: Use @unrdf/oxigraph instead of n3 (12 migrations)
# ⏱️  Duration: 4.2s

# Filter to specific package
/kgc:frontier @unrdf/oxigraph

# Output:
# 🔍 Analyzing 23 capabilities in @unrdf/oxigraph
# 🎯 Frontier: 8 capabilities (35% of package)
# ✂️  Pruned: 15 dominated capabilities
# 📊 Dominates: n3 (12), rdflib (4), graphy (7)
```

**Adversarial Verification**:

```bash
# Is frontier deterministic?
/kgc:frontier  # Run twice
diff .kgc/frontier-graph.json /tmp/frontier-1.json  # Must be identical

# Do all capabilities have evidence?
jq '.frontier[] | select(.evidence == null)' .kgc/frontier-graph.json  # Must be empty

# Are dominance claims verifiable?
jq '.recommendations[] | .migration_path' .kgc/frontier-graph.json  # Check paths exist

# Is pruning correct?
jq '.total_capabilities - .frontier_size' .kgc/frontier-graph.json  # Must equal .pruned
```

**Error Handling**:

- **No atlas manifest**: ERROR, suggest running `/kgc:scan` first
- **Circular dominance**: WARN, flag as design issue, include both in frontier
- **Missing evidence**: WARN, include capability but flag for verification
- **Filter no match**: WARN, return empty frontier with suggestion
- **Version conflict**: WARN, include both versions, recommend resolution

---

## 🤖 Agent Skills (Autonomous Behaviors)

### Receipt Discipline

**Trigger**: Agent writes documentation or code
**Action**: Auto-attach receipt pointer to nearest verifiable artifact

```javascript
// Whenever Write tool used for .md/.kgcmd files:
async function onWriteDocument(filePath, content) {
  // 1. Scan content for claims (API usage, code examples, performance numbers)
  const claims = extractClaims(content);

  // 2. For each claim, attempt to find evidence
  const receipts = await Promise.all(
    claims.map(async claim => {
      const artifact = await findEvidence(claim);
      if (!artifact) {
        return {
          claim: claim.text,
          status: 'UNVERIFIED',
          suggestion: `Add receipt for: ${claim.text}`,
        };
      }
      return {
        claim: claim.text,
        pointer: artifact.path + '#L' + artifact.line,
        hash: computeSHA256(artifact.content),
        status: 'VERIFIED',
      };
    })
  );

  // 3. If unverified claims exist, append warning
  const unverified = receipts.filter(r => r.status === 'UNVERIFIED');
  if (unverified.length > 0) {
    console.warn(`⚠️  ${unverified.length} unverified claims in ${filePath}`);
    console.warn('Run /kgc:prove to add receipts');
  }
}
```

### Evidence Discipline

**Trigger**: Agent claims capability or feature exists
**Action**: Auto-attach file:line pointer, refuse vague claims

````javascript
// Whenever agent responds to user query:
async function validateClaim(response) {
  // Pattern matching for unsubstantiated claims:
  const vaguePatterns = [
    /we have \d+ files/i, // "we have ~50 files" → /kgc:scan
    /tests? pass(es)?/i, // "tests pass" → show output
    /should work/i, // "should work" → prove it
    /mostly|almost|generally/i, // hedging without evidence
  ];

  for (const pattern of vaguePatterns) {
    if (pattern.test(response)) {
      throw new EvidenceError(
        `Vague claim detected: "${response.match(pattern)[0]}"`,
        `Provide specific evidence: file path, line number, or command output`
      );
    }
  }

  // Require evidence format: [claim](file:line) or ```output```
  const hasEvidence = /\[.*\]\(.*:\d+\)/.test(response) || /```[\s\S]*```/.test(response);
  if (!hasEvidence) {
    console.warn('⚠️  Response lacks evidence pointers. Add receipts.');
  }
}
````

### Determinism Discipline

**Trigger**: Agent generates lists, tables, JSON, or code
**Action**: Auto-sort, normalize whitespace, normalize JSON keys

```javascript
// Whenever Write tool used:
async function normalizeBefore Write(content, fileType) {
  switch (fileType) {
    case 'json':
      // Sort keys, 2-space indent, trailing newline
      return JSON.stringify(JSON.parse(content), null, 2) + '\n';

    case 'md':
      // Sort unordered lists, normalize headings
      return normalizeMarkdown(content);

    case 'js':
    case 'mjs':
      // Sort imports, normalize quotes
      return normalizeJavaScript(content);

    default:
      return content;
  }
}

function normalizeMarkdown(md) {
  // Sort unordered lists (preserve ordered)
  const lines = md.split('\n');
  let inList = false;
  let listBuffer = [];
  const normalized = [];

  for (const line of lines) {
    if (/^[-*+]\s/.test(line)) {
      if (!inList) inList = true;
      listBuffer.push(line);
    } else {
      if (inList) {
        normalized.push(...listBuffer.sort());
        listBuffer = [];
        inList = false;
      }
      normalized.push(line);
    }
  }

  return normalized.join('\n');
}
```

---

## 🔧 Subagent Delegation

### Agent Roster

| Agent              | Purpose                | Tools             | Timeout |
| ------------------ | ---------------------- | ----------------- | ------- |
| **atlas-agent**    | Discovery & indexing   | Glob, Read, Write | 10s     |
| **proof-agent**    | Validation & execution | Read, Write, Bash | 15s     |
| **diataxis-agent** | View projection        | Read, Write       | 10s     |
| **frontier-agent** | Graph analysis         | Read, Glob, Write | 15s     |
| **release-agent**  | Versioning & changelog | Read, Write, Bash | 10s     |

### Coordination Protocol

**Shared Receipt Manifest** (`.kgc/receipt-manifest.json`):

```json
{
  "version": "1.0.0",
  "last_updated": "2025-12-26T12:34:56Z",
  "agents": {
    "atlas-agent": {
      "status": "idle",
      "last_run": "2025-12-26T12:30:00Z",
      "manifest": ".kgc/atlas-manifest.json"
    },
    "proof-agent": {
      "status": "running",
      "current_task": "/kgc:prove docs/api/store.md",
      "started_at": "2025-12-26T12:34:00Z"
    }
  },
  "receipt_index": {
    "docs/api/store.md": {
      "receipts": 12,
      "valid": 11,
      "stale": 1,
      "last_verified": "2025-12-26T12:34:56Z"
    }
  }
}
```

### Delegation Rules

1. **Atlas before Proof**: Always run `/kgc:scan` before `/kgc:prove` (proof needs manifest)
2. **Refresh before Diataxis**: Source must have current outputs before projection
3. **Scan before Frontier**: Frontier needs complete atlas
4. **Single writer**: Only one agent writes to manifest at a time (file lock)
5. **Receipt validation**: Every write includes receipt generation

---

## 🚨 Error Handling

### Severity Levels

| Level     | When                    | Action                          | Exit Code |
| --------- | ----------------------- | ------------------------------- | --------- |
| **INFO**  | Successful operation    | Log, continue                   | 0         |
| **WARN**  | Degraded but functional | Log, suggest fix, continue      | 0         |
| **ERROR** | Cannot proceed          | Log, suggest remediation, abort | 1         |
| **FATAL** | System corruption       | Log, emit denial receipt, abort | 2         |

### Error Messages

**Template**: `[LEVEL] [AGENT] [CODE]: Message (Suggestion: action)`

```bash
# Example errors:

[ERROR] proof-agent KGC_RECEIPT_MISSING: No receipt found for block#5 in docs/api/store.md
Suggestion: Add <!-- receipt:pointer=/path/to/artifact --> above code block

[WARN] atlas-agent KGC_RECEIPT_STALE: Receipt hash mismatch for src/store.mjs
Suggestion: Run /kgc:refresh docs/api/store.kgcmd to update

[FATAL] proof-agent KGC_MANIFEST_CORRUPT: Cannot parse .kgc/atlas-manifest.json (invalid JSON)
Suggestion: Restore from .kgc/atlas-manifest.json.backup or re-run /kgc:scan

[ERROR] diataxis-agent KGC_TAG_INVALID: Unknown tag <!-- @custom --> in docs/source/api.kgcmd:42
Suggestion: Use @tutorial, @howto, @reference, or @explanation
```

### Denial Receipts

When error prevents completion, emit denial receipt explaining what failed and why:

```json
{
  "type": "denial_receipt",
  "command": "/kgc:prove docs/api/store.md",
  "agent": "proof-agent",
  "error_code": "KGC_RECEIPT_MISSING",
  "error_message": "Block #5 lacks receipt pointer",
  "timestamp": "2025-12-26T12:45:00Z",
  "context": {
    "file": "docs/api/store.md",
    "line": 127,
    "block_id": "block-5"
  },
  "remediation": {
    "steps": [
      "1. Identify artifact that block#5 should reference",
      "2. Add <!-- receipt:pointer=/path/to/artifact --> above block",
      "3. Re-run /kgc:prove docs/api/store.md"
    ],
    "auto_fixable": false,
    "estimated_time": "2 min"
  }
}
```

### Graceful Degradation

```javascript
// Example: Partial proof validation
async function proveDocument(docPath) {
  try {
    const receipts = extractReceiptBlocks(docPath);
    const results = [];

    for (const receipt of receipts) {
      try {
        const result = await validateReceipt(receipt);
        results.push(result);
      } catch (err) {
        // Don't abort entire proof for one bad receipt
        console.warn(`[WARN] Skipping receipt#${receipt.id}: ${err.message}`);
        results.push({
          receipt_id: receipt.id,
          valid: false,
          error: err.message,
        });
      }
    }

    return {
      total: receipts.length,
      validated: results.filter(r => r.valid).length,
      errors: results.filter(r => !r.valid),
    };
  } catch (err) {
    // Fatal error - cannot continue
    throw new FatalError('KGC_PROOF_FAILED', err);
  }
}
```

---

## 🪝 Hook Triggers

### Pre-Commit: Verify Receipts

**Purpose**: Prevent committing docs without valid receipts

```bash
#!/usr/bin/env node
// .git/hooks/pre-commit

import { glob } from 'glob';
import { execSync } from 'node:child_process';

async function preCommitVerifyReceipts() {
  // 1. Get staged .md/.kgcmd files
  const staged = execSync('git diff --cached --name-only --diff-filter=ACM')
    .toString()
    .split('\n')
    .filter(f => /\.(md|kgcmd)$/.test(f));

  if (staged.length === 0) return; // No docs changed

  console.log(`🔍 Verifying ${staged.length} documents...`);

  // 2. Run proof validation on each
  const results = await Promise.all(
    staged.map(async (file) => {
      try {
        const output = execSync(`timeout 10s /kgc:prove ${file}`, {
          encoding: 'utf-8'
        });
        const match = output.match(/(\d+)\/(\d+) valid/);
        if (!match) throw new Error('Invalid output format');

        const [_, valid, total] = match;
        return {
          file,
          valid: parseInt(valid),
          total: parseInt(total),
          success: valid === total
        };
      } catch (err) {
        return { file, success: false, error: err.message };
      }
    })
  );

  // 3. Block commit if any invalid
  const failures = results.filter(r => !r.success);
  if (failures.length > 0) {
    console.error(`❌ Cannot commit: ${failures.length} documents have invalid receipts`);
    failures.forEach(f => {
      console.error(`   ${f.file}: ${f.error || `${f.valid}/${f.total} valid`}`);
    });
    console.error('\nFix with: /kgc:refresh <file> or /kgc:prove <file>');
    process.exit(1);
  }

  console.log('✅ All documents verified');
}

await preCommitVerifyReceipts();
```

### Post-Command: Verify Determinism

**Purpose**: Ensure generated files are deterministic (run twice, compare)

```bash
#!/usr/bin/env node
// Runs after /kgc:* commands

async function postCommandVerifyDeterminism(command, args) {
  // Only verify idempotent commands
  const idempotentCommands = ['/kgc:scan', '/kgc:refresh', '/kgc:diataxis', '/kgc:frontier'];
  if (!idempotentCommands.includes(command)) return;

  console.log('🔁 Verifying determinism...');

  // 1. Capture output files before re-run
  const beforeFiles = await captureOutputFiles(command, args);

  // 2. Re-run command
  execSync(`timeout 30s ${command} ${args.join(' ')}`);

  // 3. Compare outputs (ignore timestamps)
  const afterFiles = await captureOutputFiles(command, args);

  const diffs = [];
  for (const [path, beforeContent] of Object.entries(beforeFiles)) {
    const afterContent = afterFiles[path];
    if (!afterContent) {
      diffs.push(`${path}: DELETED on re-run`);
      continue;
    }

    // Normalize: remove timestamps, sort JSON keys
    const beforeNorm = normalizeForComparison(beforeContent);
    const afterNorm = normalizeForComparison(afterContent);

    if (beforeNorm !== afterNorm) {
      diffs.push(`${path}: CONTENT DIFFERS`);
    }
  }

  if (diffs.length > 0) {
    console.warn(`⚠️  Non-deterministic output detected:`);
    diffs.forEach(d => console.warn(`   ${d}`));
    console.warn('Suggestion: Add deterministic sorting, normalize timestamps');
  } else {
    console.log('✅ Determinism verified');
  }
}
```

### On-Error: Emit Denial Receipt

**Purpose**: Create auditable trail of what failed and why

```javascript
// Triggered on any ERROR or FATAL
function onError(error, context) {
  const denialReceipt = {
    type: 'denial_receipt',
    timestamp: new Date().toISOString(),
    command: context.command,
    agent: context.agent,
    error_code: error.code,
    error_message: error.message,
    stack_trace: error.stack,
    context: context,
    remediation: suggestRemediation(error),
  };

  // Write to .kgc/denials/<timestamp>.json
  const denialPath = `.kgc/denials/${Date.now()}.json`;
  writeFileSync(denialPath, JSON.stringify(denialReceipt, null, 2));

  // Also log to console
  console.error(`❌ [${error.code}] ${error.message}`);
  console.error(`📝 Denial receipt: ${denialPath}`);
  console.error(`💡 Suggestion: ${denialReceipt.remediation.steps[0]}`);
}
```

---

## 📊 Validation & Trust Model

### OTEL Integration

Every command MUST emit OpenTelemetry spans for external validation:

```javascript
import { trace } from '@opentelemetry/api';

async function kgcCommand(name, fn) {
  const tracer = trace.getTracer('kgc-markdown');

  return tracer.startActiveSpan(`kgc.${name}`, async span => {
    try {
      const result = await fn();

      span.setAttributes({
        'kgc.command': name,
        'kgc.status': 'success',
        'kgc.files_processed': result.files_processed || 0,
        'kgc.receipts_validated': result.receipts_validated || 0,
      });

      return result;
    } catch (err) {
      span.setStatus({ code: 2, message: err.message });
      span.recordException(err);
      throw err;
    } finally {
      span.end();
    }
  });
}
```

### Adversarial Validation

**Before claiming success, answer**:

1. ❓ Did command RUN or just generate code?
   - Evidence: Exit code, stdout output, duration
2. ❓ Are file counts exact or approximate?
   - Evidence: `ls -1 | wc -l`, not "~50 files"
3. ❓ Do receipts VALIDATE or just exist?
   - Evidence: `grep "valid: true"` count matches total
4. ❓ Is output deterministic?
   - Evidence: Ran twice, diffed, zero differences (except timestamp)
5. ❓ Can user reproduce independently?
   - Evidence: Clear command sequence, no hidden state

### Trust Requirements

| Claim                  | Evidence Required              | Validation                   |
| ---------------------- | ------------------------------ | ---------------------------- |
| "Scanned X files"      | `manifest.total_files`         | `find \| wc -l` matches      |
| "12 receipts valid"    | Proof report output            | Each receipt hash matches    |
| "Generated 4 views"    | File paths                     | All 4 files exist + readable |
| "Frontier pruned 76%"  | Math: (total - frontier)/total | Recalculate from manifest    |
| "Deterministic output" | Diff result                    | `diff file1 file2` = empty   |

---

## 🎯 Usage Examples

### Complete Workflow

```bash
# 1. Initial scan
/kgc:scan @unrdf/oxigraph
# ✅ Scanned 47 files, 31 receipts found

# 2. Validate existing docs
find docs -name "*.md" -exec /kgc:prove {} \;
# ❌ 3 files have stale receipts

# 3. Refresh stale docs
/kgc:refresh docs/api/store.kgcmd
/kgc:refresh docs/tutorial/quickstart.kgcmd
# ✅ Updated outputs, new hashes

# 4. Generate Diátaxis views
/kgc:diataxis docs/source/oxigraph-api.kgcmd
# ✅ 4 views generated

# 5. Compute frontier
/kgc:frontier
# ✅ Frontier: 31 capabilities (76% pruned)

# 6. Verify before commit
git add docs/
git commit -m "Update documentation"
# 🪝 Pre-commit hook runs /kgc:prove on all staged docs
# ✅ All receipts valid, commit proceeds
```

### CI/CD Integration

```yaml
# .github/workflows/kgc-validation.yml
name: KGC Documentation Validation

on: [pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Scan documentation
        run: timeout 30s /kgc:scan "docs/**/*.md"

      - name: Validate all receipts
        run: |
          find docs -name "*.md" | while read -r file; do
            timeout 20s /kgc:prove "$file" || exit 1
          done

      - name: Verify determinism
        run: |
          /kgc:scan > /tmp/scan1.json
          /kgc:scan > /tmp/scan2.json
          diff /tmp/scan1.json /tmp/scan2.json || {
            echo "❌ Non-deterministic output detected"
            exit 1
          }

      - name: Check frontier
        run: |
          /kgc:frontier > frontier.log
          grep "Frontier:" frontier.log
          # Fail if frontier grows unexpectedly
```

---

## 🔒 Security & Safety

1. **Code execution isolation**: All `/kgc:refresh` blocks run in sandboxed env
2. **Path traversal protection**: All file writes validated against workspace root
3. **Receipt tampering detection**: SHA-256 hashes + timestamp + content
4. **Denial-of-service protection**: Timeouts on all operations (5-20s)
5. **Privilege separation**: Agents have minimal tool access (principle of least privilege)

---

## 📈 Success Metrics

| Metric               | Target                 | Measurement                                 |
| -------------------- | ---------------------- | ------------------------------------------- |
| Receipt coverage     | ≥90%                   | `(files_with_receipts / total_files) * 100` |
| Validation pass rate | 100%                   | `valid_receipts / total_receipts`           |
| Determinism          | 100%                   | Zero diffs on re-run (excluding timestamps) |
| Command timeout      | <10s (95th percentile) | P95 duration from OTEL spans                |
| False positive rate  | <1%                    | Manually audited sample                     |

---

## 🎓 Lessons from Counter-Practice

1. **DON'T** trust agent claims without OTEL validation
2. **DON'T** assume file counts - measure with `ls | wc -l`
3. **DON'T** skip timeouts - silent hangs hide performance issues
4. **DO** batch all operations in single message
5. **DO** demand evidence for every claim
6. **DO** verify determinism by running commands twice

---

## 📚 Further Reading

- [Diátaxis Framework](https://diataxis.fr/) - Documentation system theory
- [Receipt-Driven Development](docs/rdd-methodology.md) - Core philosophy
- [Big Bang 80/20 Methodology](docs/bb80-20-methodology.md) - Implementation strategy
- [OTEL Validation Guide](docs/otel-validation.md) - Trust model details

---

**Version**: 1.0.0
**Last Updated**: 2025-12-26
**Maintained By**: KGC Plugin Team
**License**: MIT
