# @unrdf/kgc-docs

KGC Markdown parser and dynamic documentation generator with cryptographic proof anchoring.

## Features

- **YAML Frontmatter Parsing**: Parse o_hash, policy_id, bounds, receipts, views, sources
- **Specialized Fenced Blocks**: Support for kgc:query, kgc:proof, kgc:extract, kgc:render
- **Deterministic AST**: Build consistent abstract syntax trees from markdown
- **4 Diataxis Views**: Generate tutorials, how-to, reference, and explanations
- **Merkle Proof Generation**: Create cryptographic proof trees with receipt hashing
- **O-Hash Linkage**: Link receipts to root hash for verification
- **CLI Tool**: Build, verify, refresh, and prove documentation

## Installation

```bash
pnpm add @unrdf/kgc-docs
```

## Quick Start

### 1. Create a .kgcmd file

```markdown
---
o_hash: my-doc-001
policy_id: policy-001
bounds: []
receipts: [receipt-001, receipt-002]
views: [tutorial, how-to, reference, explanation]
sources: []
---
# My Documentation

\`\`\`kgc:query
SELECT ?s ?p ?o WHERE { ?s ?p ?o }
\`\`\`
```

### 2. Build documentation

```bash
kgc-docs build docs/src docs
```

This generates 4 views:
- `docs/tutorials/my-doc.md`
- `docs/how-to/my-doc.md`
- `docs/reference/my-doc.md`
- `docs/explanations/my-doc.md`

### 3. Verify proofs

```bash
kgc-docs verify docs
```

### 4. Generate proof trees

```bash
kgc-docs prove docs/src proofs
```

## API

### Parser

```javascript
import { parseKGCMarkdown, buildAST } from '@unrdf/kgc-docs/parser';

const markdown = '...';
const ast = parseKGCMarkdown(markdown);
```

### Renderer

```javascript
import { renderDiataxisView } from '@unrdf/kgc-docs/renderer';

const tutorial = renderDiataxisView(ast, 'tutorial');
const reference = renderDiataxisView(ast, 'reference');
```

### Proof

```javascript
import { generateProofTree, verifyProof } from '@unrdf/kgc-docs/proof';

const proof = generateProofTree(ast);
const isValid = verifyProof(proof, ast);
```

## CLI Commands

```bash
# Build documentation
kgc-docs build [sourceDir] [outputDir]

# Verify proof appendices
kgc-docs verify [outputDir]

# Rebuild all docs
kgc-docs refresh [sourceDir] [outputDir]

# Generate proof trees
kgc-docs prove [sourceDir] [proofsDir]
```

## Proof Structure

Each generated document includes a proof appendix:

```markdown
## Proof Appendix

### Verification Data

\`\`\`json
{
  "merkle_root": "abc123...",
  "o_hash": "my-doc-001",
  "receipt_count": 2,
  "timestamp": "2025-12-27T00:00:00.000Z"
}
\`\`\`

### Receipt Hashes

- `receipt-001`
- `receipt-002`

### O-Hash Linkage

The document is cryptographically linked to o_hash `my-doc-001` via Merkle tree.
```

## Test Results

All 41 tests passing:
- ✅ Frontmatter parsing with Zod validation
- ✅ Fenced block parsing (query, proof, extract, render)
- ✅ Deterministic AST generation
- ✅ 4 Diataxis view rendering
- ✅ Proof appendix generation
- ✅ Merkle tree creation
- ✅ Proof verification
- ✅ Receipt-to-hash linkage

## Architecture

```
packages/kgc-docs/
├── src/
│   ├── kgc-markdown.mjs    # Main entry point
│   ├── parser.mjs           # YAML + fenced block parser
│   ├── renderer.mjs         # Diataxis view renderer
│   └── proof.mjs            # Merkle tree + verification
├── bin/
│   └── kgc-docs.mjs        # CLI tool
└── test/
    ├── kgc-markdown.test.mjs  # Vitest tests
    └── run-tests.mjs          # Standalone test runner
```

## License

MIT
