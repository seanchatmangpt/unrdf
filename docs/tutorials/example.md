# Knowledge Graph Circuits - Tutorial

> **Learning Path**: Get started with Knowledge Graph Circuits

## Getting Started

This tutorial will help you learn the fundamentals step by step.

### What You'll Learn

- Core Features
- Example Query
- Proof Structure
- Data Extraction
- Rendering

### Example Queries

```sparql
SELECT ?subject ?predicate ?object
WHERE {
  ?subject ?predicate ?object .
  FILTER(?predicate = rdf:type)
}
```

---

## Proof Appendix

### Verification Data

```json
{
  "merkle_root": "13303c265155675edeba85a9a6089c602c7d354e8b9e83d594f1d5254c8197b6",
  "o_hash": "kgc-example-001",
  "receipt_count": 2,
  "timestamp": "2025-12-27T00:02:06.561Z"
}
```

### Receipt Hashes

- `receipt-abc-123`
- `receipt-def-456`

### O-Hash Linkage

The document is cryptographically linked to o_hash `kgc-example-001` via Merkle tree.

### Receipt Linkages

```json
[
  {
    "receipt_hash": "d845e81178baf7e3281558cbf3be0b847de2486d55e2ca62119e0039fd76123d",
    "o_hash": "kgc-example-001",
    "combined_hash": "e548544e322cddf3b76b3eec88378760eb06b3d9bcde18a2ddfbe6a250f2b80c"
  },
  {
    "receipt_hash": "401cf4c30edf090a063531284d8ac879b9ae6099fe0a41f9377e0df1e77ea280",
    "o_hash": "kgc-example-001",
    "combined_hash": "308cec58558a222626fc9a189c26fa6008895fcab113efd36e4ffdc2be1416a3"
  }
]
```
