# Understanding Knowledge Graph Circuits

> **Conceptual Overview**: Learn the background and theory

## Why This Matters

This concept is fundamental to understanding Knowledge Graph Circuits.

## Background

The KGC framework provides:

- Deterministic proof generation
- Receipt-based verification
- O-Hash linkage for integrity

## Key Concepts

### Core Features

This concept explains the underlying principles.

### Example Query

This concept explains the underlying principles.

### Proof Structure

This concept explains the underlying principles.

### Data Extraction

This concept explains the underlying principles.

### Rendering

This concept explains the underlying principles.

## Proof Structure

Proofs use Merkle trees for efficient verification.

---

## Proof Appendix

### Verification Data

```json
{
  "merkle_root": "13303c265155675edeba85a9a6089c602c7d354e8b9e83d594f1d5254c8197b6",
  "o_hash": "kgc-example-001",
  "receipt_count": 2,
  "timestamp": "2025-12-27T00:02:06.581Z"
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
