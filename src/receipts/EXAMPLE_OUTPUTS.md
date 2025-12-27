# Example Receipt Outputs

This document shows example outputs from the receipt system.

## Example 1: Single Receipt (JSON-LD)

```json
{
  "@context": {
    "unrdf": "https://unrdf.org/vocab#",
    "xsd": "http://www.w3.org/2001/XMLSchema#",
    "prov": "http://www.w3.org/ns/prov#"
  },
  "@type": "unrdf:Receipt",
  "@id": "urn:receipt:e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9",
  "unrdf:inputHashes": {
    "unrdf:ontologyReleases": [
      "a3f5d8c9e1b2f4a7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1",
      "b4c6d9e0f2a3b5c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1"
    ],
    "unrdf:deltaCapsule": "c5d7e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9"
  },
  "unrdf:decision": "allow",
  "unrdf:epoch": "τ_2025_12_26_1430_123",
  "unrdf:outputHash": "d6e8f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0",
  "unrdf:toolchainVersion": {
    "unrdf:node": "v18.19.0",
    "unrdf:packages": {
      "@unrdf/core": "^5.0.1",
      "@unrdf/oxigraph": "workspace:*",
      "hash-wasm": "^4.12.0"
    }
  },
  "prov:generatedAtTime": {
    "@type": "xsd:dateTime",
    "@value": "2025-12-26T14:30:00.123Z"
  },
  "unrdf:beforeHash": null,
  "unrdf:merkleRoot": null,
  "unrdf:receiptHash": "e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9"
}
```

## Example 2: Single Receipt (Turtle)

```turtle
@prefix unrdf: <https://unrdf.org/vocab#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix prov: <http://www.w3.org/ns/prov#> .

<urn:receipt:e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9> a unrdf:Receipt ;
  unrdf:decision "allow" ;
  unrdf:epoch "τ_2025_12_26_1430_123" ;
  unrdf:outputHash "d6e8f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0" ;
  prov:generatedAtTime "2025-12-26T14:30:00.123Z"^^xsd:dateTime ;
  unrdf:ontologyRelease "a3f5d8c9e1b2f4a7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1" ;
  unrdf:ontologyRelease "b4c6d9e0f2a3b5c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1" ;
  unrdf:deltaCapsule "c5d7e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9" ;
  unrdf:nodeVersion "v18.19.0" ;
  unrdf:receiptHash "e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9" .
```

## Example 3: Receipt Chain (3 linked receipts)

```json
{
  "@context": {
    "unrdf": "https://unrdf.org/vocab#",
    "xsd": "http://www.w3.org/2001/XMLSchema#"
  },
  "@type": "unrdf:ReceiptChain",
  "unrdf:length": 3,
  "unrdf:receipts": [
    {
      "@type": "unrdf:Receipt",
      "@id": "urn:receipt:receipt1_hash",
      "unrdf:decision": "allow",
      "unrdf:epoch": "τ_2025_12_26_1430_000",
      "unrdf:beforeHash": null,
      "unrdf:receiptHash": "receipt1_hash"
    },
    {
      "@type": "unrdf:Receipt",
      "@id": "urn:receipt:receipt2_hash",
      "unrdf:decision": "deny",
      "unrdf:epoch": "τ_2025_12_26_1430_001",
      "unrdf:beforeHash": "receipt1_hash",
      "unrdf:receiptHash": "receipt2_hash"
    },
    {
      "@type": "unrdf:Receipt",
      "@id": "urn:receipt:receipt3_hash",
      "unrdf:decision": "allow",
      "unrdf:epoch": "τ_2025_12_26_1430_002",
      "unrdf:beforeHash": "receipt2_hash",
      "unrdf:receiptHash": "receipt3_hash"
    }
  ]
}
```

## Example 4: Merkle Tree (10 receipts)

```
Receipts:
  0: receipt_0_hash
  1: receipt_1_hash
  2: receipt_2_hash
  3: receipt_3_hash
  4: receipt_4_hash
  5: receipt_5_hash
  6: receipt_6_hash
  7: receipt_7_hash
  8: receipt_8_hash
  9: receipt_9_hash

Merkle Tree:
                      ROOT_HASH
                    /            \
           LEVEL1_LEFT          LEVEL1_RIGHT
           /        \            /         \
      L2_0        L2_1       L2_2        L2_3
      / \         / \        / \         / \
    r0  r1     r2  r3     r4  r5      r6  r7   r8  r9

Merkle Root: merkle_root_hash_for_10_receipts
```

## Example 5: Merkle Proof (for receipt at index 5)

```json
{
  "receiptHash": "receipt_5_hash",
  "merkleRoot": "merkle_root_hash_for_10_receipts",
  "proof": [
    {
      "hash": "receipt_4_hash",
      "position": "left"
    },
    {
      "hash": "hash(receipt_6, receipt_7)",
      "position": "right"
    },
    {
      "hash": "hash(receipt_0...3)",
      "position": "left"
    },
    {
      "hash": "hash(receipt_8, receipt_9)",
      "position": "right"
    }
  ]
}
```

## Example 6: Determinism Verification

Input (same for all 3 receipts):
```json
{
  "inputHashes": {
    "ontologyReleases": ["hash1", "hash2"],
    "deltaCapsule": "hash3"
  },
  "decision": "allow",
  "outputHash": "hash4",
  "toolchainVersion": {
    "node": "v18.19.0",
    "packages": { "@unrdf/core": "^5.0.1" }
  },
  "timestamp": "2025-12-26T14:30:00.000Z"
}
```

Output:
```
Receipt 1 Hash: e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9
Receipt 2 Hash: e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9
Receipt 3 Hash: e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9

✓ Determinism verified: PASS
```

## Example 7: Chain Verification

```
Receipt Chain Verification:
  Receipt 0:
    ✓ Hash valid
    ✓ First receipt (beforeHash = null)
    ✓ Epoch: τ_2025_12_26_1430_000

  Receipt 1:
    ✓ Hash valid
    ✓ Chain link (beforeHash = receipt_0_hash)
    ✓ Epoch increases: τ_2025_12_26_1430_001 > τ_2025_12_26_1430_000

  Receipt 2:
    ✓ Hash valid
    ✓ Chain link (beforeHash = receipt_1_hash)
    ✓ Epoch increases: τ_2025_12_26_1430_002 > τ_2025_12_26_1430_001

Chain Verification: ✓ VALID (3 receipts, 0 errors)
```

## Example 8: Admissibility Receipt

```json
{
  "@type": "unrdf:Receipt",
  "@id": "urn:receipt:admissibility_hash",
  "unrdf:inputHashes": {
    "unrdf:ontologyReleases": [
      "ont_core_v1.0_hash",
      "ont_domain_v2.3_hash"
    ],
    "unrdf:deltaCapsule": "delta_add_organization_hash"
  },
  "unrdf:decision": "allow",
  "unrdf:epoch": "τ_2025_12_26_1430_000",
  "unrdf:outputHash": "universe_with_organization_hash",
  "prov:generatedAtTime": {
    "@type": "xsd:dateTime",
    "@value": "2025-12-26T14:30:00.000Z"
  }
}
```

## Example 9: Validation Receipt

```json
{
  "@type": "unrdf:Receipt",
  "@id": "urn:receipt:validation_hash",
  "unrdf:inputHashes": {
    "unrdf:ontologyReleases": [
      "ont_core_v1.0_hash",
      "ont_domain_v2.3_hash"
    ],
    "unrdf:deltaCapsule": "validation_report_shacl_hash"
  },
  "unrdf:decision": "allow",
  "unrdf:epoch": "τ_2025_12_26_1430_001",
  "unrdf:outputHash": "validation_state_hash",
  "unrdf:beforeHash": "admissibility_hash",
  "prov:generatedAtTime": {
    "@type": "xsd:dateTime",
    "@value": "2025-12-26T14:30:01.000Z"
  }
}
```

## Example 10: Projection Receipt

```json
{
  "@type": "unrdf:Receipt",
  "@id": "urn:receipt:projection_hash",
  "unrdf:inputHashes": {
    "unrdf:ontologyReleases": [
      "ont_core_v1.0_hash",
      "ont_domain_v2.3_hash"
    ],
    "unrdf:deltaCapsule": "projection_query_hash"
  },
  "unrdf:decision": "allow",
  "unrdf:epoch": "τ_2025_12_26_1430_002",
  "unrdf:outputHash": "projection_output_hash",
  "unrdf:beforeHash": "validation_hash",
  "prov:generatedAtTime": {
    "@type": "xsd:dateTime",
    "@value": "2025-12-26T14:30:02.000Z"
  }
}
```
