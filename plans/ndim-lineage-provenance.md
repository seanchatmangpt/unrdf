# Phase 4: nDim Marketplace Lineage & Provenance

This plan outlines the formalization of lineage tracking, the definition of conformant proof objects, and the integration of automated verification into the UNRDF marketplace publishing workflow.

## 1. Formalize 'Lineage' gRPC RPC
We will extend the `KGCSidecar` service in `sidecar/proto/kgc-sidecar.proto` to support deep lineage inspection.

### Proto Changes
```proto
service KGCSidecar {
  // ... existing RPCs ...

  // Retrieve the full lineage (receipt chain) for an artifact or graph
  rpc GetLineage(GetLineageRequest) returns (GetLineageResponse);
}

message GetLineageRequest {
  string artifact_id = 1;
  int32 depth = 2; // Maximum depth of the lineage chain to retrieve
  bool include_merkle_proofs = 3;
}

message GetLineageResponse {
  repeated TransactionReceipt chain = 1;
  string merkle_root = 2;
  repeated MerkleProof proofs = 3;
}

message MerkleProof {
  string leaf_hash = 1;
  repeated string path = 2;
  int32 index = 3;
}
```

## 2. Conformant Proof Object (CPO)
The 'Conformant Proof Object' is a certified JSON artifact that accompanies every Decision Pack in the nDim Marketplace. It provides cryptographic evidence that the pack has passed all required quality and security gates.

### CPO Schema (Decision Pack)
```json
{
  "version": "latest",
  "pack_id": "unrdf:finance-v6",
  "timestamp": "2026-04-26T12:00:00Z",
  "fitness_score": 0.98,
  "conformance_traces": [
    {
      "hook_id": "hook:complexity-limit",
      "status": "passed",
      "metric": "cyclomatic_complexity",
      "value": 4
    },
    {
      "hook_id": "hook:dependency-safety",
      "status": "passed",
      "audit_ref": "sha256:..."
    }
  ],
  "provenance": {
    "merkle_root": "blake3:...",
    "receipt_chain_hash": "blake3:...",
    "verifying_agent": "unrdf:process-verifier:v1.2"
  },
  "signature": {
    "alg": "EdDSA",
    "pubkey": "...",
    "value": "..."
  }
}
```

## 3. Integration of `ProcessVerifier` into `unrdf pack`
The `unrdf pack` command will be upgraded from a simple bundling tool to a certification engine.

### Implementation Steps:
1.  **Initialize `AdmissionGate`**: Before packing, `unrdf pack` will initialize an internal `AdmissionGate` or connect to a local KGC Sidecar.
2.  **Verify Assets**:
    - Each asset defined in `unrdf.toml` is passed through the `AdmissionGate`.
    - `ProcessVerifier` executes the registered `KGCHooks` (e.g., RDF validation, policy checks).
3.  **Generate CPO**:
    - Collect the `TransactionReceipt` chain for all asset validations.
    - Compute the Merkle root of the chain.
    - Calculate the aggregate `fitness_score` based on hook results.
4.  **Inject `proof.json`**:
    - The generated CPO is written to `proof.json`.
    - `proof.json` is appended to the `.unrdf` (tar.gz) archive.
5.  **Manifest Update**:
    - `manifest.json` will now include a `proof_ref` pointing to `proof.json`.

## Adversarial Review

### Critique 1: "Is 'Lineage' too computationally expensive for high-frequency trading (HFT) artifacts?"
**Analysis**: HFT artifacts require microsecond-level latency. Recording a full BLAKE3 hash for every atomic change and re-computing Merkle roots can introduce overhead that exceeds HFT budgets.
**Mitigation**: 
- **Sampling**: For HFT, we implement "Probabilistic Lineage" where only a subset of transactions (e.g., 1%) are fully anchored with receipts.
- **Off-Critical Path**: Lineage generation and Merkle root computation should be asynchronous and decoupled from the execution hot-path using a non-blocking receipt queue.
- **Hardware Acceleration**: Utilize AVX-512 or dedicated FPGA/WASM acceleration for BLAKE3 hashing.

### Critique 2: "Does sharing 'Conformant' proof objects expose proprietary workflow secrets in the marketplace?"
**Analysis**: `conformance_traces` and `receipt_chain` might contain metadata (hook IDs, specific metric values, file paths) that reveal the internal manufacturing process of a proprietary Decision Pack.
**Mitigation**:
- **Zero-Knowledge Proofs (ZKP)**: Instead of sharing the full trace, we can share a ZKP that the artifact passed a specific set of rules without revealing the rules or the exact execution path.
- **Hashed Identifiers**: Anonymize `hook_id` and metadata in the public CPO, providing a mapping only to authorized auditors.
- **Redaction Policy**: Implement a `privacy_level` in `unrdf.toml` that controls the granularity of the exported `proof.json`.

## Deliverables
- [ ] Updated `sidecar/proto/kgc-sidecar.proto`
- [ ] New `packages/cli/src/lib/process-verifier.mjs`
- [ ] Modified `packages/cli/src/cli/commands/pack.mjs`
- [ ] Documentation update in `DEVELOPER_GUIDE.md`
