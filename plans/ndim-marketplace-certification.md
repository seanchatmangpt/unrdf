# Phase 1: nDim Marketplace & Artifact Certification Plan

This document outlines the implementation strategy for the nDim Marketplace and Post-Quantum (PQ) artifact certification within the UNRDF ecosystem, addressing gaps identified in "Explore Gap 6".

## 1. CLI Command Implementation: `unrdf pack` & `unrdf publish`

### 1.1 `unrdf pack`
The `pack` command bundles RDF assets into a certified "Policy Pack" artifact.

**Workflow:**
1.  **Discovery**: Read `unrdf.toml` to identify assets (ontologies, SHACL shapes, N3 rules).
2.  **Canonicalization**: Normalize all RDF content to N-Triples and sort them to ensure deterministic hashing.
3.  **Receipt Generation**: 
    - Use `@unrdf/receipts` to create a `hybrid` PQ receipt.
    - Content hash will be the BLAKE3 hash of the canonicalized triples.
    - Operations array will consist of the individual triples as subjects of the provenance record.
4.  **Bundling**: Create a `.unrdf` (tarball) containing:
    - `manifest.json`: Metadata and references to assets.
    - `receipt.json`: The PQ-enabled certification receipt.
    - `assets/`: The raw RDF files.

**Command Signature:**
```bash
unrdf pack [--config unrdf.toml] [--output ./my-pack.unrdf] [--pq-scheme hybrid]
```

### 1.2 `unrdf publish`
The `publish` command registers a packed artifact in the marketplace.

**Workflow:**
1.  **Validation**: Verify the local artifact's PQ signature before publishing.
2.  **Transport**: 
    - **Default**: Announce the artifact CID over **Hyper-Swarm**.
    - **Registry**: Upload to a configured nDim Registry (e.g., a SPARQL endpoint or IPFS gateway).
3.  **Metadata Update**: Update the decentralized registry index with the artifact's metadata and certification status.

**Command Signature:**
```bash
unrdf publish <artifact.unrdf> [--registry https://registry.unrdf.io] [--decentralized]
```

---

## 2. Policy Pack Specification (v1)

A **Policy Pack** is a portable unit of governance. It must be self-describing and verifiable.

### 2.1 Artifact Structure
- `/manifest.json`: 
  - `pack_id`: Unique Q* identifier.
  - `version`: SemVer.
  - `capabilities`: List of hooks or rules provided.
  - `dependencies`: References to other Policy Packs.
- `/receipt.json`: A standard `PQReceipt` object (see `@unrdf/receipts`).
- `/ontology.ttl`: The primary domain ontology.
- `/constraints.shacl`: SHACL shapes for structural integrity.
- `/logic.n3`: N3 rules for inference and policy enforcement.

### 2.2 Metadata Requirements
All Policy Packs must include:
- `dc:creator`: The identity URI of the pack author.
- `dc:issued`: Timestamp.
- `unrdf:verificationLevel`: L4 (Classical) or L5 (Hybrid PQ).

---

## 3. Constitutional Integrity Gate (Gate 1) Hardening

Replace the current stub in `packages/cli/src/cli/commands/sync.mjs` with actual verification logic.

**Logic:**
1.  If `--harden` is enabled:
    - Search for a companion `.receipt.json` file for the ontology source or a `Header` comment containing the JSON receipt.
    - Load the ontology triples.
    - Call `verifyPQReceipt(receipt, triples)`.
    - If `valid: false` or `signatureValid: false`, terminate with `[Gate 1] Constitutional Integrity Failure`.

---

## 4. Adversarial Review

### 4.1 "Will Post-Quantum (PQ) signatures make artifacts too large for IoT devices?"
**Critique**: Dilithium3 signatures add ~2.4KB and public keys add ~2KB. For a small 1KB sensor ontology, this is a 400% overhead, potentially exceeding MTU limits or storage capacity on ESP32/AtomVM nodes.

**Mitigation Strategy**:
- **Layered Verification**: High-power gateways verify the PQ signature and issue a "Classical Proxy Receipt" (short BLAKE3 hash) for local consumption by IoT nodes.
- **Selective Hardening**: Allow `L4` (classical only) receipts for edge devices while requiring `L5` (hybrid) for cloud/enterprise nodes.
- **Signature Stripping**: The artifact can be stripped of the heavy PQ signature after verification at the network edge, retaining only the "verified" bit for the local secure bus.

### 4.2 "Is a centralized 'publish' command contrary to the decentralized Hyper-Swarm vision?"
**Critique**: A `publish` command often implies a central "App Store" or authority, which contradicts the goal of a decentralized, self-healing knowledge graph.

**Defense**:
- `unrdf publish` is a **protocol-agnostic dispatcher**. By default, it uses Hyper-Swarm for P2P discovery.
- The "Marketplace" is an **aggregated view** of the swarm, not a central database.
- Centralized registries are supported only as high-availability mirrors for enterprise users, but the "Source of Truth" remains the signed, decentralized artifacts.

---

## 5. Implementation Schedule
1.  **Week 1**: Implement `unrdf pack` logic in `@unrdf/cli`.
2.  **Week 1**: Update `sync.mjs` Gate 1 with `verifyPQReceipt`.
3.  **Week 2**: Define Hyper-Swarm announcement protocol for `unrdf publish`.
4.  **Week 2**: Create first "Certified Compliance Pack" as a reference implementation.
