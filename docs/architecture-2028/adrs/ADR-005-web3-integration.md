# ADR-005: Web3 Integration Layer

**Status:** Proposed
**Date:** 2025-11-18
**Deciders:** System Architecture Team
**Technical Story:** Enable blockchain verification, smart contract integration, NFT metadata, and decentralized identity

## Context and Problem Statement

Web3 technologies offer decentralized trust, immutable verification, and digital asset management. How do we integrate blockchain capabilities with RDF knowledge graphs while maintaining performance, flexibility, and avoiding blockchain lock-in?

## Decision Drivers

- **Flexibility**: Support multiple blockchain networks (Ethereum, Polygon, Solana)
- **Performance**: Minimize blockchain interaction overhead
- **Cost**: Optimize for gas fees and transaction costs
- **Decentralization**: Support both public and private/consortium chains
- **Standards**: W3C DID, Verifiable Credentials compliance
- **Interoperability**: IPFS for distributed content storage

## Considered Options

### Option 1: Single Blockchain Focus (Ethereum only)
- **Pros**: Simple, focused, mature ecosystem
- **Cons**: Vendor lock-in, high gas fees, limited flexibility

### Option 2: Multi-Chain Abstraction Layer
- **Pros**: Flexibility, avoid lock-in, cost optimization
- **Cons**: Complex abstraction, more testing required

### Option 3: Hybrid On-Chain/Off-Chain (Recommended)
- **Pros**: Cost-effective, performance, flexibility
- **Cons**: More architectural complexity

## Decision Outcome

**Chosen option:** Option 3 - Hybrid On-Chain/Off-Chain

Implement a Web3 integration layer that:
1. Stores minimal critical data on-chain (hashes, proofs)
2. Stores full RDF data off-chain (IPFS, Ceramic)
3. Provides multi-chain abstraction
4. Integrates W3C DID and Verifiable Credentials

### Architecture Design

#### Component Structure

```
src/web3/
├── core/
│   ├── web3-manager.mjs            # Main orchestrator
│   ├── chain-registry.mjs          # Blockchain registry
│   ├── transaction-queue.mjs       # Transaction batching
│   └── schemas.mjs
│
├── smart-contracts/
│   ├── contract-bridge.mjs         # Smart contract interface
│   ├── rdf-contract-abi.mjs        # RDF-specific contract ABI
│   ├── event-listener.mjs          # Contract event listener
│   ├── contracts/
│   │   ├── rdf-registry.sol        # RDF graph registry contract
│   │   ├── rdf-verification.sol    # Merkle proof verification
│   │   └── rdf-access.sol          # On-chain access control
│   └── index.mjs
│
├── blockchain/
│   ├── verification-layer.mjs      # Blockchain verification
│   ├── merkle-proof.mjs            # Merkle tree proofs
│   ├── transaction-manager.mjs     # Transaction lifecycle
│   ├── adapters/
│   │   ├── ethereum-adapter.mjs    # Ethereum/EVM chains
│   │   ├── solana-adapter.mjs      # Solana
│   │   ├── polygon-adapter.mjs     # Polygon
│   │   └── substrate-adapter.mjs   # Polkadot/Substrate
│   └── index.mjs
│
├── nft/
│   ├── nft-metadata-adapter.mjs    # NFT metadata bridge
│   ├── metadata-schema.mjs         # RDF → JSON metadata
│   ├── collection-manager.mjs      # NFT collection management
│   └── index.mjs
│
├── did/
│   ├── did-resolver.mjs            # DID resolution
│   ├── did-document-manager.mjs    # DID document management
│   ├── verifiable-credentials.mjs  # VC creation/verification
│   ├── identity-manager.mjs        # Identity lifecycle
│   └── index.mjs
│
├── storage/
│   ├── ipfs-adapter.mjs            # IPFS storage
│   ├── ceramic-adapter.mjs         # Ceramic (mutable IPFS)
│   ├── arweave-adapter.mjs         # Arweave (permanent storage)
│   └── index.mjs
│
└── index.mjs
```

#### Core Interfaces

```javascript
/**
 * @typedef {Object} Web3Config
 * @property {string} network - Blockchain network
 * @property {string} rpcUrl - RPC endpoint
 * @property {Object} contracts - Contract addresses
 * @property {Object} storage - Distributed storage config
 */

/**
 * Web3 Integration Manager
 */
export class Web3Manager {
  constructor(config = {}) {
    this.config = {
      network: config.network || 'ethereum',
      rpcUrl: config.rpcUrl,
      contracts: config.contracts || {},
      storage: {
        provider: config.storage?.provider || 'ipfs',
        ...config.storage
      }
    };

    this.contractBridge = new ContractBridge(this.config);
    this.verificationLayer = new VerificationLayer(this.config);
    this.nftAdapter = new NFTMetadataAdapter(this.config);
    this.didManager = new IdentityManager(this.config);
    this.ipfsAdapter = new IPFSAdapter(this.config.storage);
  }

  /**
   * Register RDF graph on blockchain
   * @param {Store} store - N3 store
   * @param {Object} metadata - Graph metadata
   * @returns {Promise<Object>} Registration result
   */
  async registerGraph(store, metadata = {}) {
    const span = tracer.startSpan('web3.graph.register');

    try {
      span.setAttribute('graph.size', store.size);
      span.setAttribute('network', this.config.network);

      // Serialize RDF to Turtle
      const turtle = await this._serializeStore(store);

      // Store full RDF on IPFS
      const ipfsCid = await this.ipfsAdapter.add(turtle);

      span.setAttribute('ipfs.cid', ipfsCid);

      // Generate Merkle root from graph
      const merkleRoot = await this.verificationLayer.generateMerkleRoot(store);

      span.setAttribute('merkle.root', merkleRoot);

      // Register on blockchain (hash only)
      const tx = await this.contractBridge.call('registerGraph', {
        ipfsCid,
        merkleRoot,
        metadata: JSON.stringify(metadata),
        timestamp: Date.now()
      });

      span.setAttribute('tx.hash', tx.hash);
      span.setStatus({ code: SpanStatusCode.OK });

      return {
        ipfsCid,
        merkleRoot,
        transactionHash: tx.hash,
        blockNumber: tx.blockNumber,
        graphId: tx.events.GraphRegistered.graphId
      };

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Verify RDF graph against blockchain record
   * @param {Store} store - N3 store to verify
   * @param {string} graphId - On-chain graph ID
   * @returns {Promise<boolean>} Verification result
   */
  async verifyGraph(store, graphId) {
    const span = tracer.startSpan('web3.graph.verify');

    try {
      span.setAttribute('graph.id', graphId);

      // Get on-chain merkle root
      const onChainRoot = await this.contractBridge.call('getGraphMerkleRoot', {
        graphId
      });

      // Generate merkle root from provided store
      const computedRoot = await this.verificationLayer.generateMerkleRoot(store);

      // Compare roots
      const valid = onChainRoot === computedRoot;

      span.setAttribute('verification.valid', valid);
      span.setStatus({ code: SpanStatusCode.OK });

      return valid;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Create NFT with RDF metadata
   * @param {Object} metadata - NFT metadata (RDF)
   * @param {Object} options - Minting options
   * @returns {Promise<Object>} Minted NFT
   */
  async mintNFT(metadata, options = {}) {
    const span = tracer.startSpan('web3.nft.mint');

    try {
      // Convert RDF metadata to JSON-LD
      const jsonld = await this.nftAdapter.rdfToMetadata(metadata);

      // Store metadata on IPFS
      const metadataUri = await this.ipfsAdapter.add(JSON.stringify(jsonld));

      span.setAttribute('metadata.uri', metadataUri);

      // Mint NFT
      const tx = await this.contractBridge.call('mintNFT', {
        to: options.recipient,
        metadataUri,
        ...options
      });

      span.setAttribute('tx.hash', tx.hash);
      span.setAttribute('token.id', tx.events.Transfer.tokenId);
      span.setStatus({ code: SpanStatusCode.OK });

      return {
        tokenId: tx.events.Transfer.tokenId,
        metadataUri,
        transactionHash: tx.hash
      };

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Resolve Decentralized Identifier (DID)
   * @param {string} did - DID to resolve
   * @returns {Promise<Object>} DID document
   */
  async resolveDID(did) {
    const span = tracer.startSpan('web3.did.resolve');

    try {
      span.setAttribute('did', did);

      const didDocument = await this.didManager.resolve(did);

      span.setStatus({ code: SpanStatusCode.OK });
      return didDocument;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Create Verifiable Credential
   * @param {Object} claims - RDF claims
   * @param {string} issuerDID - Issuer DID
   * @returns {Promise<Object>} Verifiable Credential
   */
  async createVerifiableCredential(claims, issuerDID) {
    const span = tracer.startSpan('web3.vc.create');

    try {
      span.setAttribute('issuer.did', issuerDID);

      const vc = await this.didManager.createCredential(claims, issuerDID);

      span.setStatus({ code: SpanStatusCode.OK });
      return vc;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }
}
```

#### Blockchain Verification Layer

```javascript
/**
 * Blockchain verification using Merkle proofs
 */
export class VerificationLayer {
  constructor(config) {
    this.config = config;
  }

  /**
   * Generate Merkle root for RDF graph
   * @param {Store} store - N3 store
   * @returns {Promise<string>} Merkle root hash
   */
  async generateMerkleRoot(store) {
    const span = tracer.startSpan('web3.verification.merkle_root');

    try {
      // Canonicalize RDF graph (deterministic serialization)
      const canonicalized = await this._canonicalizeGraph(store);

      // Build Merkle tree from quads
      const leaves = [];

      for (const quad of canonicalized) {
        const quadHash = this._hashQuad(quad);
        leaves.push(quadHash);
      }

      // Build tree
      const merkleTree = this._buildMerkleTree(leaves);

      const root = merkleTree.root;

      span.setAttribute('merkle.root', root);
      span.setAttribute('merkle.leaves', leaves.length);
      span.setStatus({ code: SpanStatusCode.OK });

      return root;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Generate Merkle proof for a specific quad
   * @param {Store} store - N3 store
   * @param {Quad} quad - Quad to prove
   * @returns {Promise<Array<string>>} Merkle proof
   */
  async generateMerkleProof(store, quad) {
    const span = tracer.startSpan('web3.verification.merkle_proof');

    try {
      // Canonicalize graph
      const canonicalized = await this._canonicalizeGraph(store);

      // Find quad index
      const quadArray = Array.from(canonicalized);
      const quadIndex = quadArray.findIndex((q) => q.equals(quad));

      if (quadIndex === -1) {
        throw new Error('Quad not found in graph');
      }

      // Build Merkle tree
      const leaves = quadArray.map((q) => this._hashQuad(q));
      const merkleTree = this._buildMerkleTree(leaves);

      // Generate proof
      const proof = merkleTree.getProof(quadIndex);

      span.setAttribute('proof.length', proof.length);
      span.setStatus({ code: SpanStatusCode.OK });

      return proof;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Verify Merkle proof
   * @param {string} merkleRoot - Expected root
   * @param {Quad} quad - Quad to verify
   * @param {Array<string>} proof - Merkle proof
   * @returns {boolean} Proof valid
   */
  verifyMerkleProof(merkleRoot, quad, proof) {
    const quadHash = this._hashQuad(quad);

    let computedHash = quadHash;

    for (const proofElement of proof) {
      if (proofElement.position === 'left') {
        computedHash = this._hashPair(proofElement.hash, computedHash);
      } else {
        computedHash = this._hashPair(computedHash, proofElement.hash);
      }
    }

    return computedHash === merkleRoot;
  }

  _hashQuad(quad) {
    // Hash quad using SHA-256
    const quadString = quad.subject.value +
                       quad.predicate.value +
                       quad.object.value +
                       (quad.graph?.value || '');

    return sha256(quadString).toString('hex');
  }

  _buildMerkleTree(leaves) {
    // Build binary Merkle tree
    let currentLevel = leaves;

    while (currentLevel.length > 1) {
      const nextLevel = [];

      for (let i = 0; i < currentLevel.length; i += 2) {
        if (i + 1 < currentLevel.length) {
          const hash = this._hashPair(currentLevel[i], currentLevel[i + 1]);
          nextLevel.push(hash);
        } else {
          nextLevel.push(currentLevel[i]);
        }
      }

      currentLevel = nextLevel;
    }

    return {
      root: currentLevel[0],
      getProof: (index) => this._generateProofPath(leaves, index)
    };
  }
}
```

#### Smart Contract Bridge

```javascript
/**
 * Bridge to smart contracts for RDF operations
 */
export class ContractBridge {
  constructor(config) {
    this.config = config;
    this.provider = this._initializeProvider();
    this.contracts = this._loadContracts();
  }

  /**
   * Call smart contract function
   * @param {string} functionName - Function to call
   * @param {Object} params - Function parameters
   * @returns {Promise<Object>} Transaction result
   */
  async call(functionName, params = {}) {
    const span = tracer.startSpan('web3.contract.call');

    try {
      span.setAttribute('function', functionName);
      span.setAttribute('network', this.config.network);

      // Get contract instance
      const contract = this.contracts.rdfRegistry;

      // Estimate gas
      const gasEstimate = await contract.estimateGas[functionName](
        ...Object.values(params)
      );

      span.setAttribute('gas.estimate', gasEstimate.toString());

      // Execute transaction
      const tx = await contract[functionName](...Object.values(params), {
        gasLimit: gasEstimate.mul(120).div(100)  // 20% buffer
      });

      // Wait for confirmation
      const receipt = await tx.wait();

      span.setAttribute('tx.hash', receipt.transactionHash);
      span.setAttribute('block.number', receipt.blockNumber);
      span.setAttribute('gas.used', receipt.gasUsed.toString());
      span.setStatus({ code: SpanStatusCode.OK });

      return {
        hash: receipt.transactionHash,
        blockNumber: receipt.blockNumber,
        gasUsed: receipt.gasUsed.toNumber(),
        events: this._parseEvents(receipt)
      };

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Listen to contract events
   * @param {string} eventName - Event name
   * @param {Function} handler - Event handler
   */
  async listen(eventName, handler) {
    const span = tracer.startSpan('web3.contract.listen');

    try {
      const contract = this.contracts.rdfRegistry;

      contract.on(eventName, async (...args) => {
        const event = args[args.length - 1];  // Last arg is event object

        span.addEvent('event_received', {
          'event.name': eventName,
          'event.block': event.blockNumber,
          'event.tx': event.transactionHash
        });

        await handler(this._parseEvent(event, args.slice(0, -1)));
      });

      span.setStatus({ code: SpanStatusCode.OK });

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }
}
```

#### DID and Verifiable Credentials

```javascript
/**
 * Decentralized Identity Manager
 */
export class IdentityManager {
  constructor(config) {
    this.config = config;
    this.didResolver = new DIDResolver(config);
  }

  /**
   * Resolve DID to DID Document
   * @param {string} did - DID to resolve
   * @returns {Promise<Object>} DID Document
   */
  async resolve(did) {
    const span = tracer.startSpan('web3.did.resolve');

    try {
      span.setAttribute('did', did);

      // Parse DID method
      const method = this._parseMethod(did);

      span.setAttribute('did.method', method);

      // Resolve based on method
      const didDocument = await this.didResolver.resolve(method, did);

      span.setStatus({ code: SpanStatusCode.OK });
      return didDocument;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Create Verifiable Credential
   * @param {Object} claims - RDF claims
   * @param {string} issuerDID - Issuer DID
   * @returns {Promise<Object>} Verifiable Credential
   */
  async createCredential(claims, issuerDID) {
    const span = tracer.startSpan('web3.vc.create');

    try {
      // Resolve issuer DID to get signing key
      const issuerDoc = await this.resolve(issuerDID);

      // Create credential
      const credential = {
        '@context': [
          'https://www.w3.org/2018/credentials/v1',
          'https://w3id.org/security/suites/jws-2020/v1'
        ],
        type: ['VerifiableCredential'],
        issuer: issuerDID,
        issuanceDate: new Date().toISOString(),
        credentialSubject: claims,
      };

      // Sign credential
      const signedCredential = await this._signCredential(
        credential,
        issuerDoc.verificationMethod[0]
      );

      span.setStatus({ code: SpanStatusCode.OK });
      return signedCredential;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Verify Verifiable Credential
   * @param {Object} credential - Verifiable Credential
   * @returns {Promise<boolean>} Verification result
   */
  async verifyCredential(credential) {
    const span = tracer.startSpan('web3.vc.verify');

    try {
      // Resolve issuer DID
      const issuerDoc = await this.resolve(credential.issuer);

      // Verify signature
      const valid = await this._verifySignature(
        credential,
        issuerDoc.verificationMethod[0]
      );

      span.setAttribute('verification.valid', valid);
      span.setStatus({ code: SpanStatusCode.OK });

      return valid;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }
}
```

### Integration with Knowledge Hooks

```javascript
// Web3 hooks
defineHook('web3.graph.registered', {
  phase: 'post',
  effect: async ({ result }) => {
    // Log blockchain registration
    await logger.info(`Graph registered on-chain: ${result.graphId}`);

    // Update local metadata
    await metadata.update({ blockchainId: result.graphId });
  }
});

defineHook('web3.verification.completed', {
  phase: 'post',
  condition: async ({ valid }) => !valid,
  effect: async ({ graphId }) => {
    // Alert on verification failure
    await alertManager.send({
      severity: 'critical',
      message: `Graph verification failed: ${graphId}`
    });
  }
});

defineHook('web3.contract.event', {
  phase: 'post',
  effect: async ({ event }) => {
    // React to blockchain events
    await eventBus.emit('blockchain:event', event);
  }
});
```

### Usage Examples

```javascript
// Example 1: Register graph on blockchain
import { Web3Manager } from 'unrdf/web3';
import { useGraph } from 'unrdf';

const { store } = useGraph();
const web3 = new Web3Manager({
  network: 'polygon',
  rpcUrl: 'https://polygon-rpc.com',
  contracts: {
    rdfRegistry: '0x...'
  }
});

const result = await web3.registerGraph(store, {
  name: 'Research Dataset 2024',
  description: 'Scientific publications'
});

console.log(`Graph registered: ${result.graphId}`);
console.log(`IPFS CID: ${result.ipfsCid}`);
console.log(`Transaction: ${result.transactionHash}`);

// Example 2: Verify graph integrity
const valid = await web3.verifyGraph(store, result.graphId);
console.log(`Graph valid: ${valid}`);

// Example 3: Mint NFT with RDF metadata
const nftMetadata = {
  name: 'Digital Art #123',
  description: 'Generative art piece',
  properties: {
    artist: 'http://example.org/artists/alice',
    technique: 'http://example.org/techniques/generative',
    year: 2024
  }
};

const nft = await web3.mintNFT(nftMetadata, {
  recipient: '0x...'
});

console.log(`NFT minted: Token ID ${nft.tokenId}`);

// Example 4: Create and verify Verifiable Credential
const credential = await web3.createVerifiableCredential({
  name: 'Alice Johnson',
  degree: 'PhD in Computer Science',
  university: 'http://example.org/universities/mit'
}, 'did:ethr:0x...');

const credentialValid = await web3.verifyCredential(credential);
console.log(`Credential valid: ${credentialValid}`);
```

## Consequences

### Positive

- **Trust**: Blockchain provides immutable verification
- **Decentralization**: No central authority required
- **Interoperability**: W3C DID and VC standards
- **NFT Support**: Rich RDF metadata for digital assets
- **Flexibility**: Multi-chain support

### Negative

- **Cost**: Blockchain transactions incur fees
- **Latency**: Blockchain operations are slow (seconds/minutes)
- **Complexity**: Web3 infrastructure is complex
- **Scalability**: On-chain storage is expensive

## Performance & Cost

| Operation | Latency | Cost (Polygon) |
|-----------|---------|----------------|
| Register Graph | 2-5s | ~$0.01 |
| Verify Graph | <100ms | Free (read-only) |
| Mint NFT | 2-5s | ~$0.05 |
| DID Resolution | <500ms | Free |
| VC Creation | <200ms | Free (off-chain) |

## References

- [W3C DID Specification](https://www.w3.org/TR/did-core/)
- [Verifiable Credentials](https://www.w3.org/TR/vc-data-model/)
- [ERC-721 NFT Standard](https://eips.ethereum.org/EIPS/eip-721)
- [IPFS](https://ipfs.io/)
