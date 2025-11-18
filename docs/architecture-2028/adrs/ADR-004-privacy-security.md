# ADR-004: Privacy & Security Enhanced Layer

**Status:** Proposed
**Date:** 2025-11-18
**Deciders:** System Architecture Team
**Technical Story:** Add comprehensive privacy and security features for enterprise and regulated environments

## Context and Problem Statement

Enterprise and regulated environments require strict data privacy, access control, and audit capabilities. How do we add encryption, fine-grained access control, zero-knowledge proofs, and comprehensive auditing while maintaining performance and usability?

## Decision Drivers

- **Compliance**: GDPR, CCPA, HIPAA, SOC 2 compliance
- **Privacy**: Data encryption at rest and in transit
- **Access Control**: Fine-grained, attribute-based access control
- **Auditability**: Immutable audit trails for all operations
- **Performance**: Minimal overhead (<20% for encryption)
- **Transparency**: Zero-knowledge proofs for privacy-preserving verification
- **Key Management**: Secure key storage and rotation

## Considered Options

### Option 1: Application-Level Security Only
- **Pros**: Simple, portable, no infrastructure changes
- **Cons**: Limited protection, key management complexity

### Option 2: Database-Level Encryption
- **Pros**: Transparent, comprehensive, key management integrated
- **Cons**: Database-dependent, limited flexibility

### Option 3: Layered Security Approach (Recommended)
- **Pros**: Defense in depth, flexibility, compliance-ready
- **Cons**: Most complex, performance overhead

## Decision Outcome

**Chosen option:** Option 3 - Layered Security Approach

Implement a comprehensive security layer with:
1. Field-level encryption for sensitive data
2. Attribute-Based Access Control (ABAC)
3. Zero-knowledge proof support for privacy-preserving verification
4. Immutable audit trail integrated with lockchain
5. Key management with HSM support

### Architecture Design

#### Component Structure

```
src/security-enhanced/
├── core/
│   ├── security-manager.mjs        # Main orchestrator
│   ├── security-context.mjs        # Request security context
│   ├── schemas.mjs
│   └── constants.mjs
│
├── encryption/
│   ├── encrypted-store.mjs         # Encrypted RDF store
│   ├── field-encryptor.mjs         # Field-level encryption
│   ├── crypto-provider.mjs         # Crypto operations
│   ├── algorithms/
│   │   ├── aes-gcm.mjs             # AES-256-GCM
│   │   ├── chacha20.mjs            # ChaCha20-Poly1305
│   │   └── fpe.mjs                 # Format-preserving encryption
│   └── key-manager.mjs             # Key lifecycle management
│
├── access-control/
│   ├── abac-engine.mjs             # ABAC policy engine
│   ├── policy-enforcer.mjs         # Policy enforcement point
│   ├── policy-parser.mjs           # Policy DSL parser
│   ├── permission-resolver.mjs     # Permission resolution
│   ├── role-manager.mjs            # RBAC support
│   └── policies/
│       ├── predefined/
│       │   ├── gdpr-policy.mjs     # GDPR compliance
│       │   ├── hipaa-policy.mjs    # HIPAA compliance
│       │   └── pii-policy.mjs      # PII protection
│       └── index.mjs
│
├── zk-proofs/
│   ├── zk-engine.mjs               # ZK proof engine
│   ├── proof-generator.mjs         # Proof generation
│   ├── proof-verifier.mjs          # Proof verification
│   ├── circuits/
│   │   ├── membership-proof.mjs    # Set membership
│   │   ├── range-proof.mjs         # Range proofs
│   │   └── equality-proof.mjs      # Equality without revealing
│   └── index.mjs
│
├── audit/
│   ├── audit-logger.mjs            # Audit event logging
│   ├── audit-store.mjs             # Immutable audit storage
│   ├── compliance-reporter.mjs     # Compliance reports
│   ├── retention-manager.mjs       # Data retention policies
│   └── index.mjs
│
├── key-management/
│   ├── key-store.mjs               # Key storage
│   ├── key-rotation.mjs            # Automatic key rotation
│   ├── hsm-adapter.mjs             # HSM integration
│   ├── vault-adapter.mjs           # HashiCorp Vault
│   └── index.mjs
│
└── index.mjs
```

#### Core Interfaces

```javascript
/**
 * @typedef {Object} SecurityConfig
 * @property {Object} encryption - Encryption configuration
 * @property {Object} accessControl - Access control configuration
 * @property {Object} audit - Audit configuration
 * @property {Object} zkProofs - Zero-knowledge proof configuration
 */

/**
 * Security Manager for comprehensive privacy and security
 */
export class SecurityManager {
  constructor(config = {}) {
    this.config = {
      encryption: {
        algorithm: 'aes-256-gcm',
        keyDerivation: 'pbkdf2',
        ...config.encryption
      },
      accessControl: {
        mode: 'abac',  // 'abac' | 'rbac' | 'hybrid'
        policies: config.accessControl?.policies || []
      },
      audit: {
        enabled: true,
        immutable: true,
        ...config.audit
      },
      zkProofs: {
        enabled: false,
        ...config.zkProofs
      }
    };

    this.encryptedStore = new EncryptedStore(this.config.encryption);
    this.abacEngine = new ABACEngine(this.config.accessControl);
    this.auditLogger = new AuditLogger(this.config.audit);
    this.zkEngine = this.config.zkProofs.enabled
      ? new ZKEngine(this.config.zkProofs)
      : null;
  }

  /**
   * Create encrypted RDF store
   * @param {Object} options - Store options
   * @returns {Promise<EncryptedStore>}
   */
  async createEncryptedStore(options = {}) {
    const span = tracer.startSpan('security.store.create');

    try {
      span.setAttribute('encryption.algorithm', this.config.encryption.algorithm);

      const store = await this.encryptedStore.initialize(options);

      span.setStatus({ code: SpanStatusCode.OK });
      return store;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Enforce access control policy
   * @param {Object} context - Security context (user, resource, action)
   * @returns {Promise<boolean>} Access granted/denied
   */
  async enforce(context) {
    const span = tracer.startSpan('security.access_control.enforce');

    try {
      span.setAttribute('user.id', context.user.id);
      span.setAttribute('resource.type', context.resource.type);
      span.setAttribute('action', context.action);

      const decision = await this.abacEngine.evaluate(context);

      span.setAttribute('decision', decision.granted);

      // Audit access decision
      await this.auditLogger.log({
        type: 'access_control',
        context,
        decision,
        timestamp: Date.now()
      });

      span.setStatus({ code: SpanStatusCode.OK });
      return decision.granted;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Generate zero-knowledge proof
   * @param {Object} statement - Statement to prove
   * @param {Object} witness - Private witness data
   * @returns {Promise<Object>} ZK proof
   */
  async generateProof(statement, witness) {
    if (!this.zkEngine) {
      throw new Error('ZK proofs not enabled');
    }

    const span = tracer.startSpan('security.zk.generate_proof');

    try {
      const proof = await this.zkEngine.generate(statement, witness);

      span.setAttribute('proof.type', statement.type);
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
   * Verify zero-knowledge proof
   * @param {Object} statement - Statement to verify
   * @param {Object} proof - ZK proof
   * @returns {Promise<boolean>} Proof valid/invalid
   */
  async verifyProof(statement, proof) {
    if (!this.zkEngine) {
      throw new Error('ZK proofs not enabled');
    }

    const span = tracer.startSpan('security.zk.verify_proof');

    try {
      const valid = await this.zkEngine.verify(statement, proof);

      span.setAttribute('proof.valid', valid);
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

#### Encrypted Store Implementation

```javascript
/**
 * Encrypted RDF store with field-level encryption
 */
export class EncryptedStore {
  constructor(config) {
    this.config = config;
    this.fieldEncryptor = new FieldEncryptor(config);
    this.keyManager = new KeyManager(config);
    this.store = new Store();  // Underlying N3 store
  }

  /**
   * Initialize encrypted store
   * @param {Object} options - Initialization options
   * @returns {Promise<EncryptedStore>}
   */
  async initialize(options = {}) {
    // Load or generate master key
    await this.keyManager.initialize();

    // Set up encryption rules
    this.encryptionRules = options.encryptionRules || [];

    return this;
  }

  /**
   * Add quad with automatic encryption
   * @param {Quad} quad - RDF quad to add
   * @returns {Promise<Quad>} Added quad (with encrypted values)
   */
  async add(quad) {
    const span = tracer.startSpan('security.encrypted_store.add');

    try {
      // Determine if quad should be encrypted
      const shouldEncrypt = this._shouldEncrypt(quad);

      let encryptedQuad = quad;

      if (shouldEncrypt) {
        span.setAttribute('encrypted', true);

        // Encrypt object value
        const encryptedValue = await this.fieldEncryptor.encrypt(
          quad.object.value
        );

        // Create new quad with encrypted value
        encryptedQuad = DataFactory.quad(
          quad.subject,
          quad.predicate,
          DataFactory.literal(encryptedValue),
          quad.graph
        );
      } else {
        span.setAttribute('encrypted', false);
      }

      // Add to underlying store
      this.store.add(encryptedQuad);

      span.setStatus({ code: SpanStatusCode.OK });
      return encryptedQuad;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Query with automatic decryption
   * @param {string} sparqlQuery - SPARQL query
   * @returns {Promise<Array>} Decrypted results
   */
  async query(sparqlQuery) {
    const span = tracer.startSpan('security.encrypted_store.query');

    try {
      // Execute query on encrypted store
      const results = await this._executeSparql(this.store, sparqlQuery);

      // Decrypt encrypted fields
      const decryptedResults = await Promise.all(
        results.map(async (result) => {
          const decrypted = {};

          for (const [key, value] of Object.entries(result)) {
            if (this._isEncrypted(value)) {
              decrypted[key] = await this.fieldEncryptor.decrypt(value);
            } else {
              decrypted[key] = value;
            }
          }

          return decrypted;
        })
      );

      span.setAttribute('results.count', decryptedResults.length);
      span.setStatus({ code: SpanStatusCode.OK });

      return decryptedResults;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  _shouldEncrypt(quad) {
    // Check encryption rules
    for (const rule of this.encryptionRules) {
      if (rule.predicate === quad.predicate.value) {
        return true;
      }
    }

    // Default: encrypt literals with sensitive predicates
    const sensitivPredicates = [
      'http://example.org/ssn',
      'http://example.org/creditCard',
      'http://example.org/password',
      'http://xmlns.com/foaf/0.1/mbox'
    ];

    return sensitivPredicates.includes(quad.predicate.value);
  }
}
```

#### ABAC Engine Implementation

```javascript
/**
 * Attribute-Based Access Control engine
 */
export class ABACEngine {
  constructor(config) {
    this.config = config;
    this.policyParser = new PolicyParser();
    this.policies = this._loadPolicies(config.policies);
  }

  /**
   * Evaluate access control decision
   * @param {Object} context - Security context
   * @returns {Promise<Object>} Access decision
   */
  async evaluate(context) {
    const span = tracer.startSpan('security.abac.evaluate');

    try {
      // Collect all applicable policies
      const applicablePolicies = this.policies.filter((policy) =>
        this._isPolicyApplicable(policy, context)
      );

      span.setAttribute('policies.applicable', applicablePolicies.length);

      if (applicablePolicies.length === 0) {
        // No policy found - default deny
        return { granted: false, reason: 'No applicable policy' };
      }

      // Evaluate each policy
      const results = await Promise.all(
        applicablePolicies.map((policy) => this._evaluatePolicy(policy, context))
      );

      // Combine results (any deny = deny, all permit = permit)
      const denied = results.find((r) => r.effect === 'deny');

      if (denied) {
        span.setAttribute('decision', 'deny');
        return {
          granted: false,
          reason: denied.reason,
          policy: denied.policyId
        };
      }

      const permitted = results.find((r) => r.effect === 'permit');

      if (permitted) {
        span.setAttribute('decision', 'permit');
        return {
          granted: true,
          reason: permitted.reason,
          policy: permitted.policyId
        };
      }

      // No explicit permit - default deny
      span.setAttribute('decision', 'deny');
      return { granted: false, reason: 'No permit policy matched' };

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  async _evaluatePolicy(policy, context) {
    // Evaluate all conditions
    for (const condition of policy.conditions) {
      const result = await this._evaluateCondition(condition, context);
      if (!result) {
        return {
          effect: 'not_applicable',
          policyId: policy.id
        };
      }
    }

    // All conditions matched
    return {
      effect: policy.effect,  // 'permit' | 'deny'
      reason: policy.description,
      policyId: policy.id
    };
  }

  async _evaluateCondition(condition, context) {
    const { attribute, operator, value } = condition;

    // Resolve attribute value from context
    const attrValue = this._resolveAttribute(attribute, context);

    // Evaluate operator
    switch (operator) {
      case 'equals':
        return attrValue === value;
      case 'notEquals':
        return attrValue !== value;
      case 'in':
        return value.includes(attrValue);
      case 'notIn':
        return !value.includes(attrValue);
      case 'greaterThan':
        return attrValue > value;
      case 'lessThan':
        return attrValue < value;
      case 'matches':
        return new RegExp(value).test(attrValue);
      default:
        throw new Error(`Unknown operator: ${operator}`);
    }
  }
}
```

#### Zero-Knowledge Proof Engine

```javascript
/**
 * Zero-knowledge proof engine for privacy-preserving verification
 */
export class ZKEngine {
  constructor(config) {
    this.config = config;
    this.circuits = new Map();
  }

  /**
   * Generate zero-knowledge proof
   * @param {Object} statement - Public statement
   * @param {Object} witness - Private witness
   * @returns {Promise<Object>} ZK proof
   */
  async generate(statement, witness) {
    const span = tracer.startSpan('security.zk.generate');

    try {
      // Select appropriate circuit
      const circuit = this._selectCircuit(statement.type);

      // Generate proof
      const proof = await circuit.prove(statement.public, witness);

      span.setAttribute('proof.size', JSON.stringify(proof).length);
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
   * Verify zero-knowledge proof
   * @param {Object} statement - Public statement
   * @param {Object} proof - ZK proof
   * @returns {Promise<boolean>} Proof valid
   */
  async verify(statement, proof) {
    const span = tracer.startSpan('security.zk.verify');

    try {
      // Select appropriate circuit
      const circuit = this._selectCircuit(statement.type);

      // Verify proof
      const valid = await circuit.verify(statement.public, proof);

      span.setAttribute('proof.valid', valid);
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

  _selectCircuit(type) {
    switch (type) {
      case 'membership':
        return new MembershipProofCircuit();
      case 'range':
        return new RangeProofCircuit();
      case 'equality':
        return new EqualityProofCircuit();
      default:
        throw new Error(`Unknown proof type: ${type}`);
    }
  }
}
```

### Integration with Knowledge Hooks

```javascript
// Security hooks
defineHook('security.access.check', {
  phase: 'pre',
  condition: async () => true,  // Always check
  effect: async ({ context }) => {
    const granted = await securityManager.enforce(context);
    if (!granted) {
      throw new Error('Access denied');
    }
  }
});

defineHook('security.data.encrypted', {
  phase: 'post',
  condition: async ({ quad }) => isSensitive(quad),
  effect: async ({ quad }) => {
    await auditLogger.log({
      type: 'encryption',
      quad,
      timestamp: Date.now()
    });
  }
});

defineHook('security.audit.logged', {
  phase: 'post',
  effect: async ({ event }) => {
    // Write to immutable audit log
    await lockchainWriter.append(event);
  }
});
```

### Usage Examples

```javascript
// Example 1: Encrypted RDF store
import { SecurityManager } from 'unrdf/security-enhanced';

const security = new SecurityManager({
  encryption: {
    algorithm: 'aes-256-gcm',
    keyDerivation: 'pbkdf2'
  }
});

const encryptedStore = await security.createEncryptedStore({
  encryptionRules: [
    { predicate: 'http://example.org/ssn' },
    { predicate: 'http://example.org/creditCard' }
  ]
});

// Add sensitive data (automatically encrypted)
await encryptedStore.add(DataFactory.quad(
  DataFactory.namedNode('http://example.org/person/123'),
  DataFactory.namedNode('http://example.org/ssn'),
  DataFactory.literal('123-45-6789')
));

// Example 2: Access control
const context = {
  user: { id: 'user123', roles: ['researcher'], department: 'science' },
  resource: { type: 'dataset', classification: 'confidential' },
  action: 'read'
};

const granted = await security.enforce(context);
if (granted) {
  // Perform operation
}

// Example 3: Zero-knowledge proof
const zkSecurity = new SecurityManager({
  zkProofs: { enabled: true }
});

// Prove age > 18 without revealing actual age
const proof = await zkSecurity.generateProof(
  { type: 'range', public: { min: 18 } },
  { age: 25 }  // Private witness
);

// Verify proof
const valid = await zkSecurity.verifyProof(
  { type: 'range', public: { min: 18 } },
  proof
);
```

## Consequences

### Positive

- **Compliance**: GDPR, HIPAA, CCPA compliance support
- **Privacy**: Strong data protection through encryption
- **Auditability**: Comprehensive, immutable audit trails
- **Flexibility**: Fine-grained access control
- **Transparency**: ZK proofs enable privacy-preserving verification

### Negative

- **Performance**: 15-20% overhead for encryption/decryption
- **Complexity**: Significantly more complex than unencrypted
- **Key Management**: Requires secure key storage and rotation
- **ZK Proofs**: High computational cost for proof generation

## Performance Targets

| Operation | Target Overhead |
|-----------|----------------|
| Encrypted Add | <20% |
| Encrypted Query | <25% |
| Access Control Check | <5ms |
| ZK Proof Generation | <2s |
| ZK Proof Verification | <100ms |

## References

- [GDPR Compliance Guide](https://gdpr.eu/)
- [NIST Encryption Standards](https://csrc.nist.gov/projects/cryptographic-standards-and-guidelines)
- [ZK-SNARKs](https://z.cash/technology/zksnarks/)
