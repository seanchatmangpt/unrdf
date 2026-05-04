# @unrdf/zkp - Zero-Knowledge SPARQL

> **First production ZK-RDF system** - Privacy-preserving query proofs using zk-SNARKs

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Version](https://img.shields.io/badge/version-6.2.0--alpha.1-green.svg)](package.json)

Prove SPARQL query results are correct **without revealing the underlying RDF data** using zero-knowledge proofs (zk-SNARKs).

## Features

- **Privacy-Preserving Queries**: Prove query results without revealing data
- **Groth16 zk-SNARKs**: 128-bit security, constant-size proofs (192 bytes)
- **Fast Verification**: 1-2ms constant-time verification
- **Production-Ready**: OTEL instrumentation, comprehensive testing
- **SPARQL Support**: SELECT queries with WHERE + FILTER constraints

## Quick Start

```bash
pnpm add @unrdf/zkp
```

```javascript
import { createZKProver } from '@unrdf/zkp';

const prover = createZKProver();

const triples = [
  { subject: ':Alice', predicate: ':age', object: '25' },
  { subject: ':Bob', predicate: ':age', object: '17' },
];

const query = `
  SELECT ?person ?age
  WHERE {
    ?person :age ?age
    FILTER(?age > 18)
  }
`;

const results = [{ person: ':Alice', age: '25' }];

const { proof, publicSignals } = await prover.prove(triples, query, results);

const valid = await prover.verify(proof, publicSignals);
console.log('Query result verified:', valid);
```

## Use Cases

### Healthcare Privacy

Prove "Patient eligible for clinical trial" without revealing:
- Patient identity
- Medical history
- Trial criteria

```javascript
const query = `
  SELECT ?patient
  WHERE {
    ?patient :diagnosis :diabetes .
    ?patient :age ?age .
    FILTER(?age >= 18 && ?age <= 65)
  }
`;

const { proof } = await zkProver.prove(medicalRecords, query, eligiblePatients);
```

### Financial Compliance

Prove "Account meets regulatory requirements" without revealing:
- Account balance
- Transaction history
- Account holder

```javascript
const query = `
  SELECT ?account
  WHERE {
    ?account :balance ?balance .
    FILTER(?balance > 10000)
  }
`;
```

### Supply Chain Verification

Prove "Product authentic and meets standards" without revealing:
- Supplier details
- Manufacturing process
- Pricing

## Architecture

### Components

1. **SPARQL ZK Prover** (`sparql-zkp-prover.mjs`)
   - High-level API for query proving
   - Receipt integration
   - Batch proving

2. **Circuit Compiler** (`circuit-compiler.mjs`)
   - SPARQL → R1CS arithmetic circuits
   - Triple pattern constraints
   - FILTER arithmetic constraints

3. **Groth16 Prover** (`groth16-prover.mjs`)
   - zk-SNARK proof generation
   - Uses snarkjs for cryptography

4. **Groth16 Verifier** (`groth16-verifier.mjs`)
   - Constant-time proof verification
   - Batch verification support

### How It Works

```
SPARQL Query → Circuit Compiler → R1CS Circuit
                                      ↓
                              Groth16 Prover
                                      ↓
                              zk-SNARK Proof (192 bytes)
                                      ↓
                              Groth16 Verifier → Valid/Invalid
```

## Performance

| Operation | Target | Typical |
|-----------|--------|---------|
| Circuit Compilation | <100ms | 10-50ms |
| Proof Generation | 1-10s | 2-5s |
| Proof Verification | <2ms | 1ms |
| Proof Size | 192 bytes | 192 bytes (constant) |

## Security

- **Protocol**: Groth16 zk-SNARKs
- **Security Level**: 128-bit
- **Curve**: BN128 (alt_bn128)
- **Soundness**: Computational (trusted setup)
- **Zero-Knowledge**: Perfect

## Supported SPARQL Features

✅ **Supported**:
- SELECT queries
- Triple patterns (subject, predicate, object)
- FILTER constraints (>, <, =, !=, >=, <=)
- AND logic (multiple patterns)
- Arithmetic comparisons

❌ **Not Yet Supported**:
- ASK, CONSTRUCT, DESCRIBE
- OPTIONAL, UNION
- GROUP BY, ORDER BY
- Aggregations (COUNT, SUM, etc.)
- String operations
- REGEX

## API

### createZKProver(config)

Create ZK prover instance.

```javascript
const prover = createZKProver({
  maxTriples: 10000,
  maxResults: 1000,
});
```

### prover.prove(triples, query, results)

Generate zk-SNARK proof for query result.

```javascript
const { proof, publicSignals } = await prover.prove(
  triples,  // RDF triples
  query,    // SPARQL query string
  results   // Query results
);
```

### prover.verify(proof, publicSignals)

Verify zk-SNARK proof.

```javascript
const valid = await prover.verify(proof, publicSignals);
```

### compileSPARQL(query)

Compile SPARQL query to arithmetic circuit.

```javascript
import { compileSPARQL } from '@unrdf/zkp';

const circuit = await compileSPARQL('SELECT * WHERE { ?s ?p ?o }');
console.log(`Circuit has ${circuit.nConstraints} constraints`);
```

## Testing

```bash
pnpm test              # Run all tests
pnpm test:coverage     # With coverage
pnpm test:watch        # Watch mode
```

## Examples

See `test/sparql-zkp.test.mjs` for comprehensive examples:
- Simple triple matching
- FILTER constraints
- Complex queries
- Performance benchmarks

## Limitations

- **Trusted Setup**: Groth16 requires trusted setup ceremony
- **Circuit Capacity**: Fixed maximum triples (10,000 default)
- **Query Complexity**: Limited SPARQL feature support
- **Proving Time**: 1-10s for complex queries

## Roadmap

- [ ] Support ASK, CONSTRUCT queries
- [ ] OPTIONAL, UNION patterns
- [ ] Aggregation functions
- [ ] String operations
- [ ] Recursive proof composition
- [ ] PLONK/Halo2 (no trusted setup)

## License

MIT

## Contributing

See [CONTRIBUTING.md](../../CONTRIBUTING.md)

## References

- [Groth16 Paper](https://eprint.iacr.org/2016/260.pdf)
- [snarkjs](https://github.com/iden3/snarkjs)
- [SPARQL 1.1](https://www.w3.org/TR/sparql11-query/)
