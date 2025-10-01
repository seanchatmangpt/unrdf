# Hyperdimensional Computing: Quick Reference Card

## Core Definitions

### Hyperdimensional Space
```
ℍᵈ = {v ∈ ℝᵈ : ||v||₂ = 1}
```
**Recommended**: d ≥ 10,000

### Binding Operators

**Circular Convolution** (primary):
```
v ⊛ w = ℱ⁻¹(ℱ(v) ⊙ ℱ(w))
Complexity: O(d log d)
```

**Element-wise Product**:
```
(v ⊙ w)ᵢ = vᵢ · wᵢ
Complexity: O(d)
```

**Permutation**:
```
Π(v) = (vₚ₍₀₎, vₚ₍₁₎, ..., vₚ₍ᵈ₋₁₎)
Complexity: O(d)
```

### Similarity Metrics

**Cosine Similarity**:
```
sim(v, w) = ⟨v, w⟩  (for unit vectors)
Range: [-1, 1]
Threshold: τ ∈ [0.7, 0.9]
```

**Hamming Distance** (binary):
```
dist_H(v, w) = (1/d) · Σᵢ 𝟙[vᵢ ≠ wᵢ]
Relation: sim(v, w) = 1 - 2·dist_H(v, w)
```

## Key Theorems

### Theorem 1.1: Concentration of Measure
```
P(|⟨u, v⟩| > ε) ≤ 2 exp(-dε²/2)
```
**For d=10k, ε=0.1**: P < 3.8 × 10⁻²²

### Theorem 2.1: Binding Preserves Orthogonality
```
E[⟨u ⊛ v, w⟩] = 0
Var[⟨u ⊛ v, w⟩] = 1/d
```
**Standard deviation**: σ = 1/√d ≈ 0.01

### Theorem 3.1: Retrieval Accuracy
```
P(sim(v, v+ε) ≥ τ) = Φ((1-τ)/σ)
```
**For σ=0.1, τ=0.7**: P = 99.87%

### Theorem 4.1: Cleanup Error Bound
```
Correctness requires: σ < δ/(2√2)
where δ = min separation
```

### Theorem 5.1: Superposition Capacity
```
E[sim(aggregate, vⱼ)] = 1/√n
Capacity: n ≤ (1/τ)²
```
**For τ=0.7**: n ≤ 2

### Theorem 6.1: Field Complexity
```
T_field(k, n, d) = O(kd + nd)
T_direct(k, n) = O(kn·C_hook)
Speedup: 500-5000×
```

## Complexity Reference

| Operation | Time | Space |
|-----------|------|-------|
| Random projection | O(nd) | O(nd) |
| Circular conv (FFT) | O(d log d) | O(d) |
| Element-wise product | O(d) | O(d) |
| Cosine similarity | O(d) | O(1) |
| Cleanup (naive) | O(nd) | O(nd) |
| Cleanup (LSH) | O(d log n) | O(nd) |
| Superposition | O(kd) | O(d) |
| Hook evaluation | O(kd) | O(kd) |

## Error Bounds

| Source | Bound | Typical |
|--------|-------|---------|
| Orthogonality | 1/√d | 0.01 |
| Binding noise | 1/√d | 0.01 |
| Cleanup error | δ/(2√2) | 0.35δ |
| Composition depth k | k/√d | 0.05 (k=5) |
| Retrieval | Φ⁻¹(P) | 99.87% |

## Design Guidelines

### Dimensionality Selection
```
d = 10,000:   Standard (99.87% accuracy)
d = 50,000:   High precision (99.999%)
d = 100,000:  Ultra-precise (6+ nines)
```

### Threshold Selection
```
τ = 0.7:  Robust (70% similarity)
τ = 0.8:  Precise (80% similarity)
τ = 0.9:  Exact (90% similarity)
```

### Capacity Planning
```
Simple superposition: n ≤ 2
Weighted superposition: n ≤ 10
Hierarchical: unlimited (with error k/√d)
```

### Noise Tolerance
```
Requirement: σ < δ/(2√2)
Example (δ=0.4): σ < 0.14 (14% noise OK)
```

## Python Implementation

### Basic Operations
```python
import numpy as np
from numpy.fft import fft, ifft

# Random hypervector
def random_hv(d=10000):
    v = np.random.randn(d)
    return v / np.linalg.norm(v)

# Circular convolution
def bind(v, w):
    return np.real(ifft(fft(v) * fft(w)))

# Similarity
def sim(v, w):
    return np.dot(v, w)

# Cleanup
def cleanup(query, prototypes, tau=0.7):
    sims = [sim(query, p) for p in prototypes]
    idx = np.argmax(sims)
    return prototypes[idx] if sims[idx] >= tau else None
```

### Hook Encoding
```python
# Encode Knowledge Hook
def encode_hook(query_vec, predicate_vecs):
    hook = bind(query_vec, sum(predicate_vecs))
    return hook / np.linalg.norm(hook)

# Evaluate hook
def evaluate_hook(hook_vec, state_vec, tau=0.7):
    activation = sim(hook_vec, state_vec)
    return activation >= tau, activation
```

### Field Computation
```python
# Superpose hooks
def create_field(hook_vecs, weights):
    field = sum(w * h for w, h in zip(weights, hook_vecs))
    return field / np.linalg.norm(field)

# Evaluate field
def field_value(field_vec, state_vec):
    return sim(field_vec, state_vec)
```

## Common Patterns

### 1. Entity Encoding
```python
alice = random_hv()
owns = random_hv()
house = random_hv()

# "Alice owns house"
fact = bind(bind(alice, owns), house)
```

### 2. Role-Filler Binding
```python
role = random_hv()
filler = random_hv()

binding = bind(role, filler)
extracted = bind(binding, role)  # ≈ filler
```

### 3. Hierarchical Structure
```python
root = random_hv()
left = random_hv()
right = random_hv()

tree = bind(root, left + right)
```

### 4. Sequence Encoding
```python
a, b, c = random_hv(), random_hv(), random_hv()
perm = lambda v: np.roll(v, 1)  # Simple permutation

seq = a + perm(b) + perm(perm(c))
```

## Performance Metrics

### KGC Reference Implementation
```
p50 hook evaluation: 185 µs
p99 transaction: 1.8 ms
Hook throughput: 12,450 ops/min
Memory (k=100): 128 MB
```

### Theoretical Predictions
```
Hook complexity: O(kd) = O(100 × 10k) = 1M ops
Field evaluation: O(kd + nd)
Speedup vs tree search: 500-5000×
```

## Cross-References

- **Chapter 1**: Field theory foundations
- **Chapter 3**: Knowledge Hooks calculus
- **Chapter 6**: Performance validation
- **Appendix A**: Complete proofs
- **Appendix B**: Complexity analysis

## Literature

1. Kanerva (2009): Hyperdimensional Computing
2. Plate (2003): Holographic Reduced Representations
3. Gayler (2003): Vector Symbolic Architectures
4. Enßlin+ (2009): Information Field Theory
5. Mikolov+ (2013): Word2Vec and Geometric Analogies

## Quick Start Checklist

- [ ] Choose d ≥ 10,000 for robustness
- [ ] Set threshold τ ∈ [0.7, 0.9]
- [ ] Verify σ < δ/(2√2) for cleanup
- [ ] Use FFT for O(d log d) binding
- [ ] Monitor capacity n ≤ (1/τ)²
- [ ] Profile for O(kd) scaling
- [ ] Cross-reference Chapter 3 for semantics

---

**See full chapter**: [README.md](README.md)
**Proofs**: [proofs.md](proofs.md)
**Examples**: [examples.md](examples.md)
