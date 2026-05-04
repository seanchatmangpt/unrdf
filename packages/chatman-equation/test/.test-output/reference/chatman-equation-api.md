


# Chatman Equation API Reference

> **Module**: `@unrdf/chatman-equation`
> **Version**: 1.0.0
> **Updated**: 2026-01-18

---

## Overview

The Chatman Equation is a mathematical framework for modeling information flow
in conversational AI systems, providing deterministic measurement of semantic
coherence and context propagation.


## Equations

### Semantic Coherence

**Formula**: `C(t) = ∑(w_i × s_i) / √(∑w_i²)`

Calculates the semantic coherence of a token sequence based on weighted
similarity scores between consecutive tokens.


**Parameters**:
- **`tokens`** (`Array&lt;Token&gt;`): Input token sequence
- **`weights`** (`Array&lt;number&gt;`): Token importance weights
- Default: `[1.0, ...]`- **`contextWindow`** (`number`): Maximum context window size
- Default: `100000`- Constraints: Must be positive integer
**Returns**: `CoherenceScore` - Semantic coherence score between 0 and 1



**See Also**: [Information Entropy](#information-entropy)
---

### Context Propagation

**Formula**: `P(c_t+1 | c_t) = exp(-λ × d(c_t, c_t+1))`

Models the probability of context propagation from one state to the next
based on semantic distance.


**Parameters**:
- **`currentContext`** (`Context`): Current conversation context
- **`nextContext`** (`Context`): Next conversation context
- **`lambda`** (`number`): Decay parameter
- Default: `0.1`
**Returns**: `number` - Propagation probability [0, 1]




---


## Constants

| Name | Type | Value | Description |
|------|------|-------|-------------|
| `MAX_CONTEXT_WINDOW` | `number` | `200000` | Maximum supported context window |
| `MIN_COHERENCE_THRESHOLD` | `number` | `0.5` | Minimum coherence threshold for valid results |
| `DEFAULT_LAMBDA` | `number` | `0.1` | Default decay parameter for context propagation |

## Type Definitions

### Token

```typescript
interface Token {
  text: string;
  embedding: number[];
  weight: number;
}

```

Represents a single token with semantic embedding

**Properties**:
- **`text`** (`string`): Token text content
- **`embedding`** (`number[]`): Vector embedding representation
- **`weight`** (`number`): Token importance weight

---

### CoherenceScore

```typescript
interface CoherenceScore {
  coherence: number;
  entropy: number;
  tokens: number;
}

```

Result of coherence calculation


---


## Usage

```javascript
import { ChatmanEquation } from &#39;@unrdf/chatman-equation&#39;;

const equation = new ChatmanEquation({
  contextWindow: 100000,
  coherenceThreshold: 0.85
});

const result = equation.calculate({
  tokens: tokenStream,
  context: conversationHistory
});

console.log(&#39;Coherence Score:&#39;, result.coherence);
console.log(&#39;Information Entropy:&#39;, result.entropy);

```


---

*Generated from TOML configuration by Chatman Equation Template Engine*
