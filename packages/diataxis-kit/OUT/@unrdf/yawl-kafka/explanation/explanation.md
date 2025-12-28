---
title: "Understanding @unrdf/yawl-kafka"
type: "explanation"
packageName: "@unrdf/yawl-kafka"
version: "1.0.0"
generatedAt: "2000-01-01T00:00:00.000Z"
confidenceScore: 0.7
proof: "e5d4f157d6a9c5a9557c25e3ad9c4cd68ae5e29a456cf9f5810248c8c37bc62b"
---

## Concepts

- yawl
- kafka
- event-streaming
- avro
- workflow

## Architecture

Events are serialized using Avro for compact, schema-validated payloads. All schemas include cryptographic receipts with BLAKE3 hashes.

## Tradeoffs

- Ease of use vs. advanced configuration options
- Memory usage vs. processing speed
- Bundle size vs. feature completeness

## Proof

This file was generated from the following evidence sources:

- inferred
- keywords
- readme

```json
{
  "fingerprintInput": "inferred|keywords|readme|Understanding @unrdf/yawl-kafka|0.7",
  "hash": "e5d4f157d6a9c5a9557c25e3ad9c4cd68ae5e29a456cf9f5810248c8c37bc62b",
  "sources": [
    "inferred",
    "keywords",
    "readme"
  ]
}
```
