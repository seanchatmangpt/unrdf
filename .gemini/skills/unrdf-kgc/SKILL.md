---
name: unrdf-kgc
description: Manages Knowledge Graph Construction, ingestion, and substrate operations. Use for migrating databases to RDF or managing KGC-4D probe networks.
---
# KGC (Knowledge Graph Construction)

## Overview
KGC transforms raw data/schemas into semantic graphs.

## Critical Capabilities
- **SQL-to-RDF**: Use `kgc-runtime` to ingest SQL backends directly into RDF.
- **KGC-4D**: Manages 4-dimensional temporal state tracking.
- **Substrate**: The `kgc-substrate` package provides the foundational graph representation.

## Best Practices
- Always maintain `capability-map.md` for new KGC extensions.
- Validate KGC outputs using the `shacl` engine integrated into `packages/validation/`.
