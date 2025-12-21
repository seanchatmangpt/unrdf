---
description: Validate RDF/Turtle files, SHACL shapes, and ontology consistency for UNRDF packages
---

# RDF Validator

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Purpose

Validate Turtle syntax, SHACL shape conformance, and semantic consistency for all RDF files in the UNRDF monorepo.

## Quick Commands

```bash
# Validate specific file
node -e "import('@unrdf/oxigraph').then(m => { const s = m.createStore(); s.load(require('fs').readFileSync('$FILE', 'utf8'), {format: 'text/turtle'}); console.log('✅ Valid'); })"

# Validate all TTL files
find . -name "*.ttl" -type f | while read f; do
  echo -n "$f: "
  node -e "import('@unrdf/oxigraph').then(m => { const s = m.createStore(); s.load(require('fs').readFileSync('$f', 'utf8'), {format: 'text/turtle'}); console.log('✅'); }).catch(e => console.log('❌', e.message))"
done
```

## Execution Steps

### 1. Find All TTL Files

```bash
# List all Turtle files
find packages -name "*.ttl" -type f 2>/dev/null

# List ontology files specifically
find packages/core/src/ontologies -name "*.ttl" -type f 2>/dev/null
```

### 2. Syntax Validation

```javascript
// validation-script.mjs
import { createStore } from '@unrdf/oxigraph';
import { readFileSync, readdirSync } from 'fs';
import { join } from 'path';

async function validateTurtle(filePath) {
  try {
    const store = createStore();
    const content = readFileSync(filePath, 'utf8');
    store.load(content, { format: 'text/turtle' });
    const count = store.size;
    console.log(`✅ ${filePath}: ${count} triples`);
    return { valid: true, triples: count };
  } catch (error) {
    console.log(`❌ ${filePath}: ${error.message}`);
    return { valid: false, error: error.message };
  }
}
```

### 3. SHACL Validation

```bash
# Run SHACL validation against shapes
node -e "
import { createStore } from '@unrdf/oxigraph';
import { readFileSync } from 'fs';

const dataStore = createStore();
const shapesStore = createStore();

// Load data
dataStore.load(readFileSync('data.ttl', 'utf8'), {format: 'text/turtle'});

// Load shapes
shapesStore.load(readFileSync('shapes.ttl', 'utf8'), {format: 'text/turtle'});

// Validate (pseudo-code - actual SHACL validation via @unrdf/validation)
console.log('SHACL validation would run here');
"
```

### 4. Prefix Consistency Check

```bash
# Extract all prefixes used
grep -h "^@prefix" packages/**/*.ttl 2>/dev/null | sort -u

# Check for required UNRDF prefixes
REQUIRED_PREFIXES="mat: spec: schema: sh: xsd:"
for prefix in $REQUIRED_PREFIXES; do
  grep -l "@prefix $prefix" packages/**/*.ttl 2>/dev/null || echo "⚠️ Missing $prefix"
done
```

### 5. Namespace Validation

Expected UNRDF namespaces:

| Prefix  | Namespace                            | Usage             |
| ------- | ------------------------------------ | ----------------- |
| mat:    | https://unrdf.org/ontology/maturity# | Maturity ontology |
| spec:   | https://unrdf.org/ontology/spec#     | Specification     |
| unrdf:  | https://unrdf.org/resource/          | Instance data     |
| schema: | https://schema.org/                  | Schema.org types  |
| sh:     | http://www.w3.org/ns/shacl#          | SHACL shapes      |

```bash
# Verify namespace URIs
grep -E "^@prefix (mat|spec|unrdf):" packages/**/*.ttl 2>/dev/null
```

## Maturity Ontology Validation

### Required Classes

```turtle
# packages/core/src/ontologies/maturity.ttl must define:
mat:Package a owl:Class .
mat:MaturityLevel a owl:Class .
mat:SynergyCategory a owl:Class .
mat:Assessment a owl:Class .
```

### Required Properties

```turtle
mat:hasMaturityLevel a owl:ObjectProperty .
mat:maturityScore a owl:DatatypeProperty .
mat:coveragePercent a owl:DatatypeProperty .
mat:participatesInSynergy a owl:ObjectProperty .
```

### Required Instances

```turtle
mat:L1_Alpha a mat:MaturityLevel .
mat:L2_Beta a mat:MaturityLevel .
mat:L3_RC a mat:MaturityLevel .
mat:L4_Stable a mat:MaturityLevel .
mat:L5_LTS a mat:MaturityLevel .
```

## Output Format

```markdown
## RDF Validation Report

**Date**: [timestamp]
**Files Scanned**: [count]

### Syntax Validation

| File         | Status | Triples | Errors |
| ------------ | ------ | ------- | ------ |
| maturity.ttl | ✅     | 45      | -      |
| shapes.ttl   | ✅     | 32      | -      |

### SHACL Conformance

| Shape           | Target         | Violations | Status |
| --------------- | -------------- | ---------- | ------ |
| PackageShape    | mat:Package    | 0          | ✅     |
| AssessmentShape | mat:Assessment | 2          | ❌     |

### Prefix Consistency

- mat: ✅ Consistent across 5 files
- spec: ⚠️ Missing in 2 files

### Issues Found

1. [CRITICAL/HIGH/MEDIUM] Description
   - File: path/to/file.ttl
   - Line: ~X
   - Fix: Suggested resolution

### Recommendations

- [actionable fixes]
```

## Common Errors

### 1. Missing Prefix Declaration

```turtle
# ❌ Wrong
mat:Package a owl:Class .

# ✅ Correct
@prefix mat: <https://unrdf.org/ontology/maturity#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
mat:Package a owl:Class .
```

### 2. Invalid IRI

```turtle
# ❌ Wrong (space in IRI)
<https://unrdf.org/my resource> a mat:Package .

# ✅ Correct
<https://unrdf.org/my-resource> a mat:Package .
```

### 3. Missing Datatype

```turtle
# ❌ Ambiguous
mat:coveragePercent "85.5" .

# ✅ Explicit
mat:coveragePercent "85.5"^^xsd:decimal .
```

## Integration with Validation Package

```bash
# Use @unrdf/validation for comprehensive checks
pnpm --filter "@unrdf/validation" run validate:rdf
```

End Command ---
