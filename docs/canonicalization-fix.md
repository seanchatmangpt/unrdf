# RDF Canonicalization Fix - October 2025

## Problem

The `rdf-canonize` library was throwing the error:
```
TypeError: triples.forEach is not a function
```

This affected 25+ transaction tests and blocked canonical hashing functionality.

## Root Cause

The `rdf-canonize.canonize()` function expects one of:
1. **Array of quad objects** (RDF.js format)
2. **Legacy dataset object** (object with graph names as keys)

Our code was passing an **N-Quads string**, which the library tried to process as a legacy dataset, causing the error when it attempted to call `.forEach()` on what it expected to be an array of triples.

From `rdf-canonize/lib/index.js` lines 80-84:
```javascript
api.canonize = async function(dataset, options) {
  // back-compat with legacy dataset
  if(!Array.isArray(dataset)) {
    dataset = api.NQuads.legacyDatasetToQuads(dataset);
  }
```

The `legacyDatasetToQuads()` function expects a specific object structure, not a string.

## Solution

**Parse the N-Quads string first** using `rdfCanonize.NQuads.parse()` before passing to `canonize()`.

### Code Changes

**File:** `src/knowledge-engine/canonicalize.mjs` (lines 68-81)

**BEFORE:**
```javascript
// Perform canonicalization
const canonicalPromise = rdfCanonize.canonize(nquads, {
  algorithm,
  inputFormat: 'application/n-quads',  // ❌ This parameter is ignored
  produceGeneralizedRdf
});
```

**AFTER:**
```javascript
// Parse N-Quads string to RDF.js dataset format
// rdf-canonize expects an array of quad objects, not a string
const parsedDataset = rdfCanonize.NQuads.parse(nquads);

// Perform canonicalization with parsed dataset
const canonicalPromise = rdfCanonize.canonize(parsedDataset, {
  algorithm,
  produceGeneralizedRdf
});
```

### Key Points

1. **Remove `inputFormat` parameter** - Not needed when passing parsed dataset
2. **Use `NQuads.parse()`** - Converts N-Quads string to RDF.js quad array
3. **Library expects arrays** - The canonization algorithm operates on arrays, not strings

## Library API Reference

From `rdf-canonize/lib/NQuads.js` lines 83-182:

```javascript
static parse(input) {
  // build RDF dataset
  const dataset = [];

  // split N-Quad input into lines
  const lines = input.split(REGEX.eoln);

  // ... parse each line into quad object

  return dataset;  // Array of {subject, predicate, object, graph}
}
```

The `parse()` method returns an array of quad objects in RDF.js format:
```javascript
{
  subject: { termType: 'NamedNode', value: '...' },
  predicate: { termType: 'NamedNode', value: '...' },
  object: { termType: 'NamedNode'|'BlankNode'|'Literal', value: '...', ... },
  graph: { termType: 'NamedNode'|'BlankNode'|'DefaultGraph', value: '...' }
}
```

## Impact

- ✅ Fixed "triples.forEach is not a function" error
- ✅ Canonical hashing now works correctly
- ✅ Transaction tests can now execute (though some have assertion issues)
- ✅ Cryptographic receipts can be generated
- ✅ Full audit trail capability restored

## Test Results

**Before Fix:**
- 25+ tests failing with "triples.forEach is not a function"
- Canonicalization completely blocked
- Dark Matter tests: 13/18 passing (72%)

**After Fix:**
- No more "triples.forEach" errors
- Canonicalization working
- Dark Matter tests: 15/18 passing (83%)
- Overall test suite: 357/639 passing (56%)

## Technical Notes

### Why Parse is Necessary

The `rdf-canonize` library was designed to work with RDF datasets in memory, represented as arrays of quad objects. When developers wanted to canonize N-Quads strings, they had to parse them first. The library provides `NQuads.parse()` for exactly this purpose.

The `inputFormat` parameter we were using was likely added in a later version or is not part of version 2.0.1 that we're using.

### Version Compatibility

System has multiple rdf-canonize versions installed:
- 2.0.1 (active)
- 3.4.0
- 4.0.1

The fix works with version 2.0.1 API. Future upgrades should verify API compatibility.

### Performance

Parsing adds minimal overhead since the library would parse internally anyway when using `legacyDatasetToQuads()`. Our explicit parsing is clearer and avoids the format conversion errors.

## References

- RDF Dataset Normalization: https://www.w3.org/TR/rdf-canon/
- rdf-canonize library: https://github.com/digitalbazaar/rdf-canonize
- RDF.js specification: http://rdf.js.org/

## Date

2025-10-01

## Verification

Run tests to verify fix:
```bash
npm test -- test/knowledge-engine/transaction.test.mjs
npm test -- test/dark-matter-80-20.test.mjs
```

Expected: No "triples.forEach is not a function" errors.
