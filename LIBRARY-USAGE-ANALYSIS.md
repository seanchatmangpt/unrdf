# 📦 Library Usage Analysis - Missing Opportunities

## Available Libraries vs. Current Usage

After analyzing the package.json dependencies and current composable implementations, here are the libraries that are **available but underutilized**:

## 🔴 **Critically Underutilized Libraries**

### 1. **`@rdfjs/types`** - Type Definitions
**Status**: ❌ **NOT USED**
- **Available**: TypeScript definitions for RDF.js
- **Current Usage**: None
- **Opportunity**: Add proper JSDoc type annotations using these types
- **Impact**: Better IDE support, type safety, documentation

### 2. **`rdf-ext`** - RDF Extensions
**Status**: ⚠️ **MINIMALLY USED**
- **Available**: Extended RDF functionality
- **Current Usage**: Only imported in RdfEngine, not exposed to composables
- **Opportunity**: Use for advanced RDF operations, dataset management
- **Impact**: More powerful RDF operations

### 3. **`@zazuko/env`** - RDF Environment
**Status**: ⚠️ **MINIMALLY USED**
- **Available**: RDF environment utilities
- **Current Usage**: Only imported in RdfEngine as `$rdf`
- **Opportunity**: Use for environment-specific RDF operations
- **Impact**: Better RDF environment integration

## 🟡 **Moderately Underutilized Libraries**

### 4. **`jsonld`** - JSON-LD Processing
**Status**: ⚠️ **PARTIALLY USED**
- **Available**: Full JSON-LD processing capabilities
- **Current Usage**: Only basic conversion in RdfEngine
- **Opportunity**: Rich JSON-LD operations, framing, compaction
- **Impact**: Better JSON-LD support in composables

### 5. **`rdf-canonize`** - Canonicalization
**Status**: ⚠️ **PARTIALLY USED**
- **Available**: URDNA2015 canonicalization
- **Current Usage**: Only basic canonicalization in RdfEngine
- **Opportunity**: Advanced canonicalization features
- **Impact**: Better canonicalization support

## 🟢 **Well-Utilized Libraries**

### ✅ **Properly Used**
- **`n3`** - Core RDF operations ✅
- **`@comunica/query-sparql`** - SPARQL queries ✅
- **`eyereasoner`** - Reasoning ✅
- **`rdf-validate-shacl`** - SHACL validation ✅
- **`unctx`** - Context management ✅
- **`zod`** - Validation ✅

## 🚀 **Recommended Implementations**

### 1. **Create `useTypes` Composable**
```javascript
// Use @rdfjs/types for better type safety
import { NamedNode, Literal, BlankNode, Quad } from '@rdfjs/types';

export function useTypes() {
  return {
    isNamedNode: (term) => term.termType === 'NamedNode',
    isLiteral: (term) => term.termType === 'Literal',
    isBlankNode: (term) => term.termType === 'BlankNode',
    isQuad: (term) => term.termType === 'Quad',
    // ... more type utilities
  };
}
```

### 2. **Enhance `useCanon` with rdf-canonize**
```javascript
// Use full rdf-canonize capabilities
import rdfCanonize from 'rdf-canonize';

export function useCanon() {
  return {
    async canonicalize(store, options = {}) {
      // Use advanced canonicalization options
      return await rdfCanonize.canonicalize(store, options);
    },
    
    async canonicalizeSync(store, options = {}) {
      // Synchronous canonicalization
      return rdfCanonize.canonicalizeSync(store, options);
    }
  };
}
```

### 3. **Create `useJSONLD` Composable**
```javascript
// Use full jsonld capabilities
import jsonld from 'jsonld';

export function useJSONLD() {
  return {
    async compact(doc, context) {
      return await jsonld.compact(doc, context);
    },
    
    async expand(doc) {
      return await jsonld.expand(doc);
    },
    
    async frame(doc, frame) {
      return await jsonld.frame(doc, frame);
    },
    
    async flatten(doc) {
      return await jsonld.flatten(doc);
    }
  };
}
```

### 4. **Create `useRDFExt` Composable**
```javascript
// Use rdf-ext capabilities
import rdf from 'rdf-ext';

export function useRDFExt() {
  return {
    createDataset: () => rdf.dataset(),
    createGraph: () => rdf.graph(),
    createNamespace: (baseIRI) => rdf.namespace(baseIRI),
    // ... more rdf-ext utilities
  };
}
```

### 5. **Enhance `useTerms` with @rdfjs/types**
```javascript
// Use proper type definitions
import { NamedNode, Literal, BlankNode } from '@rdfjs/types';

export function useTerms() {
  return {
    iri(iri) {
      const term = namedNode(iri);
      // Add type checking
      if (!this.isNamedNode(term)) {
        throw new Error('Invalid IRI term');
      }
      return term;
    },
    
    isNamedNode: (term) => term.termType === 'NamedNode',
    isLiteral: (term) => term.termType === 'Literal',
    isBlankNode: (term) => term.termType === 'BlankNode'
  };
}
```

## 📊 **Priority Implementation Order**

1. **High Priority**: `useTypes` composable with `@rdfjs/types`
2. **High Priority**: Enhanced `useCanon` with `rdf-canonize`
3. **Medium Priority**: `useJSONLD` composable with `jsonld`
4. **Medium Priority**: `useRDFExt` composable with `rdf-ext`
5. **Low Priority**: Enhanced `useTerms` with type checking

## 🎯 **Expected Benefits**

- **Better Type Safety**: Using `@rdfjs/types` for proper type definitions
- **Advanced Canonicalization**: Full `rdf-canonize` capabilities
- **Rich JSON-LD Support**: Complete JSON-LD processing
- **Extended RDF Operations**: `rdf-ext` dataset and graph management
- **Better Developer Experience**: More comprehensive composable APIs

## 💡 **Conclusion**

While the core libraries are well-utilized, there are significant opportunities to leverage the **underutilized libraries** (`@rdfjs/types`, `rdf-ext`, `@zazuko/env`, `jsonld`, `rdf-canonize`) to create more powerful and type-safe composables. This would significantly enhance the developer experience and provide more comprehensive RDF functionality.
