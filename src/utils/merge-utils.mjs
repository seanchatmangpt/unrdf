/**
 * @fileoverview Merge utilities - RDF store operations and data merging
 *
 * These utilities provide comprehensive store operations including merging,
 * diffing, intersection, union, and other set operations on RDF stores.
 *
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { createStore } from '@unrdf/core';
import { DataFactory as _DataFactory } from 'n3';
import { asNamedNode as _asNamedNode, getIRI as _getIRI } from './term-utils.mjs';

/**
 * Merge multiple stores into one
 * @param {...object} stores - Stores to merge
 * @returns {object} Merged store
 */
export const mergeStores = (...stores) => {
  const mergedStore = createStore();

  for (const store of stores) {
    for (const quad of store) {
      mergedStore.add(quad);
    }
  }

  return mergedStore;
};

/**
 * Union of two stores (all quads from both stores)
 * @param {import('n3').Store} store1 - First store
 * @param {import('n3').Store} store2 - Second store
 * @returns {import('n3').Store} Union store
 */
export const unionStores = (store1, store2) => {
  return mergeStores(store1, store2);
};

/**
 * Intersection of two stores (quads present in both stores)
 * @param {object} store1 - First store
 * @param {object} store2 - Second store
 * @returns {object} Intersection store
 */
export const intersectStores = (store1, store2) => {
  const intersectionStore = createStore();
  const store2Quads = new Set();

  // Create a set of quads from store2 for fast lookup
  for (const quad of store2) {
    store2Quads.add(quadToString(quad));
  }

  // Check each quad in store1
  for (const quad of store1) {
    if (store2Quads.has(quadToString(quad))) {
      intersectionStore.add(quad);
    }
  }

  return intersectionStore;
};

/**
 * Difference of two stores (quads in store1 but not in store2)
 * @param {object} store1 - First store
 * @param {object} store2 - Second store
 * @returns {object} Difference store
 */
export const differenceStores = (store1, store2) => {
  const differenceStore = createStore();
  const store2Quads = new Set();

  // Create a set of quads from store2 for fast lookup
  for (const quad of store2) {
    store2Quads.add(quadToString(quad));
  }

  // Check each quad in store1
  for (const quad of store1) {
    if (!store2Quads.has(quadToString(quad))) {
      differenceStore.add(quad);
    }
  }

  return differenceStore;
};

/**
 * Symmetric difference of two stores (quads in either store but not both)
 * @param {import('n3').Store} store1 - First store
 * @param {import('n3').Store} store2 - Second store
 * @returns {import('n3').Store} Symmetric difference store
 */
export const symmetricDifferenceStores = (store1, store2) => {
  const diff1 = differenceStores(store1, store2);
  const diff2 = differenceStores(store2, store1);
  return mergeStores(diff1, diff2);
};

/**
 * Check if store1 is a subset of store2
 * @param {import('n3').Store} store1 - First store
 * @param {import('n3').Store} store2 - Second store
 * @returns {boolean} True if store1 is a subset of store2
 */
export const isSubset = (store1, store2) => {
  const store2Quads = new Set();

  // Create a set of quads from store2 for fast lookup
  for (const quad of store2) {
    store2Quads.add(quadToString(quad));
  }

  // Check if all quads in store1 are in store2
  for (const quad of store1) {
    if (!store2Quads.has(quadToString(quad))) {
      return false;
    }
  }

  return true;
};

/**
 * Check if store1 is a superset of store2
 * @param {import('n3').Store} store1 - First store
 * @param {import('n3').Store} store2 - Second store
 * @returns {boolean} True if store1 is a superset of store2
 */
export const isSuperset = (store1, store2) => {
  return isSubset(store2, store1);
};

/**
 * Check if two stores are equal
 * @param {import('n3').Store} store1 - First store
 * @param {import('n3').Store} store2 - Second store
 * @returns {boolean} True if stores are equal
 */
export const areStoresEqual = (store1, store2) => {
  if (store1.size !== store2.size) {
    return false;
  }

  return isSubset(store1, store2);
};

/**
 * Get the difference between two stores with detailed information
 * @param {import('n3').Store} store1 - First store
 * @param {import('n3').Store} store2 - Second store
 * @returns {Object} Detailed difference information
 */
export const getStoreDiff = (store1, store2) => {
  const store1Quads = new Set();
  const store2Quads = new Set();

  // Create sets of quads for both stores
  for (const quad of store1) {
    store1Quads.add(quadToString(quad));
  }

  for (const quad of store2) {
    store2Quads.add(quadToString(quad));
  }

  // Find differences
  const onlyInStore1 = [];
  const onlyInStore2 = [];
  const inBoth = [];

  for (const quad of store1) {
    const quadStr = quadToString(quad);
    if (store2Quads.has(quadStr)) {
      inBoth.push(quad);
    } else {
      onlyInStore1.push(quad);
    }
  }

  for (const quad of store2) {
    const quadStr = quadToString(quad);
    if (!store1Quads.has(quadStr)) {
      onlyInStore2.push(quad);
    }
  }

  return {
    onlyInStore1,
    onlyInStore2,
    inBoth,
    store1Size: store1.size,
    store2Size: store2.size,
    commonSize: inBoth.length,
    differenceSize: onlyInStore1.length + onlyInStore2.length,
  };
};

/**
 * Merge stores with conflict resolution
 * @param {import('n3').Store} store1 - First store
 * @param {import('n3').Store} store2 - Second store
 * @param {Object} [options] - Merge options
 * @param {string} [options.strategy='union'] - Merge strategy (union, intersection, store1, store2)
 * @param {Function} [options.conflictResolver] - Function to resolve conflicts
 * @returns {import('n3').Store} Merged store
 */
export const mergeStoresWithStrategy = (store1, store2, options = {}) => {
  const { strategy = 'union', conflictResolver } = options;

  switch (strategy) {
    case 'union': {
      return unionStores(store1, store2);
    }
    case 'intersection': {
      return intersectStores(store1, store2);
    }
    case 'store1': {
      const s1 = createStore();
      for (const quad of store1) s1.add(quad);
      return s1;
    }
    case 'store2': {
      const s2 = createStore();
      for (const quad of store2) s2.add(quad);
      return s2;
    }
    case 'custom': {
      if (!conflictResolver) {
        throw new Error('Conflict resolver required for custom strategy');
      }
      return conflictResolver(store1, store2);
    }
    default: {
      throw new Error(`Unknown merge strategy: ${strategy}`);
    }
  }
};

/**
 * Merge stores by subject (merge quads with the same subject)
 * @param {object} store1 - First store
 * @param {object} store2 - Second store
 * @returns {object} Merged store
 */
export const mergeStoresBySubject = (store1, store2) => {
  const mergedStore = createStore();
  const subjectQuads = new Map();

  // Collect all quads by subject
  for (const quad of store1) {
    const subject = quad.subject.value;
    if (!subjectQuads.has(subject)) {
      subjectQuads.set(subject, []);
    }
    subjectQuads.get(subject).push(quad);
  }

  for (const quad of store2) {
    const subject = quad.subject.value;
    if (!subjectQuads.has(subject)) {
      subjectQuads.set(subject, []);
    }
    subjectQuads.get(subject).push(quad);
  }

  // Add all quads to merged store
  for (const quads of subjectQuads.values()) {
    for (const quad of quads) {
      mergedStore.add(quad);
    }
  }

  return mergedStore;
};

/**
 * Merge stores by predicate (merge quads with the same predicate)
 * @param {object} store1 - First store
 * @param {object} store2 - Second store
 * @returns {object} Merged store
 */
export const mergeStoresByPredicate = (store1, store2) => {
  const mergedStore = createStore();
  const predicateQuads = new Map();

  // Collect all quads by predicate
  for (const quad of store1) {
    const predicate = quad.predicate.value;
    if (!predicateQuads.has(predicate)) {
      predicateQuads.set(predicate, []);
    }
    predicateQuads.get(predicate).push(quad);
  }

  for (const quad of store2) {
    const predicate = quad.predicate.value;
    if (!predicateQuads.has(predicate)) {
      predicateQuads.set(predicate, []);
    }
    predicateQuads.get(predicate).push(quad);
  }

  // Add all quads to merged store
  for (const quads of predicateQuads.values()) {
    for (const quad of quads) {
      mergedStore.add(quad);
    }
  }

  return mergedStore;
};

/**
 * Remove duplicate quads from a store
 * @param {object} store - Store to deduplicate
 * @returns {object} Deduplicated store
 */
export const deduplicateStore = store => {
  const deduplicatedStore = createStore();
  const seenQuads = new Set();

  for (const quad of store) {
    const quadStr = quadToString(quad);
    if (!seenQuads.has(quadStr)) {
      seenQuads.add(quadStr);
      deduplicatedStore.add(quad);
    }
  }

  return deduplicatedStore;
};

/**
 * Get statistics about store merging
 * @param {import('n3').Store} store1 - First store
 * @param {import('n3').Store} store2 - Second store
 * @returns {Object} Merge statistics
 */
export const getMergeStats = (store1, store2) => {
  const diff = getStoreDiff(store1, store2);
  const union = unionStores(store1, store2);
  const intersection = intersectStores(store1, store2);

  return {
    store1Size: store1.size,
    store2Size: store2.size,
    unionSize: union.size,
    intersectionSize: intersection.size,
    onlyInStore1: diff.onlyInStore1.length,
    onlyInStore2: diff.onlyInStore2.length,
    common: diff.inBoth.length,
    overlapRatio: intersection.size / Math.max(store1.size, store2.size),
    jaccardIndex: intersection.size / union.size,
  };
};

/**
 * Convert a quad to a string representation for comparison
 * @param {import('n3').Quad} quad - RDF quad
 * @returns {string} String representation
 */
const quadToString = quad => {
  return `${quad.subject.value} ${quad.predicate.value} ${quad.object.value} ${quad.graph?.value || ''}`;
};

/**
 * Merge stores with validation
 * @param {import('n3').Store} store1 - First store
 * @param {import('n3').Store} store2 - Second store
 * @param {Object} [options] - Merge options
 * @param {Function} [options.validator] - Validation function
 * @returns {Object} Merge result with validation
 */
export const mergeStoresWithValidation = (store1, store2, options = {}) => {
  const { validator } = options;
  const mergedStore = mergeStores(store1, store2);

  let validationResult = { valid: true, issues: [] };

  if (validator) {
    try {
      validationResult = validator(mergedStore);
    } catch (error) {
      validationResult = {
        valid: false,
        issues: [{ type: 'error', message: error.message }],
      };
    }
  }

  return {
    store: mergedStore,
    validation: validationResult,
    stats: getMergeStats(store1, store2),
  };
};

/**
 * Merge stores by graph (merge quads with the same graph)
 * @param {object} store1 - First store
 * @param {object} store2 - Second store
 * @returns {object} Merged store
 */
export const mergeStoresByGraph = (store1, store2) => {
  const mergedStore = createStore();
  const graphQuads = new Map();

  // Collect all quads by graph
  for (const quad of store1) {
    const graph = quad.graph?.value || 'default';
    if (!graphQuads.has(graph)) {
      graphQuads.set(graph, []);
    }
    graphQuads.get(graph).push(quad);
  }

  for (const quad of store2) {
    const graph = quad.graph?.value || 'default';
    if (!graphQuads.has(graph)) {
      graphQuads.set(graph, []);
    }
    graphQuads.get(graph).push(quad);
  }

  // Add all quads to merged store
  for (const quads of graphQuads.values()) {
    for (const quad of quads) {
      mergedStore.add(quad);
    }
  }

  return mergedStore;
};

/**
 * Merge stores with conflict detection
 * @param {object} store1 - First store
 * @param {object} store2 - Second store
 * @returns {Object} Merge result with conflict information
 */
export const mergeStoresWithConflictDetection = (store1, store2) => {
  const mergedStore = createStore();
  const conflicts = [];
  const subjectPredicateMap = new Map();

  // First pass: collect all quads by subject+predicate
  for (const quad of store1) {
    const key = `${quad.subject.value} ${quad.predicate.value}`;
    if (!subjectPredicateMap.has(key)) {
      subjectPredicateMap.set(key, []);
    }
    subjectPredicateMap.get(key).push({ quad, source: 'store1' });
  }

  for (const quad of store2) {
    const key = `${quad.subject.value} ${quad.predicate.value}`;
    if (!subjectPredicateMap.has(key)) {
      subjectPredicateMap.set(key, []);
    }
    subjectPredicateMap.get(key).push({ quad, source: 'store2' });
  }

  // Second pass: detect conflicts and merge
  for (const [_key, quads] of subjectPredicateMap) {
    if (quads.length === 1) {
      // No conflict, add the quad
      mergedStore.add(quads[0].quad);
    } else {
      // Potential conflict - check if objects are different
      const objects = quads.map(q => q.quad.object.value);
      const uniqueObjects = [...new Set(objects)];

      if (uniqueObjects.length === 1) {
        // No actual conflict, objects are the same
        mergedStore.add(quads[0].quad);
      } else {
        // Conflict detected
        conflicts.push({
          subject: quads[0].quad.subject.value,
          predicate: quads[0].quad.predicate.value,
          objects: uniqueObjects,
          sources: quads.map(q => q.source),
        });

        // Add all conflicting quads to merged store
        for (const { quad } of quads) {
          mergedStore.add(quad);
        }
      }
    }
  }

  return {
    store: mergedStore,
    conflicts,
    conflictCount: conflicts.length,
  };
};
