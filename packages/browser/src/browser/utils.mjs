/**
 * @unrdf/browser - Utilities
 *
 * Browser-specific utilities for RDF storage and serialization.
 *
 * @module @unrdf/browser/utils
 */

import { z } from 'zod';

/**
 * Serialize quad for browser storage
 * Converts quad to plain object suitable for IndexedDB
 *
 * @param {Object} quad - Quad to serialize
 * @returns {Object} Serialized quad
 *
 * @example
 * const serialized = serializeQuadForStorage(quad);
 * // { subject: ':alice', predicate: 'foaf:name', ... }
 */
export function serializeQuadForStorage(quad) {
  return {
    subject: quad.subject.value,
    subjectType: quad.subject.termType,
    predicate: quad.predicate.value,
    object: quad.object.value,
    objectType: quad.object.termType,
    objectLanguage: quad.object.language || null,
    objectDatatype: quad.object.datatype?.value || null,
    graph: quad.graph.value,
    graphType: quad.graph.termType,
  };
}

/**
 * Deserialize quad from storage format
 * Reconstructs quad from plain object
 *
 * @param {Object} data - Serialized quad data
 * @returns {Object} Reconstructed quad
 *
 * @example
 * const quad = deserializeQuad(storedData);
 */
export function deserializeQuad(data) {
  const subject = {
    value: data.subject,
    termType: data.subjectType || 'NamedNode',
  };

  const predicate = {
    value: data.predicate,
    termType: 'NamedNode',
  };

  const object = {
    value: data.object,
    termType: data.objectType || 'Literal',
  };

  if (data.objectLanguage) {
    object.language = data.objectLanguage;
  }

  if (data.objectDatatype) {
    object.datatype = { value: data.objectDatatype };
  }

  const graph = {
    value: data.graph || '',
    termType: data.graphType || 'DefaultGraph',
  };

  return { subject, predicate, object, graph };
}

/**
 * Calculate approximate storage size of a quad
 *
 * @param {Object} quad - Quad to measure
 * @returns {number} Size in bytes (approximate)
 *
 * @example
 * const size = calculateQuadSize(quad);
 * console.log(`Quad size: ${size} bytes`);
 */
export function calculateQuadSize(quad) {
  const serialized = serializeQuadForStorage(quad);
  const jsonString = JSON.stringify(serialized);
  return new Blob([jsonString]).size;
}

/**
 * Get browser storage quota information
 *
 * @returns {Promise<Object>} Storage quota details
 * @property {number} available - Available storage in bytes
 * @property {number} used - Used storage in bytes
 * @property {number} quota - Total quota in bytes
 *
 * @example
 * const quota = await getStorageQuota();
 * console.log(`${quota.available} bytes available`);
 */
export async function getStorageQuota() {
  if (typeof navigator === 'undefined' || !navigator.storage) {
    return { available: 0, used: 0, quota: 0 };
  }

  if (navigator.storage.estimate) {
    const estimate = await navigator.storage.estimate();
    return {
      quota: estimate.quota || 0,
      used: estimate.usage || 0,
      available: (estimate.quota || 0) - (estimate.usage || 0),
    };
  }

  return { available: 0, used: 0, quota: 0 };
}

/**
 * Estimate how many quads can fit in available storage
 *
 * @param {Object} sampleQuad - Sample quad for size estimation
 * @returns {Promise<number>} Estimated quad capacity
 *
 * @example
 * const capacity = await estimateCapacity(sampleQuad);
 * console.log(`Can store approximately ${capacity} quads`);
 */
export async function estimateCapacity(sampleQuad) {
  const quadSize = calculateQuadSize(sampleQuad);
  const quota = await getStorageQuota();

  if (quadSize === 0) {
    return 0;
  }

  // Use 80% of available quota as safety margin
  const safeQuota = quota.available * 0.8;
  return Math.floor(safeQuota / quadSize);
}

/**
 * Check if storage is approaching quota limit
 *
 * @param {number} [threshold=0.8] - Warning threshold (0-1)
 * @returns {Promise<boolean>} True if approaching limit
 *
 * @example
 * if (await isStorageApproachingLimit(0.9)) {
 *   console.warn('Storage almost full!');
 * }
 */
export async function isStorageApproachingLimit(threshold = 0.8) {
  z.number().min(0).max(1).parse(threshold);

  const quota = await getStorageQuota();
  if (quota.quota === 0) {
    return false;
  }

  const usedPercent = quota.used / quota.quota;
  return usedPercent >= threshold;
}

/**
 * Format storage size for display
 *
 * @param {number} bytes - Size in bytes
 * @returns {string} Formatted size string
 *
 * @example
 * formatStorageSize(1024); // "1.0 KB"
 * formatStorageSize(1048576); // "1.0 MB"
 */
export function formatStorageSize(bytes) {
  z.number().nonnegative().parse(bytes);

  const units = ['B', 'KB', 'MB', 'GB', 'TB'];
  let size = bytes;
  let unitIndex = 0;

  while (size >= 1024 && unitIndex < units.length - 1) {
    size /= 1024;
    unitIndex++;
  }

  return `${size.toFixed(1)} ${units[unitIndex]}`;
}

/**
 * Export store data to JSON
 *
 * @param {Object} store - Store to export
 * @returns {string} JSON string
 *
 * @example
 * const json = exportStoreToJSON(store);
 * localStorage.setItem('backup', json);
 */
export function exportStoreToJSON(store) {
  const quads = [];
  for (const quad of store.match()) {
    quads.push(serializeQuadForStorage(quad));
  }
  return JSON.stringify(quads, null, 2);
}

/**
 * Import store data from JSON
 *
 * @param {Object} store - Target store
 * @param {string} jsonString - JSON data
 * @returns {number} Number of quads imported
 *
 * @example
 * const count = importStoreFromJSON(store, json);
 * console.log(`Imported ${count} quads`);
 */
export function importStoreFromJSON(store, jsonString) {
  z.string().parse(jsonString);

  const quads = JSON.parse(jsonString);
  let count = 0;

  for (const data of quads) {
    const quad = deserializeQuad(data);
    store.addQuad(quad.subject, quad.predicate, quad.object, quad.graph);
    count++;
  }

  return count;
}
