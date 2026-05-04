/**
 * @file GeoSPARQL query functions
 * @module @unrdf/geosparql/query-functions
 * @description OGC GeoSPARQL function implementations (geof:distance, geof:within, etc.)
 */

import { distance } from './distance.mjs';
import { within, contains, intersects, touches } from './spatial-relations.mjs';
import * as turf from '@turf/turf';

/**
 * GeoSPARQL namespace
 */
export const GEOF = 'http://www.opengis.net/def/function/geosparql/';

/**
 * geof:distance - Calculate distance between two geometries
 * @param {Object} g1 - First geometry
 * @param {Object} g2 - Second geometry
 * @param {string} [unit='meters'] - Distance unit
 * @returns {number} Distance in meters
 * @example
 * const d = geof_distance(point1, point2); // distance in meters
 */
export function geof_distance(g1, g2, unit = 'meters') {
  return distance(g1, g2, unit);
}

/**
 * geof:within - Test if g1 is within g2
 * @param {Object} g1 - First geometry
 * @param {Object} g2 - Second geometry
 * @returns {boolean} True if g1 within g2
 */
export function geof_within(g1, g2) {
  return within(g1, g2);
}

/**
 * geof:contains - Test if g1 contains g2
 * @param {Object} g1 - First geometry
 * @param {Object} g2 - Second geometry
 * @returns {boolean} True if g1 contains g2
 */
export function geof_contains(g1, g2) {
  return contains(g1, g2);
}

/**
 * geof:intersects - Test if geometries intersect
 * @param {Object} g1 - First geometry
 * @param {Object} g2 - Second geometry
 * @returns {boolean} True if geometries intersect
 */
export function geof_intersects(g1, g2) {
  return intersects(g1, g2);
}

/**
 * geof:touches - Test if geometries touch
 * @param {Object} g1 - First geometry
 * @param {Object} g2 - Second geometry
 * @returns {boolean} True if geometries touch
 */
export function geof_touches(g1, g2) {
  return touches(g1, g2);
}

/**
 * geof:buffer - Create buffer around geometry
 * @param {Object} geometry - Input geometry
 * @param {number} distance - Buffer distance in meters
 * @returns {Object} Buffered polygon geometry
 * @example
 * const buffered = geof_buffer(point, 1000); // 1km buffer
 */
export function geof_buffer(geometry, distance) {
  // Convert meters to kilometers for Turf
  const distanceKm = distance / 1000;
  const buffered = turf.buffer(geometry, distanceKm, { units: 'kilometers' });
  return buffered.geometry;
}

/**
 * geof:convexHull - Calculate convex hull of geometry
 * @param {Object} geometry - Input geometry
 * @returns {Object} Convex hull polygon
 */
export function geof_convexHull(geometry) {
  const hull = turf.convex(turf.featureCollection([turf.feature(geometry)]));
  return hull ? hull.geometry : null;
}

/**
 * geof:envelope - Calculate bounding box envelope
 * @param {Object} geometry - Input geometry
 * @returns {Object} Envelope polygon
 */
export function geof_envelope(geometry) {
  const envelope = turf.envelope(turf.featureCollection([turf.feature(geometry)]));
  return envelope.geometry;
}

/**
 * geof:intersection - Calculate intersection of two geometries
 * @param {Object} g1 - First geometry
 * @param {Object} g2 - Second geometry
 * @returns {Object|null} Intersection geometry or null
 */
export function geof_intersection(g1, g2) {
  try {
    const intersection = turf.intersect(
      turf.featureCollection([turf.feature(g1), turf.feature(g2)])
    );
    return intersection ? intersection.geometry : null;
  } catch (_error) {
    return null;
  }
}

/**
 * geof:union - Calculate union of two geometries
 * @param {Object} g1 - First geometry
 * @param {Object} g2 - Second geometry
 * @returns {Object} Union geometry
 */
export function geof_union(g1, g2) {
  const union = turf.union(
    turf.featureCollection([turf.feature(g1), turf.feature(g2)])
  );
  return union.geometry;
}

/**
 * geof:difference - Calculate difference between geometries (g1 - g2)
 * @param {Object} g1 - First geometry
 * @param {Object} g2 - Second geometry
 * @returns {Object|null} Difference geometry or null
 */
export function geof_difference(g1, g2) {
  try {
    const difference = turf.difference(
      turf.featureCollection([turf.feature(g1), turf.feature(g2)])
    );
    return difference ? difference.geometry : null;
  } catch (_error) {
    return null;
  }
}

/**
 * geof:area - Calculate area of geometry
 * @param {Object} geometry - Input geometry
 * @returns {number} Area in square meters
 */
export function geof_area(geometry) {
  if (geometry.type === 'Point' || geometry.type === 'LineString') {
    return 0;
  }
  return turf.area(geometry);
}

/**
 * geof:length - Calculate length of geometry
 * @param {Object} geometry - Input geometry
 * @returns {number} Length in meters
 */
export function geof_length(geometry) {
  if (geometry.type === 'Point') {
    return 0;
  }
  if (geometry.type === 'LineString') {
    return turf.length(geometry, { units: 'meters' });
  }
  if (geometry.type === 'Polygon') {
    // Calculate perimeter
    let totalLength = 0;
    for (const ring of geometry.coordinates) {
      const lineString = { type: 'LineString', coordinates: ring };
      totalLength += turf.length(lineString, { units: 'meters' });
    }
    return totalLength;
  }
  return 0;
}

/**
 * Registry of all GeoSPARQL functions
 */
export const GEOSPARQL_FUNCTIONS = {
  [`${GEOF}distance`]: geof_distance,
  [`${GEOF}within`]: geof_within,
  [`${GEOF}contains`]: geof_contains,
  [`${GEOF}intersects`]: geof_intersects,
  [`${GEOF}touches`]: geof_touches,
  [`${GEOF}buffer`]: geof_buffer,
  [`${GEOF}convexHull`]: geof_convexHull,
  [`${GEOF}envelope`]: geof_envelope,
  [`${GEOF}intersection`]: geof_intersection,
  [`${GEOF}union`]: geof_union,
  [`${GEOF}difference`]: geof_difference,
  [`${GEOF}area`]: geof_area,
  [`${GEOF}length`]: geof_length
};
