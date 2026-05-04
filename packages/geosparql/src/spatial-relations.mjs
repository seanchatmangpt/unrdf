/**
 * @file Topological spatial relations
 * @module @unrdf/geosparql/spatial-relations
 * @description OGC-compliant spatial predicates (within, intersects, contains, etc.)
 */

import * as turf from '@turf/turf';
import { SpatialRelationSchema } from './schemas.mjs';

/**
 * Checks if geometry g1 is within geometry g2
 * @param {Object} g1 - First geometry
 * @param {Object} g2 - Second geometry
 * @returns {boolean} True if g1 is within g2
 * @example
 * const point = Point([-122.4, 37.8]);
 * const polygon = Polygon([[...]]);
 * within(point, polygon) // true if point inside polygon
 */
export function within(g1, g2) {
  try {
    // Point within polygon
    if (g1.type === 'Point' && g2.type === 'Polygon') {
      return turf.booleanPointInPolygon(g1, g2);
    }

    // General case
    return turf.booleanWithin(g1, g2);
  } catch (_error) {
    return false;
  }
}

/**
 * Checks if geometry g1 contains geometry g2
 * @param {Object} g1 - First geometry
 * @param {Object} g2 - Second geometry
 * @returns {boolean} True if g1 contains g2
 * @example
 * contains(polygon, point) // true if point inside polygon
 */
export function contains(g1, g2) {
  try {
    // Polygon contains point
    if (g1.type === 'Polygon' && g2.type === 'Point') {
      return turf.booleanPointInPolygon(g2, g1);
    }

    // General case
    return turf.booleanContains(g1, g2);
  } catch (_error) {
    return false;
  }
}

/**
 * Checks if two geometries intersect
 * @param {Object} g1 - First geometry
 * @param {Object} g2 - Second geometry
 * @returns {boolean} True if geometries intersect
 * @example
 * intersects(line1, line2) // true if lines cross
 */
export function intersects(g1, g2) {
  try {
    // Check for overlap or intersection
    if (g1.type === 'Point' && g2.type === 'Polygon') {
      return turf.booleanPointInPolygon(g1, g2);
    }

    if (g2.type === 'Point' && g1.type === 'Polygon') {
      return turf.booleanPointInPolygon(g2, g1);
    }

    // General intersection test
    return turf.booleanIntersects(g1, g2);
  } catch (_error) {
    return false;
  }
}

/**
 * Checks if two geometries touch (share boundary but not interior)
 * @param {Object} g1 - First geometry
 * @param {Object} g2 - Second geometry
 * @returns {boolean} True if geometries touch
 */
export function touches(g1, g2) {
  try {
    // Points can't touch
    if (g1.type === 'Point' || g2.type === 'Point') {
      return false;
    }

    // Check if geometries intersect but don't overlap
    const doesIntersect = turf.booleanIntersects(g1, g2);
    if (!doesIntersect) return false;

    // Check if intersection is only on boundary
    const intersection = turf.intersect(turf.featureCollection([
      turf.feature(g1),
      turf.feature(g2)
    ]));

    return intersection === null || intersection.geometry.type === 'LineString';
  } catch (_error) {
    return false;
  }
}

/**
 * Checks if two geometries are disjoint (don't intersect)
 * @param {Object} g1 - First geometry
 * @param {Object} g2 - Second geometry
 * @returns {boolean} True if geometries are disjoint
 */
export function disjoint(g1, g2) {
  return !intersects(g1, g2);
}

/**
 * Checks if two geometries overlap
 * @param {Object} g1 - First geometry
 * @param {Object} g2 - Second geometry
 * @returns {boolean} True if geometries overlap
 */
export function overlaps(g1, g2) {
  try {
    return turf.booleanOverlap(g1, g2);
  } catch (_error) {
    // If types don't support overlap check, fall back to intersects
    return intersects(g1, g2) && !within(g1, g2) && !contains(g1, g2);
  }
}

/**
 * Evaluates a spatial relation between two geometries
 * @param {Object} g1 - First geometry
 * @param {Object} g2 - Second geometry
 * @param {string} relation - Spatial relation type
 * @returns {boolean} True if relation holds
 * @example
 * evaluate(point, polygon, 'within') // same as within(point, polygon)
 */
export function evaluate(g1, g2, relation) {
  const validRelation = SpatialRelationSchema.parse(relation);

  const relations = {
    within: () => within(g1, g2),
    contains: () => contains(g1, g2),
    intersects: () => intersects(g1, g2),
    touches: () => touches(g1, g2),
    disjoint: () => disjoint(g1, g2),
    overlaps: () => overlaps(g1, g2)
  };

  return relations[validRelation]();
}

/**
 * Finds all geometries that satisfy a spatial relation with target
 * @param {Array<Object>} geometries - Array of geometries to test
 * @param {Object} target - Target geometry
 * @param {string} relation - Spatial relation
 * @returns {Array<Object>} Geometries that satisfy relation
 */
export function filter(geometries, target, relation) {
  return geometries.filter(geom => evaluate(geom, target, relation));
}
