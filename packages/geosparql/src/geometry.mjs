/**
 * @file OGC-compliant geometry types
 * @module @unrdf/geosparql/geometry
 * @description Point, LineString, and Polygon geometry implementations
 */

import { PointSchema, LineStringSchema, PolygonSchema } from './schemas.mjs';
import { validateCRS } from './crs.mjs';

/**
 * Creates a Point geometry
 * @param {[number, number]} coordinates - [longitude, latitude]
 * @param {string} [crs='CRS84'] - Coordinate reference system
 * @returns {Object} Point geometry
 * @throws {Error} If coordinates are invalid
 * @example
 * const sf = Point([-122.4194, 37.7749]); // San Francisco
 */
export function Point(coordinates, crs = 'CRS84') {
  const geometry = {
    type: 'Point',
    coordinates,
    crs: validateCRS(crs)
  };

  return PointSchema.parse(geometry);
}

/**
 * Creates a LineString geometry
 * @param {Array<[number, number]>} coordinates - Array of coordinate pairs
 * @param {string} [crs='CRS84'] - Coordinate reference system
 * @returns {Object} LineString geometry
 * @throws {Error} If coordinates are invalid
 * @example
 * const route = LineString([
 *   [-122.4194, 37.7749],
 *   [-122.4089, 37.7849]
 * ]);
 */
export function LineString(coordinates, crs = 'CRS84') {
  const geometry = {
    type: 'LineString',
    coordinates,
    crs: validateCRS(crs)
  };

  return LineStringSchema.parse(geometry);
}

/**
 * Creates a Polygon geometry
 * @param {Array<Array<[number, number]>>} coordinates - Array of linear rings
 * @param {string} [crs='CRS84'] - Coordinate reference system
 * @returns {Object} Polygon geometry
 * @throws {Error} If coordinates are invalid
 * @example
 * const bbox = Polygon([[
 *   [-122.5, 37.7], [-122.3, 37.7],
 *   [-122.3, 37.9], [-122.5, 37.9],
 *   [-122.5, 37.7]
 * ]]);
 */
export function Polygon(coordinates, crs = 'CRS84') {
  const geometry = {
    type: 'Polygon',
    coordinates,
    crs: validateCRS(crs)
  };

  return PolygonSchema.parse(geometry);
}

/**
 * Gets bounding box for geometry
 * @param {Object} geometry - GeoJSON geometry
 * @returns {[number, number, number, number]} [minLon, minLat, maxLon, maxLat]
 */
export function getBBox(geometry) {
  let coords = [];

  if (geometry.type === 'Point') {
    coords = [geometry.coordinates];
  } else if (geometry.type === 'LineString') {
    coords = geometry.coordinates;
  } else if (geometry.type === 'Polygon') {
    coords = geometry.coordinates.flat();
  }

  const lons = coords.map(c => c[0]);
  const lats = coords.map(c => c[1]);

  return [
    Math.min(...lons),
    Math.min(...lats),
    Math.max(...lons),
    Math.max(...lats)
  ];
}

/**
 * Converts geometry to Well-Known Text (WKT) format
 * @param {Object} geometry - GeoJSON geometry
 * @returns {string} WKT representation
 * @example
 * toWKT(Point([-122.4194, 37.7749])) // "POINT(-122.4194 37.7749)"
 */
export function toWKT(geometry) {
  if (geometry.type === 'Point') {
    const [lon, lat] = geometry.coordinates;
    return `POINT(${lon} ${lat})`;
  }

  if (geometry.type === 'LineString') {
    const points = geometry.coordinates
      .map(([lon, lat]) => `${lon} ${lat}`)
      .join(', ');
    return `LINESTRING(${points})`;
  }

  if (geometry.type === 'Polygon') {
    const rings = geometry.coordinates
      .map(ring => {
        const points = ring.map(([lon, lat]) => `${lon} ${lat}`).join(', ');
        return `(${points})`;
      })
      .join(', ');
    return `POLYGON(${rings})`;
  }

  throw new Error(`Unsupported geometry type: ${geometry.type}`);
}

/**
 * Parses Well-Known Text (WKT) to geometry
 * @param {string} wkt - WKT string
 * @returns {Object} GeoJSON geometry
 * @example
 * fromWKT("POINT(-122.4194 37.7749)")
 */
export function fromWKT(wkt) {
  const trimmed = wkt.trim();

  if (trimmed.startsWith('POINT')) {
    const match = trimmed.match(/POINT\(([^ ]+) ([^ ]+)\)/);
    if (!match) throw new Error('Invalid POINT WKT');
    return Point([parseFloat(match[1]), parseFloat(match[2])]);
  }

  if (trimmed.startsWith('LINESTRING')) {
    const match = trimmed.match(/LINESTRING\((.+)\)/);
    if (!match) throw new Error('Invalid LINESTRING WKT');
    const coords = match[1]
      .split(',')
      .map(p => {
        const [lon, lat] = p.trim().split(' ');
        return [parseFloat(lon), parseFloat(lat)];
      });
    return LineString(coords);
  }

  if (trimmed.startsWith('POLYGON')) {
    const match = trimmed.match(/POLYGON\((.+)\)/);
    if (!match) throw new Error('Invalid POLYGON WKT');
    const rings = match[1]
      .split(/\),\s*\(/)
      .map(ring => {
        const cleaned = ring.replace(/^\(|\)$/g, '');
        return cleaned.split(',').map(p => {
          const [lon, lat] = p.trim().split(' ');
          return [parseFloat(lon), parseFloat(lat)];
        });
      });
    return Polygon(rings);
  }

  throw new Error(`Unsupported WKT geometry type`);
}
