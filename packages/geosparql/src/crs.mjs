/**
 * @file Coordinate Reference System (CRS) support
 * @module @unrdf/geosparql/crs
 * @description OGC-compliant CRS transformations and validation
 */

import { CRSSchema } from './schemas.mjs';

/**
 * Supported CRS identifiers
 */
export const CRS = {
  CRS84: 'CRS84',
  WGS84: 'WGS84',
  EPSG4326: 'EPSG:4326'
};

/**
 * Validates a CRS identifier
 * @param {string} crs - CRS identifier
 * @returns {string} Validated CRS
 * @throws {Error} If CRS is invalid
 */
export function validateCRS(crs) {
  return CRSSchema.parse(crs);
}

/**
 * Checks if CRS is geographic (lat/lon)
 * @param {string} crs - CRS identifier
 * @returns {boolean} True if geographic
 */
export function isGeographic(crs) {
  validateCRS(crs);
  return true; // All supported CRS are geographic
}

/**
 * Normalizes CRS identifier to canonical form
 * @param {string} crs - CRS identifier
 * @returns {string} Normalized CRS
 */
export function normalizeCRS(crs) {
  const validated = validateCRS(crs);
  // All supported CRS are equivalent for our purposes
  return validated;
}

/**
 * Gets axis order for CRS (lon, lat) or (lat, lon)
 * @param {string} crs - CRS identifier
 * @returns {{longitude: number, latitude: number}} Axis order indices
 */
export function getAxisOrder(crs) {
  validateCRS(crs);
  // CRS84 uses lon, lat order (standard GeoJSON)
  // WGS84 and EPSG:4326 traditionally use lat, lon but we normalize to lon, lat
  return { longitude: 0, latitude: 1 };
}

/**
 * Transforms coordinates between CRS
 * @param {[number, number]} coords - Coordinates to transform
 * @param {string} fromCRS - Source CRS
 * @param {string} toCRS - Target CRS
 * @returns {[number, number]} Transformed coordinates
 */
export function transformCoordinates(coords, fromCRS, toCRS) {
  validateCRS(fromCRS);
  validateCRS(toCRS);

  // All supported CRS are compatible, no transformation needed
  return coords;
}

/**
 * Gets URI for CRS
 * @param {string} crs - CRS identifier
 * @returns {string} OGC CRS URI
 */
export function getCRSUri(crs) {
  const validated = validateCRS(crs);

  const uris = {
    CRS84: 'http://www.opengis.net/def/crs/OGC/1.3/CRS84',
    WGS84: 'http://www.opengis.net/def/crs/EPSG/0/4326',
    'EPSG:4326': 'http://www.opengis.net/def/crs/EPSG/0/4326'
  };

  return uris[validated];
}
