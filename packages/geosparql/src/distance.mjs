/**
 * @file Distance calculations for geospatial coordinates
 * @module @unrdf/geosparql/distance
 * @description Haversine formula and distance utilities
 */

import { distance as turfDistance } from '@turf/turf';
import { DistanceUnitSchema } from './schemas.mjs';

/**
 * Earth radius in meters
 */
const EARTH_RADIUS_METERS = 6371000;

/**
 * Converts degrees to radians
 * @param {number} degrees - Angle in degrees
 * @returns {number} Angle in radians
 */
function toRadians(degrees) {
  return degrees * Math.PI / 180;
}

/**
 * Calculates distance between two points using Haversine formula
 * @param {[number, number]} coord1 - First coordinate [lon, lat]
 * @param {[number, number]} coord2 - Second coordinate [lon, lat]
 * @param {string} [unit='meters'] - Distance unit
 * @returns {number} Distance in specified unit
 * @example
 * const d = haversineDistance(
 *   [-122.4194, 37.7749], // San Francisco
 *   [-118.2437, 34.0522]  // Los Angeles
 * ); // ~559,120 meters
 */
export function haversineDistance(coord1, coord2, unit = 'meters') {
  DistanceUnitSchema.parse(unit);

  const [lon1, lat1] = coord1;
  const [lon2, lat2] = coord2;

  const lat1Rad = toRadians(lat1);
  const lat2Rad = toRadians(lat2);
  const deltaLat = toRadians(lat2 - lat1);
  const deltaLon = toRadians(lon2 - lon1);

  const a = Math.sin(deltaLat / 2) * Math.sin(deltaLat / 2) +
            Math.cos(lat1Rad) * Math.cos(lat2Rad) *
            Math.sin(deltaLon / 2) * Math.sin(deltaLon / 2);

  const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
  const distanceMeters = EARTH_RADIUS_METERS * c;

  return convertDistance(distanceMeters, 'meters', unit);
}

/**
 * Converts distance between units
 * @param {number} distance - Distance value
 * @param {string} fromUnit - Source unit
 * @param {string} toUnit - Target unit
 * @returns {number} Converted distance
 */
export function convertDistance(distance, fromUnit, toUnit) {
  DistanceUnitSchema.parse(fromUnit);
  DistanceUnitSchema.parse(toUnit);

  if (fromUnit === toUnit) return distance;

  // Convert to meters first
  let meters = distance;
  if (fromUnit === 'kilometers') meters = distance * 1000;
  if (fromUnit === 'miles') meters = distance * 1609.34;
  if (fromUnit === 'feet') meters = distance * 0.3048;

  // Convert from meters to target
  if (toUnit === 'meters') return meters;
  if (toUnit === 'kilometers') return meters / 1000;
  if (toUnit === 'miles') return meters / 1609.34;
  if (toUnit === 'feet') return meters / 0.3048;

  return meters;
}

/**
 * Calculates distance between two geometries
 * @param {Object} geom1 - First geometry
 * @param {Object} geom2 - Second geometry
 * @param {string} [unit='meters'] - Distance unit
 * @returns {number} Distance in specified unit
 */
export function distance(geom1, geom2, unit = 'meters') {
  DistanceUnitSchema.parse(unit);

  // Use Turf.js for accurate calculations
  const distanceKm = turfDistance(geom1, geom2, { units: 'kilometers' });
  const distanceMeters = distanceKm * 1000;

  return convertDistance(distanceMeters, 'meters', unit);
}

/**
 * Checks if point is within distance of another point
 * @param {Object} point - Point geometry
 * @param {Object} center - Center point geometry
 * @param {number} radius - Radius distance
 * @param {string} [unit='meters'] - Distance unit
 * @returns {boolean} True if within distance
 */
export function withinDistance(point, center, radius, unit = 'meters') {
  const dist = distance(point, center, unit);
  return dist <= radius;
}

/**
 * Calculates bearing between two points
 * @param {[number, number]} coord1 - First coordinate [lon, lat]
 * @param {[number, number]} coord2 - Second coordinate [lon, lat]
 * @returns {number} Bearing in degrees (0-360)
 */
export function bearing(coord1, coord2) {
  const [lon1, lat1] = coord1;
  const [lon2, lat2] = coord2;

  const lat1Rad = toRadians(lat1);
  const lat2Rad = toRadians(lat2);
  const deltaLon = toRadians(lon2 - lon1);

  const y = Math.sin(deltaLon) * Math.cos(lat2Rad);
  const x = Math.cos(lat1Rad) * Math.sin(lat2Rad) -
            Math.sin(lat1Rad) * Math.cos(lat2Rad) * Math.cos(deltaLon);

  const bearingRad = Math.atan2(y, x);
  const bearingDeg = (bearingRad * 180 / Math.PI + 360) % 360;

  return bearingDeg;
}
