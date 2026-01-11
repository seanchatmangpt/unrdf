/**
 * @file Zod schemas for GeoSPARQL geometry types
 * @module @unrdf/geosparql/schemas
 * @description OGC-compliant geometry validation schemas
 */

import { z } from 'zod';

/**
 * Coordinate pair schema [longitude, latitude]
 */
export const CoordinateSchema = z.tuple([
  z.number().min(-180).max(180),
  z.number().min(-90).max(90)
]);

/**
 * Point geometry schema
 */
export const PointSchema = z.object({
  type: z.literal('Point'),
  coordinates: CoordinateSchema,
  crs: z.string().default('CRS84')
});

/**
 * LineString geometry schema
 */
export const LineStringSchema = z.object({
  type: z.literal('LineString'),
  coordinates: z.array(CoordinateSchema).min(2),
  crs: z.string().default('CRS84')
});

/**
 * Linear ring for polygon (closed loop)
 */
const LinearRingSchema = z.array(CoordinateSchema)
  .min(4)
  .refine(
    (coords) => {
      const first = coords[0];
      const last = coords[coords.length - 1];
      return first[0] === last[0] && first[1] === last[1];
    },
    { message: 'Linear ring must be closed (first point = last point)' }
  );

/**
 * Polygon geometry schema
 */
export const PolygonSchema = z.object({
  type: z.literal('Polygon'),
  coordinates: z.array(LinearRingSchema).min(1),
  crs: z.string().default('CRS84')
});

/**
 * Generic geometry schema (union of all types)
 */
export const GeometrySchema = z.union([
  PointSchema,
  LineStringSchema,
  PolygonSchema
]);

/**
 * Bounding box schema [minLon, minLat, maxLon, maxLat]
 */
export const BBoxSchema = z.tuple([
  z.number().min(-180).max(180),
  z.number().min(-90).max(90),
  z.number().min(-180).max(180),
  z.number().min(-90).max(90)
]).refine(
  ([minLon, minLat, maxLon, maxLat]) => minLon <= maxLon && minLat <= maxLat,
  { message: 'Invalid bounding box: min must be <= max' }
);

/**
 * Spatial relation enum
 */
export const SpatialRelationSchema = z.enum([
  'within',
  'intersects',
  'touches',
  'contains',
  'disjoint',
  'overlaps'
]);

/**
 * CRS identifier schema
 */
export const CRSSchema = z.enum(['CRS84', 'WGS84', 'EPSG:4326']);

/**
 * Distance unit schema
 */
export const DistanceUnitSchema = z.enum(['meters', 'kilometers', 'miles', 'feet']);
