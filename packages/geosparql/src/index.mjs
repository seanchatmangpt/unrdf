/**
 * @file GeoSPARQL package exports
 * @module @unrdf/geosparql
 * @description OGC GeoSPARQL standard compliance for spatial RDF queries
 */

export {
  Point,
  LineString,
  Polygon,
  getBBox,
  toWKT,
  fromWKT
} from './geometry.mjs';

export {
  haversineDistance,
  convertDistance,
  distance,
  withinDistance,
  bearing
} from './distance.mjs';

export {
  within,
  contains,
  intersects,
  touches,
  disjoint,
  overlaps,
  evaluate,
  filter
} from './spatial-relations.mjs';

export {
  createSpatialIndex,
  kNearest
} from './rtree-index.mjs';

export {
  CRS,
  validateCRS,
  isGeographic,
  normalizeCRS,
  getAxisOrder,
  transformCoordinates,
  getCRSUri
} from './crs.mjs';

export {
  GEOF,
  geof_distance,
  geof_within,
  geof_contains,
  geof_intersects,
  geof_touches,
  geof_buffer,
  geof_convexHull,
  geof_envelope,
  geof_intersection,
  geof_union,
  geof_difference,
  geof_area,
  geof_length,
  GEOSPARQL_FUNCTIONS
} from './query-functions.mjs';

export {
  createGeoSPARQLEngine
} from './geosparql-engine.mjs';

export {
  CoordinateSchema,
  PointSchema,
  LineStringSchema,
  PolygonSchema,
  GeometrySchema,
  BBoxSchema,
  SpatialRelationSchema,
  CRSSchema,
  DistanceUnitSchema
} from './schemas.mjs';
