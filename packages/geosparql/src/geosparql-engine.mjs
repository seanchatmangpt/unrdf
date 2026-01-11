/**
 * @file Main GeoSPARQL engine
 * @module @unrdf/geosparql/geosparql-engine
 * @description OGC GeoSPARQL query engine with spatial indexing and OTEL instrumentation
 */

import { trace } from '@opentelemetry/api';
import { createSpatialIndex } from './rtree-index.mjs';
import { GEOSPARQL_FUNCTIONS } from './query-functions.mjs';
import { evaluate } from './spatial-relations.mjs';
import { fromWKT } from './geometry.mjs';

const tracer = trace.getTracer('@unrdf/geosparql');

/**
 * Creates a GeoSPARQL query engine
 * @param {Object} [options] - Engine options
 * @param {boolean} [options.enableIndex=true] - Enable R-tree indexing
 * @param {number} [options.indexMaxEntries=9] - R-tree max entries per node
 * @returns {Object} GeoSPARQL engine
 * @example
 * const engine = createGeoSPARQLEngine();
 * engine.addGeometry(point, { id: 'sf', name: 'San Francisco' });
 * const nearby = engine.query({ distance: { from: point, max: 1000 } });
 */
export function createGeoSPARQLEngine(options = {}) {
  const {
    enableIndex = true,
    indexMaxEntries = 9
  } = options;

  const index = enableIndex ? createSpatialIndex(indexMaxEntries) : null;
  const geometries = new Map();

  return {
    /**
     * Adds geometry to engine
     * @param {Object} geometry - GeoJSON geometry
     * @param {*} [data] - Associated data
     * @returns {string} Geometry ID
     */
    addGeometry(geometry, data = null) {
      return tracer.startActiveSpan('geosparql.addGeometry', (span) => {
        try {
          const id = crypto.randomUUID();

          if (index) {
            const indexId = index.insert(geometry, data);
            geometries.set(id, { indexId, geometry, data });
          } else {
            geometries.set(id, { geometry, data });
          }

          span.setAttribute('geometry.id', id);
          span.setAttribute('geometry.type', geometry.type);

          return id;
        } finally {
          span.end();
        }
      });
    },

    /**
     * Removes geometry from engine
     * @param {string} id - Geometry ID
     * @returns {boolean} True if removed
     */
    removeGeometry(id) {
      return tracer.startActiveSpan('geosparql.removeGeometry', (span) => {
        try {
          const entry = geometries.get(id);
          if (!entry) {
            span.setAttribute('removed', false);
            return false;
          }

          if (index && entry.indexId) {
            index.remove(entry.indexId);
          }

          geometries.delete(id);
          span.setAttribute('removed', true);
          return true;
        } finally {
          span.end();
        }
      });
    },

    /**
     * Queries geometries by spatial criteria
     * @param {Object} criteria - Query criteria
     * @param {Object} [criteria.bbox] - Bounding box filter
     * @param {Object} [criteria.relation] - Spatial relation filter
     * @param {Object} [criteria.distance] - Distance filter
     * @returns {Array<Object>} Matching geometries with data
     */
    query(criteria) {
      return tracer.startActiveSpan('geosparql.query', (span) => {
        try {
          let results = [];

          // BBox query using spatial index
          if (criteria.bbox && index) {
            const items = index.search(criteria.bbox);
            results = items.map(item => ({
              id: item.id,
              geometry: item.geometry,
              data: item.data
            }));
            span.setAttribute('query.type', 'bbox');
            span.setAttribute('results.count', results.length);
          }
          // Spatial relation query
          else if (criteria.relation) {
            const { geometry, type } = criteria.relation;
            const target = geometry;

            // Use index for initial filtering if available
            let candidates = [];
            if (index) {
              const items = index.searchGeometry(target);
              candidates = items;
            } else {
              candidates = Array.from(geometries.entries()).map(([id, entry]) => ({
                id,
                geometry: entry.geometry,
                data: entry.data
              }));
            }

            // Apply spatial relation filter
            results = candidates.filter(item =>
              evaluate(item.geometry, target, type)
            );

            span.setAttribute('query.type', 'relation');
            span.setAttribute('query.relation', type);
            span.setAttribute('candidates.count', candidates.length);
            span.setAttribute('results.count', results.length);
          }
          // Distance query
          else if (criteria.distance) {
            const { from, max, unit = 'meters' } = criteria.distance;

            // Get all geometries
            const allGeometries = Array.from(geometries.entries()).map(([id, entry]) => ({
              id,
              geometry: entry.geometry,
              data: entry.data
            }));

            // Calculate distances and filter
            const distanceFn = GEOSPARQL_FUNCTIONS['http://www.opengis.net/def/function/geosparql/distance'];
            results = allGeometries
              .map(item => ({
                ...item,
                distance: distanceFn(from, item.geometry, unit)
              }))
              .filter(item => item.distance <= max);

            span.setAttribute('query.type', 'distance');
            span.setAttribute('query.maxDistance', max);
            span.setAttribute('query.unit', unit);
            span.setAttribute('results.count', results.length);
          }
          // Return all
          else {
            results = Array.from(geometries.entries()).map(([id, entry]) => ({
              id,
              geometry: entry.geometry,
              data: entry.data
            }));
            span.setAttribute('query.type', 'all');
            span.setAttribute('results.count', results.length);
          }

          return results;
        } finally {
          span.end();
        }
      });
    },

    /**
     * Executes a GeoSPARQL function
     * @param {string} functionUri - Function URI
     * @param {...*} args - Function arguments
     * @returns {*} Function result
     */
    executeFunction(functionUri, ...args) {
      return tracer.startActiveSpan('geosparql.executeFunction', (span) => {
        try {
          const fn = GEOSPARQL_FUNCTIONS[functionUri];
          if (!fn) {
            throw new Error(`Unknown GeoSPARQL function: ${functionUri}`);
          }

          span.setAttribute('function.uri', functionUri);
          const result = fn(...args);

          return result;
        } finally {
          span.end();
        }
      });
    },

    /**
     * Converts WKT to geometry and adds to engine
     * @param {string} wkt - Well-Known Text
     * @param {*} [data] - Associated data
     * @returns {string} Geometry ID
     */
    addWKT(wkt, data = null) {
      const geometry = fromWKT(wkt);
      return this.addGeometry(geometry, data);
    },

    /**
     * Gets geometry by ID
     * @param {string} id - Geometry ID
     * @returns {Object|undefined} Geometry with data
     */
    getGeometry(id) {
      const entry = geometries.get(id);
      if (!entry) return undefined;

      return {
        id,
        geometry: entry.geometry,
        data: entry.data
      };
    },

    /**
     * Gets all geometries
     * @returns {Array<Object>} All geometries
     */
    getAllGeometries() {
      return Array.from(geometries.entries()).map(([id, entry]) => ({
        id,
        geometry: entry.geometry,
        data: entry.data
      }));
    },

    /**
     * Clears all geometries
     */
    clear() {
      if (index) {
        index.clear();
      }
      geometries.clear();
    },

    /**
     * Gets statistics about engine
     * @returns {Object} Engine statistics
     */
    getStats() {
      return {
        geometryCount: geometries.size,
        indexEnabled: enableIndex,
        indexSize: index ? index.size() : 0
      };
    }
  };
}
