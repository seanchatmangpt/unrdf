/**
 * @file Comprehensive GeoSPARQL test suite
 * @description Tests for OGC GeoSPARQL compliance, spatial operations, and performance
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  Point,
  LineString,
  Polygon,
  getBBox,
  toWKT,
  fromWKT,
  haversineDistance,
  distance,
  withinDistance,
  bearing,
  convertDistance,
  within,
  contains,
  intersects,
  disjoint,
  evaluate,
  filter,
  createSpatialIndex,
  kNearest,
  validateCRS,
  getCRSUri,
  geof_distance,
  geof_within,
  geof_contains,
  geof_intersects,
  geof_buffer,
  geof_area,
  geof_length,
  createGeoSPARQLEngine
} from '../src/index.mjs';

describe('GeoSPARQL - Geometry Types', () => {
  it('should create valid Point geometry', () => {
    const point = Point([-122.4194, 37.7749]); // San Francisco

    expect(point.type).toBe('Point');
    expect(point.coordinates).toEqual([-122.4194, 37.7749]);
    expect(point.crs).toBe('CRS84');
  });

  it('should create valid LineString geometry', () => {
    const line = LineString([
      [-122.4194, 37.7749],
      [-122.4089, 37.7849]
    ]);

    expect(line.type).toBe('LineString');
    expect(line.coordinates).toHaveLength(2);
    expect(line.crs).toBe('CRS84');
  });

  it('should create valid Polygon geometry with closed ring', () => {
    const polygon = Polygon([[
      [-122.5, 37.7],
      [-122.3, 37.7],
      [-122.3, 37.9],
      [-122.5, 37.9],
      [-122.5, 37.7]
    ]]);

    expect(polygon.type).toBe('Polygon');
    expect(polygon.coordinates).toHaveLength(1);
    expect(polygon.coordinates[0]).toHaveLength(5);
    expect(polygon.crs).toBe('CRS84');
  });

  it('should reject invalid Point coordinates', () => {
    expect(() => Point([200, 37.7749])).toThrow(); // Invalid longitude
    expect(() => Point([-122.4194, 95])).toThrow(); // Invalid latitude
  });

  it('should reject LineString with less than 2 points', () => {
    expect(() => LineString([[-122.4194, 37.7749]])).toThrow();
  });

  it('should reject Polygon with unclosed ring', () => {
    expect(() => Polygon([[
      [-122.5, 37.7],
      [-122.3, 37.7],
      [-122.3, 37.9]
    ]])).toThrow();
  });

  it('should calculate bounding box for Point', () => {
    const point = Point([-122.4194, 37.7749]);
    const bbox = getBBox(point);

    expect(bbox).toEqual([-122.4194, 37.7749, -122.4194, 37.7749]);
  });

  it('should calculate bounding box for Polygon', () => {
    const polygon = Polygon([[
      [-122.5, 37.7],
      [-122.3, 37.7],
      [-122.3, 37.9],
      [-122.5, 37.9],
      [-122.5, 37.7]
    ]]);
    const bbox = getBBox(polygon);

    expect(bbox).toEqual([-122.5, 37.7, -122.3, 37.9]);
  });
});

describe('GeoSPARQL - WKT Conversion', () => {
  it('should convert Point to WKT', () => {
    const point = Point([-122.4194, 37.7749]);
    const wkt = toWKT(point);

    expect(wkt).toBe('POINT(-122.4194 37.7749)');
  });

  it('should convert LineString to WKT', () => {
    const line = LineString([
      [-122.4194, 37.7749],
      [-122.4089, 37.7849]
    ]);
    const wkt = toWKT(line);

    expect(wkt).toBe('LINESTRING(-122.4194 37.7749, -122.4089 37.7849)');
  });

  it('should convert Polygon to WKT', () => {
    const polygon = Polygon([[
      [-122.5, 37.7],
      [-122.3, 37.7],
      [-122.3, 37.9],
      [-122.5, 37.9],
      [-122.5, 37.7]
    ]]);
    const wkt = toWKT(polygon);

    expect(wkt).toContain('POLYGON');
    expect(wkt).toContain('-122.5 37.7');
  });

  it('should parse WKT to Point', () => {
    const wkt = 'POINT(-122.4194 37.7749)';
    const point = fromWKT(wkt);

    expect(point.type).toBe('Point');
    expect(point.coordinates).toEqual([-122.4194, 37.7749]);
  });

  it('should parse WKT to LineString', () => {
    const wkt = 'LINESTRING(-122.4194 37.7749, -122.4089 37.7849)';
    const line = fromWKT(wkt);

    expect(line.type).toBe('LineString');
    expect(line.coordinates).toHaveLength(2);
  });

  it('should roundtrip Point through WKT', () => {
    const original = Point([-122.4194, 37.7749]);
    const wkt = toWKT(original);
    const parsed = fromWKT(wkt);

    expect(parsed.coordinates).toEqual(original.coordinates);
  });
});

describe('GeoSPARQL - Distance Calculations', () => {
  it('should calculate haversine distance between two points', () => {
    const sf = [-122.4194, 37.7749]; // San Francisco
    const la = [-118.2437, 34.0522]; // Los Angeles

    const dist = haversineDistance(sf, la);

    // Expected: ~559km (559000 meters)
    expect(dist).toBeGreaterThan(550000);
    expect(dist).toBeLessThan(570000);
  });

  it('should calculate distance with Turf.js accuracy', () => {
    const point1 = Point([-122.4194, 37.7749]);
    const point2 = Point([-118.2437, 34.0522]);

    const dist = distance(point1, point2);

    expect(dist).toBeGreaterThan(550000);
    expect(dist).toBeLessThan(570000);
  });

  it('should convert distance between units', () => {
    const meters = 1000;
    const km = convertDistance(meters, 'meters', 'kilometers');
    const miles = convertDistance(meters, 'meters', 'miles');

    expect(km).toBe(1);
    expect(miles).toBeCloseTo(0.621371, 2);
  });

  it('should check if point is within distance', () => {
    const point1 = Point([-122.4194, 37.7749]);
    const point2 = Point([-122.4189, 37.7750]); // ~50 meters away

    const isWithin = withinDistance(point2, point1, 100, 'meters');
    const isNotWithin = withinDistance(point2, point1, 10, 'meters');

    expect(isWithin).toBe(true);
    expect(isNotWithin).toBe(false);
  });

  it('should calculate bearing between two points', () => {
    const sf = [-122.4194, 37.7749];
    const la = [-118.2437, 34.0522];

    const bear = bearing(sf, la);

    // Expected: ~130 degrees (southeast)
    expect(bear).toBeGreaterThan(120);
    expect(bear).toBeLessThan(140);
  });

  it('should meet performance target for distance calculation', () => {
    const point1 = Point([-122.4194, 37.7749]);
    const point2 = Point([-118.2437, 34.0522]);

    const start = performance.now();
    distance(point1, point2);
    const elapsed = performance.now() - start;

    expect(elapsed).toBeLessThan(0.5); // <0.5ms target
  });
});

describe('GeoSPARQL - Spatial Relations', () => {
  it('should test point within polygon', () => {
    const point = Point([-122.4, 37.8]);
    const polygon = Polygon([[
      [-122.5, 37.7],
      [-122.3, 37.7],
      [-122.3, 37.9],
      [-122.5, 37.9],
      [-122.5, 37.7]
    ]]);

    expect(within(point, polygon)).toBe(true);
  });

  it('should test point outside polygon', () => {
    const point = Point([-122.2, 37.8]);
    const polygon = Polygon([[
      [-122.5, 37.7],
      [-122.3, 37.7],
      [-122.3, 37.9],
      [-122.5, 37.9],
      [-122.5, 37.7]
    ]]);

    expect(within(point, polygon)).toBe(false);
  });

  it('should test polygon contains point', () => {
    const polygon = Polygon([[
      [-122.5, 37.7],
      [-122.3, 37.7],
      [-122.3, 37.9],
      [-122.5, 37.9],
      [-122.5, 37.7]
    ]]);
    const point = Point([-122.4, 37.8]);

    expect(contains(polygon, point)).toBe(true);
  });

  it('should test geometries intersect', () => {
    const line1 = LineString([
      [-122.5, 37.8],
      [-122.3, 37.8]
    ]);
    const line2 = LineString([
      [-122.4, 37.7],
      [-122.4, 37.9]
    ]);

    expect(intersects(line1, line2)).toBe(true);
  });

  it('should test geometries are disjoint', () => {
    const point1 = Point([-122.5, 37.7]);
    const point2 = Point([-118.2, 34.0]);

    expect(disjoint(point1, point2)).toBe(true);
  });

  it('should evaluate spatial relation by name', () => {
    const point = Point([-122.4, 37.8]);
    const polygon = Polygon([[
      [-122.5, 37.7],
      [-122.3, 37.7],
      [-122.3, 37.9],
      [-122.5, 37.9],
      [-122.5, 37.7]
    ]]);

    expect(evaluate(point, polygon, 'within')).toBe(true);
    expect(evaluate(polygon, point, 'contains')).toBe(true);
  });

  it('should filter geometries by spatial relation', () => {
    const geometries = [
      Point([-122.4, 37.8]),
      Point([-122.35, 37.85]),
      Point([-122.2, 38.0])
    ];
    const polygon = Polygon([[
      [-122.5, 37.7],
      [-122.3, 37.7],
      [-122.3, 37.9],
      [-122.5, 37.9],
      [-122.5, 37.7]
    ]]);

    const filtered = filter(geometries, polygon, 'within');

    expect(filtered).toHaveLength(2);
  });

  it('should meet performance target for spatial relation check', () => {
    const point = Point([-122.4, 37.8]);
    const polygon = Polygon([[
      [-122.5, 37.7],
      [-122.3, 37.7],
      [-122.3, 37.9],
      [-122.5, 37.9],
      [-122.5, 37.7]
    ]]);

    const start = performance.now();
    within(point, polygon);
    const elapsed = performance.now() - start;

    expect(elapsed).toBeLessThan(1); // <1ms target
  });
});

describe('GeoSPARQL - R-tree Spatial Index', () => {
  let index;

  beforeEach(() => {
    index = createSpatialIndex();
  });

  it('should insert and retrieve geometries', () => {
    const point = Point([-122.4194, 37.7749]);
    const id = index.insert(point, { name: 'San Francisco' });

    expect(index.has(id)).toBe(true);
    expect(index.size()).toBe(1);

    const item = index.get(id);
    expect(item.data.name).toBe('San Francisco');
  });

  it('should search by bounding box', () => {
    index.insert(Point([-122.4, 37.8]), { name: 'Point A' });
    index.insert(Point([-122.35, 37.85]), { name: 'Point B' });
    index.insert(Point([-118.2, 34.0]), { name: 'Point C' });

    const bbox = [-122.5, 37.7, -122.3, 37.9];
    const results = index.search(bbox);

    expect(results).toHaveLength(2);
  });

  it('should search by geometry', () => {
    index.insert(Point([-122.4, 37.8]), { name: 'Point A' });
    index.insert(Point([-122.35, 37.85]), { name: 'Point B' });
    index.insert(Point([-118.2, 34.0]), { name: 'Point C' });

    const polygon = Polygon([[
      [-122.5, 37.7],
      [-122.3, 37.7],
      [-122.3, 37.9],
      [-122.5, 37.9],
      [-122.5, 37.7]
    ]]);

    const results = index.searchGeometry(polygon);

    expect(results).toHaveLength(2);
  });

  it('should remove geometries', () => {
    const point = Point([-122.4194, 37.7749]);
    const id = index.insert(point);

    expect(index.size()).toBe(1);
    index.remove(id);
    expect(index.size()).toBe(0);
  });

  it('should bulk load geometries efficiently', () => {
    const entries = Array.from({ length: 1000 }, (_, i) => ({
      geometry: Point([-122.4 + i * 0.01, 37.7 + i * 0.01]),
      data: { id: i }
    }));

    const start = performance.now();
    index.bulkLoad(entries);
    const elapsed = performance.now() - start;

    expect(index.size()).toBe(1000);
    expect(elapsed).toBeLessThan(100); // <100ms for 1000 items
  });

  it('should find k nearest neighbors', () => {
    index.insert(Point([-122.4, 37.8]), { name: 'A' });
    index.insert(Point([-122.35, 37.85]), { name: 'B' });
    index.insert(Point([-122.3, 37.9]), { name: 'C' });

    const target = Point([-122.38, 37.82]);
    const nearest = kNearest(index, target, 2);

    expect(nearest).toHaveLength(2);
    expect(nearest[0].distance).toBeLessThan(nearest[1].distance);
  });

  it('should meet performance target for R-tree query with 10K geometries', () => {
    const entries = Array.from({ length: 10000 }, (_, i) => ({
      geometry: Point([-122.4 + (i % 100) * 0.01, 37.7 + Math.floor(i / 100) * 0.01]),
      data: { id: i }
    }));

    index.bulkLoad(entries);

    const bbox = [-122.5, 37.7, -122.3, 37.9];

    const start = performance.now();
    const results = index.search(bbox);
    const elapsed = performance.now() - start;

    expect(results.length).toBeGreaterThan(0);
    expect(elapsed).toBeLessThan(10); // <10ms target
  });
});

describe('GeoSPARQL - CRS Support', () => {
  it('should validate CRS identifiers', () => {
    expect(validateCRS('CRS84')).toBe('CRS84');
    expect(validateCRS('WGS84')).toBe('WGS84');
    expect(validateCRS('EPSG:4326')).toBe('EPSG:4326');
  });

  it('should reject invalid CRS', () => {
    expect(() => validateCRS('INVALID')).toThrow();
  });

  it('should get CRS URI', () => {
    const uri = getCRSUri('CRS84');

    expect(uri).toBe('http://www.opengis.net/def/crs/OGC/1.3/CRS84');
  });

  it('should create geometry with custom CRS', () => {
    const point = Point([-122.4194, 37.7749], 'WGS84');

    expect(point.crs).toBe('WGS84');
  });
});

describe('GeoSPARQL - Query Functions', () => {
  it('should execute geof:distance function', () => {
    const point1 = Point([-122.4194, 37.7749]);
    const point2 = Point([-118.2437, 34.0522]);

    const dist = geof_distance(point1, point2);

    expect(dist).toBeGreaterThan(550000);
    expect(dist).toBeLessThan(570000);
  });

  it('should execute geof:within function', () => {
    const point = Point([-122.4, 37.8]);
    const polygon = Polygon([[
      [-122.5, 37.7],
      [-122.3, 37.7],
      [-122.3, 37.9],
      [-122.5, 37.9],
      [-122.5, 37.7]
    ]]);

    expect(geof_within(point, polygon)).toBe(true);
  });

  it('should execute geof:contains function', () => {
    const polygon = Polygon([[
      [-122.5, 37.7],
      [-122.3, 37.7],
      [-122.3, 37.9],
      [-122.5, 37.9],
      [-122.5, 37.7]
    ]]);
    const point = Point([-122.4, 37.8]);

    expect(geof_contains(polygon, point)).toBe(true);
  });

  it('should execute geof:intersects function', () => {
    const line1 = LineString([
      [-122.5, 37.8],
      [-122.3, 37.8]
    ]);
    const line2 = LineString([
      [-122.4, 37.7],
      [-122.4, 37.9]
    ]);

    expect(geof_intersects(line1, line2)).toBe(true);
  });

  it('should execute geof:buffer function', () => {
    const point = Point([-122.4194, 37.7749]);
    const buffered = geof_buffer(point, 1000); // 1km buffer

    expect(buffered.type).toBe('Polygon');
  });

  it('should execute geof:area function', () => {
    const polygon = Polygon([[
      [-122.5, 37.7],
      [-122.3, 37.7],
      [-122.3, 37.9],
      [-122.5, 37.9],
      [-122.5, 37.7]
    ]]);

    const area = geof_area(polygon);

    expect(area).toBeGreaterThan(0);
  });

  it('should execute geof:length function', () => {
    const line = LineString([
      [-122.4194, 37.7749],
      [-122.4089, 37.7849]
    ]);

    const length = geof_length(line);

    expect(length).toBeGreaterThan(0);
  });
});

describe('GeoSPARQL - Engine', () => {
  let engine;

  beforeEach(() => {
    engine = createGeoSPARQLEngine();
  });

  it('should create engine with spatial indexing', () => {
    const stats = engine.getStats();

    expect(stats.indexEnabled).toBe(true);
    expect(stats.geometryCount).toBe(0);
  });

  it('should add and retrieve geometries', () => {
    const point = Point([-122.4194, 37.7749]);
    const id = engine.addGeometry(point, { name: 'San Francisco' });

    const geom = engine.getGeometry(id);

    expect(geom.data.name).toBe('San Francisco');
    expect(geom.geometry.type).toBe('Point');
  });

  it('should query by bounding box', () => {
    engine.addGeometry(Point([-122.4, 37.8]), { name: 'A' });
    engine.addGeometry(Point([-122.35, 37.85]), { name: 'B' });
    engine.addGeometry(Point([-118.2, 34.0]), { name: 'C' });

    const results = engine.query({
      bbox: [-122.5, 37.7, -122.3, 37.9]
    });

    expect(results).toHaveLength(2);
  });

  it('should query by spatial relation', () => {
    engine.addGeometry(Point([-122.4, 37.8]), { name: 'A' });
    engine.addGeometry(Point([-122.35, 37.85]), { name: 'B' });
    engine.addGeometry(Point([-118.2, 34.0]), { name: 'C' });

    const polygon = Polygon([[
      [-122.5, 37.7],
      [-122.3, 37.7],
      [-122.3, 37.9],
      [-122.5, 37.9],
      [-122.5, 37.7]
    ]]);

    const results = engine.query({
      relation: { geometry: polygon, type: 'within' }
    });

    expect(results).toHaveLength(2);
  });

  it('should query by distance', () => {
    engine.addGeometry(Point([-122.4, 37.8]), { name: 'A' });
    engine.addGeometry(Point([-122.4001, 37.8001]), { name: 'B' });
    engine.addGeometry(Point([-118.2, 34.0]), { name: 'C' });

    const center = Point([-122.4, 37.8]);

    const results = engine.query({
      distance: { from: center, max: 500, unit: 'meters' }
    });

    expect(results.length).toBeGreaterThanOrEqual(1);
    expect(results.length).toBeLessThanOrEqual(2);
  });

  it('should execute GeoSPARQL functions', () => {
    const point1 = Point([-122.4194, 37.7749]);
    const point2 = Point([-118.2437, 34.0522]);

    const dist = engine.executeFunction(
      'http://www.opengis.net/def/function/geosparql/distance',
      point1,
      point2
    );

    expect(dist).toBeGreaterThan(550000);
  });

  it('should add geometry from WKT', () => {
    const id = engine.addWKT('POINT(-122.4194 37.7749)', { name: 'SF' });
    const geom = engine.getGeometry(id);

    expect(geom.geometry.type).toBe('Point');
    expect(geom.data.name).toBe('SF');
  });

  it('should clear all geometries', () => {
    engine.addGeometry(Point([-122.4, 37.8]));
    engine.addGeometry(Point([-122.35, 37.85]));

    expect(engine.getStats().geometryCount).toBe(2);

    engine.clear();

    expect(engine.getStats().geometryCount).toBe(0);
  });

  it('should meet performance target for Point creation', () => {
    const start = performance.now();
    Point([-122.4194, 37.7749]);
    const elapsed = performance.now() - start;

    expect(elapsed).toBeLessThan(0.1); // <0.1ms target
  });
});
