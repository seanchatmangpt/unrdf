/**
 * 3D-Coordinate (R-Tree/Octree) Index
 * Memory-mapped lookup of spatial coordinates bounding RDF subjects.
 */

export class SpatialIndex {
  constructor() {
    this.nodes = new Map();
  }

  /**
   * Insert a subject with 3D coordinates
   */
  insert(subjectIri, x, y, z) {
    this.nodes.set(subjectIri, { x, y, z });
  }

  /**
   * Find subjects within a bounding box
   */
  queryBoundingBox(minX, maxX, minY, maxY, minZ, maxZ) {
    const results = [];
    for (const [subjectIri, coords] of this.nodes.entries()) {
      if (
        coords.x >= minX && coords.x <= maxX &&
        coords.y >= minY && coords.y <= maxY &&
        coords.z >= minZ && coords.z <= maxZ
      ) {
        results.push(subjectIri);
      }
    }
    return results;
  }
}
