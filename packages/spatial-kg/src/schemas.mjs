/**
 * @file Zod schemas for Spatial Knowledge Graph
 * @module @unrdf/spatial-kg/schemas
 */

import { z } from 'zod';

/**
 * Vector3 schema
 */
export const Vector3Schema = z.object({
  x: z.number().finite(),
  y: z.number().finite(),
  z: z.number().finite(),
});

/**
 * Node3D schema
 */
export const Node3DSchema = z.object({
  id: z.string().min(1),
  uri: z.string().optional(),
  label: z.string().optional(),
  position: Vector3Schema,
  velocity: Vector3Schema.optional(),
  mass: z.number().positive().default(1),
  fixed: z.boolean().default(false),
  metadata: z.record(z.unknown()).optional(),
});

/**
 * Edge3D schema
 */
export const Edge3DSchema = z.object({
  id: z.string().min(1),
  source: z.string().min(1),
  target: z.string().min(1),
  predicate: z.string().optional(),
  strength: z.number().min(0).max(1).default(1),
  metadata: z.record(z.unknown()).optional(),
});

/**
 * Spatial graph configuration
 */
export const SpatialGraphConfigSchema = z.object({
  layout: z.object({
    iterations: z.number().int().positive().default(100),
    cooldown: z.number().min(0).max(1).default(0.95),
    repulsionStrength: z.number().positive().default(100),
    attractionStrength: z.number().positive().default(0.1),
    gravityStrength: z.number().nonnegative().default(0.01),
    centerForce: z.boolean().default(true),
    dimensions: z.literal(3).default(3),
  }).default({}),
  rendering: z.object({
    targetFPS: z.number().int().positive().default(60),
    enableVR: z.boolean().default(false),
    enableAR: z.boolean().default(false),
    nodeSize: z.number().positive().default(1),
    edgeWidth: z.number().positive().default(0.1),
    backgroundColor: z.number().int().default(0x000000),
  }).default({}),
  lod: z.object({
    enabled: z.boolean().default(true),
    levels: z.array(z.object({
      distance: z.number().positive(),
      complexity: z.enum(['high', 'medium', 'low']),
    })).default([
      { distance: 10, complexity: 'high' },
      { distance: 50, complexity: 'medium' },
      { distance: 100, complexity: 'low' },
    ]),
  }).default({}),
  collaboration: z.object({
    enabled: z.boolean().default(false),
    serverUrl: z.string().url().optional(),
    username: z.string().optional(),
  }).default({}),
});

/**
 * Spatial query options
 */
export const SpatialQueryOptionsSchema = z.object({
  type: z.enum(['proximity', 'ray', 'knn', 'box', 'sphere']),
  origin: Vector3Schema.optional(),
  direction: Vector3Schema.optional(),
  radius: z.number().positive().optional(),
  k: z.number().int().positive().optional(),
  bounds: z.object({
    min: Vector3Schema,
    max: Vector3Schema,
  }).optional(),
});

/**
 * Gesture event schema
 */
export const GestureEventSchema = z.object({
  type: z.enum(['point', 'grab', 'teleport', 'select', 'pinch']),
  controller: z.enum(['left', 'right', 'hand-left', 'hand-right']),
  position: Vector3Schema.optional(),
  target: z.string().optional(),
  timestamp: z.number().int().positive(),
  intensity: z.number().min(0).max(1).optional(),
});

/**
 * Collaboration state schema
 */
export const CollaborationStateSchema = z.object({
  userId: z.string().min(1),
  position: Vector3Schema,
  rotation: z.object({
    x: z.number().finite(),
    y: z.number().finite(),
    z: z.number().finite(),
    w: z.number().finite(),
  }),
  selectedNode: z.string().optional(),
  timestamp: z.number().int().positive(),
  metadata: z.record(z.unknown()).optional(),
});

/**
 * LOD level schema
 */
export const LODLevelSchema = z.object({
  nodeId: z.string().min(1),
  currentLevel: z.number().int().min(0).max(2),
  distance: z.number().nonnegative(),
  shouldUpdate: z.boolean(),
});

/**
 * Performance metrics schema
 */
export const PerformanceMetricsSchema = z.object({
  fps: z.number().nonnegative(),
  frameTime: z.number().nonnegative(),
  nodeCount: z.number().int().nonnegative(),
  edgeCount: z.number().int().nonnegative(),
  renderTime: z.number().nonnegative(),
  layoutTime: z.number().nonnegative(),
  timestamp: z.number().int().positive(),
});
