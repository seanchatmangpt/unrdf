/**
 * @fileoverview YAWL Resource Type Definitions
 *
 * Shared type definitions, namespaces, and schemas for YAWL resources.
 * This module has NO dependencies to avoid circular dependency issues.
 *
 * @module @unrdf/yawl/resources/types
 * @version 1.0.0
 */

import { dataFactory } from '@unrdf/oxigraph';
import { z } from 'zod';

const { namedNode } = dataFactory;

/* ========================================================================= */
/* Namespace Definitions                                                     */
/* ========================================================================= */

/**
 * YAWL Resource namespace
 * @constant {string}
 */
export const YAWL_NS = 'http://yawlfoundation.org/yawlschema#';

/**
 * FOAF namespace for person properties
 * @constant {string}
 */
export const FOAF_NS = 'http://xmlns.com/foaf/0.1/';

/**
 * RDF namespace
 * @constant {string}
 */
export const RDF_NS = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';

/**
 * XSD namespace for datatypes
 * @constant {string}
 */
export const XSD_NS = 'http://www.w3.org/2001/XMLSchema#';

/**
 * TIME namespace for temporal properties
 * @constant {string}
 */
export const TIME_NS = 'http://www.w3.org/2006/time#';

/**
 * Namespace helper functions
 * @public
 */
export const yawl = (localName) => namedNode(`${YAWL_NS}${localName}`);
export const foaf = (localName) => namedNode(`${FOAF_NS}${localName}`);
export const rdf = (localName) => namedNode(`${RDF_NS}${localName}`);
export const xsd = (localName) => `${XSD_NS}${localName}`;
export const time = (localName) => namedNode(`${TIME_NS}${localName}`);

/* ========================================================================= */
/* Resource Type Enum                                                        */
/* ========================================================================= */

/**
 * Resource type enum
 * @constant
 */
export const ResourceType = /** @type {const} */ ({
  PARTICIPANT: 'Participant',
  TOOL: 'Tool',
  ROLE: 'Role',
});

/* ========================================================================= */
/* Zod Schemas for Validation                                                */
/* ========================================================================= */

/**
 * Resource definition schema
 */
export const ResourceSchema = z.object({
  id: z.string().min(1),
  type: z.enum([ResourceType.PARTICIPANT, ResourceType.TOOL, ResourceType.ROLE]),
  name: z.string().optional(),
  capacity: z.number().int().min(-1).default(1), // -1 = unlimited
  sparql: z.string().optional(),
  metadata: z.record(z.string(), z.unknown()).optional(),
});

/**
 * Work item schema for allocation
 */
export const WorkItemSchema = z.object({
  id: z.string().min(1),
  taskId: z.string().min(1),
  caseId: z.string().min(1),
  status: z.string().optional(),
  createdAt: z.string().datetime().optional(),
  data: z.record(z.string(), z.unknown()).optional(),
});

/**
 * Policy pack resource rules schema
 */
export const PolicyPackSchema = z.object({
  id: z.string().min(1),
  name: z.string().optional(),
  version: z.string().optional(),
  resources: z.array(ResourceSchema),
  priority: z.number().int().min(0).default(0),
  enabled: z.boolean().default(true),
});

/**
 * Time window schema for availability
 */
export const TimeWindowSchema = z.object({
  start: z.string().datetime(),
  end: z.string().datetime(),
  available: z.boolean().default(true),
});

/**
 * Allocation receipt schema
 */
export const AllocationReceiptSchema = z.object({
  id: z.string(),
  workItemId: z.string(),
  resourceId: z.string(),
  resourceType: z.enum([ResourceType.PARTICIPANT, ResourceType.TOOL, ResourceType.ROLE]),
  allocatedAt: z.string().datetime(),
  expiresAt: z.string().datetime().optional(),
  proof: z.object({
    capacityCheck: z.boolean(),
    eligibilityCheck: z.boolean(),
    policyPackId: z.string().optional(),
    sparqlResult: z.boolean().optional(),
  }),
});

/**
 * @typedef {z.infer<typeof ResourceSchema>} Resource
 * @typedef {z.infer<typeof WorkItemSchema>} WorkItem
 * @typedef {z.infer<typeof PolicyPackSchema>} PolicyPack
 * @typedef {z.infer<typeof TimeWindowSchema>} TimeWindow
 * @typedef {z.infer<typeof AllocationReceiptSchema>} AllocationReceipt
 */
