/**
 * @fileoverview YAWL Resource Allocation Semantics - Main Entry Point
 *
 * Barrel export for YAWL resource management modules:
 * - Core: ResourceManager with allocation/deallocation
 * - Pools: ResourcePool with allocation strategies
 * - Calendar: Availability and scheduling utilities
 *
 * @module @unrdf/yawl/resources
 * @version 1.0.0
 */

/* ========================================================================= */
/* Re-export Core Module                                                     */
/* ========================================================================= */

export {
  // Class
  YawlResourceManager,
  createResourceManager,
  // Schemas
  ResourceSchema,
  WorkItemSchema,
  PolicyPackSchema,
  TimeWindowSchema,
  AllocationReceiptSchema,
  // Constants
  ResourceType,
  YAWL_NS,
  FOAF_NS,
  RDF_NS,
  XSD_NS,
  TIME_NS,
} from './yawl-resources-core.mjs';

/* ========================================================================= */
/* Re-export Pools Module                                                    */
/* ========================================================================= */

export {
  // Class
  ResourcePool,
  // Factories
  createParticipant,
  createTool,
  createRole,
  createPolicyPack,
  // Pool strategies
  selectRoundRobin,
  selectByPriority,
  selectRandom,
  selectWeightedByCapacity,
} from './yawl-resources-pools.mjs';

/* ========================================================================= */
/* Re-export Calendar Module                                                 */
/* ========================================================================= */

export {
  // SPARQL helpers
  RESOURCE_SPARQL_PREFIXES,
  createRoleMembershipQuery,
  createCapabilityQuery,
  createAvailabilityQuery,
  createConflictQuery,
  // Time window utilities
  windowsOverlap,
  mergeTimeWindows,
  calculateAvailableSlots,
  calculateTotalAvailableDuration,
  findNextAvailableSlot,
  // Schedule validation
  validateTimeWindow,
  isWithinBusinessHours,
  generateWorkingHoursWindows,
} from './yawl-resources-calendar.mjs';

/* ========================================================================= */
/* Wire Up Pool and Calendar Methods to ResourceManager                      */
/* ========================================================================= */

import { YawlResourceManager } from './yawl-resources-core.mjs';
import { createPoolForManager } from './yawl-resources-pools.mjs';
import {
  getResourceAvailability,
  setResourceAvailability,
} from './yawl-resources-calendar-impl.mjs';

/**
 * Wire createResourcePool method to use pools module
 */
YawlResourceManager.prototype.createResourcePool = function(config) {
  return createPoolForManager(this, config);
};

/**
 * Wire availability methods to use calendar module
 */
YawlResourceManager.prototype.getAvailability = function(resourceId, options = {}) {
  return getResourceAvailability(this.getStore(), resourceId, options);
};

YawlResourceManager.prototype.setAvailability = function(resourceId, available, windows = []) {
  return setResourceAvailability(this.getStore(), resourceId, available, windows);
};
