/**
 * @fileoverview YAWL Resource Calendar - Availability and Scheduling
 *
 * Calendar and availability utilities:
 * - Time window calculations
 * - Availability queries
 * - SPARQL helpers for calendar operations
 * - Schedule management utilities
 *
 * @module @unrdf/yawl/resources/calendar
 * @version 1.0.0
 */

import {
  YAWL_NS,
  FOAF_NS,
  RDF_NS,
  XSD_NS,
  TIME_NS,
} from './yawl-resources-core.mjs';

/* ========================================================================= */
/* SPARQL Query Helpers                                                      */
/* ========================================================================= */

/**
 * Common SPARQL prefixes for resource queries
 * @constant {string}
 */
export const RESOURCE_SPARQL_PREFIXES = `
PREFIX yawl: <${YAWL_NS}>
PREFIX foaf: <${FOAF_NS}>
PREFIX rdf: <${RDF_NS}>
PREFIX xsd: <${XSD_NS}>
PREFIX time: <${TIME_NS}>
`;

/**
 * Create an eligibility SPARQL ASK query for role membership
 *
 * @param {string} roleUri - Role URI to check membership
 * @returns {string} SPARQL ASK query
 *
 * @example
 * const sparql = createRoleMembershipQuery('http://example.org/roles/approvers');
 */
export function createRoleMembershipQuery(roleUri) {
  return `
    ${RESOURCE_SPARQL_PREFIXES}
    ASK {
      ?person foaf:hasRole <${roleUri}> ;
              foaf:available true .
    }
  `;
}

/**
 * Create an eligibility SPARQL ASK query for capability check
 *
 * @param {string} capabilityUri - Required capability URI
 * @returns {string} SPARQL ASK query
 *
 * @example
 * const sparql = createCapabilityQuery('http://example.org/capabilities/sign-documents');
 */
export function createCapabilityQuery(capabilityUri) {
  return `
    ${RESOURCE_SPARQL_PREFIXES}
    ASK {
      ?resource yawl:hasCapability <${capabilityUri}> .
    }
  `;
}

/**
 * Create SPARQL query to find available resources in time window
 *
 * @param {string} startTime - ISO datetime start
 * @param {string} endTime - ISO datetime end
 * @param {string} [resourceType] - Optional resource type filter
 * @returns {string} SPARQL SELECT query
 *
 * @example
 * const sparql = createAvailabilityQuery(
 *   '2024-01-15T09:00:00Z',
 *   '2024-01-15T17:00:00Z',
 *   'Participant'
 * );
 */
export function createAvailabilityQuery(startTime, endTime, resourceType) {
  const typeFilter = resourceType ? `?resource rdf:type yawl:${resourceType} .` : '';

  return `
    ${RESOURCE_SPARQL_PREFIXES}
    SELECT ?resource ?name WHERE {
      ?resource foaf:available true .
      ${typeFilter}
      OPTIONAL { ?resource foaf:name ?name }
      OPTIONAL {
        ?resource yawl:hasAvailabilityWindow ?window .
        ?window yawl:scheduleStart ?windowStart ;
                yawl:scheduleEnd ?windowEnd ;
                foaf:available ?windowAvailable .
        FILTER(?windowStart <= "${startTime}"^^xsd:dateTime)
        FILTER(?windowEnd >= "${endTime}"^^xsd:dateTime)
        FILTER(?windowAvailable = true)
      }
    }
  `;
}

/**
 * Create SPARQL query to check resource conflicts in time window
 *
 * @param {string} resourceId - Resource identifier
 * @param {string} startTime - ISO datetime start
 * @param {string} endTime - ISO datetime end
 * @returns {string} SPARQL ASK query
 *
 * @example
 * const sparql = createConflictQuery('alice', '2024-01-15T09:00:00Z', '2024-01-15T10:00:00Z');
 */
export function createConflictQuery(resourceId, startTime, endTime) {
  return `
    ${RESOURCE_SPARQL_PREFIXES}
    ASK {
      ?allocation rdf:type yawl:Allocation ;
                  yawl:resource <${YAWL_NS}resource/${resourceId}> ;
                  yawl:allocatedAt ?allocStart .
      OPTIONAL { ?allocation yawl:expiresAt ?allocEnd }
      FILTER NOT EXISTS { ?allocation yawl:status "deallocated" }
      FILTER(
        (?allocStart <= "${endTime}"^^xsd:dateTime) &&
        (COALESCE(?allocEnd, "${endTime}"^^xsd:dateTime) >= "${startTime}"^^xsd:dateTime)
      )
    }
  `;
}

/* ========================================================================= */
/* Time Window Utilities                                                     */
/* ========================================================================= */

/**
 * Check if two time windows overlap
 *
 * @param {Object} window1 - First time window
 * @param {string} window1.start - ISO datetime start
 * @param {string} window1.end - ISO datetime end
 * @param {Object} window2 - Second time window
 * @param {string} window2.start - ISO datetime start
 * @param {string} window2.end - ISO datetime end
 * @returns {boolean} True if windows overlap
 *
 * @example
 * const overlaps = windowsOverlap(
 *   { start: '2024-01-15T09:00:00Z', end: '2024-01-15T10:00:00Z' },
 *   { start: '2024-01-15T09:30:00Z', end: '2024-01-15T10:30:00Z' }
 * );
 */
export function windowsOverlap(window1, window2) {
  const start1 = new Date(window1.start);
  const end1 = new Date(window1.end);
  const start2 = new Date(window2.start);
  const end2 = new Date(window2.end);

  return start1 < end2 && start2 < end1;
}

/**
 * Merge overlapping time windows
 *
 * @param {Array<{start: string, end: string, available: boolean}>} windows - Time windows to merge
 * @returns {Array<{start: string, end: string, available: boolean}>} Merged windows
 *
 * @example
 * const merged = mergeTimeWindows([
 *   { start: '2024-01-15T09:00:00Z', end: '2024-01-15T10:00:00Z', available: true },
 *   { start: '2024-01-15T09:30:00Z', end: '2024-01-15T11:00:00Z', available: true }
 * ]);
 */
export function mergeTimeWindows(windows) {
  if (windows.length === 0) return [];

  const sorted = windows
    .slice()
    .sort((a, b) => new Date(a.start).getTime() - new Date(b.start).getTime());

  const merged = [sorted[0]];

  for (let i = 1; i < sorted.length; i++) {
    const current = sorted[i];
    const last = merged[merged.length - 1];

    if (
      current.available === last.available &&
      new Date(current.start) <= new Date(last.end)
    ) {
      last.end = new Date(Math.max(
        new Date(last.end).getTime(),
        new Date(current.end).getTime()
      )).toISOString();
    } else {
      merged.push(current);
    }
  }

  return merged;
}

/**
 * Calculate available time slots within a time range
 *
 * @param {Array<{start: string, end: string, available: boolean}>} windows - Availability windows
 * @param {string} rangeStart - Range start ISO datetime
 * @param {string} rangeEnd - Range end ISO datetime
 * @returns {Array<{start: string, end: string}>} Available time slots
 *
 * @example
 * const slots = calculateAvailableSlots(
 *   [{ start: '2024-01-15T09:00:00Z', end: '2024-01-15T17:00:00Z', available: true }],
 *   '2024-01-15T08:00:00Z',
 *   '2024-01-15T18:00:00Z'
 * );
 */
export function calculateAvailableSlots(windows, rangeStart, rangeEnd) {
  const availableWindows = windows.filter(w => w.available);
  const slots = [];

  for (const window of availableWindows) {
    const start = new Date(Math.max(
      new Date(window.start).getTime(),
      new Date(rangeStart).getTime()
    ));
    const end = new Date(Math.min(
      new Date(window.end).getTime(),
      new Date(rangeEnd).getTime()
    ));

    if (start < end) {
      slots.push({
        start: start.toISOString(),
        end: end.toISOString(),
      });
    }
  }

  return mergeTimeWindows(slots.map(s => ({ ...s, available: true })))
    .map(({ start, end }) => ({ start, end }));
}

/**
 * Calculate total available duration in milliseconds
 *
 * @param {Array<{start: string, end: string, available: boolean}>} windows - Availability windows
 * @returns {number} Total available time in milliseconds
 *
 * @example
 * const duration = calculateTotalAvailableDuration([
 *   { start: '2024-01-15T09:00:00Z', end: '2024-01-15T12:00:00Z', available: true },
 *   { start: '2024-01-15T13:00:00Z', end: '2024-01-15T17:00:00Z', available: true }
 * ]);
 */
export function calculateTotalAvailableDuration(windows) {
  const availableWindows = windows.filter(w => w.available);

  return availableWindows.reduce((total, window) => {
    const duration = new Date(window.end).getTime() - new Date(window.start).getTime();
    return total + duration;
  }, 0);
}

/**
 * Find next available time slot after a given time
 *
 * @param {Array<{start: string, end: string, available: boolean}>} windows - Availability windows
 * @param {string} afterTime - ISO datetime to search after
 * @param {number} [minDuration=0] - Minimum duration required in milliseconds
 * @returns {{start: string, end: string}|null} Next available slot or null
 *
 * @example
 * const nextSlot = findNextAvailableSlot(
 *   [{ start: '2024-01-15T14:00:00Z', end: '2024-01-15T17:00:00Z', available: true }],
 *   '2024-01-15T12:00:00Z',
 *   3600000
 * );
 */
export function findNextAvailableSlot(windows, afterTime, minDuration = 0) {
  const after = new Date(afterTime);
  const availableWindows = windows
    .filter(w => w.available)
    .sort((a, b) => new Date(a.start).getTime() - new Date(b.start).getTime());

  for (const window of availableWindows) {
    const windowStart = new Date(window.start);
    const windowEnd = new Date(window.end);

    if (windowEnd > after) {
      const slotStart = windowStart > after ? windowStart : after;
      const duration = windowEnd.getTime() - slotStart.getTime();

      if (duration >= minDuration) {
        return {
          start: slotStart.toISOString(),
          end: windowEnd.toISOString(),
        };
      }
    }
  }

  return null;
}

/* ========================================================================= */
/* Schedule Validation                                                       */
/* ========================================================================= */

/**
 * Validate that a time window is well-formed
 *
 * @param {Object} window - Time window to validate
 * @param {string} window.start - ISO datetime start
 * @param {string} window.end - ISO datetime end
 * @returns {{ valid: boolean, error?: string }}
 *
 * @example
 * const validation = validateTimeWindow({
 *   start: '2024-01-15T09:00:00Z',
 *   end: '2024-01-15T08:00:00Z'
 * });
 */
export function validateTimeWindow(window) {
  try {
    const start = new Date(window.start);
    const end = new Date(window.end);

    if (isNaN(start.getTime())) {
      return { valid: false, error: 'Invalid start datetime' };
    }

    if (isNaN(end.getTime())) {
      return { valid: false, error: 'Invalid end datetime' };
    }

    if (start >= end) {
      return { valid: false, error: 'Start time must be before end time' };
    }

    return { valid: true };
  } catch (error) {
    return { valid: false, error: error.message };
  }
}

/**
 * Check if a time is within business hours
 *
 * @param {string} datetime - ISO datetime to check
 * @param {Object} [businessHours] - Business hours configuration
 * @param {number} [businessHours.startHour=9] - Business day start hour (0-23)
 * @param {number} [businessHours.endHour=17] - Business day end hour (0-23)
 * @param {number[]} [businessHours.weekdays=[1,2,3,4,5]] - Working days (1=Mon, 7=Sun)
 * @returns {boolean} True if within business hours
 *
 * @example
 * const isDuringWork = isWithinBusinessHours('2024-01-15T14:00:00Z');
 */
export function isWithinBusinessHours(datetime, businessHours = {}) {
  const {
    startHour = 9,
    endHour = 17,
    weekdays = [1, 2, 3, 4, 5],
  } = businessHours;

  const date = new Date(datetime);
  const hour = date.getUTCHours();
  const dayOfWeek = date.getUTCDay() || 7;

  return hour >= startHour && hour < endHour && weekdays.includes(dayOfWeek);
}

/**
 * Generate working hours windows for a date range
 *
 * @param {string} startDate - Start date ISO string
 * @param {string} endDate - End date ISO string
 * @param {Object} [businessHours] - Business hours configuration
 * @param {number} [businessHours.startHour=9] - Business day start hour
 * @param {number} [businessHours.endHour=17] - Business day end hour
 * @param {number[]} [businessHours.weekdays=[1,2,3,4,5]] - Working days
 * @returns {Array<{start: string, end: string, available: boolean}>} Working hours windows
 *
 * @example
 * const windows = generateWorkingHoursWindows(
 *   '2024-01-15T00:00:00Z',
 *   '2024-01-19T23:59:59Z'
 * );
 */
export function generateWorkingHoursWindows(startDate, endDate, businessHours = {}) {
  const {
    startHour = 9,
    endHour = 17,
    weekdays = [1, 2, 3, 4, 5],
  } = businessHours;

  const windows = [];
  const current = new Date(startDate);
  const end = new Date(endDate);

  while (current <= end) {
    const dayOfWeek = current.getUTCDay() || 7;

    if (weekdays.includes(dayOfWeek)) {
      const dayStart = new Date(current);
      dayStart.setUTCHours(startHour, 0, 0, 0);

      const dayEnd = new Date(current);
      dayEnd.setUTCHours(endHour, 0, 0, 0);

      if (dayStart >= new Date(startDate) && dayEnd <= end) {
        windows.push({
          start: dayStart.toISOString(),
          end: dayEnd.toISOString(),
          available: true,
        });
      }
    }

    current.setUTCDate(current.getUTCDate() + 1);
  }

  return windows;
}
