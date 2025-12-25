/**
 * @fileoverview YAWL Resource Calendar - Implementation Functions
 *
 * Implementation of calendar methods for ResourceManager
 *
 * @module @unrdf/yawl/resources/calendar-impl
 * @version 1.0.0
 */

import { dataFactory } from '@unrdf/oxigraph';
import {
  YAWL_NS,
  FOAF_NS,
  RDF_NS,
  XSD_NS,
  TIME_NS,
  yawl,
  foaf,
  xsd,
  TimeWindowSchema,
} from './yawl-resources-core.mjs';

const { namedNode, literal, quad, blankNode, defaultGraph } = dataFactory;

/**
 * Get availability for a resource from store
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {string} resourceId - Resource identifier
 * @param {Object} [options] - Query options
 * @param {string} [options.from] - Start of time range (ISO datetime)
 * @param {string} [options.to] - End of time range (ISO datetime)
 * @returns {{ available: boolean, windows: Array<{start: string, end: string, available: boolean}> }}
 */
export function getResourceAvailability(store, resourceId, options = {}) {
  const resourceNode = namedNode(`${YAWL_NS}resource/${resourceId}`);
  const now = new Date();

  // Query for availability windows using blank node pattern
  const query = `
    PREFIX yawl: <${YAWL_NS}>
    PREFIX foaf: <${FOAF_NS}>

    SELECT ?available ?windowAvailable ?startTime ?endTime WHERE {
      <${resourceNode.value}> foaf:available ?available .
      OPTIONAL {
        <${resourceNode.value}> yawl:hasAvailabilityWindow ?window .
        ?window yawl:scheduleStart ?startTime ;
                yawl:scheduleEnd ?endTime ;
                foaf:available ?windowAvailable .
      }
    }
  `;

  try {
    const results = store.query(query);
    const bindings = Array.from(results);

    if (bindings.length === 0) {
      return {
        available: true,
        windows: [{
          start: (options.from || now.toISOString()),
          end: (options.to || new Date(now.getTime() + 86400000).toISOString()),
          available: true,
        }],
      };
    }

    const windows = [];
    let isCurrentlyAvailable = true;

    for (const binding of bindings) {
      const available = binding.get('available')?.value;
      const windowAvailable = binding.get('windowAvailable')?.value;
      const startTime = binding.get('startTime')?.value;
      const endTime = binding.get('endTime')?.value;

      // Check overall availability
      if (available === 'false' || available === '0') {
        isCurrentlyAvailable = false;
      }

      // Collect time windows if present
      if (startTime && endTime) {
        windows.push({
          start: startTime,
          end: endTime,
          available: windowAvailable !== 'false' && windowAvailable !== '0',
        });
      }
    }

    // If no windows found, use overall availability
    if (windows.length === 0) {
      return {
        available: isCurrentlyAvailable,
        windows: [{
          start: options.from || now.toISOString(),
          end: options.to || new Date(now.getTime() + 86400000).toISOString(),
          available: isCurrentlyAvailable,
        }],
      };
    }

    // Filter windows by requested time range
    const filteredWindows = windows.filter(w => {
      if (options.from && new Date(w.end) < new Date(options.from)) return false;
      if (options.to && new Date(w.start) > new Date(options.to)) return false;
      return true;
    });

    return {
      available: isCurrentlyAvailable && (filteredWindows.length === 0 || filteredWindows.some(w => w.available)),
      windows: filteredWindows.length > 0 ? filteredWindows : [{
        start: options.from || now.toISOString(),
        end: options.to || new Date(now.getTime() + 86400000).toISOString(),
        available: isCurrentlyAvailable,
      }],
    };
  } catch {
    return {
      available: true,
      windows: [{
        start: options.from || now.toISOString(),
        end: options.to || new Date(now.getTime() + 86400000).toISOString(),
        available: true,
      }],
    };
  }
}

/**
 * Set availability for a resource in store
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store
 * @param {string} resourceId - Resource identifier
 * @param {boolean} available - Availability status
 * @param {Array<{start: string, end: string, available: boolean}>} [windows] - Time windows
 * @returns {void}
 */
export function setResourceAvailability(store, resourceId, available, windows = []) {
  const resourceNode = namedNode(`${YAWL_NS}resource/${resourceId}`);

  // Set overall availability
  store.add(quad(
    resourceNode,
    foaf('available'),
    literal(String(available), namedNode(xsd('boolean'))),
    defaultGraph()
  ));

  // Store time windows
  for (const window of windows) {
    const validated = TimeWindowSchema.parse(window);
    const windowNode = blankNode();

    store.add(quad(resourceNode, yawl('hasAvailabilityWindow'), windowNode, defaultGraph()));

    store.add(quad(
      windowNode,
      yawl('scheduleStart'),
      literal(validated.start, namedNode(xsd('dateTime'))),
      defaultGraph()
    ));

    store.add(quad(
      windowNode,
      yawl('scheduleEnd'),
      literal(validated.end, namedNode(xsd('dateTime'))),
      defaultGraph()
    ));

    store.add(quad(
      windowNode,
      foaf('available'),
      literal(String(validated.available), namedNode(xsd('boolean'))),
      defaultGraph()
    ));
  }
}
