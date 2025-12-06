'use client';

/**
 * useCausalCone - Build causal dependency chain for a target event
 *
 * Uses vector clock happened-before relation to find all events
 * that causally precede the target event (the "causal cone").
 *
 * @example
 * const cone = useCausalCone(targetEventId, eventLog);
 * // Returns: [event1, event2, ...] sorted by time
 */

import { useMemo } from 'react';
import { happenedBefore } from '../utils/vector-clock.mjs';

export function useCausalCone(targetEventId, eventLog) {
  return useMemo(() => {
    if (!targetEventId || !eventLog || eventLog.length === 0) {
      return null;
    }

    // Find target event
    const target = eventLog.find((e) => e.id === targetEventId);
    if (!target) {
      console.warn(`[useCausalCone] Target event ${targetEventId} not found`);
      return null;
    }

    // Build causal cone (all events that happened-before target)
    const cone = [];
    const visited = new Set();

    function traverse(event) {
      if (visited.has(event.id)) return;
      visited.add(event.id);
      cone.push(event);

      // Find all predecessors via vector clock happened-before
      const predecessors = eventLog.filter((e) => {
        if (e.id === event.id) return false; // Don't include self
        if (!e.vectorClock || !event.vectorClock) return false;
        return happenedBefore(e.vectorClock, event.vectorClock);
      });

      predecessors.forEach(traverse);
    }

    traverse(target);

    // Sort by timestamp (ascending) for correct replay order
    cone.sort((a, b) => {
      const aTime = BigInt(a.t_ns || '0');
      const bTime = BigInt(b.t_ns || '0');
      if (aTime < bTime) return -1;
      if (aTime > bTime) return 1;
      return 0;
    });

    return {
      target,
      events: cone,
      count: cone.length,
      earliestTime: cone.length > 0 ? cone[0].t_ns : null,
      latestTime: cone.length > 0 ? cone[cone.length - 1].t_ns : null,
    };
  }, [targetEventId, eventLog]);
}
