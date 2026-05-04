/**
 * OWL-Time Temporal Index
 * Monotonically increasing axis for state tracking.
 */

export class TemporalIndex {
  constructor() {
    this.timeline = []; // sorted by time
  }

  /**
   * Insert a state change event
   */
  insert(subjectIri, timestamp, receiptHash) {
    this.timeline.push({ subjectIri, timestamp, receiptHash });
    // In production this would be an ordered tree insertion
    this.timeline.sort((a, b) => a.timestamp - b.timestamp);
  }

  /**
   * Find states active at a specific time
   */
  queryTime(targetTimestamp) {
    // Basic implementation: find latest state before or at target time
    const results = new Map();
    for (const entry of this.timeline) {
      if (entry.timestamp <= targetTimestamp) {
        results.set(entry.subjectIri, entry.receiptHash);
      }
    }
    return Array.from(results.entries());
  }
}