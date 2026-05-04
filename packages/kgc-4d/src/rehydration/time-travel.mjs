/**
 * Graph Time-Travel (Manufacturing Closure)
 * Rehydrates graph state from PROV-O receipts.
 */

export class TimeTravelEngine {
  constructor(eventStore) {
    this.eventStore = eventStore; // the ledger of PROV-O events
  }

  /**
   * Rehydrate graph to an exact state at time T
   */
  async rehydrateToTime(targetTimestamp) {
    // Load baseline snapshot if available
    const graphState = new Map();

    // Iterate forward applying deltas up to T
    const events = await this.eventStore.queryEventsUpTo(targetTimestamp);
    
    for (const event of events) {
      this._applyDelta(graphState, event.deltas);
    }

    return graphState;
  }

  _applyDelta(graphState, deltas) {
    for (const delta of deltas) {
      if (delta.type === 'ADD') {
        graphState.set(delta.id, delta.data);
      } else if (delta.type === 'DELETE') {
        graphState.delete(delta.id);
      }
    }
  }
}
