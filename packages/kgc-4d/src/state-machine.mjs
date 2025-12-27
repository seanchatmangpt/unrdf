/**
 * Universe State Machine - Poka-Yoke Pattern
 * Enforces valid state transitions: MUTABLE → FROZEN → SEALED
 * Makes illegal states unrepresentable
 */

import { z } from 'zod';

// Zod schema: Universe state is one of 3 valid states
const UniverseStateSchema = z.enum(['MUTABLE', 'FROZEN', 'SEALED']);

/**
 * State machine for universe lifecycle
 * 
 * States:
 * - MUTABLE: Accepts mutations (appendEvent, admit)
 * - FROZEN: Read-only, time-travel enabled, no mutations
 * - SEALED: Terminal state, immutable forever
 * 
 * Transitions:
 * - MUTABLE → FROZEN: freezeUniverse()
 * - FROZEN → SEALED: seal()
 * - No other transitions allowed (poka-yoke: invalid transitions throw)
 * 
 * @example
 * import { UniverseStateMachine } from './state-machine.mjs';
 * const sm = new UniverseStateMachine();
 * sm.guardMutableOperation('appendEvent'); // OK in MUTABLE
 * sm.freeze(); // Transition to FROZEN
 * sm.guardMutableOperation('appendEvent'); // Throws: cannot mutate FROZEN universe
 */
export class UniverseStateMachine {
  constructor(initialState = 'MUTABLE') {
    this._state = UniverseStateSchema.parse(initialState);
  }

  get state() {
    return this._state;
  }

  /**
   * Guard: Can the universe accept new events?
   * @param {string} operationName - Operation name for error message
   * @throws {Error} if state is FROZEN or SEALED
   */
  guardMutableOperation(operationName) {
    if (this._state === 'FROZEN') {
      throw new Error(
        `Cannot ${operationName}: Universe is FROZEN. ` +
        `Use time-travel (reconstructState) to view past, or seal() to finalize.`
      );
    }
    if (this._state === 'SEALED') {
      throw new Error(
        `Cannot ${operationName}: Universe is SEALED (immutable forever). ` +
        `Create a new universe or fork from this snapshot.`
      );
    }
    // MUTABLE: operation allowed
  }

  /**
   * Transition: MUTABLE → FROZEN
   * @throws {Error} if already FROZEN or SEALED
   */
  freeze() {
    if (this._state === 'FROZEN') {
      throw new Error('Cannot freeze: Universe already FROZEN');
    }
    if (this._state === 'SEALED') {
      throw new Error('Cannot freeze: Universe is SEALED (already immutable)');
    }
    this._state = 'FROZEN';
  }

  /**
   * Transition: FROZEN → SEALED
   * @throws {Error} if not FROZEN
   */
  seal() {
    if (this._state === 'MUTABLE') {
      throw new Error('Cannot seal: Must freeze universe first');
    }
    if (this._state === 'SEALED') {
      throw new Error('Cannot seal: Universe already SEALED');
    }
    this._state = 'SEALED';
  }

  /**
   * Check if state is terminal (no further transitions)
   */
  isTerminal() {
    return this._state === 'SEALED';
  }

  /**
   * Serialize state for storage/transmission
   */
  toJSON() {
    return {
      state: this._state,
      isTerminal: this.isTerminal(),
    };
  }

  /**
   * Deserialize state from JSON
   */
  static fromJSON(json) {
    return new UniverseStateMachine(json.state);
  }
}
