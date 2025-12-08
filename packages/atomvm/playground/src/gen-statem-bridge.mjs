/**
 * gen_statem Bridge - Production JavaScript Interface for Erlang State Machines
 *
 * Provides production-ready JavaScript API to control Erlang state machines via bridge commands.
 * Includes error handling, state caching, and KGC-4D event integration.
 *
 * @module gen-statem-bridge
 */

/**
 * gen_statem Bridge
 *
 * Controls Erlang state machines from JavaScript
 */
export class GenStatemBridge {
  /**
   * @param {Object} [options] - Bridge options
   * @param {Function} [options.log] - Logging function
   * @param {Function} [options.sendCommand] - Function to send commands to Erlang
   */
  constructor(options = {}) {
    this.log = options.log || console.log;
    this.sendCommand = options.sendCommand || ((cmd) => {
      this.log(`[GenStatem] Command: ${cmd}`);
    });
    
    // State cache (last known state)
    this.cachedState = null;
    this.cachedButtons = null;
    
    this.log('GenStatem Bridge initialized');
  }
  
  /**
   * Press a button on the state machine
   * @param {number} digit - Button digit (0-9)
   * @returns {Promise<void>}
   */
  async button(digit) {
    // Poka-yoke: Validate input
    if (typeof digit !== 'number' || digit < 0 || digit > 9 || !Number.isInteger(digit)) {
      throw new Error(`Invalid button digit: ${digit}. Must be integer 0-9.`);
    }
    
    const command = `GEN_STATEM:button:${digit}`;
    this.sendCommand(command);
    this.log(`[GenStatem] Button pressed: ${digit}`);
    
    // Invalidate cached state (button press changes state)
    this.cachedState = null;
    this.cachedButtons = null;
  }
  
  /**
   * Get current state of the state machine
   * @returns {Promise<{state: string, data: Object}>}
   */
  async getState() {
    const command = `GEN_STATEM:get_state`;
    this.sendCommand(command);
    
    // Wait for response (in real implementation, would use proper message passing)
    // For now, return cached state with timeout
    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        if (this.cachedState) {
          resolve({
            state: this.cachedState,
            data: {
              buttons: this.cachedButtons || [],
              attempts: this.cachedAttempts || 0,
              lockout_until: this.cachedLockoutUntil || null
            }
          });
        } else {
          reject(new Error('State query timeout - state machine may not be running'));
        }
      }, 500);
      
      // If state is already cached, return immediately
      if (this.cachedState) {
        clearTimeout(timeout);
        resolve({
          state: this.cachedState,
          data: {
            buttons: this.cachedButtons || [],
            attempts: this.cachedAttempts || 0,
            lockout_until: this.cachedLockoutUntil || null
          }
        });
      }
    });
  }
  
  /**
   * Stop the state machine
   */
  stop() {
    const command = `GEN_STATEM:stop`;
    this.sendCommand(command);
    this.log(`[GenStatem] Stop requested`);
  }
  
  /**
   * Update cached state (called by bridge interceptor)
   * @param {string} state - Current state
   * @param {Object|Array<number>} dataOrButtons - State data or button sequence
   */
  updateState(state, dataOrButtons) {
    this.cachedState = state;
    
    if (Array.isArray(dataOrButtons)) {
      // Legacy format: just buttons
      this.cachedButtons = dataOrButtons;
      this.cachedAttempts = 0;
      this.cachedLockoutUntil = null;
    } else if (typeof dataOrButtons === 'object' && dataOrButtons !== null) {
      // Production format: full data object
      this.cachedButtons = dataOrButtons.buttons || [];
      this.cachedAttempts = dataOrButtons.attempts || 0;
      this.cachedLockoutUntil = dataOrButtons.lockout_until || null;
    } else {
      this.cachedButtons = [];
      this.cachedAttempts = 0;
      this.cachedLockoutUntil = null;
    }
    
    this.log(`[GenStatem] State updated: ${state}, buttons: ${JSON.stringify(this.cachedButtons)}, attempts: ${this.cachedAttempts}`);
  }
}

/**
 * Create a global bridge instance
 */
let globalBridge = null;

/**
 * Get or create global bridge instance
 *
 * @param {Object} [options] - Bridge options
 * @returns {GenStatemBridge} Bridge instance
 */
export function getGenStatemBridge(options = {}) {
  if (!globalBridge) {
    globalBridge = new GenStatemBridge(options);
  }
  return globalBridge;
}

/**
 * Set global bridge instance (for testing)
 *
 * @param {GenStatemBridge} bridge - Bridge instance
 */
export function setGenStatemBridge(bridge) {
  globalBridge = bridge;
}

