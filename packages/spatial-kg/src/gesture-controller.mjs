/**
 * @file Gesture Controller for VR/AR
 * @module @unrdf/spatial-kg/gesture-controller
 * @description Handle VR/AR gestures (point, grab, teleport, pinch)
 */

import { GestureEventSchema } from './schemas.mjs';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('@unrdf/spatial-kg/gesture-controller');

/**
 * Gesture Controller
 */
export class GestureController {
  /**
   * @param {Object} [options] - Controller options
   * @param {number} [options.pinchThreshold=0.8] - Pinch detection threshold
   * @param {number} [options.grabThreshold=0.9] - Grab detection threshold
   */
  constructor(options = {}) {
    this.pinchThreshold = options.pinchThreshold || 0.8;
    this.grabThreshold = options.grabThreshold || 0.9;
    this.listeners = new Map();
    this.activeGestures = new Map();
    this.lastEvents = new Map();
  }

  /**
   * Process controller input (from WebXR)
   * @param {string} controllerId - Controller ID
   * @param {Object} state - Controller state
   * @param {Object} state.position - 3D position
   * @param {Object} state.buttons - Button states
   * @param {Object} [state.handPose] - Hand tracking data
   * @returns {Array<Object>} Detected gestures
   */
  processInput(controllerId, state) {
    return tracer.startActiveSpan('gesture.process-input', (span) => {
      try {
        const gestures = [];
        const timestamp = Date.now();

        // Button-based gestures
        if (state.buttons) {
          // Select/trigger
          if (state.buttons.trigger > 0.9) {
            gestures.push(this._createGesture('select', controllerId, state.position, timestamp));
          }

          // Grab/grip
          if (state.buttons.grip > this.grabThreshold) {
            gestures.push(this._createGesture('grab', controllerId, state.position, timestamp, state.buttons.grip));
          }

          // Teleport (thumbstick click)
          if (state.buttons.thumbstick === 1) {
            gestures.push(this._createGesture('teleport', controllerId, state.position, timestamp));
          }
        }

        // Hand tracking gestures
        if (state.handPose) {
          const pinch = this._detectPinch(state.handPose);
          if (pinch.detected) {
            gestures.push(this._createGesture('pinch', controllerId, pinch.position, timestamp, pinch.strength));
          }
        }

        // Emit events
        for (const gesture of gestures) {
          this._emit(gesture);
        }

        span.setAttributes({
          'gesture.controller': controllerId,
          'gesture.count': gestures.length,
        });

        return gestures;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Create gesture event
   * @private
   */
  _createGesture(type, controller, position, timestamp, intensity) {
    const gesture = {
      type,
      controller,
      position,
      timestamp,
      intensity,
    };

    return GestureEventSchema.parse(gesture);
  }

  /**
   * Detect pinch gesture from hand pose
   * @private
   */
  _detectPinch(handPose) {
    if (!handPose.joints) {
      return { detected: false };
    }

    // Get thumb tip and index finger tip
    const thumbTip = handPose.joints['thumb-tip'];
    const indexTip = handPose.joints['index-finger-tip'];

    if (!thumbTip || !indexTip) {
      return { detected: false };
    }

    // Calculate distance
    const dx = thumbTip.position.x - indexTip.position.x;
    const dy = thumbTip.position.y - indexTip.position.y;
    const dz = thumbTip.position.z - indexTip.position.z;
    const distance = Math.sqrt(dx * dx + dy * dy + dz * dz);

    // Pinch threshold (typically ~2cm)
    const threshold = 0.02;
    const detected = distance < threshold;

    if (detected) {
      // Strength based on how close fingers are
      const strength = Math.max(0, 1 - (distance / threshold));

      // Midpoint between fingers
      const position = {
        x: (thumbTip.position.x + indexTip.position.x) / 2,
        y: (thumbTip.position.y + indexTip.position.y) / 2,
        z: (thumbTip.position.z + indexTip.position.z) / 2,
      };

      return { detected: true, strength, position };
    }

    return { detected: false };
  }

  /**
   * Register gesture event listener
   * @param {string} gestureType - Gesture type
   * @param {Function} callback - Event handler
   * @returns {Function} Unsubscribe function
   */
  on(gestureType, callback) {
    if (!this.listeners.has(gestureType)) {
      this.listeners.set(gestureType, new Set());
    }

    this.listeners.get(gestureType).add(callback);

    return () => {
      const listeners = this.listeners.get(gestureType);
      if (listeners) {
        listeners.delete(callback);
      }
    };
  }

  /**
   * Emit gesture event to listeners
   * @private
   */
  _emit(gesture) {
    const listeners = this.listeners.get(gesture.type);
    if (listeners) {
      for (const callback of listeners) {
        try {
          callback(gesture);
        } catch (error) {
          console.error(`Gesture listener error:`, error);
        }
      }
    }

    // Store last event
    this.lastEvents.set(`${gesture.controller}-${gesture.type}`, gesture);
  }

  /**
   * Get last gesture of type
   * @param {string} controller - Controller ID
   * @param {string} type - Gesture type
   * @returns {Object|null} Last gesture event
   */
  getLastGesture(controller, type) {
    return this.lastEvents.get(`${controller}-${type}`) || null;
  }

  /**
   * Clear all listeners
   * @returns {void}
   */
  clear() {
    this.listeners.clear();
    this.activeGestures.clear();
    this.lastEvents.clear();
  }

  /**
   * Simulate gesture (for testing)
   * @param {Object} gesture - Gesture event
   * @returns {void}
   */
  simulateGesture(gesture) {
    const validated = GestureEventSchema.parse(gesture);
    this._emit(validated);
  }
}
