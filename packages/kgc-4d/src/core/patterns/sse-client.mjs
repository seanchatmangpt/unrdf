/**
 * SSE Client - Real-Time Event Stream Handler
 *
 * Handles Server-Sent Events with automatic reconnection, heartbeat validation,
 * and event dispatching. Works in browser environments.
 *
 * Usage:
 * ```javascript
 * const client = new SSEClient('/api/events');
 * client.on('message', (data) => console.log('Received:', data));
 * client.connect();
 * ```
 */

/**
 *
 */
export class SSEClient {
  /**
   *
   */
  constructor(url, options = {}) {
    this.url = url;
    this.options = {
      reconnectDelay: 5000,
      heartbeatInterval: 30000,
      heartbeatTimeout: 35000,
      maxReconnectAttempts: null, // null = infinite
      ...options,
    };

    this.eventSource = null;
    this.isConnected = false;
    this.reconnectAttempts = 0;
    this.reconnectTimeoutId = null;
    this.heartbeatTimeoutId = null;
    this.listeners = new Map();
  }

  /**
   * Register event listener
   * @param {string} eventType - Event type to listen for
   * @param {Function} callback - Handler function
   */
  on(eventType, callback) {
    if (!this.listeners.has(eventType)) {
      this.listeners.set(eventType, []);
    }
    this.listeners.get(eventType).push(callback);
  }

  /**
   * Unregister event listener
   * @param {string} eventType
   * @param {Function} callback
   */
  off(eventType, callback) {
    if (!this.listeners.has(eventType)) return;

    const callbacks = this.listeners.get(eventType);
    const index = callbacks.indexOf(callback);
    if (index > -1) {
      callbacks.splice(index, 1);
    }
  }

  /**
   * Emit event to all listeners
   * @private
   */
  _emit(eventType, data) {
    const callbacks = this.listeners.get(eventType) || [];
    for (const callback of callbacks) {
      try {
        callback(data);
      } catch (error) {
        console.error(`Error in ${eventType} listener:`, error);
      }
    }
  }

  /**
   * Connect to SSE stream
   */
  connect() {
    if (this.isConnected || this.eventSource) {
      return;
    }

    this._emit('connecting');

    try {
      this.eventSource = new EventSource(this.url);

      // Default message handler
      this.eventSource.addEventListener('message', (e) => {
        this._resetHeartbeatTimeout();
        try {
          const data = JSON.parse(e.data);
          this._emit('message', data);
        } catch (error) {
          this._emit('error', new Error(`Failed to parse message: ${error.message}`));
        }
      });

      // Generic event handler
      this.eventSource.addEventListener('connected', (e) => {
        this._handleConnected(e);
      });

      this.eventSource.addEventListener('shard', (e) => {
        this._handleEvent(e, 'shard');
      });

      this.eventSource.addEventListener('delta', (e) => {
        this._handleEvent(e, 'delta');
      });

      this.eventSource.addEventListener('heartbeat', (e) => {
        this._handleHeartbeat(e);
      });

      this.eventSource.addEventListener('error', (e) => {
        this._handleError(e);
      });

      this.eventSource.onerror = () => {
        this._handleError(new Error('EventSource error'));
      };

      this.isConnected = true;
      this.reconnectAttempts = 0;
      this._emit('connected');
      this._startHeartbeatTimeout();
    } catch (error) {
      this._emit('error', error);
      this._scheduleReconnect();
    }
  }

  /**
   * Disconnect from SSE stream
   */
  disconnect() {
    this.isConnected = false;

    if (this.eventSource) {
      this.eventSource.close();
      this.eventSource = null;
    }

    if (this.reconnectTimeoutId) {
      clearTimeout(this.reconnectTimeoutId);
      this.reconnectTimeoutId = null;
    }

    if (this.heartbeatTimeoutId) {
      clearTimeout(this.heartbeatTimeoutId);
      this.heartbeatTimeoutId = null;
    }

    this._emit('disconnected');
  }

  /**
   * Check if currently connected
   */
  getStatus() {
    return {
      isConnected: this.isConnected,
      reconnectAttempts: this.reconnectAttempts,
      url: this.url,
    };
  }

  // Private methods

  /**
   * Handle connected event
   * @private
   */
  _handleConnected(e) {
    this._resetHeartbeatTimeout();
    try {
      const data = JSON.parse(e.data);
      this._emit('connected-event', data);
    } catch (error) {
      this._emit('error', new Error(`Failed to parse connected event: ${error.message}`));
    }
  }

  /**
   * Handle generic SSE event
   * @private
   */
  _handleEvent(e, eventType) {
    this._resetHeartbeatTimeout();
    try {
      const data = JSON.parse(e.data);
      this._emit(eventType, data);
    } catch (error) {
      this._emit('error', new Error(`Failed to parse ${eventType} event: ${error.message}`));
    }
  }

  /**
   * Handle heartbeat (keep-alive)
   * @private
   */
  _handleHeartbeat(e) {
    this._resetHeartbeatTimeout();
    try {
      const data = JSON.parse(e.data);
      this._emit('heartbeat', data);
    } catch (error) {
      // Silently ignore heartbeat parse errors
    }
  }

  /**
   * Handle error
   * @private
   */
  _handleError(error) {
    this.isConnected = false;
    this._emit('error', error);
    this._scheduleReconnect();
  }

  /**
   * Schedule reconnection attempt
   * @private
   */
  _scheduleReconnect() {
    // Check max attempts
    if (
      this.options.maxReconnectAttempts !== null &&
      this.reconnectAttempts >= this.options.maxReconnectAttempts
    ) {
      this._emit('max-reconnect-attempts', this.reconnectAttempts);
      return;
    }

    this.reconnectAttempts++;
    const delay = this.options.reconnectDelay;

    this._emit('reconnecting', {
      attempt: this.reconnectAttempts,
      delay,
    });

    this.reconnectTimeoutId = setTimeout(() => {
      this.eventSource = null;
      this.connect();
    }, delay);
  }

  /**
   * Start heartbeat timeout (detect stale connections)
   * @private
   */
  _startHeartbeatTimeout() {
    this.heartbeatTimeoutId = setTimeout(() => {
      this._emit('heartbeat-timeout');
      this.disconnect();
      this._scheduleReconnect();
    }, this.options.heartbeatTimeout);
  }

  /**
   * Reset heartbeat timeout
   * @private
   */
  _resetHeartbeatTimeout() {
    if (this.heartbeatTimeoutId) {
      clearTimeout(this.heartbeatTimeoutId);
    }
    this._startHeartbeatTimeout();
  }
}
