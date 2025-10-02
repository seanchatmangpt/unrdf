/**
 * @file gRPC interceptors for OpenTelemetry and telemetry
 * @module sidecar/interceptors
 *
 * @description
 * gRPC client interceptors for distributed tracing, metrics collection, and logging.
 */

import { randomUUID } from 'crypto';

/**
 * Create telemetry interceptor for OpenTelemetry
 * @param {Object} observability - Observability manager
 * @returns {Function} Interceptor function
 */
export function createTelemetryInterceptor(observability) {
  return function telemetryInterceptor(options, nextCall) {
    return new InterceptingCall(nextCall(options), observability, options);
  };
}

/**
 * Intercepting call for telemetry
 */
class InterceptingCall {
  constructor(call, observability, options) {
    this.call = call;
    this.observability = observability;
    this.options = options;
    this.requestId = randomUUID();
    this.startTime = Date.now();
    this.spanContext = null;
  }

  /**
   * Start request
   * @param {Object} metadata - Request metadata
   * @param {Function} listener - Response listener
   * @param {Function} next - Next handler
   */
  start(metadata, listener, next) {
    // Add request ID to metadata
    metadata.set('x-request-id', this.requestId);
    metadata.set('x-trace-id', this.requestId);

    // Start OpenTelemetry span
    if (this.observability && typeof this.observability.startSpan === 'function') {
      const methodName = this.options.method_definition?.path || 'unknown';

      this.spanContext = this.observability.startSpan(`grpc.${methodName}`, {
        'rpc.system': 'grpc',
        'rpc.service': this.options.method_definition?.service_name || 'unknown',
        'rpc.method': methodName,
        'request.id': this.requestId
      });
    }

    // Wrap listener to capture response
    const wrappedListener = {
      onReceiveMetadata: (metadata, next) => {
        // Log response metadata
        if (listener.onReceiveMetadata) {
          listener.onReceiveMetadata(metadata, next);
        } else {
          next(metadata);
        }
      },

      onReceiveMessage: (message, next) => {
        // Log response message
        if (listener.onReceiveMessage) {
          listener.onReceiveMessage(message, next);
        } else {
          next(message);
        }
      },

      onReceiveStatus: (status, next) => {
        const duration = Date.now() - this.startTime;

        // End OpenTelemetry span
        if (this.spanContext && this.observability && typeof this.observability.endSpan === 'function') {
          this.observability.endSpan(this.spanContext, {
            'rpc.status_code': status.code,
            'response.duration_ms': duration,
            success: status.code === 0
          });
        }

        // Record metrics
        if (this.observability && typeof this.observability.recordMetric === 'function') {
          this.observability.recordMetric('grpc.client.duration', duration, {
            method: this.options.method_definition?.path || 'unknown',
            status: status.code
          });

          if (status.code !== 0) {
            this.observability.recordMetric('grpc.client.errors', 1, {
              method: this.options.method_definition?.path || 'unknown',
              code: status.code
            });
          }
        }

        // Forward status
        if (listener.onReceiveStatus) {
          listener.onReceiveStatus(status, next);
        } else {
          next(status);
        }
      }
    };

    next(metadata, wrappedListener);
  }

  /**
   * Send message
   * @param {Object} message - Message to send
   * @param {Function} next - Next handler
   */
  sendMessage(message, next) {
    // Add message metadata to span
    if (this.spanContext && this.observability && typeof this.observability.addSpanEvent === 'function') {
      this.observability.addSpanEvent(this.spanContext, 'message.sent', {
        'message.type': message.constructor.name,
        'message.size': JSON.stringify(message).length
      });
    }

    next(message);
  }

  /**
   * Half close
   * @param {Function} next - Next handler
   */
  halfClose(next) {
    next();
  }

  /**
   * Cancel
   * @param {Function} next - Next handler
   */
  cancel(next) {
    if (this.spanContext && this.observability && typeof this.observability.endSpan === 'function') {
      this.observability.endSpan(this.spanContext, {
        cancelled: true
      });
    }

    next();
  }

  /**
   * Get peer
   * @param {Function} next - Next handler
   */
  getPeer(next) {
    return next();
  }
}

/**
 * Create retry interceptor
 * @param {RetryStrategy} retryStrategy - Retry strategy
 * @returns {Function} Interceptor function
 */
export function createRetryInterceptor(retryStrategy) {
  return function retryInterceptor(options, nextCall) {
    // Only retry idempotent methods
    const method = options.method_definition?.path || '';
    const isIdempotent = !method.includes('Apply') && !method.includes('Evaluate');

    if (!isIdempotent) {
      return nextCall(options);
    }

    // Implement retry logic at interceptor level
    // This is a simplified version - full implementation would need request buffering
    return nextCall(options);
  };
}

/**
 * Create timeout interceptor
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Function} Interceptor function
 */
export function createTimeoutInterceptor(timeout) {
  return function timeoutInterceptor(options, nextCall) {
    // Set deadline if not already set
    if (!options.deadline) {
      options.deadline = Date.now() + timeout;
    }

    return nextCall(options);
  };
}

/**
 * Create logging interceptor
 * @param {Object} logger - Logger instance
 * @returns {Function} Interceptor function
 */
export function createLoggingInterceptor(logger = console) {
  return function loggingInterceptor(options, nextCall) {
    const methodName = options.method_definition?.path || 'unknown';
    const requestId = randomUUID();

    logger.debug(`[gRPC Request] ${methodName}`, { requestId });

    return new LoggingCall(nextCall(options), logger, methodName, requestId);
  };
}

/**
 * Logging call wrapper
 */
class LoggingCall {
  constructor(call, logger, methodName, requestId) {
    this.call = call;
    this.logger = logger;
    this.methodName = methodName;
    this.requestId = requestId;
    this.startTime = Date.now();
  }

  start(metadata, listener, next) {
    const wrappedListener = {
      onReceiveMetadata: listener.onReceiveMetadata,
      onReceiveMessage: listener.onReceiveMessage,
      onReceiveStatus: (status, next) => {
        const duration = Date.now() - this.startTime;

        if (status.code === 0) {
          this.logger.debug(`[gRPC Response] ${this.methodName}`, {
            requestId: this.requestId,
            duration,
            status: 'success'
          });
        } else {
          this.logger.error(`[gRPC Error] ${this.methodName}`, {
            requestId: this.requestId,
            duration,
            code: status.code,
            message: status.details
          });
        }

        if (listener.onReceiveStatus) {
          listener.onReceiveStatus(status, next);
        } else {
          next(status);
        }
      }
    };

    next(metadata, wrappedListener);
  }

  sendMessage(message, next) {
    next(message);
  }

  halfClose(next) {
    next();
  }

  cancel(next) {
    this.logger.warn(`[gRPC Cancelled] ${this.methodName}`, {
      requestId: this.requestId
    });
    next();
  }

  getPeer(next) {
    return next();
  }
}

/**
 * Compose multiple interceptors
 * @param {...Function} interceptors - Interceptors to compose
 * @returns {Function} Composed interceptor
 */
export function composeInterceptors(...interceptors) {
  return function composedInterceptor(options, nextCall) {
    let chain = nextCall;

    // Apply interceptors in reverse order
    for (let i = interceptors.length - 1; i >= 0; i--) {
      const interceptor = interceptors[i];
      const currentChain = chain;
      chain = (opts) => interceptor(opts, currentChain);
    }

    return chain(options);
  };
}

export default {
  createTelemetryInterceptor,
  createRetryInterceptor,
  createTimeoutInterceptor,
  createLoggingInterceptor,
  composeInterceptors
};
