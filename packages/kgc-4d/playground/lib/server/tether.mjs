/**
 * KGC-4D Tether - Server-Sent Events for Real-Time Sync
 *
 * The Tether is the active connection between the Universe (Server)
 * and Shards (Browser clients). It enables:
 * - Real-time push of Universe changes to subscribed clients
 * - Streaming of Shard updates as they happen
 *
 * We use Server-Sent Events (SSE) for simplicity and HTTP compatibility.
 * WebSocket can be added for bidirectional communication if needed.
 */

import { registerSubscription, unregisterSubscription, now } from './universe.mjs';
import { projectShard } from './shard.mjs';

/**
 * Create an SSE stream encoder
 * @returns {TransformStream}
 */
export function createSSEStream() {
  const encoder = new TextEncoder();

  return new TransformStream({
    transform(event, controller) {
      // Format as SSE event
      let data = '';

      if (event.event) {
        data += `event: ${event.event}\n`;
      }

      if (event.id) {
        data += `id: ${event.id}\n`;
      }

      if (event.retry) {
        data += `retry: ${event.retry}\n`;
      }

      const payload = typeof event.data === 'string' ? event.data : JSON.stringify(event.data);
      data += `data: ${payload}\n\n`;

      controller.enqueue(encoder.encode(data));
    },
  });
}

/**
 * Create a Tether stream for a client subscription
 *
 * @param {Object} options
 * @param {string} options.subscriptionId - Unique subscription ID
 * @param {Object} [options.query] - Initial query for Shard projection
 * @param {boolean} [options.includeInitialShard] - Send initial Shard on connect
 * @returns {ReadableStream}
 */
export function createTetherStream(options = {}) {
  const { subscriptionId, query, includeInitialShard = true } = options;

  let intervalId = null;
  let cancelled = false;

  const stream = new ReadableStream({
    async start(controller) {
      const sseStream = createSSEStream();
      const writer = sseStream.writable.getWriter();
      const reader = sseStream.readable.getReader();

      // Forward transformed data to controller
      (async () => {
        while (!cancelled) {
          const { done, value } = await reader.read();
          if (done) break;
          controller.enqueue(value);
        }
      })();

      // Send connection established event
      await writer.write({
        event: 'connected',
        id: subscriptionId,
        data: {
          type: 'CONNECTED',
          subscriptionId,
          t_ns: now().toString(),
          timestamp: new Date().toISOString(),
        },
      });

      // Send initial Shard if requested
      if (includeInitialShard && query) {
        try {
          const shard = await projectShard(query);
          await writer.write({
            event: 'shard',
            id: shard.id,
            data: {
              type: 'SHARD',
              ...shard,
            },
          });
        } catch (error) {
          await writer.write({
            event: 'error',
            data: {
              type: 'ERROR',
              message: error.message,
            },
          });
        }
      }

      // Register for real-time updates
      registerSubscription(subscriptionId, query, async (update) => {
        if (cancelled) return;

        await writer.write({
          event: 'delta',
          id: crypto.randomUUID(),
          data: update,
        });
      });

      // Keep-alive heartbeat every 30 seconds
      intervalId = setInterval(async () => {
        if (cancelled) return;

        await writer.write({
          event: 'heartbeat',
          data: {
            type: 'HEARTBEAT',
            t_ns: now().toString(),
            timestamp: new Date().toISOString(),
          },
        });
      }, 30000);
    },

    cancel() {
      cancelled = true;

      if (intervalId) {
        clearInterval(intervalId);
      }

      unregisterSubscription(subscriptionId);
    },
  });

  return stream;
}

/**
 * Parse SSE query parameters from URL
 * @param {URL} url
 * @returns {Object}
 */
export function parseSSEQuery(url) {
  const params = url.searchParams;

  const query = {};

  if (params.has('subject')) {
    query.subject = params.get('subject');
  }

  if (params.has('predicate')) {
    query.predicate = params.get('predicate');
  }

  if (params.has('type')) {
    query.type = params.get('type');
  }

  if (params.has('belongsTo')) {
    query.belongsTo = params.get('belongsTo');
  }

  return query;
}
