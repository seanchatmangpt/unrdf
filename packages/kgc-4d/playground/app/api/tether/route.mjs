/**
 * Tether SSE API - GET /api/tether
 *
 * Server-Sent Events stream for real-time Shard updates
 *
 * Query Parameters:
 * - subject, predicate, type, belongsTo: Subscription filters
 * - initial: Include initial Shard on connect (default: true)
 *
 * Events:
 * - connected: Connection established
 * - shard: Initial Shard projection
 * - delta: Real-time update
 * - heartbeat: Keep-alive ping
 * - error: Error notification
 */

import { createTetherStream, parseSSEQuery } from '../../../lib/server/tether.mjs';

export const dynamic = 'force-dynamic';
export const runtime = 'nodejs';

export async function GET(request) {
  try {
    const url = new URL(request.url);

    // Generate unique subscription ID
    const subscriptionId = crypto.randomUUID();

    // Parse query filters
    const query = parseSSEQuery(url);

    // Check if initial Shard should be included
    const includeInitialShard = url.searchParams.get('initial') !== 'false';

    // Create SSE stream
    const stream = createTetherStream({
      subscriptionId,
      query: Object.keys(query).length > 0 ? query : undefined,
      includeInitialShard,
    });

    // Return SSE response
    return new Response(stream, {
      headers: {
        'Content-Type': 'text/event-stream',
        'Cache-Control': 'no-cache, no-transform',
        Connection: 'keep-alive',
        'X-Accel-Buffering': 'no', // Disable Nginx buffering
      },
    });
  } catch (error) {
    console.error('[API] Tether error:', error);
    return Response.json(
      {
        error: error.message,
      },
      { status: 500 }
    );
  }
}
