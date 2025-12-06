/**
 * Time-Travel API - GET /api/time-travel?t_ns=<nanosecond_timestamp>
 *
 * Reconstructs universe state at a specific nanosecond timestamp
 * using KGC 4D's reconstructState() time-travel algorithm.
 *
 * Query Parameters:
 * - t_ns (required): Target nanosecond timestamp (BigInt as string)
 * - subject, predicate, type: Optional filters
 *
 * Returns:
 * - shard: Reconstructed RDF quads at target time
 * - t_ns: Target timestamp
 * - timestamp: ISO 8601 formatted time
 */

import { reconstructAtTime } from '../../../lib/server/time-travel.mjs';

export const dynamic = 'force-dynamic';
export const runtime = 'nodejs';

export async function GET(request) {
  try {
    const url = new URL(request.url);

    // Extract target time from query
    const tNsParam = url.searchParams.get('t_ns');
    if (!tNsParam) {
      return Response.json(
        {
          error: 'Missing required parameter: t_ns',
          example: '/api/time-travel?t_ns=1704657600000000000',
        },
        { status: 400 }
      );
    }

    // Convert to BigInt
    const targetTime = BigInt(tNsParam);

    // Build filters from query params
    const filters = {};
    if (url.searchParams.has('subject')) {
      filters.subject = url.searchParams.get('subject');
    }
    if (url.searchParams.has('predicate')) {
      filters.predicate = url.searchParams.get('predicate');
    }
    if (url.searchParams.has('type')) {
      filters.type = url.searchParams.get('type');
    }

    // Reconstruct state at target time
    const shard = await reconstructAtTime(targetTime, filters);

    return Response.json({
      shard,
      t_ns: targetTime.toString(),
      timestamp: shard.timestamp_iso,
      reconstructed_from_snapshot: true,
    });
  } catch (error) {
    console.error('[API] Time-travel error:', error);

    return Response.json(
      {
        error: error.message,
        details:
          'Failed to reconstruct state. Ensure at least one snapshot exists (run freezeUniverse).',
      },
      { status: 500 }
    );
  }
}
