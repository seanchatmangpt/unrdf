/**
 * Fork API - POST /api/multiverse/fork
 *
 * Creates a new forked reality from a specific timestamp.
 *
 * Request Body:
 * {
 *   "forkId": "unique-fork-id",
 *   "fromTime": "1704657600000000000" // BigInt as string
 * }
 *
 * Response:
 * {
 *   "forkId": "...",
 *   "baseTime": "...",
 *   "baseTimeIso": "...",
 *   "quadCount": 123,
 *   "status": "active"
 * }
 */

import { createFork } from '../../../../lib/server/multiverse.mjs';

export const dynamic = 'force-dynamic';
export const runtime = 'nodejs';

export async function POST(request) {
  try {
    const body = await request.json();

    if (!body.forkId) {
      return Response.json(
        {
          error: 'Missing required field: forkId',
          example: { forkId: 'my-fork', fromTime: '1704657600000000000' },
        },
        { status: 400 }
      );
    }

    if (!body.fromTime) {
      return Response.json(
        {
          error: 'Missing required field: fromTime',
          example: { forkId: 'my-fork', fromTime: '1704657600000000000' },
        },
        { status: 400 }
      );
    }

    const forkId = body.forkId;
    const fromTime = BigInt(body.fromTime);

    const result = await createFork(forkId, fromTime);

    return Response.json(result);
  } catch (error) {
    console.error('[API] Fork creation error:', error);

    return Response.json(
      {
        error: error.message,
        details: 'Failed to create fork. Ensure snapshot exists at target time.',
      },
      { status: 500 }
    );
  }
}
