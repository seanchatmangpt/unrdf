/**
 * Merge API - POST /api/multiverse/merge
 *
 * Merges a forked reality back into the main Universe.
 *
 * Request Body:
 * {
 *   "forkId": "unique-fork-id",
 *   "strategy": "auto" | "manual"
 * }
 *
 * Response (success):
 * {
 *   "status": "success",
 *   "mergedEvents": 5,
 *   "forkId": "..."
 * }
 *
 * Response (conflict):
 * {
 *   "status": "conflict",
 *   "conflicts": [...],
 *   "message": "..."
 * }
 */

import { mergeFork } from '../../../../lib/server/multiverse.mjs';

export const dynamic = 'force-dynamic';
export const runtime = 'nodejs';

export async function POST(request) {
  try {
    const body = await request.json();

    if (!body.forkId) {
      return Response.json(
        {
          error: 'Missing required field: forkId',
          example: { forkId: 'my-fork', strategy: 'auto' },
        },
        { status: 400 }
      );
    }

    const forkId = body.forkId;
    const strategy = body.strategy || 'auto';

    const result = await mergeFork(forkId, strategy);

    if (result.status === 'conflict') {
      return Response.json(result, { status: 409 }); // HTTP 409 Conflict
    }

    if (result.status === 'error') {
      return Response.json(result, { status: 400 });
    }

    return Response.json(result);
  } catch (error) {
    console.error('[API] Merge error:', error);

    return Response.json(
      {
        error: error.message,
        details: 'Failed to merge fork.',
      },
      { status: 500 }
    );
  }
}
