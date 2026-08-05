/**
 * Merge API - POST /api/multiverse/merge
 *
 * Request body:
 * {
 *   "forkId": "unique-fork-id",
 *   "strategy": "auto" | "manual",
 *   "resolutions": [
 *     { "subject": "...", "predicate": "...", "decision": "fork" | "main" }
 *   ]
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

    const mode = body.strategy || 'auto';
    const strategy = mode === 'manual'
      ? { mode, resolutions: body.resolutions || [] }
      : mode;
    const result = await mergeFork(body.forkId, strategy);

    if (result.status === 'conflict') return Response.json(result, { status: 409 });
    if (result.status === 'error') return Response.json(result, { status: 400 });
    return Response.json(result);
  } catch (error) {
    console.error('[API] Merge error:', error);
    return Response.json(
      { error: error.message, details: 'Failed to merge fork.' },
      { status: 500 }
    );
  }
}
