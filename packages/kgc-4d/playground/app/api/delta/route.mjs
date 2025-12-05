/**
 * Delta Submission API - POST /api/delta
 *
 * Check-In: Submit user intent for validation and commit
 *
 * Request Body:
 * {
 *   operations: [{ type, subject, predicate, object }],
 *   client_vector_clock: { nodeId, counters },
 *   source: string
 * }
 *
 * Response:
 * - ACK: { status: 'ACK', t_ns, vector_clock, ... }
 * - REJECT: { status: 'REJECT', reason, ... }
 */

import { submitDelta, submitUpdate } from '../../../lib/server/delta.mjs';

export async function POST(request) {
  try {
    const body = await request.json();

    // Simple update shorthand
    if (body.subject && body.predicate && body.value !== undefined) {
      const result = await submitUpdate(body.subject, body.predicate, body.value, {
        termType: body.termType,
        datatype: body.datatype,
        language: body.language,
        source: body.source,
        client_vector_clock: body.client_vector_clock,
      });

      return Response.json(result, {
        status: result.status === 'ACK' ? 200 : 400,
      });
    }

    // Full delta submission
    if (!body.operations || !Array.isArray(body.operations)) {
      return Response.json(
        {
          status: 'REJECT',
          reason: 'Missing or invalid operations array',
        },
        { status: 400 }
      );
    }

    const result = await submitDelta(body);

    return Response.json(result, {
      status: result.status === 'ACK' ? 200 : 400,
    });
  } catch (error) {
    console.error('[API] Delta submission error:', error);
    return Response.json(
      {
        status: 'REJECT',
        reason: error.message,
      },
      { status: 500 }
    );
  }
}
