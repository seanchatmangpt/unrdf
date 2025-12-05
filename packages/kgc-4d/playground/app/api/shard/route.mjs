/**
 * Shard Projection API - GET /api/shard
 *
 * Check-Out: Project a filtered view of the Universe
 *
 * Query Parameters:
 * - subject: Filter by subject IRI
 * - predicate: Filter by predicate IRI
 * - type: Filter by rdf:type
 * - belongsTo: Filter by belongsTo relationship
 */

import { projectShard, getUniverseStats } from '../../../lib/server/shard.mjs';

export async function GET(request) {
  try {
    const url = new URL(request.url);
    const params = url.searchParams;

    // Check for stats request
    if (params.get('stats') === 'true') {
      const stats = await getUniverseStats();
      return Response.json(stats);
    }

    // Build query from parameters
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

    // Project the Shard
    const shard = await projectShard(query);

    return Response.json({
      success: true,
      shard,
    });
  } catch (error) {
    console.error('[API] Shard projection error:', error);
    return Response.json(
      {
        success: false,
        error: error.message,
      },
      { status: 500 }
    );
  }
}
