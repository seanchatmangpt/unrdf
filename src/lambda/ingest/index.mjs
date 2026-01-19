/**
 * RDF Ingest Lambda Handler
 */
export async function handler(event) {
  const body = JSON.parse(event.body || '{}');
  const { triples } = body;

  // Simulate batch insert
  const inserted = triples?.length || 0;

  return {
    statusCode: 200,
    headers: {
      'Content-Type': 'application/json',
      'Access-Control-Allow-Origin': '*'
    },
    body: JSON.stringify({ inserted })
  };
}