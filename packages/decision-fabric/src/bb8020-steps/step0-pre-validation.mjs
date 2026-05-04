/**
 * BB8020 Step 0: Pre-validation
 * Socratic analysis + entropy check
 */

import { SocraticAgent } from '../socratic-agent.mjs';

/**
 * Execute Step 0: Pre-validation (Socratic analysis + entropy check)
 * FAIL FAST - Throws if Socratic analysis blocks
 */
export async function executeStep0PreValidation({ specification, store }) {
  const start = Date.now();

  console.log(`\n[Step 0] Running Socratic pre-validation...`);

  // Socratic analysis
  const socratic = new SocraticAgent({ knowledgeStore: store });
  const analysis = await socratic.analyze(specification.statement || specification.description);

  if (!analysis.recommendation.proceed) {
    throw new Error(`Socratic analysis blocked: ${analysis.recommendation.reason}`);
  }

  console.log(`[Step 0] ✓ Socratic analysis passed`);

  return {
    analysis,
    duration_ms: Date.now() - start,
    summary: {
      proceed: analysis.recommendation.proceed,
      reason: analysis.recommendation.reason
    }
  };
}
