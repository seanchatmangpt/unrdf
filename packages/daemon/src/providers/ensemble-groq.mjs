/**
 * @file Ensemble Groq Provider
 * @module @unrdf/daemon/providers/ensemble-groq
 * @description Run parallel specialist prompts, aggregate with confidence
 */

/**
 * Ensemble Groq provider - parallel reasoning with multiple specialists
 */
export class EnsembleGroqProvider {
  constructor(baseProvider, config = {}) {
    this.baseProvider = baseProvider;
    this.specialists = config.specialists || [
      {
        role: 'hypothesis-generator',
        prefix: 'Generate a hypothesis: ',
      },
      {
        role: 'validator',
        prefix: 'Validate this hypothesis: ',
      },
      {
        role: 'refiner',
        prefix: 'Refine and improve this: ',
      },
    ];
  }

  /**
   * Generate text using ensemble of specialists
   */
  async generateText(input) {
    // Run all specialists in parallel
    const specialistResults = await Promise.all(
      this.specialists.map(spec =>
        this.baseProvider.generateText({
          ...input,
          prompt: spec.prefix + input.prompt,
        })
      )
    );

    // Aggregate results
    const aggregated = {
      text: this.aggregate(specialistResults),
      specialist_responses: specialistResults.map((result, idx) => ({
        role: this.specialists[idx].role,
        text: result.text,
      })),
      confidence: this.computeConfidence(specialistResults),
      ensemble_size: this.specialists.length,
    };

    return aggregated;
  }

  /**
   * Aggregate specialist responses
   */
  aggregate(results) {
    // Combine all responses with separators
    return results
      .map((r, i) => `[${this.specialists[i].role}]:\n${r.text}`)
      .join('\n\n---\n\n');
  }

  /**
   * Compute confidence from specialist agreement
   */
  computeConfidence(results) {
    if (results.length < 2) return 1.0;

    // Simple heuristic: agreement based on text similarity
    // Count common keywords across responses
    const responses = results.map(r => r.text.toLowerCase().split(/\s+/));
    const allWords = new Set(responses.flat());

    let agreement = 0;
    for (const word of allWords) {
      const count = responses.filter(words => words.includes(word)).length;
      if (count >= results.length * 0.7) {
        // Word appears in 70%+ of responses
        agreement++;
      }
    }

    // Confidence: ratio of agreed words to total unique words
    const confidence = Math.min(1.0, agreement / Math.max(allWords.size, 1));
    return Math.round(confidence * 100) / 100;
  }

  /**
   * Set custom specialists
   */
  setSpecialists(specialists) {
    this.specialists = specialists;
  }

  /**
   * Get current specialists
   */
  getSpecialists() {
    return this.specialists;
  }
}

export default EnsembleGroqProvider;
