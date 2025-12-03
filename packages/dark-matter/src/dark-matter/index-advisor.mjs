/**
 * @file Index Advisor - Recommend indexes based on query patterns
 * @module @unrdf/dark-matter/index-advisor
 */

import { z } from 'zod';
import { analyzeSparqlQuery } from './query-analyzer.mjs';

/**
 * @typedef {import('n3').Store} Store
 */

/**
 * Index recommendation schema
 */
const IndexRecommendationSchema = z.object({
  type: z.enum(['predicate', 'subject_predicate', 'object', 'composite']),
  priority: z.enum(['low', 'medium', 'high', 'critical']),
  estimatedBenefit: z.number().min(0).max(100),
  reason: z.string(),
  indexConfig: z.object({
    fields: z.array(z.string()),
    unique: z.boolean().optional(),
  }),
});

/**
 * Analyze index needs based on query log
 * @param {Store} store - RDF store
 * @param {Array<string>} queryLog - Array of executed queries
 * @returns {Array<Object>} Index recommendations
 *
 * @throws {TypeError} If store or queryLog is invalid
 *
 * @example
 * const recommendations = analyzeIndexNeeds(store, [query1, query2]);
 * recommendations.forEach(r => console.log(r.type, r.priority));
 */
export function analyzeIndexNeeds(store, queryLog) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('analyzeIndexNeeds: store must be a valid Store instance');
  }

  if (!Array.isArray(queryLog)) {
    throw new TypeError('analyzeIndexNeeds: queryLog must be an array');
  }

  const recommendations = [];
  const predicateFrequency = new Map();
  const subjectPredicateFrequency = new Map();

  // Analyze query patterns
  for (const query of queryLog) {
    if (typeof query !== 'string') {
      continue;
    }

    const analysis = analyzeSparqlQuery(query);

    for (const pattern of analysis.patterns) {
      // Track predicate frequency
      if (!pattern.predicate.startsWith('?')) {
        const count = predicateFrequency.get(pattern.predicate) || 0;
        predicateFrequency.set(pattern.predicate, count + 1);
      }

      // Track subject+predicate combinations
      if (!pattern.subject.startsWith('?') && !pattern.predicate.startsWith('?')) {
        const key = `${pattern.subject}|${pattern.predicate}`;
        const count = subjectPredicateFrequency.get(key) || 0;
        subjectPredicateFrequency.set(key, count + 1);
      }
    }
  }

  // Generate predicate index recommendations
  for (const [predicate, frequency] of predicateFrequency.entries()) {
    if (frequency >= 3) {
      const priority = frequency >= 10 ? 'high' : frequency >= 5 ? 'medium' : 'low';
      const estimatedBenefit = Math.min(frequency * 10, 100);

      recommendations.push({
        type: 'predicate',
        priority,
        estimatedBenefit,
        reason: `Predicate ${predicate} queried ${frequency} times`,
        indexConfig: {
          fields: ['predicate'],
          unique: false,
        },
      });
    }
  }

  // Generate composite index recommendations
  for (const [_key, frequency] of subjectPredicateFrequency.entries()) {
    if (frequency >= 2) {
      const priority = frequency >= 5 ? 'high' : 'medium';
      const estimatedBenefit = Math.min(frequency * 15, 100);

      recommendations.push({
        type: 'subject_predicate',
        priority,
        estimatedBenefit,
        reason: `Subject+Predicate combination queried ${frequency} times`,
        indexConfig: {
          fields: ['subject', 'predicate'],
          unique: false,
        },
      });
    }
  }

  // Sort by estimated benefit
  recommendations.sort((a, b) => b.estimatedBenefit - a.estimatedBenefit);

  return recommendations.map(r => IndexRecommendationSchema.parse(r));
}

/**
 * Suggest index for specific pattern
 * @param {Object} pattern - Triple pattern
 * @returns {Object} Index suggestion
 *
 * @throws {TypeError} If pattern is invalid
 *
 * @example
 * const suggestion = suggestIndexForPattern({
 *   subject: '?s',
 *   predicate: '<http://xmlns.com/foaf/0.1/name>',
 *   object: '?name'
 * });
 */
export function suggestIndexForPattern(pattern) {
  if (!pattern || typeof pattern !== 'object') {
    throw new TypeError('suggestIndexForPattern: pattern must be an object');
  }

  const { subject, predicate, object } = pattern;

  if (!subject || !predicate || !object) {
    throw new TypeError('suggestIndexForPattern: pattern must have subject, predicate, and object');
  }

  // Specific predicate - recommend predicate index
  if (!predicate.startsWith('?')) {
    return {
      type: 'predicate',
      priority: 'high',
      estimatedBenefit: 70,
      reason: 'Specific predicate benefits from dedicated index',
      indexConfig: {
        fields: ['predicate'],
      },
    };
  }

  // Specific subject - recommend subject index
  if (!subject.startsWith('?')) {
    return {
      type: 'subject_predicate',
      priority: 'medium',
      estimatedBenefit: 50,
      reason: 'Specific subject can use subject-based index',
      indexConfig: {
        fields: ['subject'],
      },
    };
  }

  // Specific object - recommend object index
  if (!object.startsWith('?')) {
    return {
      type: 'object',
      priority: 'low',
      estimatedBenefit: 30,
      reason: 'Specific object may benefit from object index',
      indexConfig: {
        fields: ['object'],
      },
    };
  }

  // All wildcards - no specific index recommended
  return {
    type: 'composite',
    priority: 'low',
    estimatedBenefit: 10,
    reason: 'Pattern too general for specific index',
    indexConfig: {
      fields: ['subject', 'predicate', 'object'],
    },
  };
}

/**
 * Calculate index benefit for pattern
 * @param {Object} pattern - Triple pattern
 * @param {Object} indexConfig - Index configuration
 * @returns {number} Benefit score 0-100
 *
 * @throws {TypeError} If parameters are invalid
 *
 * @example
 * const benefit = calculateIndexBenefit(pattern, {
 *   fields: ['predicate'],
 *   unique: false
 * });
 */
export function calculateIndexBenefit(pattern, indexConfig) {
  if (!pattern || typeof pattern !== 'object') {
    throw new TypeError('calculateIndexBenefit: pattern must be an object');
  }

  if (!indexConfig || typeof indexConfig !== 'object') {
    throw new TypeError('calculateIndexBenefit: indexConfig must be an object');
  }

  const { subject, predicate, object } = pattern;
  const { fields } = indexConfig;

  if (!Array.isArray(fields)) {
    throw new TypeError('calculateIndexBenefit: indexConfig.fields must be an array');
  }

  let benefit = 0;

  // Check if indexed fields are bound (not variables)
  for (const field of fields) {
    if (field === 'subject' && !subject.startsWith('?')) {
      benefit += 30;
    }
    if (field === 'predicate' && !predicate.startsWith('?')) {
      benefit += 40;
    }
    if (field === 'object' && !object.startsWith('?')) {
      benefit += 30;
    }
  }

  return Math.min(benefit, 100);
}
