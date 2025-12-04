/**
 * @fileoverview Quality utilities - RDF data quality assessment and improvement
 *
 * These utilities provide comprehensive data quality assessment, validation,
 * and improvement capabilities for RDF data.
 *
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { createStore } from '@unrdf/core';
import { DataFactory } from 'n3';
import {
  asNamedNode as _asNamedNode,
  getIRI as _getIRI,
  isNamedNode as _isNamedNode,
  isLiteral as _isLiteral,
  isBlankNode as _isBlankNode,
} from './term-utils.mjs';
import {
  getVocabularyStats as _getVocabularyStats,
  validateNamespaces,
} from './namespace-utils.mjs';

const { _namedNode, _literal, _blankNode } = DataFactory;

/**
 * Data quality assessment result
 */
export class QualityAssessment {
  /**
   *
   */
  constructor() {
    this.overallScore = 0;
    this.dimensions = {};
    this.issues = [];
    this.recommendations = [];
    this.metrics = {};
  }

  /**
   * Add a quality dimension
   * @param {string} name - Dimension name
   * @param {number} score - Score (0-100)
   * @param {string[]} issues - Issues found
   * @param {string[]} recommendations - Recommendations
   */
  addDimension(name, score, issues = [], recommendations = []) {
    this.dimensions[name] = {
      score,
      issues,
      recommendations,
    };
    this.issues.push(...issues);
    this.recommendations.push(...recommendations);
  }

  /**
   * Add a metric
   * @param {string} name - Metric name
   * @param {any} value - Metric value
   */
  addMetric(name, value) {
    this.metrics[name] = value;
  }

  /**
   * Calculate overall score
   */
  calculateOverallScore() {
    const scores = Object.values(this.dimensions).map(d => d.score);
    this.overallScore = scores.length > 0 ? scores.reduce((a, b) => a + b, 0) / scores.length : 0;
    return this.overallScore;
  }

  /**
   * Get quality grade
   * @returns {string} Quality grade (A, B, C, D, F)
   */
  getGrade() {
    if (this.overallScore >= 90) return 'A';
    if (this.overallScore >= 80) return 'B';
    if (this.overallScore >= 70) return 'C';
    if (this.overallScore >= 60) return 'D';
    return 'F';
  }
}

/**
 * Assess data quality of an RDF store
 * @param {import('n3').Store} store - RDF store to assess
 * @param {Object} [options] - Assessment options
 * @returns {QualityAssessment} Quality assessment result
 */
export const assessDataQuality = (store, _options = {}) => {
  const assessment = new QualityAssessment();

  // Basic metrics
  assessment.addMetric('totalQuads', store.size);
  assessment.addMetric('uniqueSubjects', new Set([...store].map(q => q.subject.value)).size);
  assessment.addMetric('uniquePredicates', new Set([...store].map(q => q.predicate.value)).size);
  assessment.addMetric('uniqueObjects', new Set([...store].map(q => q.object.value)).size);

  // Assess different quality dimensions
  assessCompleteness(store, assessment);
  assessConsistency(store, assessment);
  assessAccuracy(store, assessment);
  assessValidity(store, assessment);
  assessUniqueness(store, assessment);
  assessTimeliness(store, assessment);
  assessAccessibility(store, assessment);

  // Calculate overall score
  assessment.calculateOverallScore();

  return assessment;
};

/**
 * Assess completeness dimension
 * @param {import('n3').Store} store - RDF store
 * @param {QualityAssessment} assessment - Assessment object
 */
const assessCompleteness = (store, assessment) => {
  const issues = [];
  const recommendations = [];
  let score = 100;

  // Check for empty store
  if (store.size === 0) {
    issues.push({ type: 'error', message: 'Store is empty' });
    score = 0;
  }

  // Check for subjects without types
  const subjectsWithoutTypes = new Set();
  const subjectsWithTypes = new Set();

  for (const quad of store) {
    if (quad.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type') {
      subjectsWithTypes.add(quad.subject.value);
    }
    subjectsWithoutTypes.add(quad.subject.value);
  }

  const untypedSubjects = [...subjectsWithoutTypes].filter(s => !subjectsWithTypes.has(s));
  if (untypedSubjects.length > 0) {
    issues.push({
      type: 'warning',
      message: `${untypedSubjects.length} subjects without rdf:type`,
      count: untypedSubjects.length,
    });
    score -= Math.min(20, untypedSubjects.length * 2);
  }

  // Check for subjects without labels
  const subjectsWithoutLabels = new Set();
  const subjectsWithLabels = new Set();

  for (const quad of store) {
    if (quad.predicate.value === 'http://www.w3.org/2000/01/rdf-schema#label') {
      subjectsWithLabels.add(quad.subject.value);
    }
    subjectsWithoutLabels.add(quad.subject.value);
  }

  const unlabeledSubjects = [...subjectsWithoutLabels].filter(s => !subjectsWithLabels.has(s));
  if (unlabeledSubjects.length > 0) {
    issues.push({
      type: 'info',
      message: `${unlabeledSubjects.length} subjects without rdfs:label`,
      count: unlabeledSubjects.length,
    });
    score -= Math.min(10, unlabeledSubjects.length);
  }

  if (untypedSubjects.length > 0) {
    recommendations.push('Add rdf:type statements for all subjects');
  }
  if (unlabeledSubjects.length > 0) {
    recommendations.push('Add rdfs:label statements for better human readability');
  }

  assessment.addDimension('completeness', Math.max(0, score), issues, recommendations);
};

/**
 * Assess consistency dimension
 * @param {import('n3').Store} store - RDF store
 * @param {QualityAssessment} assessment - Assessment object
 */
const assessConsistency = (store, assessment) => {
  const issues = [];
  const recommendations = [];
  let score = 100;

  // Check for inconsistent datatypes
  const literalDatatypes = new Map();
  for (const quad of store) {
    if (quad.object.termType === 'Literal' && quad.object.datatype) {
      const datatype = quad.object.datatype.value;
      if (!literalDatatypes.has(datatype)) {
        literalDatatypes.set(datatype, []);
      }
      literalDatatypes.get(datatype).push(quad);
    }
  }

  // Check for inconsistent namespace usage
  const namespaceValidation = validateNamespaces(store);
  if (!namespaceValidation.valid) {
    issues.push({
      type: 'warning',
      message: `${namespaceValidation.issueCount} unknown namespaces found`,
      count: namespaceValidation.issueCount,
    });
    score -= Math.min(15, namespaceValidation.issueCount * 3);
  }

  // Check for inconsistent blank node usage
  const blankNodes = new Set();
  for (const quad of store) {
    if (quad.subject.termType === 'BlankNode') {
      blankNodes.add(quad.subject.value);
    }
    if (quad.object.termType === 'BlankNode') {
      blankNodes.add(quad.object.value);
    }
  }

  if (blankNodes.size > 0) {
    issues.push({
      type: 'info',
      message: `${blankNodes.size} blank nodes found`,
      count: blankNodes.size,
    });
    score -= Math.min(5, blankNodes.size);
  }

  if (namespaceValidation.issueCount > 0) {
    recommendations.push('Use consistent namespace declarations');
  }
  if (blankNodes.size > 0) {
    recommendations.push(
      'Consider using named nodes instead of blank nodes for better interoperability'
    );
  }

  assessment.addDimension('consistency', Math.max(0, score), issues, recommendations);
};

/**
 * Assess accuracy dimension
 * @param {import('n3').Store} store - RDF store
 * @param {QualityAssessment} assessment - Assessment object
 */
const assessAccuracy = (store, assessment) => {
  const issues = [];
  const recommendations = [];
  let score = 100;

  // Check for invalid IRIs
  const invalidIRIs = [];
  for (const quad of store) {
    if (quad.subject.termType === 'NamedNode') {
      try {
        new URL(quad.subject.value);
      } catch {
        invalidIRIs.push(quad.subject.value);
      }
    }
    if (quad.predicate.termType === 'NamedNode') {
      try {
        new URL(quad.predicate.value);
      } catch {
        invalidIRIs.push(quad.predicate.value);
      }
    }
    if (quad.object.termType === 'NamedNode') {
      try {
        new URL(quad.object.value);
      } catch {
        invalidIRIs.push(quad.object.value);
      }
    }
  }

  if (invalidIRIs.length > 0) {
    issues.push({
      type: 'error',
      message: `${invalidIRIs.length} invalid IRIs found`,
      count: invalidIRIs.length,
    });
    score -= Math.min(30, invalidIRIs.length * 5);
  }

  // Check for malformed literals
  const malformedLiterals = [];
  for (const quad of store) {
    if (quad.object.termType === 'Literal') {
      const value = quad.object.value;
      const datatype = quad.object.datatype?.value;

      // Check for empty literals
      if (value.trim() === '') {
        malformedLiterals.push({ quad, issue: 'empty literal' });
      }

      // Check for XML schema validation
      if (datatype === 'http://www.w3.org/2001/XMLSchema#integer') {
        if (!/^-?\d+$/.test(value)) {
          malformedLiterals.push({ quad, issue: 'invalid integer' });
        }
      } else if (
        datatype === 'http://www.w3.org/2001/XMLSchema#boolean' &&
        !/^(true|false)$/.test(value)
      ) {
        malformedLiterals.push({ quad, issue: 'invalid boolean' });
      }
    }
  }

  if (malformedLiterals.length > 0) {
    issues.push({
      type: 'error',
      message: `${malformedLiterals.length} malformed literals found`,
      count: malformedLiterals.length,
    });
    score -= Math.min(20, malformedLiterals.length * 3);
  }

  if (invalidIRIs.length > 0) {
    recommendations.push('Fix invalid IRIs');
  }
  if (malformedLiterals.length > 0) {
    recommendations.push('Fix malformed literals');
  }

  assessment.addDimension('accuracy', Math.max(0, score), issues, recommendations);
};

/**
 * Assess validity dimension
 * @param {import('n3').Store} store - RDF store
 * @param {QualityAssessment} assessment - Assessment object
 */
const assessValidity = (store, assessment) => {
  const issues = [];
  const recommendations = [];
  let score = 100;

  // Check for RDF constraints violations
  for (const quad of store) {
    // Predicate must be a named node
    if (quad.predicate.termType !== 'NamedNode') {
      issues.push({
        type: 'error',
        message: 'Predicate must be a named node',
        quad,
      });
      score -= 10;
    }

    // Subject cannot be a literal
    if (quad.subject.termType === 'Literal') {
      issues.push({
        type: 'error',
        message: 'Subject cannot be a literal',
        quad,
      });
      score -= 10;
    }
  }

  // Check for duplicate quads
  const quadStrings = new Set();
  const duplicates = [];
  for (const quad of store) {
    const quadStr = `${quad.subject.value} ${quad.predicate.value} ${quad.object.value} ${quad.graph?.value || ''}`;
    if (quadStrings.has(quadStr)) {
      duplicates.push(quad);
    } else {
      quadStrings.add(quadStr);
    }
  }

  if (duplicates.length > 0) {
    issues.push({
      type: 'warning',
      message: `${duplicates.length} duplicate quads found`,
      count: duplicates.length,
    });
    score -= Math.min(15, duplicates.length * 2);
  }

  if (duplicates.length > 0) {
    recommendations.push('Remove duplicate quads');
  }

  assessment.addDimension('validity', Math.max(0, score), issues, recommendations);
};

/**
 * Assess uniqueness dimension
 * @param {import('n3').Store} store - RDF store
 * @param {QualityAssessment} assessment - Assessment object
 */
const assessUniqueness = (store, assessment) => {
  const issues = [];
  const recommendations = [];
  let score = 100;

  // Check for duplicate subjects with different types
  const subjectTypes = new Map();
  for (const quad of store) {
    if (quad.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type') {
      const subject = quad.subject.value;
      if (!subjectTypes.has(subject)) {
        subjectTypes.set(subject, []);
      }
      subjectTypes.get(subject).push(quad.object.value);
    }
  }

  const conflictingTypes = [];
  for (const [subject, types] of subjectTypes) {
    if (types.length > 1) {
      conflictingTypes.push({ subject, types });
    }
  }

  if (conflictingTypes.length > 0) {
    issues.push({
      type: 'warning',
      message: `${conflictingTypes.length} subjects with multiple types`,
      count: conflictingTypes.length,
    });
    score -= Math.min(10, conflictingTypes.length * 2);
  }

  // Check for duplicate property values
  const propertyValues = new Map();
  for (const quad of store) {
    const key = `${quad.subject.value} ${quad.predicate.value}`;
    if (!propertyValues.has(key)) {
      propertyValues.set(key, []);
    }
    propertyValues.get(key).push(quad.object.value);
  }

  const duplicateValues = [];
  for (const [key, values] of propertyValues) {
    const uniqueValues = [...new Set(values)];
    if (values.length > uniqueValues.length) {
      duplicateValues.push({ key, values, uniqueValues });
    }
  }

  if (duplicateValues.length > 0) {
    issues.push({
      type: 'info',
      message: `${duplicateValues.length} properties with duplicate values`,
      count: duplicateValues.length,
    });
    score -= Math.min(5, duplicateValues.length);
  }

  if (conflictingTypes.length > 0) {
    recommendations.push('Review subjects with multiple types');
  }
  if (duplicateValues.length > 0) {
    recommendations.push('Consider removing duplicate property values');
  }

  assessment.addDimension('uniqueness', Math.max(0, score), issues, recommendations);
};

/**
 * Assess timeliness dimension
 * @param {import('n3').Store} store - RDF store
 * @param {QualityAssessment} assessment - Assessment object
 */
const assessTimeliness = (store, assessment) => {
  const issues = [];
  const recommendations = [];
  let score = 100;

  // Check for temporal information
  const temporalProperties = new Set([
    'http://purl.org/dc/terms/created',
    'http://purl.org/dc/terms/modified',
    'http://purl.org/dc/terms/date',
    'http://www.w3.org/2002/07/owl#versionInfo',
  ]);

  const temporalQuads = [];
  for (const quad of store) {
    if (temporalProperties.has(quad.predicate.value)) {
      temporalQuads.push(quad);
    }
  }

  if (temporalQuads.length === 0) {
    issues.push({
      type: 'info',
      message: 'No temporal information found',
      count: 0,
    });
    score -= 10;
  }

  // Check for valid date formats
  const invalidDates = [];
  for (const quad of temporalQuads) {
    if (quad.object.termType === 'Literal') {
      const value = quad.object.value;
      const datatype = quad.object.datatype?.value;

      if (datatype === 'http://www.w3.org/2001/XMLSchema#dateTime') {
        try {
          new Date(value);
        } catch {
          invalidDates.push(quad);
        }
      }
    }
  }

  if (invalidDates.length > 0) {
    issues.push({
      type: 'error',
      message: `${invalidDates.length} invalid date formats found`,
      count: invalidDates.length,
    });
    score -= Math.min(15, invalidDates.length * 3);
  }

  if (temporalQuads.length === 0) {
    recommendations.push('Add temporal information (creation/modification dates)');
  }
  if (invalidDates.length > 0) {
    recommendations.push('Fix invalid date formats');
  }

  assessment.addDimension('timeliness', Math.max(0, score), issues, recommendations);
};

/**
 * Assess accessibility dimension
 * @param {import('n3').Store} store - RDF store
 * @param {QualityAssessment} assessment - Assessment object
 */
const assessAccessibility = (store, assessment) => {
  const issues = [];
  const recommendations = [];
  let score = 100;

  // Check for human-readable labels
  const subjectsWithLabels = new Set();
  const subjectsWithoutLabels = new Set();

  for (const quad of store) {
    if (quad.predicate.value === 'http://www.w3.org/2000/01/rdf-schema#label') {
      subjectsWithLabels.add(quad.subject.value);
    }
    subjectsWithoutLabels.add(quad.subject.value);
  }

  const unlabeledSubjects = [...subjectsWithoutLabels].filter(s => !subjectsWithLabels.has(s));
  if (unlabeledSubjects.length > 0) {
    issues.push({
      type: 'warning',
      message: `${unlabeledSubjects.length} subjects without human-readable labels`,
      count: unlabeledSubjects.length,
    });
    score -= Math.min(20, unlabeledSubjects.length * 2);
  }

  // Check for descriptions
  const subjectsWithDescriptions = new Set();
  for (const quad of store) {
    if (quad.predicate.value === 'http://www.w3.org/2000/01/rdf-schema#comment') {
      subjectsWithDescriptions.add(quad.subject.value);
    }
  }

  const undescribedSubjects = [...subjectsWithoutLabels].filter(
    s => !subjectsWithDescriptions.has(s)
  );
  if (undescribedSubjects.length > 0) {
    issues.push({
      type: 'info',
      message: `${undescribedSubjects.length} subjects without descriptions`,
      count: undescribedSubjects.length,
    });
    score -= Math.min(10, undescribedSubjects.length);
  }

  // Check for multilingual support
  const multilingualLabels = [];
  for (const quad of store) {
    if (
      quad.predicate.value === 'http://www.w3.org/2000/01/rdf-schema#label' &&
      quad.object.language
    ) {
      multilingualLabels.push(quad);
    }
  }

  if (multilingualLabels.length === 0) {
    issues.push({
      type: 'info',
      message: 'No multilingual labels found',
      count: 0,
    });
    score -= 5;
  }

  if (unlabeledSubjects.length > 0) {
    recommendations.push('Add human-readable labels for all subjects');
  }
  if (undescribedSubjects.length > 0) {
    recommendations.push('Add descriptions for better understanding');
  }
  if (multilingualLabels.length === 0) {
    recommendations.push('Consider adding multilingual labels');
  }

  assessment.addDimension('accessibility', Math.max(0, score), issues, recommendations);
};

/**
 * Generate quality report
 * @param {QualityAssessment} assessment - Quality assessment
 * @param {Object} [options] - Report options
 * @returns {string} Quality report
 */
export const generateQualityReport = (assessment, options = {}) => {
  const { format = 'text' } = options;

  if (format === 'json') {
    return JSON.stringify(assessment, null, 2);
  }

  let report = `# Data Quality Assessment Report\n\n`;
  report += `**Overall Score:** ${assessment.overallScore.toFixed(1)}/100 (Grade: ${assessment.getGrade()})\n\n`;

  report += `## Metrics\n`;
  for (const [name, value] of Object.entries(assessment.metrics)) {
    report += `- **${name}:** ${value}\n`;
  }
  report += `\n`;

  report += `## Quality Dimensions\n`;
  for (const [name, dimension] of Object.entries(assessment.dimensions)) {
    report += `### ${name.charAt(0).toUpperCase() + name.slice(1)} (${dimension.score.toFixed(1)}/100)\n`;

    if (dimension.issues.length > 0) {
      report += `**Issues:**\n`;
      for (const issue of dimension.issues) {
        report += `- ${issue.message}\n`;
      }
    }

    if (dimension.recommendations.length > 0) {
      report += `**Recommendations:**\n`;
      for (const rec of dimension.recommendations) {
        report += `- ${rec}\n`;
      }
    }
    report += `\n`;
  }

  if (assessment.issues.length > 0) {
    report += `## All Issues\n`;
    for (const issue of assessment.issues) {
      report += `- ${issue.message}\n`;
    }
    report += `\n`;
  }

  if (assessment.recommendations.length > 0) {
    report += `## All Recommendations\n`;
    for (const rec of assessment.recommendations) {
      report += `- ${rec}\n`;
    }
  }

  return report;
};

/**
 * Fix common quality issues
 * @param {import('n3').Store} store - RDF store to fix
 * @param {Object} [options] - Fix options
 * @returns {Object} Fix result
 */
export const fixQualityIssues = (store, options = {}) => {
  const fixedStore = createStore();
  for (const quad of store) {
    fixedStore.add(quad);
  }
  const fixes = [];

  // Remove duplicate quads
  if (options.removeDuplicates) {
    const _originalSize = fixedStore.size;
    const seenQuads = new Set();
    const quadsToRemove = [];

    for (const quad of fixedStore) {
      const quadStr = `${quad.subject.value} ${quad.predicate.value} ${quad.object.value} ${quad.graph?.value || ''}`;
      if (seenQuads.has(quadStr)) {
        quadsToRemove.push(quad);
      } else {
        seenQuads.add(quadStr);
      }
    }

    for (const quad of quadsToRemove) {
      fixedStore.delete(quad);
    }

    if (quadsToRemove.length > 0) {
      fixes.push(`Removed ${quadsToRemove.length} duplicate quads`);
    }
  }

  // Fix invalid IRIs
  if (options.fixInvalidIRIs) {
    const invalidIRIs = [];
    for (const quad of fixedStore) {
      if (quad.subject.termType === 'NamedNode') {
        try {
          new URL(quad.subject.value);
        } catch {
          invalidIRIs.push(quad.subject.value);
        }
      }
    }

    if (invalidIRIs.length > 0) {
      fixes.push(`Found ${invalidIRIs.length} invalid IRIs (manual review required)`);
    }
  }

  return {
    store: fixedStore,
    fixes,
    fixCount: fixes.length,
  };
};
