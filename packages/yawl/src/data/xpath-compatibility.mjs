/**
 * @file XPath Compatibility Layer
 * @module @unrdf/yawl/data/xpath-compatibility
 * @description Subset of XPath for legacy YAWL specs. Maps XPath to JSONPath where possible.
 * Warns on unsupported XPath features. Provides migration guide.
 */

import { z } from 'zod';
import { evaluateJSONPath } from '../multiple-instance/expression-evaluator.mjs';

// =============================================================================
// XPath Patterns and Mappings
// =============================================================================

/**
 * XPath to JSONPath mapping rules
 */
const XPATH_MAPPINGS = [
  // count(//item) => $.items
  { pattern: /^count\(\/\/(\w+)\)$/, replacement: (match) => `$.${match[1]}s` },

  // /root/item => $.root.item
  { pattern: /^\/(\w+(?:\/\w+)*)$/, replacement: (match) => `$.${match[1].replace(/\//g, '.')}` },

  // //item => $.items (assumes plural)
  { pattern: /^\/\/(\w+)$/, replacement: (match) => `$.${match[1]}s` },

  // /root/item/@id => $.root.item.id
  { pattern: /^\/(\w+(?:\/\w+)*)\/\@(\w+)$/, replacement: (match) => `$.${match[1].replace(/\//g, '.')}.${match[2]}` },

  // count(/root/items/item) => $.root.items
  { pattern: /^count\(\/(\w+(?:\/\w+)*)\)$/, replacement: (match) => `$.${match[1].replace(/\//g, '.')}` },
];

/**
 * Unsupported XPath features
 */
const UNSUPPORTED_FEATURES = [
  { pattern: /\[.*\]/, feature: 'Predicates [...]' },
  { pattern: /::/, feature: 'Axes (ancestor::, child::, etc.)' },
  { pattern: /\bor\b|\band\b/, feature: 'Boolean operators (and, or)' },
  { pattern: /\+|\-|\*|\bdiv\b|\bmod\b/, feature: 'Arithmetic operators' },
  { pattern: /starts-with|contains|substring/, feature: 'String functions' },
  { pattern: /sum|avg|min|max/, feature: 'Aggregate functions (use aggregation-functions.mjs)' },
];

// =============================================================================
// XPath Conversion
// =============================================================================

/**
 * Convert XPath expression to JSONPath
 * @param {string} xpath - XPath expression
 * @returns {Object} Conversion result
 * @returns {boolean} result.success - Whether conversion succeeded
 * @returns {string} result.jsonpath - JSONPath equivalent (if successful)
 * @returns {string} result.warning - Warning message (if unsupported features detected)
 * @example
 * convertXPathToJSONPath('count(//item)') // => { success: true, jsonpath: '$.items' }
 * convertXPathToJSONPath('//item[@id>5]') // => { success: false, warning: 'Unsupported: Predicates' }
 */
export function convertXPathToJSONPath(xpath) {
  if (typeof xpath !== 'string') {
    return { success: false, warning: 'XPath must be a string' };
  }

  const trimmed = xpath.trim();

  for (const { pattern, feature } of UNSUPPORTED_FEATURES) {
    if (pattern.test(trimmed)) {
      return {
        success: false,
        warning: `Unsupported XPath feature: ${feature}`,
        xpath: trimmed,
      };
    }
  }

  for (const { pattern, replacement } of XPATH_MAPPINGS) {
    const match = trimmed.match(pattern);
    if (match) {
      const jsonpath = replacement(match);
      return {
        success: true,
        jsonpath,
        xpath: trimmed,
      };
    }
  }

  return {
    success: false,
    warning: 'No matching conversion rule found',
    xpath: trimmed,
  };
}

/**
 * Evaluate XPath expression (converts to JSONPath first)
 * @param {string} xpath - XPath expression
 * @param {Object} data - Data context
 * @returns {any} Evaluation result
 * @throws {Error} If XPath cannot be converted or evaluation fails
 * @example
 * evaluateXPath('count(//item)', { items: [1, 2, 3] }) // => 3
 */
export function evaluateXPath(xpath, data) {
  const conversion = convertXPathToJSONPath(xpath);

  if (!conversion.success) {
    throw new Error(`XPath conversion failed: ${conversion.warning}`);
  }

  console.warn(
    `XPath deprecated: "${xpath}" converted to JSONPath: "${conversion.jsonpath}". ` +
    `Please update your specification.`
  );

  return evaluateJSONPath(conversion.jsonpath, data);
}

// =============================================================================
// Migration Guide
// =============================================================================

/**
 * Get migration guide for XPath expression
 * @param {string} xpath - XPath expression
 * @returns {Object} Migration guide
 * @returns {string} guide.xpath - Original XPath
 * @returns {string} guide.jsonpath - Recommended JSONPath
 * @returns {string} guide.status - Migration status (supported|unsupported|manual)
 * @returns {string} guide.recommendation - Migration recommendation
 * @example
 * getMigrationGuide('count(//item)')
 * // => { xpath: 'count(//item)', jsonpath: '$.items', status: 'supported', ... }
 */
export function getMigrationGuide(xpath) {
  const conversion = convertXPathToJSONPath(xpath);

  if (conversion.success) {
    return {
      xpath,
      jsonpath: conversion.jsonpath,
      status: 'supported',
      recommendation: `Replace "${xpath}" with "${conversion.jsonpath}"`,
    };
  }

  return {
    xpath,
    jsonpath: null,
    status: 'unsupported',
    recommendation: `XPath "${xpath}" is not supported. ${conversion.warning}. Manual migration required.`,
  };
}

/**
 * Validate XPath compatibility
 * @param {string} xpath - XPath expression
 * @returns {Object} Validation result
 * @returns {boolean} result.compatible - Whether XPath is compatible
 * @returns {Array<string>} result.issues - Compatibility issues
 * @returns {string} result.jsonpath - Suggested JSONPath (if compatible)
 * @example
 * validateXPathCompatibility('count(//item)') // => { compatible: true, jsonpath: '$.items' }
 */
export function validateXPathCompatibility(xpath) {
  const issues = [];

  for (const { pattern, feature } of UNSUPPORTED_FEATURES) {
    if (pattern.test(xpath)) {
      issues.push(`Unsupported: ${feature}`);
    }
  }

  if (issues.length > 0) {
    return {
      compatible: false,
      issues,
      xpath,
    };
  }

  const conversion = convertXPathToJSONPath(xpath);

  return {
    compatible: conversion.success,
    issues: conversion.success ? [] : [conversion.warning],
    jsonpath: conversion.jsonpath,
    xpath,
  };
}

// =============================================================================
// Exports
// =============================================================================

export default {
  convertXPathToJSONPath,
  evaluateXPath,
  getMigrationGuide,
  validateXPathCompatibility,
};
