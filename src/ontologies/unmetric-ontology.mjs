/**
 * @file UNRDF Code Metrics Ontology - Named node constants
 * @module ontologies/unmetric
 */

import { dataFactory } from '@unrdf/oxigraph';

const { namedNode } = dataFactory;

// Base namespace
const UNMETRIC = 'http://example.org/unrdf/metrics#';

// Classes
export const ComplexityReport = namedNode(UNMETRIC + 'ComplexityReport');
export const FileComplexity = namedNode(UNMETRIC + 'FileComplexity');
export const FunctionComplexity = namedNode(UNMETRIC + 'FunctionComplexity');

// Metric Properties
export const cyclomatic = namedNode(UNMETRIC + 'cyclomatic');
export const halsteadVolume = namedNode(UNMETRIC + 'halsteadVolume');
export const maintainabilityIndex = namedNode(UNMETRIC + 'maintainabilityIndex');
export const linesOfCode = namedNode(UNMETRIC + 'linesOfCode');
export const location = namedNode(UNMETRIC + 'location');

// Relationship Properties
export const hasFileComplexity = namedNode(UNMETRIC + 'hasFileComplexity');
export const hasFunctionComplexity = namedNode(UNMETRIC + 'hasFunctionComplexity');

// Metadata Properties
export const analysisMode = namedNode(UNMETRIC + 'analysisMode');
export const filesAnalyzed = namedNode(UNMETRIC + 'filesAnalyzed');
export const analysisTimestamp = namedNode(UNMETRIC + 'analysisTimestamp');

// Severity Flags
export const highComplexity = namedNode(UNMETRIC + 'highComplexity');
export const lowMaintainability = namedNode(UNMETRIC + 'lowMaintainability');

// Export namespace for use in other modules
export const UNMETRIC_NAMESPACE = UNMETRIC;
