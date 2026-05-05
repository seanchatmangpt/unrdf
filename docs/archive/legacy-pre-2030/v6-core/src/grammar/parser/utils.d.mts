/**
 * @fileoverview Shared utility functions for grammar parsing
 * @module @unrdf/v6-core/grammar/parser/utils
 * @version 6.0.0-alpha.1
 */
/**
 * Count AST nodes (recursive)
 * @param {*} obj - AST object
 * @returns {number} Node count
 */
export function countASTNodes(obj: any): number;
/**
 * Create empty complexity bounds
 * @returns {Object} Empty complexity
 */
export function createEmptyComplexity(): any;
/**
 * Create parse receipt metadata
 * @param {string} grammarType - Grammar type
 * @returns {Object} Parse receipt
 */
export function createParseReceipt(grammarType: string): any;
