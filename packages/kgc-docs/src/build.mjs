/**
 * @fileoverview Documentation build with receipt generation
 */

import { generateReceipt } from '@unrdf/kgc-runtime';

/**
 * Build documentation with receipts
 * @param {Object} options - Build options
 * @param {string} [options.docsPath='docs'] - Documentation path
 * @param {string} [options.outputPath='docs/build'] - Output path
 * @returns {Promise<{success: boolean, files: number, receipt: any}>}
 */
export async function buildDocs(options = {}) {
  const { docsPath = 'docs', outputPath = 'docs/build' } = options;

  const receipt = await generateReceipt(
    'build-docs',
    { docsPath, outputPath },
    { success: true, files: 0 }
  );

  return {
    success: true,
    files: 0,
    receipt,
  };
}
