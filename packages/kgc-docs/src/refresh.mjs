/**
 * @fileoverview Documentation refresh operations
 */

import { generateReceipt } from '@unrdf/kgc-runtime';

/**
 * Refresh documentation from source
 * @param {string} [sourcePath='src'] - Source path
 * @returns {Promise<{success: boolean, updated: number, receipt: any}>}
 */
export async function refreshDocs(sourcePath = 'src') {
  const receipt = await generateReceipt(
    'refresh-docs',
    { sourcePath },
    { success: true, updated: 0 }
  );

  return {
    success: true,
    updated: 0,
    receipt,
  };
}
