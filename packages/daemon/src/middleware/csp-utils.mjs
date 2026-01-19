/**
 * @file CSP Utilities
 * @module @unrdf/daemon/middleware/csp-utils
 * @description Content Security Policy generation and nonce utilities
 */

import { randomBytes } from 'crypto';
import { CSPConfigSchema } from './security-schemas.mjs';

/**
 * Generate CSP header value
 * @param {Object} cspConfig - CSP configuration
 * @param {string} [nonce] - Optional nonce for inline scripts
 * @returns {string} CSP header value
 */
export function generateCSPHeader(cspConfig, nonce) {
  const csp = cspConfig || CSPConfigSchema.parse({});
  const directives = [];

  const addDirective = (name, values) => {
    if (values && values.length > 0) {
      const valueStr = values.map(v => {
        if (v === "'nonce'" && nonce) {
          return `'nonce-${nonce}'`;
        }
        return v;
      }).join(' ');
      directives.push(`${name} ${valueStr}`);
    }
  };

  addDirective('default-src', csp.defaultSrc);
  addDirective('script-src', csp.scriptSrc);
  addDirective('style-src', csp.styleSrc);
  addDirective('img-src', csp.imgSrc);
  addDirective('connect-src', csp.connectSrc);
  addDirective('font-src', csp.fontSrc);
  addDirective('object-src', csp.objectSrc);
  addDirective('media-src', csp.mediaSrc);
  addDirective('frame-src', csp.frameSrc);

  if (csp.reportUri) {
    directives.push(`report-uri ${csp.reportUri}`);
  }

  return directives.join('; ');
}

/**
 * Generate random nonce for CSP
 * @returns {string} Base64 nonce
 */
export function generateNonce() {
  return randomBytes(16).toString('base64');
}
