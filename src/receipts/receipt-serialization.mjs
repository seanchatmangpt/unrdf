/**
 * @fileoverview Receipt Serialization - JSON-LD, Turtle, and binary formats
 *
 * @module receipts/receipt-serialization
 */

import { UniversalReceiptSchema } from './receipt-schemas.mjs';

// ============================================================================
// RDF Namespace Prefixes
// ============================================================================

/**
 * RDF Namespace prefixes
 */
const RDF_PREFIXES = {
  unrdf: 'https://unrdf.org/receipts#',
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  prov: 'http://www.w3.org/ns/prov#',
  dct: 'http://purl.org/dc/terms/',
};

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Capitalize first letter
 * @param {string} str
 * @returns {string}
 */
function capitalize(str) {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

/**
 * Escape string for TTL
 * @param {string} str
 * @returns {string}
 */
function escapeString(str) {
  return str
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n')
    .replace(/\r/g, '\\r');
}

// ============================================================================
// Serialization Functions
// ============================================================================

/**
 * Convert receipt to JSON-LD format
 *
 * @param {import('./receipt-schemas.mjs').UniversalReceipt} receipt
 * @returns {object} JSON-LD representation
 */
export function receiptToJSONLD(receipt) {
  return {
    '@context': {
      unrdf: RDF_PREFIXES.unrdf,
      rdf: RDF_PREFIXES.rdf,
      xsd: RDF_PREFIXES.xsd,
      prov: RDF_PREFIXES.prov,
      dct: RDF_PREFIXES.dct,
    },
    '@id': receipt.id,
    '@type': `unrdf:${capitalize(receipt.type)}Receipt`,
    'unrdf:version': receipt.version,
    'unrdf:package': receipt.package,
    'unrdf:epoch': receipt.epoch,
    'prov:generatedAtTime': {
      '@type': 'xsd:dateTime',
      '@value': receipt.timestamp,
    },
    'unrdf:decision': receipt.decision,
    'dct:description': receipt.reason,
    'prov:wasAttributedTo': {
      '@type': 'prov:Agent',
      'prov:label': receipt.provenance.agent,
    },
    'unrdf:toolchain': {
      'unrdf:nodeVersion': receipt.toolchain.node,
      'unrdf:platform': receipt.toolchain.platform,
    },
    'unrdf:inputHash': receipt.input.hashes,
    'unrdf:outputHash': receipt.output.hash,
    'unrdf:beforeHash': receipt.beforeHash,
    'unrdf:merkleRoot': receipt.merkleRoot,
    'unrdf:receiptHash': receipt.receiptHash,
    'unrdf:extension': receipt.extension,
  };
}

/**
 * Convert receipt to Turtle (TTL) format
 *
 * @param {import('./receipt-schemas.mjs').UniversalReceipt} receipt
 * @returns {string} Turtle serialization
 */
export function receiptToTurtle(receipt) {
  const lines = [
    `@prefix unrdf: <${RDF_PREFIXES.unrdf}> .`,
    `@prefix rdf: <${RDF_PREFIXES.rdf}> .`,
    `@prefix xsd: <${RDF_PREFIXES.xsd}> .`,
    `@prefix prov: <${RDF_PREFIXES.prov}> .`,
    `@prefix dct: <${RDF_PREFIXES.dct}> .`,
    '',
    `<${receipt.id}> a unrdf:${capitalize(receipt.type)}Receipt ;`,
    `  unrdf:version "${receipt.version}" ;`,
    `  unrdf:package "${receipt.package}" ;`,
    `  unrdf:epoch "${receipt.epoch}" ;`,
    `  prov:generatedAtTime "${receipt.timestamp}"^^xsd:dateTime ;`,
    `  unrdf:decision "${receipt.decision}" ;`,
    `  dct:description "${escapeString(receipt.reason)}" ;`,
    `  prov:wasAttributedTo [ prov:label "${receipt.provenance.agent}" ] ;`,
    `  unrdf:outputHash "${receipt.output.hash}" ;`,
  ];

  if (receipt.beforeHash) {
    lines.push(`  unrdf:beforeHash "${receipt.beforeHash}" ;`);
  }

  if (receipt.merkleRoot) {
    lines.push(`  unrdf:merkleRoot "${receipt.merkleRoot}" ;`);
  }

  lines.push(`  unrdf:receiptHash "${receipt.receiptHash}" .`);

  return lines.join('\n');
}

/**
 * Convert receipt to binary format (for efficient storage)
 *
 * @param {import('./receipt-schemas.mjs').UniversalReceipt} receipt
 * @returns {Uint8Array} Binary representation
 */
export function receiptToBinary(receipt) {
  const json = JSON.stringify(receipt);
  return new TextEncoder().encode(json);
}

/**
 * Parse receipt from binary format
 *
 * @param {Uint8Array} binary
 * @returns {import('./receipt-schemas.mjs').UniversalReceipt}
 */
export function receiptFromBinary(binary) {
  const json = new TextDecoder().decode(binary);
  return UniversalReceiptSchema.parse(JSON.parse(json));
}
