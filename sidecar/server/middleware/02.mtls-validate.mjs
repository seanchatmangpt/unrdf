/**
 * mTLS Client Certificate Validation Middleware
 * Validates client certificates using Byzantine consensus
 */

import { validateClientCertificate } from '../utils/mtls-validator.mjs';

/**
 * Validate mTLS client certificate
 */
export default defineEventHandler(async (event) => {
  const config = useRuntimeConfig();

  // Skip if mTLS is disabled
  if (!config.mtlsEnabled) {
    return;
  }

  // Skip in development if not explicitly required
  if (process.env.NODE_ENV === 'development' && !config.mtlsRequireClientCert) {
    return;
  }

  const req = event.node.req;

  // Check if request has TLS connection
  if (!req.socket.encrypted) {
    // Not an HTTPS connection - let HTTPS enforcement middleware handle
    return;
  }

  // Get client certificate from TLS socket
  const tlsSocket = /** @type {import('tls').TLSSocket} */ (req.socket);
  const peerCert = tlsSocket.getPeerCertificate();

  // Validate with Byzantine consensus
  const validation = await validateClientCertificate(peerCert);

  if (!validation.valid) {
    throw createError({
      statusCode: 401,
      statusMessage: 'Unauthorized',
      message: `Client certificate validation failed: ${validation.reason}`,
      data: {
        consensusVotes: validation.votes
      }
    });
  }

  // Store certificate info in event context for later use
  event.context.clientCert = {
    subject: peerCert.subject,
    issuer: peerCert.issuer,
    fingerprint: peerCert.fingerprint,
    validFrom: peerCert.valid_from,
    validTo: peerCert.valid_to,
    consensusVotes: validation.votes
  };
});
