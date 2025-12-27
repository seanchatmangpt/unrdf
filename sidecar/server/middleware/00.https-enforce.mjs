/**
 * HTTPS Enforcement Middleware with TLS 1.3 Requirement
 * Redirects HTTP to HTTPS and enforces strict TLS settings
 */

/**
 * @typedef {Object} TLSInfo
 * @property {string} protocol - TLS protocol version
 * @property {string} cipher - Cipher suite
 */

/**
 * HTTPS enforcement middleware
 * @param {import('h3').H3Event} event - H3 event
 */
export default defineEventHandler((event) => {
  const req = event.node.req;
  const res = event.node.res;

  // Skip in development if HTTPS not configured
  if (process.env.NODE_ENV === 'development' && !process.env.ENFORCE_HTTPS) {
    return;
  }

  // Check if request is HTTPS
  const isHttps = req.connection.encrypted ||
                  req.headers['x-forwarded-proto'] === 'https';

  if (!isHttps) {
    const host = req.headers.host || 'localhost';
    const redirectUrl = `https://${host}${req.url}`;

    // Redirect to HTTPS
    res.writeHead(301, {
      'Location': redirectUrl,
      'Strict-Transport-Security': 'max-age=31536000; includeSubDomains; preload'
    });
    res.end();
    return;
  }

  // Verify TLS 1.3 is used (if TLS connection info available)
  if (req.connection.encrypted) {
    const tlsSocket = /** @type {import('tls').TLSSocket} */ (req.socket);
    const protocol = tlsSocket.getProtocol?.();

    if (protocol && !protocol.includes('TLSv1.3')) {
      res.writeHead(426, {
        'Content-Type': 'application/json',
        'Upgrade': 'TLS/1.3'
      });
      res.end(JSON.stringify({
        error: 'TLS 1.3 Required',
        message: 'This service requires TLS 1.3 or higher',
        currentProtocol: protocol
      }));
      return;
    }
  }

  // Set HSTS headers for HTTPS responses
  setResponseHeaders(event, {
    'Strict-Transport-Security': 'max-age=31536000; includeSubDomains; preload',
    'X-Content-Type-Options': 'nosniff',
    'X-Frame-Options': 'DENY',
    'X-XSS-Protection': '1; mode=block'
  });
});
