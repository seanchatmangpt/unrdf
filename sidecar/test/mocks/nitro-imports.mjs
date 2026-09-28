/**
 * Minimal stand-in for Nitro's auto-import module (`#imports`) so server
 * middleware and handlers can be unit tested without booting Nuxt.
 */

export const defineEventHandler = (handler) => handler;

export function createError({ statusCode, statusMessage, message, data }) {
  const error = new Error(message || statusMessage);
  error.statusCode = statusCode;
  error.statusMessage = statusMessage;
  error.data = data;
  return error;
}

export const getMethod = (event) => event.node.req.method;
export const getRequestHeader = (event, name) => event.node.req.headers[name.toLowerCase()];
export const getRequestIP = (event) => event.node.req.socket?.remoteAddress;
export const getQuery = (event) => event.query || {};
export const readBody = async (event) => event.body;

export function setResponseHeaders(event, headers) {
  Object.assign(event.responseHeaders, headers);
}

export function setResponseStatus(event, code) {
  event.responseStatus = code;
}

/**
 * Build a fake H3 event
 * @param {Object} opts
 * @returns {Object}
 */
export function makeEvent({
  path = '/api/hooks/list',
  method = 'GET',
  headers = {},
  ip = '10.0.0.1',
  auth,
  body,
  query
} = {}) {
  return {
    path,
    query,
    body,
    context: auth ? { auth } : {},
    responseHeaders: {},
    responseStatus: 200,
    node: { req: { method, url: path, headers, socket: { remoteAddress: ip } } }
  };
}
