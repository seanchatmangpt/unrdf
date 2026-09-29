/**
 * @file Authorization Middleware
 * @module sidecar/server/middleware/authorization
 *
 * Role-Based Access Control middleware:
 * - Runs after authentication (01.authentication.mjs)
 * - Extracts roles from JWT
 * - Validates permissions using RBAC engine
 * - Logs denied access attempts
 */

import { defineEventHandler, createError, getMethod, getRequestHeader, getQuery, getRequestIP } from '#imports';
import { trace } from '@opentelemetry/api';
import { getRBACEngine, Resources, Actions } from '../utils/rbac.mjs';
import logger from '../utils/logger.mjs';

const tracer = trace.getTracer('authorization-middleware');

/**
 * Map HTTP methods to RBAC actions
 * @param {string} method
 * @returns {string}
 */
function methodToAction(method) {
  const mapping = {
    GET: Actions.READ,
    HEAD: Actions.READ,
    OPTIONS: Actions.READ,
    POST: Actions.WRITE,
    PUT: Actions.WRITE,
    PATCH: Actions.WRITE,
    DELETE: Actions.DELETE
  };

  return mapping[method.toUpperCase()] || Actions.READ;
}

/**
 * Map request path to resource
 * @param {string} path
 * @returns {string}
 */
function pathToResource(path) {
  // Extract resource from path
  if (path.startsWith('/api/hooks')) return Resources.KNOWLEDGE_HOOK;
  if (path.startsWith('/api/effects')) return Resources.EFFECT;
  if (path.startsWith('/api/transaction')) return Resources.TRANSACTION;
  if (path.startsWith('/api/policy') || path.startsWith('/api/policies')) return Resources.POLICY;
  if (path.startsWith('/api/admin/roles')) return Resources.ROLE;
  if (path.startsWith('/api/admin')) return Resources.SYSTEM;
  if (path.startsWith('/api/audit')) return Resources.AUDIT_LOG;

  return 'unknown';
}

/**
 * Public endpoints that skip authorization (authentication is skipped for the same set in 00.auth)
 */
const PUBLIC_PATHS = [
  '/api/health',
  '/api/auth/login',
  '/api/auth/register',
  '/api/auth/refresh',
  '/metrics',
  '/docs',
  '/openapi.json',
  '/_nuxt',
  '/favicon.ico'
];

/**
 * Build an h3 error
 * @param {number} statusCode
 * @param {string} statusText
 * @param {string} message
 * @param {Object} [data]
 * @returns {Error}
 */
function httpError(statusCode, statusText, message, data) {
  return createError({ statusCode, statusMessage: statusText, message, data });
}

/**
 * Authorization middleware (h3 event handler)
 * Validates user permissions using RBAC. Requires 00.auth to have set event.context.auth.
 */
export default defineEventHandler(async (event) => {
  return tracer.startActiveSpan('authorization.middleware', async (span) => {
    try {
      const path = (event.path || event.node.req.url || '').split('?')[0];
      const method = getMethod(event);

      // Only API routes are subject to RBAC; pages/assets are not
      if (!path.startsWith('/api/') || PUBLIC_PATHS.some(p => path.startsWith(p))) {
        span.setAttribute('authorization.skipped', true);
        return;
      }

      const auth = event.context.auth;
      if (!auth || !auth.userId) {
        span.setAttribute('authorization.failed', true);
        span.setAttribute('authorization.reason', 'not_authenticated');
        logger.warn('Authorization failed: Not authenticated', { path, method });
        throw httpError(401, 'Unauthorized', 'Authentication required');
      }

      const userId = auth.userId;
      const roles = auth.roles || [];

      span.setAttributes({
        'authorization.user_id': userId,
        'authorization.roles': roles.join(','),
        'authorization.path': path,
        'authorization.method': method
      });

      const rbac = getRBACEngine();

      // Auto-assign roles from the JWT if the RBAC engine does not know the user yet
      if (rbac.getUserRoles(userId).length === 0) {
        for (const role of roles) {
          rbac.assignRole(userId, role);
        }
      }

      const resource = pathToResource(path);
      const action = methodToAction(method);

      span.setAttributes({
        'authorization.resource': resource,
        'authorization.action': action
      });

      const attributes = {
        path,
        method,
        ip: getRequestIP(event),
        userAgent: getRequestHeader(event, 'user-agent'),
        query: getQuery(event)
      };

      let decision;
      try {
        decision = await rbac.evaluate(userId, resource, action, attributes);
      } catch (error) {
        span.recordException(error);
        logger.error('Authorization evaluation error', {
          error: error.message,
          userId,
          resource,
          action,
          path
        });
        throw httpError(500, 'Internal Server Error', 'Authorization evaluation failed');
      }

      span.setAttribute('authorization.decision', decision.allowed ? 'allow' : 'deny');
      span.setAttribute('authorization.decision_id', decision.decisionId);
      event.context.authDecision = decision;

      if (!decision.allowed) {
        logger.warn('Authorization denied', {
          userId,
          resource,
          action,
          path,
          method,
          reason: decision.reason,
          decisionId: decision.decisionId
        });
        throw httpError(403, 'Forbidden', decision.reason, { decisionId: decision.decisionId });
      }

      logger.debug('Authorization granted', {
        userId,
        resource,
        action,
        path,
        decisionId: decision.decisionId
      });
    } catch (error) {
      if (!error.statusCode) {
        span.recordException(error);
        logger.error('Authorization middleware error', { error: error.message });
        throw httpError(500, 'Internal Server Error', 'Authorization failed');
      }
      throw error;
    } finally {
      span.end();
    }
  });
});

/**
 * Assert that the authenticated user holds at least one of the roles.
 * Call from inside an h3 handler.
 * @param {import('h3').H3Event} event
 * @param {...string} requiredRoles
 * @throws {Error} h3 401/403 error
 */
export function requireRoles(event, ...requiredRoles) {
  const userId = event.context.auth?.userId;
  if (!userId) {
    throw httpError(401, 'Unauthorized', 'Authentication required');
  }

  const userRoles = getRBACEngine().getUserRoles(userId);
  if (!requiredRoles.some(role => userRoles.includes(role))) {
    logger.warn('Role check failed', { userId, requiredRoles, userRoles, path: event.path });
    throw httpError(403, 'Forbidden', `Required role: ${requiredRoles.join(' or ')}`);
  }
}

/**
 * Assert that the authenticated user holds a permission.
 * Call from inside an h3 handler.
 * @param {import('h3').H3Event} event
 * @param {string} resource
 * @param {string} action
 * @returns {Promise<Object>} the RBAC decision
 * @throws {Error} h3 401/403/500 error
 */
export async function requirePermission(event, resource, action) {
  const userId = event.context.auth?.userId;
  if (!userId) {
    throw httpError(401, 'Unauthorized', 'Authentication required');
  }

  let decision;
  try {
    decision = await getRBACEngine().evaluate(userId, resource, action);
  } catch (error) {
    logger.error('Permission check error', { error: error.message, userId, resource, action });
    throw httpError(500, 'Internal Server Error', 'Permission check failed');
  }

  if (!decision.allowed) {
    logger.warn('Permission check failed', { userId, resource, action, reason: decision.reason });
    throw httpError(403, 'Forbidden', decision.reason, { decisionId: decision.decisionId });
  }
  event.context.authDecision = decision;
  return decision;
}
