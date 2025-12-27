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
  if (path.startsWith('/api/transactions')) return Resources.TRANSACTION;
  if (path.startsWith('/api/policies')) return Resources.POLICY;
  if (path.startsWith('/api/admin/roles')) return Resources.ROLE;
  if (path.startsWith('/api/admin')) return Resources.SYSTEM;
  if (path.startsWith('/api/audit')) return Resources.AUDIT_LOG;

  return 'unknown';
}

/**
 * Authorization middleware
 * Validates user permissions using RBAC
 */
export default async function authorizationMiddleware(req, res, next) {
  return tracer.startActiveSpan('authorization.middleware', async (span) => {
    try {
      // Skip authorization for public endpoints
      const publicPaths = [
        '/health',
        '/metrics',
        '/api/auth/login',
        '/api/auth/register',
        '/docs',
        '/openapi.json'
      ];

      if (publicPaths.some(path => req.path.startsWith(path))) {
        span.setAttribute('authorization.skipped', true);
        span.setAttribute('authorization.reason', 'public_endpoint');
        return next();
      }

      // Check if user is authenticated
      if (!req.user || !req.user.id) {
        span.setAttribute('authorization.failed', true);
        span.setAttribute('authorization.reason', 'not_authenticated');

        logger.warn('Authorization failed: Not authenticated', {
          path: req.path,
          method: req.method,
          ip: req.ip
        });

        return res.status(401).json({
          error: 'Unauthorized',
          message: 'Authentication required'
        });
      }

      const userId = req.user.id;
      const roles = req.user.roles || [];

      span.setAttributes({
        'authorization.user_id': userId,
        'authorization.roles': roles.join(','),
        'authorization.path': req.path,
        'authorization.method': req.method
      });

      // Get RBAC engine
      const rbac = getRBACEngine();

      // Ensure user has roles assigned
      const userRoles = rbac.getUserRoles(userId);
      if (userRoles.length === 0 && roles.length > 0) {
        // Auto-assign roles from JWT if not already in RBAC
        for (const role of roles) {
          rbac.assignRole(userId, role);
        }
      }

      // Determine resource and action
      const resource = pathToResource(req.path);
      const action = methodToAction(req.method);

      span.setAttributes({
        'authorization.resource': resource,
        'authorization.action': action
      });

      // Collect attributes for ABAC
      const attributes = {
        path: req.path,
        method: req.method,
        ip: req.ip,
        userAgent: req.headers['user-agent'],
        // Add any additional context
        body: req.body,
        query: req.query,
        params: req.params
      };

      // Evaluate authorization
      try {
        const decision = await rbac.evaluate(userId, resource, action, attributes);

        span.setAttribute('authorization.decision', decision.allowed ? 'allow' : 'deny');
        span.setAttribute('authorization.decision_id', decision.decisionId);

        if (!decision.allowed) {
          // Log denied access
          logger.warn('Authorization denied', {
            userId,
            resource,
            action,
            path: req.path,
            method: req.method,
            reason: decision.reason,
            decisionId: decision.decisionId,
            ip: req.ip
          });

          // Attach decision to response for audit
          res.locals.authDecision = decision;

          return res.status(403).json({
            error: 'Forbidden',
            message: decision.reason,
            decisionId: decision.decisionId
          });
        }

        // Access granted
        logger.debug('Authorization granted', {
          userId,
          resource,
          action,
          path: req.path,
          decisionId: decision.decisionId
        });

        // Attach decision to request for downstream use
        req.authDecision = decision;
        res.locals.authDecision = decision;

        next();
      } catch (error) {
        span.recordException(error);

        logger.error('Authorization evaluation error', {
          error: error.message,
          userId,
          resource,
          action,
          path: req.path
        });

        return res.status(500).json({
          error: 'Internal Server Error',
          message: 'Authorization evaluation failed'
        });
      }
    } catch (error) {
      span.recordException(error);
      logger.error('Authorization middleware error', { error: error.message });

      return res.status(500).json({
        error: 'Internal Server Error',
        message: 'Authorization failed'
      });
    } finally {
      span.end();
    }
  });
}

/**
 * Create role-checking middleware
 * @param {string[]} requiredRoles
 * @returns {Function}
 */
export function requireRoles(...requiredRoles) {
  return async (req, res, next) => {
    if (!req.user || !req.user.id) {
      return res.status(401).json({
        error: 'Unauthorized',
        message: 'Authentication required'
      });
    }

    const rbac = getRBACEngine();
    const userRoles = rbac.getUserRoles(req.user.id);

    const hasRequiredRole = requiredRoles.some(role => userRoles.includes(role));

    if (!hasRequiredRole) {
      logger.warn('Role check failed', {
        userId: req.user.id,
        requiredRoles,
        userRoles,
        path: req.path
      });

      return res.status(403).json({
        error: 'Forbidden',
        message: `Required role: ${requiredRoles.join(' or ')}`
      });
    }

    next();
  };
}

/**
 * Create permission-checking middleware
 * @param {string} resource
 * @param {string} action
 * @returns {Function}
 */
export function requirePermission(resource, action) {
  return async (req, res, next) => {
    if (!req.user || !req.user.id) {
      return res.status(401).json({
        error: 'Unauthorized',
        message: 'Authentication required'
      });
    }

    const rbac = getRBACEngine();

    try {
      const decision = await rbac.evaluate(req.user.id, resource, action);

      if (!decision.allowed) {
        logger.warn('Permission check failed', {
          userId: req.user.id,
          resource,
          action,
          reason: decision.reason
        });

        return res.status(403).json({
          error: 'Forbidden',
          message: decision.reason,
          decisionId: decision.decisionId
        });
      }

      req.authDecision = decision;
      next();
    } catch (error) {
      logger.error('Permission check error', {
        error: error.message,
        userId: req.user.id,
        resource,
        action
      });

      return res.status(500).json({
        error: 'Internal Server Error',
        message: 'Permission check failed'
      });
    }
  };
}
