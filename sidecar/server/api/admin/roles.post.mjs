/**
 * @file Admin Role Assignment Endpoint
 * @module sidecar/server/api/admin/roles
 *
 * POST /api/admin/roles
 *
 * Assign or revoke roles for users (admin only)
 */

import { defineEventHandler, readBody, setResponseStatus } from '#imports';
import { z } from 'zod';
import { trace } from '@opentelemetry/api';
import { getRBACEngine, Roles } from '../../utils/rbac.mjs';
import logger from '../../utils/logger.mjs';

const tracer = trace.getTracer('admin-roles-api');

/**
 * Request body schema
 */
const RoleAssignmentSchema = z.object({
  userId: z.string().min(1, 'User ID is required'),
  role: z.enum([Roles.ADMIN, Roles.AGENT, Roles.WRITER, Roles.READER], {
    message: 'Invalid role'
  }),
  action: z.enum(['assign', 'revoke'], {
    message: 'Action must be "assign" or "revoke"'
  })
});

/**
 * POST /api/admin/roles
 * Assign or revoke user roles
 *
 * @param {import("h3").H3Event} event
 */
export default defineEventHandler(async (event) => {
  const adminId = event.context.auth?.userId;
  const rawBody = await readBody(event);
  /** @param {number} code @param {Object} body */
  const reply = (code, body) => {
    setResponseStatus(event, code);
    return body;
  };

  return tracer.startActiveSpan('admin.roles.post', async (span) => {
    try {
      // Validate request body
      const validation = RoleAssignmentSchema.safeParse(rawBody);

      if (!validation.success) {
        span.setAttribute('validation.failed', true);
        return reply(400, {
          error: 'Bad Request',
          message: 'Invalid request body',
          details: validation.error.issues
        });
      }

      const { userId, role, action } = validation.data;

      span.setAttributes({
        'admin.target_user_id': userId,
        'admin.role': role,
        'admin.action': action,
        'admin.admin_user_id': adminId
      });

      // Get RBAC engine
      const rbac = getRBACEngine();

      // Verify requester is admin (middleware should have checked this)
      if (!rbac.hasRole(adminId, Roles.ADMIN)) {
        span.setAttribute('authorization.failed', true);

        logger.warn('Non-admin attempted role assignment', {
          adminUserId: adminId,
          targetUserId: userId,
          role,
          action
        });

        return reply(403, {
          error: 'Forbidden',
          message: 'Admin role required for role management'
        });
      }

      // Prevent self-demotion from admin
      if (userId === adminId && role === Roles.ADMIN && action === 'revoke') {
        span.setAttribute('self_demotion.prevented', true);

        logger.warn('Admin attempted self-demotion', {
          userId: adminId
        });

        return reply(400, {
          error: 'Bad Request',
          message: 'Cannot revoke your own admin role'
        });
      }

      // Perform action
      if (action === 'assign') {
        rbac.assignRole(userId, role);

        logger.info('Role assigned', {
          adminUserId: adminId,
          targetUserId: userId,
          role
        });

        span.setAttribute('role.assigned', true);

        return reply(200, {
          success: true,
          message: `Role "${role}" assigned to user ${userId}`,
          userId,
          role,
          currentRoles: rbac.getUserRoles(userId)
        });
      } else {
        rbac.revokeRole(userId, role);

        logger.info('Role revoked', {
          adminUserId: adminId,
          targetUserId: userId,
          role
        });

        span.setAttribute('role.revoked', true);

        return reply(200, {
          success: true,
          message: `Role "${role}" revoked from user ${userId}`,
          userId,
          role,
          currentRoles: rbac.getUserRoles(userId)
        });
      }
    } catch (error) {
      span.recordException(error);

      logger.error('Role assignment error', {
        error: error.message,
        adminUserId: adminId,
        body: rawBody
      });

      return reply(500, {
        error: 'Internal Server Error',
        message: 'Failed to process role assignment'
      });
    } finally {
      span.end();
    }
  });
});
