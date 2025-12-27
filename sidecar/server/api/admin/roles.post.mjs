/**
 * @file Admin Role Assignment Endpoint
 * @module sidecar/server/api/admin/roles
 *
 * POST /api/admin/roles
 *
 * Assign or revoke roles for users (admin only)
 */

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
    errorMap: () => ({ message: 'Invalid role' })
  }),
  action: z.enum(['assign', 'revoke'], {
    errorMap: () => ({ message: 'Action must be "assign" or "revoke"' })
  })
});

/**
 * POST /api/admin/roles
 * Assign or revoke user roles
 *
 * @param {import('express').Request} req
 * @param {import('express').Response} res
 */
export default async function rolesPost(req, res) {
  return tracer.startActiveSpan('admin.roles.post', async (span) => {
    try {
      // Validate request body
      const validation = RoleAssignmentSchema.safeParse(req.body);

      if (!validation.success) {
        span.setAttribute('validation.failed', true);
        return res.status(400).json({
          error: 'Bad Request',
          message: 'Invalid request body',
          details: validation.error.errors
        });
      }

      const { userId, role, action } = validation.data;

      span.setAttributes({
        'admin.target_user_id': userId,
        'admin.role': role,
        'admin.action': action,
        'admin.admin_user_id': req.user?.id
      });

      // Get RBAC engine
      const rbac = getRBACEngine();

      // Verify requester is admin (middleware should have checked this)
      if (!rbac.hasRole(req.user.id, Roles.ADMIN)) {
        span.setAttribute('authorization.failed', true);

        logger.warn('Non-admin attempted role assignment', {
          adminUserId: req.user.id,
          targetUserId: userId,
          role,
          action
        });

        return res.status(403).json({
          error: 'Forbidden',
          message: 'Admin role required for role management'
        });
      }

      // Prevent self-demotion from admin
      if (userId === req.user.id && role === Roles.ADMIN && action === 'revoke') {
        span.setAttribute('self_demotion.prevented', true);

        logger.warn('Admin attempted self-demotion', {
          userId: req.user.id
        });

        return res.status(400).json({
          error: 'Bad Request',
          message: 'Cannot revoke your own admin role'
        });
      }

      // Perform action
      if (action === 'assign') {
        rbac.assignRole(userId, role);

        logger.info('Role assigned', {
          adminUserId: req.user.id,
          targetUserId: userId,
          role
        });

        span.setAttribute('role.assigned', true);

        return res.status(200).json({
          success: true,
          message: `Role "${role}" assigned to user ${userId}`,
          userId,
          role,
          currentRoles: rbac.getUserRoles(userId)
        });
      } else {
        rbac.revokeRole(userId, role);

        logger.info('Role revoked', {
          adminUserId: req.user.id,
          targetUserId: userId,
          role
        });

        span.setAttribute('role.revoked', true);

        return res.status(200).json({
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
        adminUserId: req.user?.id,
        body: req.body
      });

      return res.status(500).json({
        error: 'Internal Server Error',
        message: 'Failed to process role assignment'
      });
    } finally {
      span.end();
    }
  });
}
