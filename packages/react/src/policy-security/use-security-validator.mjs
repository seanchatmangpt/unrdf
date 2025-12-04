/**
 * @file use-security-validator.mjs
 * @description React hook for security validation and access control
 */

import { useState, useCallback } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for validating security policies and access control
 *
 * @param {Object} config - Security validator configuration
 * @returns {Object} Security validator state and operations
 *
 * @example
 * const {
 *   checkAccess,
 *   validatePermissions,
 *   auditLog,
 *   securityScore
 * } = useSecurityValidator();
 */
export function useSecurityValidator(_config = {}) {
  const { engine } = useKnowledgeEngineContext();
  const [auditLog, setAuditLog] = useState([]);
  const [securityScore, _setSecurityScore] = useState(100);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const checkAccess = useCallback(
    async (user, resource, action) => {
      try {
        setLoading(true);

        const result = await engine.query(`
        SELECT * WHERE {
          <${user}> <urn:hasPermission> ?perm .
          ?perm <urn:resource> <${resource}> ;
                <urn:action> "${action}"
        }
      `);

        const hasAccess = result.length > 0;

        setAuditLog((prev) => [
          ...prev,
          {
            user,
            resource,
            action,
            allowed: hasAccess,
            timestamp: new Date().toISOString(),
          },
        ]);

        setLoading(false);
        return hasAccess;
      } catch (err) {
        setError(err);
        setLoading(false);
        throw err;
      }
    },
    [engine]
  );

  const validatePermissions = useCallback(
    async (userId) => {
      try {
        setLoading(true);

        const permissions = await engine.query(`
        SELECT * WHERE {
          <${userId}> <urn:hasPermission> ?perm
        }
      `);

        setLoading(false);
        return permissions;
      } catch (err) {
        setError(err);
        setLoading(false);
        throw err;
      }
    },
    [engine]
  );

  return {
    checkAccess,
    validatePermissions,
    auditLog,
    securityScore,
    loading,
    error,
  };
}
