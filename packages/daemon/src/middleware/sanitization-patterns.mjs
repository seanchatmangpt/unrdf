/**
 * @file Input Sanitization Patterns
 * @module @unrdf/daemon/middleware/sanitization-patterns
 * @description Regular expressions for detecting and sanitizing malicious input
 */

/**
 * Input sanitization patterns
 */
export const SANITIZATION_PATTERNS = {
  // SQL Injection patterns
  sql: /(\b(SELECT|INSERT|UPDATE|DELETE|DROP|CREATE|ALTER|EXEC|UNION|SCRIPT)\b)|(-{2})|([';])/gi,

  // XSS patterns
  xss: /<script[^>]*>[\s\S]*?<\/script>|<\/script>|<script[^>]*>|<iframe[^>]*>[\s\S]*?<\/iframe>|<\/iframe>|<iframe[^>]*>|javascript:|on\w+\s*=/gi,

  // Path traversal
  pathTraversal: /\.\.[\/\\]/g,

  // Command injection
  commandInjection: /[;&|`$(){}[\]<>]/g,

  // LDAP injection
  ldap: /[()&|!*]/g,

  // NoSQL injection
  nosql: /[${}]/g,
};
