/**
 * Constants for KGEN Injection System
 *
 * Defines all constants, error codes, and configuration values
 * used throughout the injection operations system.
 */

export const INJECTION_MODES = {
  APPEND: 'append',
  PREPEND: 'prepend',
  BEFORE: 'before',
  AFTER: 'after',
  REPLACE: 'replace',
  LINE_AT: 'lineAt',
  CREATE: 'create'
};

export const SKIP_IF_LOGIC = {
  AND: 'AND',
  OR: 'OR'
};

export const VALIDATION_RULES = {
  SYNTAX: 'syntax',
  SEMANTICS: 'semantics',
  CONFLICTS: 'conflicts',
  ENCODING: 'encoding',
  SIZE: 'size',
  PERMISSIONS: 'permissions'
};

export const ERROR_CODES = {
  TARGET_NOT_FOUND: 'TARGET_NOT_FOUND',
  BINARY_FILE: 'BINARY_FILE',
  READ_ONLY: 'READ_ONLY',
  FILE_LOCKED: 'FILE_LOCKED',
  PATH_TRAVERSAL: 'PATH_TRAVERSAL',
  SIZE_EXCEEDED: 'SIZE_EXCEEDED',
  VALIDATION_FAILED: 'VALIDATION_FAILED',
  ATOMIC_FAILURE: 'ATOMIC_FAILURE',
  ROLLBACK_FAILURE: 'ROLLBACK_FAILURE',
  INSUFFICIENT_SPACE: 'INSUFFICIENT_SPACE',
  ENCODING_ERROR: 'ENCODING_ERROR',
  CONFLICT_DETECTED: 'CONFLICT_DETECTED'
};

export const DEFAULT_CONFIG = {
  // Atomic operations
  atomicWrites: true,
  backupEnabled: true,
  transactionTimeout: 30000, // 30 seconds

  // File validation
  maxFileSize: 10 * 1024 * 1024, // 10MB
  maxLineCount: 10000,
  validateEncoding: true,
  preservePermissions: true,

  // Deterministic behavior
  sortGlobResults: true,
  consistentLineEndings: true,
  preserveWhitespace: true,

  // Security
  preventPathTraversal: true,
  allowedExtensions: ['.js', '.ts', '.jsx', '.tsx', '.json', '.md', '.txt', '.html', '.css', '.scss'],

  // Performance
  streamLargeFiles: true,
  streamThreshold: 1024 * 1024, // 1MB
  maxConcurrentOperations: 5,

  // Backup and recovery
  backupSuffix: '.kgen-backup',
  maxBackups: 10,
  autoCleanup: true
};

export const REGEX_FLAGS = {
  GLOBAL: 'g',
  MULTILINE: 'm',
  CASE_INSENSITIVE: 'i',
  DOTALL: 's',
  UNICODE: 'u'
};

export const LINE_ENDINGS = {
  LF: '\n',      // Unix/Linux/macOS
  CRLF: '\r\n',  // Windows
  CR: '\r'       // Classic Mac
};

export const ENCODINGS = {
  UTF8: 'utf8',
  UTF16: 'utf16le',
  ASCII: 'ascii',
  LATIN1: 'latin1'
};

export const CHECKSUM_ALGORITHMS = {
  SHA256: 'sha256',
  MD5: 'md5',
  SHA1: 'sha1'
};

// File type detection patterns
export const BINARY_PATTERNS = [
  /^\x00/,                    // Null bytes at start
  /[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]/,  // Control characters
  /\uFFFD/,                   // Replacement character (indicates binary)
];

// Content validation patterns
export const CONTENT_PATTERNS = {
  IMPORT_STATEMENT: /^import\s+.*from\s+['"][^'"]+['"];?\s*$/gm,
  EXPORT_STATEMENT: /^export\s+.*$/gm,
  FUNCTION_DECLARATION: /^(async\s+)?function\s+\w+\s*\(/gm,
  CLASS_DECLARATION: /^class\s+\w+/gm,
  INTERFACE_DECLARATION: /^interface\s+\w+/gm,
  TYPE_DECLARATION: /^type\s+\w+/gm
};

// Operation metadata
export const OPERATION_METADATA = {
  CREATED_BY: 'KGEN Injection System',
  VERSION: '1.0.0',
  TIMESTAMP_FORMAT: 'YYYY-MM-DD HH:mm:ss UTC',
  HASH_ALGORITHM: 'sha256'
};

// Concurrency control
export const LOCK_CONFIG = {
  TIMEOUT: 10000,     // 10 seconds
  RETRY_DELAY: 100,   // 100ms
  MAX_RETRIES: 100
};