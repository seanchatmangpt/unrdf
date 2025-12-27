/**
 * Application error matching profile conventions
 * @generated Convention-preserving code generator (Agent 7)
 */

/**
 * AppError - Custom error class
 * @extends Error
 */
export class AppError extends Error {
  /**
   * Create application error
   * @param {string} code - Error code
   * @param {string} message - Error message
   * @param {Object} [details={}] - Additional error details
   */
  constructor(code, message, details = {}) {
    super(message);
    this.code = code;
    this.message = message;
    this.details = details;
    this.name = this.constructor.name;
    Error.captureStackTrace(this, this.constructor);
  }

  /**
   * Convert to JSON representation
   * @returns {Object} JSON object
   */
  toJSON() {
    return {
      code: this.code,
      message: this.message,
      details: this.details,
      name: this.name,
      stack: this.stack
    };
  }
}
