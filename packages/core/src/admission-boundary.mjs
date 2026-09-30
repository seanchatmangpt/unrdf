/** Composable admission boundary with typed refusals. */
export class AdmissionRefusal extends Error {
  /**
   * Create a typed refusal error.
   * @param {string} code - Machine-readable refusal code.
   * @param {string} message - Human-readable refusal message.
   * @param {*} [detail] - Extra detail about the refusal (rule result or cause); defaults to null.
   */
  constructor(code, message, detail = null) {
    super(message);
    this.name = 'AdmissionRefusal';
    this.code = code;
    this.detail = detail;
  }
}

/**
 * Ordered list of admission rules that a subject must satisfy in full.
 */
export class AdmissionBoundary {
  #rules = [];

  /**
   * Register an admission rule; rules are evaluated in registration order.
   * @param {Object} spec - Rule definition.
   * @param {string} spec.id - Rule identifier (required).
   * @param {Function} spec.test - Predicate `(subject, context)` returning true, or an object with `passed: true`, to admit (may be async).
   * @param {string} [spec.code] - Refusal code used when the rule fails.
   * @param {string} [spec.message] - Refusal message; defaults to the rule id.
   * @returns {AdmissionBoundary} This boundary, for chaining.
   * @throws {TypeError} If `id` is missing or `test` is not a function.
   */
  rule({ id, test, code = 'ADMISSION_REFUSED', message = id }) {
    if (!id || typeof test !== 'function') throw new TypeError('rule id and test are required');
    this.#rules.push({ id, test, code, message });
    return this;
  }

  /**
   * Run every rule against the subject, stopping at the first failure.
   * @param {*} subject - Value being admitted.
   * @param {Object} [context] - Extra context passed to each rule test.
   * @returns {Promise<{admitted: true, subject: *, checks: Array<{id: string, passed: boolean, detail: *}>}>} Admission result with per-rule checks.
   * @throws {AdmissionRefusal} If a rule fails or its test throws.
   */
  async admit(subject, context = {}) {
    const checks = [];
    for (const rule of this.#rules) {
      try {
        const result = await rule.test(subject, context);
        const passed = result === true || result?.passed === true;
        checks.push({ id: rule.id, passed, detail: result === true ? null : result });
        if (!passed) throw new AdmissionRefusal(rule.code, rule.message, result);
      } catch (error) {
        if (error instanceof AdmissionRefusal) throw error;
        throw new AdmissionRefusal(rule.code, `${rule.message}: ${error.message}`, {
          cause: error.message,
        });
      }
    }
    return { admitted: true, subject, checks };
  }
}

export const rules = Object.freeze({
  required: field => ({
    id: `required:${field}`,
    code: 'REQUIRED_FIELD_MISSING',
    message: `${field} is required`,
    test: subject =>
      subject?.[field] !== undefined && subject?.[field] !== null && subject?.[field] !== '',
  }),
  oneOf: (field, values) => ({
    id: `oneOf:${field}`,
    code: 'VALUE_NOT_ADMITTED',
    message: `${field} is not admitted`,
    test: subject => values.includes(subject?.[field]),
  }),
  predicate: (id, predicate, code = 'PREDICATE_REFUSED') => ({
    id,
    test: predicate,
    code,
    message: id,
  }),
});

/**
 * Create an empty admission boundary.
 * @returns {AdmissionBoundary} A new boundary with no rules.
 */
export function createAdmissionBoundary() {
  return new AdmissionBoundary();
}
