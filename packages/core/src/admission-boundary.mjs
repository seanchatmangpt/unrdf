/** Composable admission boundary with typed refusals. */
export class AdmissionRefusal extends Error {
  constructor(code, message, detail = null) {
    super(message);
    this.name = 'AdmissionRefusal';
    this.code = code;
    this.detail = detail;
  }
}

export class AdmissionBoundary {
  #rules = [];

  rule({ id, test, code = 'ADMISSION_REFUSED', message = id }) {
    if (!id || typeof test !== 'function') throw new TypeError('rule id and test are required');
    this.#rules.push({ id, test, code, message });
    return this;
  }

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
        throw new AdmissionRefusal(rule.code, `${rule.message}: ${error.message}`, { cause: error.message });
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
    test: subject => subject?.[field] !== undefined && subject?.[field] !== null && subject?.[field] !== '',
  }),
  oneOf: (field, values) => ({
    id: `oneOf:${field}`,
    code: 'VALUE_NOT_ADMITTED',
    message: `${field} is not admitted`,
    test: subject => values.includes(subject?.[field]),
  }),
  predicate: (id, predicate, code = 'PREDICATE_REFUSED') => ({ id, test: predicate, code, message: id }),
});

export function createAdmissionBoundary() { return new AdmissionBoundary(); }
