#!/usr/bin/env node

/**
 * @fileoverview Validation-Composables - Validation + Composables
 * @module @unrdf/microfw-6-validation-composables
 *
 * Adversarial Innovation: Constraint validation + reactive state = live form validation
 * Use Case: Reactive forms with real-time validation feedback
 */

// ============================================================================
// MOCK IMPLEMENTATIONS
// ============================================================================

// Composables (reactive state)
function ref(initialValue) {
  let value = initialValue;
  const listeners = [];

  return {
    get value() {
      return value;
    },
    set value(newValue) {
      value = newValue;
      listeners.forEach(listener => listener(value));
    },
    watch(callback) {
      listeners.push(callback);
    },
  };
}

function reactive(obj) {
  const listeners = [];

  return new Proxy(obj, {
    set(target, key, value) {
      const oldValue = target[key];
      target[key] = value;
      listeners.forEach(listener => listener({ key, oldValue, value }));
      return true;
    },
    get(target, key) {
      if (key === 'watch') {
        return (callback) => listeners.push(callback);
      }
      return target[key];
    },
  });
}

function computed(getter) {
  const result = ref(getter());
  return result;
}

// Validation
async function validateField(value, rules) {
  const errors = [];

  for (const rule of rules) {
    if (rule.required && !value) {
      errors.push(rule.message || 'Field is required');
    }
    if (rule.minLength && value.length < rule.minLength) {
      errors.push(rule.message || `Minimum length is ${rule.minLength}`);
    }
    if (rule.maxLength && value.length > rule.maxLength) {
      errors.push(rule.message || `Maximum length is ${rule.maxLength}`);
    }
    if (rule.pattern && !rule.pattern.test(value)) {
      errors.push(rule.message || 'Invalid format');
    }
    if (rule.custom && !await rule.custom(value)) {
      errors.push(rule.message || 'Custom validation failed');
    }
  }

  return { valid: errors.length === 0, errors };
}

// ============================================================================
// VALIDATION-COMPOSABLES FRAMEWORK
// ============================================================================

/**
 * ValidationComposablesFramework - Reactive forms with live validation
 */
class ValidationComposablesFramework {
  constructor() {
    this.forms = new Map();
    this.stats = {
      formsCreated: 0,
      validationsRun: 0,
      errorsDetected: 0,
    };
  }

  /**
   * Create reactive form with validation
   */
  createForm(name, schema) {
    const form = {
      name,
      schema,
      data: reactive({}),
      errors: reactive({}),
      isValid: ref(false),
      isDirty: ref(false),
    };

    // Initialize fields
    for (const [fieldName, fieldSchema] of Object.entries(schema)) {
      form.data[fieldName] = fieldSchema.default || '';
      form.errors[fieldName] = [];

      // Watch field changes for live validation
      const validateThis = async () => {
        await this.validateField(form, fieldName);
        await this.validateForm(form);
      };

      // Add watcher (simplified)
      form.data.watch(async (change) => {
        if (change.key === fieldName) {
          form.isDirty.value = true;
          await validateThis();
        }
      });
    }

    this.forms.set(name, form);
    this.stats.formsCreated++;

    console.log(`[Form] Created: ${name} (${Object.keys(schema).length} fields)`);

    return form;
  }

  /**
   * Validate single field
   */
  async validateField(form, fieldName) {
    const fieldSchema = form.schema[fieldName];
    const value = form.data[fieldName];

    const validation = await validateField(value, fieldSchema.rules || []);

    form.errors[fieldName] = validation.errors;
    this.stats.validationsRun++;

    if (!validation.valid) {
      this.stats.errorsDetected += validation.errors.length;
      console.log(`  [Validation] ${fieldName}: ${validation.errors.join(', ')}`);
    }

    return validation;
  }

  /**
   * Validate entire form
   */
  async validateForm(form) {
    let formValid = true;

    for (const fieldName of Object.keys(form.schema)) {
      const validation = await this.validateField(form, fieldName);
      if (!validation.valid) {
        formValid = false;
      }
    }

    form.isValid.value = formValid;

    return { valid: formValid };
  }

  /**
   * Set field value (triggers reactive validation)
   */
  async setFieldValue(formName, fieldName, value) {
    const form = this.forms.get(formName);
    if (!form) {
      throw new Error(`Form not found: ${formName}`);
    }

    console.log(`[Set] ${formName}.${fieldName} = "${value}"`);

    form.data[fieldName] = value;

    return form.data[fieldName];
  }

  /**
   * Get form data
   */
  getFormData(formName) {
    const form = this.forms.get(formName);
    if (!form) return null;

    return {
      data: { ...form.data },
      errors: { ...form.errors },
      isValid: form.isValid.value,
      isDirty: form.isDirty.value,
    };
  }

  /**
   * Get statistics
   */
  getStats() {
    return {
      ...this.stats,
      forms: this.forms.size,
    };
  }
}

// ============================================================================
// DEMO
// ============================================================================

async function demo() {
  console.log('╔════════════════════════════════════════════════════════════╗');
  console.log('║ Validation-Composables Framework Demo                     ║');
  console.log('║ Validation + Composables = Live form validation           ║');
  console.log('╚════════════════════════════════════════════════════════════╝\n');

  const framework = new ValidationComposablesFramework();

  // Create form with validation rules
  const userForm = framework.createForm('user-registration', {
    username: {
      default: '',
      rules: [
        { required: true, message: 'Username is required' },
        { minLength: 3, message: 'Username must be at least 3 characters' },
        { maxLength: 20, message: 'Username must not exceed 20 characters' },
      ],
    },
    email: {
      default: '',
      rules: [
        { required: true, message: 'Email is required' },
        { pattern: /^[^\s@]+@[^\s@]+\.[^\s@]+$/, message: 'Invalid email format' },
      ],
    },
    password: {
      default: '',
      rules: [
        { required: true, message: 'Password is required' },
        { minLength: 8, message: 'Password must be at least 8 characters' },
        {
          custom: async (value) => /[A-Z]/.test(value),
          message: 'Password must contain uppercase letter',
        },
      ],
    },
  });

  console.log('\n[Demo] Testing live validation...\n');

  // Test 1: Invalid username (too short)
  console.log('[Test 1] Setting username to "ab" (too short)');
  await framework.setFieldValue('user-registration', 'username', 'ab');

  // Test 2: Valid username
  console.log('\n[Test 2] Setting username to "alice123" (valid)');
  await framework.setFieldValue('user-registration', 'username', 'alice123');

  // Test 3: Invalid email
  console.log('\n[Test 3] Setting email to "invalid-email" (invalid format)');
  await framework.setFieldValue('user-registration', 'email', 'invalid-email');

  // Test 4: Valid email
  console.log('\n[Test 4] Setting email to "alice@example.com" (valid)');
  await framework.setFieldValue('user-registration', 'email', 'alice@example.com');

  // Test 5: Invalid password
  console.log('\n[Test 5] Setting password to "short" (too short)');
  await framework.setFieldValue('user-registration', 'password', 'short');

  // Test 6: Valid password
  console.log('\n[Test 6] Setting password to "SecurePass123" (valid)');
  await framework.setFieldValue('user-registration', 'password', 'SecurePass123');

  console.log('\n[Form] Final state:');
  const formData = framework.getFormData('user-registration');
  console.log(`  Valid: ${formData.isValid}`);
  console.log(`  Dirty: ${formData.isDirty}`);
  console.log(`  Data:`, formData.data);
  console.log(`  Errors:`, formData.errors);

  console.log('\n[Stats] Final Statistics:');
  const stats = framework.getStats();
  Object.entries(stats).forEach(([key, value]) => {
    console.log(`  ${key}: ${value}`);
  });

  console.log('\n╔════════════════════════════════════════════════════════════╗');
  console.log('║ Adversarial Innovation:                                    ║');
  console.log('║ - Validation provides constraint checking rules            ║');
  console.log('║ - Composables enable reactive data binding                 ║');
  console.log('║ - Live validation on every field change                    ║');
  console.log('║ - Instant feedback improves user experience                ║');
  console.log('╚════════════════════════════════════════════════════════════╝');
}

if (import.meta.url === `file://${process.argv[1]}`) {
  demo().catch(console.error);
}

export { ValidationComposablesFramework, demo };
