/**
 * @file use-form-validation.mjs
 * @description React hook for form validation with Zod
 */

import { useState, useCallback } from 'react';

/**
 * Hook for form validation using Zod schemas
 *
 * @param {Object} schema - Zod validation schema
 * @returns {Object} Form validation state and operations
 *
 * @example
 * const {
 *   values,
 *   errors,
 *   handleChange,
 *   handleSubmit,
 *   reset
 * } = useFormValidation(ProductSchema);
 */
export function useFormValidation(schema) {
  const [values, setValues] = useState({});
  const [errors, setErrors] = useState({});
  const [touched, setTouched] = useState({});
  const [isSubmitting, setIsSubmitting] = useState(false);

  const handleChange = useCallback((field, value) => {
    setValues(prev => ({ ...prev, [field]: value }));

    // Clear error when user types
    if (errors[field]) {
      setErrors(prev => {
        const newErrors = { ...prev };
        delete newErrors[field];
        return newErrors;
      });
    }
  }, [errors]);

  const handleBlur = useCallback((field) => {
    setTouched(prev => ({ ...prev, [field]: true }));

    // Validate single field
    try {
      schema.pick({ [field]: true }).parse({ [field]: values[field] });
    } catch (err) {
      setErrors(prev => ({
        ...prev,
        [field]: err.errors[0]?.message
      }));
    }
  }, [schema, values]);

  const validate = useCallback(() => {
    try {
      schema.parse(values);
      setErrors({});
      return true;
    } catch (err) {
      const fieldErrors = {};
      err.errors.forEach(error => {
        const field = error.path[0];
        fieldErrors[field] = error.message;
      });
      setErrors(fieldErrors);
      return false;
    }
  }, [schema, values]);

  const handleSubmit = useCallback(async (onSubmit) => {
    setIsSubmitting(true);

    const isValid = validate();

    if (!isValid) {
      setIsSubmitting(false);
      return;
    }

    try {
      await onSubmit(values);
      reset();
    } catch (err) {
      setErrors({ submit: err.message });
    } finally {
      setIsSubmitting(false);
    }
  }, [values, validate]);

  const reset = useCallback(() => {
    setValues({});
    setErrors({});
    setTouched({});
    setIsSubmitting(false);
  }, []);

  return {
    values,
    errors,
    touched,
    isSubmitting,
    handleChange,
    handleBlur,
    handleSubmit,
    validate,
    reset
  };
}
