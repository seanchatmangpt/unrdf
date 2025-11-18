/**
 * @fileoverview useValidation - Hook for Zod validation integration
 * @module react-hooks/utils/useValidation
 */

import { useState, useCallback } from 'react';

export function useValidation(schema) {
  const [errors, setErrors] = useState([]);
  const [isValid, setIsValid] = useState(true);

  const validate = useCallback((data) => {
    try {
      schema.parse(data);
      setErrors([]);
      setIsValid(true);
      return { success: true, data };
    } catch (err) {
      setErrors(err.errors || [err]);
      setIsValid(false);
      return { success: false, errors: err.errors };
    }
  }, [schema]);

  return { validate, errors, isValid };
}
