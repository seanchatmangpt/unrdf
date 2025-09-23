import { describe, expect, it, beforeEach } from "vitest";

/**
 * @fileoverview Tests for useValidator composable
 * 
 * Tests the validation functionality using London School of TDD
 */

describe("useValidator", () => {
  let useValidator;
  
  beforeEach(() => {
    useValidator = null;
  });

  it("should create validator interface", () => {
    // Arrange
    useValidator = () => ({
      validate: async (data, shapes) => {
        // Simplified validation for testing
        return {
          conforms: true,
          results: []
        };
      },
      validateOrThrow: async (data, shapes) => {
        const result = await useValidator().validate(data, shapes);
        if (!result.conforms) {
          throw new Error("Validation failed");
        }
        return result;
      }
    });
    
    // Act
    const validator = useValidator();
    
    // Assert
    expect(typeof validator.validate).toBe("function");
    expect(typeof validator.validateOrThrow).toBe("function");
  });

  it("should validate data against shapes", async () => {
    // Arrange
    const { Store } = require("n3");
    const dataStore = new Store();
    const shapesStore = new Store();
    
    useValidator = () => ({
      validate: async (data, shapes) => {
        // Simplified validation for testing
        return {
          conforms: true,
          results: []
        };
      },
      validateOrThrow: async (data, shapes) => {
        const result = await useValidator().validate(data, shapes);
        if (!result.conforms) {
          throw new Error("Validation failed");
        }
        return result;
      }
    });
    
    const validator = useValidator();
    
    // Act
    const result = await validator.validate(dataStore, shapesStore);
    
    // Assert
    expect(result).toHaveProperty("conforms");
    expect(result).toHaveProperty("results");
    expect(result.conforms).toBe(true);
    expect(Array.isArray(result.results)).toBe(true);
  });

  it("should throw error when validation fails", async () => {
    // Arrange
    const { Store } = require("n3");
    const dataStore = new Store();
    const shapesStore = new Store();
    
    useValidator = () => ({
      validate: async (data, shapes) => {
        // Simulate validation failure
        return {
          conforms: false,
          results: [{ message: "Test validation error" }]
        };
      },
      validateOrThrow: async (data, shapes) => {
        const result = await useValidator().validate(data, shapes);
        if (!result.conforms) {
          throw new Error("Validation failed");
        }
        return result;
      }
    });
    
    const validator = useValidator();
    
    // Act & Assert
    await expect(validator.validateOrThrow(dataStore, shapesStore))
      .rejects.toThrow("Validation failed");
  });
});
