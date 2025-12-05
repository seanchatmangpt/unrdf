# Types: @unrdf/knowledge-engine

Complete reference for all type definitions and interfaces.

---

## Type System Overview

The type system provides strong typing for all values passed through the system. Understanding types helps prevent bugs and improve performance.

---

## Core Interfaces

### Options Interface
Configuration object for initialization and operations.

### Result Interface
Return value from operations containing data and metadata.

### Error Interface
Standard error type with code and details.

---

## Type Categories

### Input Types
Types for data flowing into operations.

### Output Types
Types for results returning from operations.

### Configuration Types
Types for configuration objects.

### Error Types
Types for error handling.

---

## Generic Types

The system uses generic types for flexibility:

### Promise Types
Async operations return promises of specific types.

### Stream Types
Streaming operations use stream type parameters.

### Optional Types
Some fields may be optional using union types.

---

## Type Compatibility

Understanding type compatibility helps write flexible code:

- Subtype compatibility
- Union types
- Optional chaining
- Type narrowing

---

## TypeScript Support

While implemented in JavaScript, the code includes JSDoc types compatible with TypeScript.

---

## Common Type Patterns

- Optional fields with defaults
- Union types for alternatives
- Generic types for flexibility
- Branded types for safety

---

## Type Documentation

Each type is fully documented with:
- Description
- Fields and properties
- Valid values
- Usage examples

---

## See Also

- API reference for function signatures
- Configuration guide for valid options
- Error reference for error types
