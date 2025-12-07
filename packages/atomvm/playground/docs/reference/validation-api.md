# Validation API Reference

**Information-oriented**: Complete API documentation for the validation suite.

## ValidationSuite

Main orchestrator for all validations.

### Constructor

```javascript
new ValidationSuite(options)
```

**Parameters:**
- `options.log` - Logging function (default: `console.log`)
- `options.onResult` - Callback for validation results (default: no-op)

### Methods

#### `runAll(moduleName)`

Run all validations.

**Parameters:**
- `moduleName` (string, required) - Module name to validate

**Returns:** `Promise<ValidationResult[]>`

**Throws:** `Error` if `moduleName` is invalid

#### `getResults()`

Get current validation results.

**Returns:** `ValidationResult[]`

#### `clear()`

Clear all results.

## ProcessValidator

Validates process lifecycle.

### Constructor

```javascript
new ProcessValidator(options)
```

**Parameters:**
- `options.log` - Logging function (default: `console.log`)

### Methods

#### `validate(moduleName)`

Validate process lifecycle.

**Parameters:**
- `moduleName` (string, required) - Module name to validate

**Returns:** `Promise<{passed: boolean, message: string, error?: Error}>`

## SupervisionValidator

Validates supervision tree.

### Constructor

```javascript
new SupervisionValidator(options)
```

**Parameters:**
- `options.log` - Logging function (default: `console.log`)

### Methods

#### `validate(moduleName)`

Validate supervision tree.

**Parameters:**
- `moduleName` (string, required) - Module name to validate

**Returns:** `Promise<{passed: boolean, message: string, error?: Error}>`

## KGC4DValidator

Validates KGC-4D event integration.

### Constructor

```javascript
new KGC4DValidator(options)
```

**Parameters:**
- `options.log` - Logging function (default: `console.log`)

### Methods

#### `validate(moduleName)`

Validate KGC-4D event integration.

**Parameters:**
- `moduleName` (string, required) - Module name to validate

**Returns:** `Promise<{passed: boolean, message: string, error?: Error}>`

#### `validateEventStructure(event)`

Validate event structure.

**Parameters:**
- `event` (object) - Event to validate

**Returns:** `boolean`

## RuntimeValidator

Validates state machine and dual runtime support.

### Constructor

```javascript
new RuntimeValidator(options)
```

**Parameters:**
- `options.log` - Logging function (default: `console.log`)

### Methods

#### `validateStateMachine(moduleName)`

Validate state machine integrity.

**Parameters:**
- `moduleName` (string, required) - Module name to validate

**Returns:** `Promise<{passed: boolean, message: string, error?: Error}>`

#### `validateDualRuntime(moduleName)`

Validate dual runtime support.

**Parameters:**
- `moduleName` (string, required) - Module name to validate

**Returns:** `Promise<{passed: boolean, message: string, error?: Error}>`

## Types

### ValidationResult

```typescript
{
  name: string;
  status: 'pending' | 'running' | 'pass' | 'fail';
  message?: string;
  error?: Error;
}
```

## See Also

- [Production Scenarios](./production-scenarios.md)
- [How-To Guides](../how-to/)

