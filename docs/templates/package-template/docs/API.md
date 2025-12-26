# API Reference

## Functions

### `createThing(options)`

Creates a new thing instance.

**Parameters**:
- `options` (Object, optional): Configuration options
  - `option` (string, default: `'default'`): Description

**Returns**: `Thing` - Thing instance

**Throws**: `TypeError` if options are invalid

**Example**:
```javascript
const thing = createThing({ option: 'value' });
```

### `thing.doSomething()`

Does something with the thing.

**Returns**: `void`

**Example**:
```javascript
thing.doSomething();
```

## Validation

### `validateThing(config)`

Validates thing configuration.

**Parameters**:
- `config` (any): Configuration to validate

**Returns**: `boolean` - True if valid

**Example**:
```javascript
if (validateThing(config)) {
  const thing = createThing(config);
}
```

## Constants

### `DEFAULT_OPTIONS`

Default configuration options.

**Type**: `Object`

**Value**:
```javascript
{
  option: 'default'
}
```

### `VERSION`

Package version.

**Type**: `string`
