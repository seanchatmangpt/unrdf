# Agent 3 - DTO/Error/Logging Canonical Schemas

Canonical schemas for the enterprise migration system providing standardized data structures, error handling, and logging.

## Modules

### 1. id-rules.mjs
Deterministic ID generation and validation.

**ID Format**: `{type}_{timestamp}_{hash8}`

**Types**:
- `receipt` - Migration receipts
- `capsule` - Data capsules
- `domain` - Domain identifiers
- `lens` - Transformation lenses
- `migration` - Migration operations

**Functions**:
- `generateId(type, inputs)` - Generate deterministic ID
- `generateMigrationId(params)` - Generate migration ID
- `generateDomainId(params)` - Generate domain ID
- `generateCapsuleId(params)` - Generate capsule ID
- `generateReceiptId(params)` - Generate receipt ID
- `generateLensId(params)` - Generate lens ID
- `validateId(id, expectedType)` - Validate ID format
- `parseId(id)` - Parse ID into components
- `extractTimestamp(id)` - Extract timestamp from ID
- `verifyId(id, type, data)` - Verify ID matches expected data

### 2. dto-schemas.mjs
Request/Response data transfer objects.

**Types**:
- `MigrationRequest` - Migration request payload
- `MigrationResponse` - Migration response payload
- `DomainMigration` - Domain migration configuration
- `MigrationStats` - Migration statistics
- `DomainResult` - Per-domain migration result
- `ReceiptInfo` - Receipt information

**Functions**:
- `validateMigrationRequest(obj)` - Validate request DTO
- `validateDomainMigration(obj)` - Validate domain DTO
- `validateMigrationResponse(obj)` - Validate response DTO
- `createEmptyStats()` - Create empty statistics object
- `createDomainResult(domain, status, receipts, errors)` - Create domain result

### 3. error-schemas.mjs
Error handling and error codes.

**Error Codes**:
- `CONTRACT_DRIFT` - Contract drift detected (CRITICAL)
- `LENS_FAILURE` - Lens transformation failed (ERROR)
- `RECEIPT_TAMPER` - Receipt tampering detected (CRITICAL)
- `SHADOW_MISMATCH` - Shadow verification mismatch (ERROR)
- `ROUTING_ERROR` - Message routing error (ERROR)
- `VALIDATION_ERROR` - Validation failure (WARNING)
- `TIMEOUT_ERROR` - Operation timeout (ERROR)
- `NETWORK_ERROR` - Network failure (ERROR)
- `UNKNOWN_ERROR` - Unknown error (ERROR)

**Classes**:
- `MigrationError` - Structured error class

**Functions**:
- `createContractDriftError(message, context, correlationId)`
- `createLensFailureError(message, context, correlationId, cause)`
- `createReceiptTamperError(message, context, correlationId)`
- `createShadowMismatchError(message, context, correlationId)`
- `createRoutingError(message, context, correlationId)`
- `createValidationError(message, context, correlationId)`
- `wrapError(error, correlationId, context)` - Wrap unknown error
- `deserializeError(json)` - Deserialize error from JSON
- `isRecoverableError(error)` - Check if error is recoverable
- `isCriticalError(error)` - Check if error is critical
- `aggregateErrors(errors)` - Aggregate multiple errors

### 4. logging-schemas.mjs
Structured logging schemas.

**Log Levels**:
- `DEBUG`, `INFO`, `WARN`, `ERROR`, `FATAL`

**Migration Phases**:
- `INITIALIZATION`
- `VALIDATION`
- `LENS_APPLICATION`
- `CAPSULE_PROCESSING`
- `RECEIPT_GENERATION`
- `SHADOW_VERIFICATION`
- `FINALIZATION`

**Types**:
- `LogEntry` - Structured log entry
- `PerformanceMetrics` - Performance metrics

**Functions**:
- `createLogEntry(params)` - Create structured log entry
- `debug/info/warn/error/fatal(correlationId, agentId, phase, message, context)` - Log helpers
- `performance(correlationId, agentId, phase, message, duration, metrics)` - Performance log
- `formatLogEntry(entry)` - Format for console output
- `formatLogEntryJSON(entry)` - Format as JSON
- `validateLogEntry(obj)` - Validate log entry
- `createLogger(agentId)` - Create logger instance
- `aggregateByPhase(entries)` - Group logs by phase
- `calculatePhaseStats(entries)` - Calculate phase statistics

### 5. index.mjs
Central export of all schemas.

## Usage Examples

### ID Generation
```javascript
import { generateMigrationId, validateId } from './agent-3/index.mjs';

const id = generateMigrationId({
  sourceSystem: 'legacy-crm',
  targetSystem: 'salesforce',
  batchId: 'batch-001',
});
// Result: migration_1703587200_527442d8

const isValid = validateId(id, 'migration');
// Result: true
```

### Error Handling
```javascript
import { createLensFailureError, aggregateErrors } from './agent-3/index.mjs';

const error = createLensFailureError(
  'Transform failed',
  { lensId: 'lens_123', operation: 'transform' },
  'corr-123'
);

const summary = aggregateErrors([error]);
```

### Logging
```javascript
import { createLogger, MigrationPhase } from './agent-3/index.mjs';

const logger = createLogger('agent-3');
const entry = logger.info(
  'corr-456',
  MigrationPhase.VALIDATION,
  'Validation completed'
);
```

### DTO Validation
```javascript
import { validateMigrationRequest } from './agent-3/index.mjs';

const validation = validateMigrationRequest(requestData);
if (!validation.valid) {
  console.error('Validation errors:', validation.errors);
}
```

## Testing

Run the test suite:
```bash
node agent-3/test-schemas.mjs
```

## Design Principles

1. **Deterministic IDs**: Hash-based generation ensures reproducibility
2. **Pure Functions**: No side effects, no external dependencies
3. **Comprehensive Validation**: All DTOs have validation functions
4. **Structured Errors**: Rich error context with correlation IDs
5. **Performance Tracking**: Built-in metrics and duration tracking
6. **Type Safety**: Complete JSDoc annotations for IDE support

## File Sizes
- `id-rules.mjs`: 5.7K
- `dto-schemas.mjs`: 9.5K
- `error-schemas.mjs`: 8.1K
- `logging-schemas.mjs`: 12K
- `index.mjs`: 1.2K
- **Total**: ~37K
