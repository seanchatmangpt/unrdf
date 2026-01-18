# API Key Authentication

Enterprise-grade API key authentication for UNRDF daemon operations.

## Features

- **BLAKE3 Hashing** - High-performance cryptographic hashing
- **Constant-Time Comparison** - Timing attack prevention
- **Environment Variables** - Support for `UNRDF_API_KEY` env var
- **Graceful Degradation** - Development vs production modes
- **Audit Logging** - Complete authentication attempt tracking
- **Zero Dependencies** - Uses only `hash-wasm` and Node.js crypto

## Quick Start

```javascript
import { createAuthenticator } from '@unrdf/daemon';

// Generate API key and create authenticator
const { authenticator, key } = await createAuthenticator({
  environment: 'production'
});

console.log('API Key:', key); // Give to user (shown once)

// Authenticate requests
const result = await authenticator.authenticate({
  headers: { 'x-api-key': key }
});

console.log('Authenticated:', result.authenticated);
```

## API Reference

### Crypto Utils

#### `generateSecureApiKey(length?: number): string`

Generate a cryptographically secure random API key.

- **Parameters:**
  - `length` (optional) - Byte length of key (16-64 bytes, default: 32)
- **Returns:** Hexadecimal string (2x length characters)
- **Throws:** `RangeError` if length out of range

```javascript
import { generateSecureApiKey } from '@unrdf/daemon';

const key = generateSecureApiKey(); // 64-char hex string
```

#### `hashApiKey(apiKey: string): Promise<string>`

Hash API key using BLAKE3.

- **Parameters:**
  - `apiKey` - API key to hash
- **Returns:** Promise resolving to BLAKE3 hash (64 hex chars)
- **Throws:** `TypeError` if not a string, `Error` if empty

```javascript
const hash = await hashApiKey('my-api-key-12345678');
// Returns: "c4d2e3f4a5b6..." (64 characters)
```

#### `verifyApiKey(providedKey: string, storedHash: string): Promise<boolean>`

Verify API key against stored hash using constant-time comparison.

- **Parameters:**
  - `providedKey` - User-provided API key
  - `storedHash` - Stored BLAKE3 hash
- **Returns:** Promise resolving to `true` if valid, `false` otherwise
- **Throws:** `TypeError` if arguments not strings

```javascript
const isValid = await verifyApiKey(userKey, storedHash);
if (isValid) {
  // Proceed with operation
}
```

#### `generateApiKeyPair(length?: number): Promise<{key: string, hash: string}>`

Generate API key and hash together.

```javascript
const { key, hash } = await generateApiKeyPair();
// Store hash in database
// Give key to user (only shown once)
```

### ApiKeyAuthenticator

Main authentication class for managing API key validation.

#### Constructor

```javascript
new ApiKeyAuthenticator(options?: {
  storedKeyHash?: string,
  environment?: 'development' | 'production' | 'test',
  requireInDev?: boolean,
  logger?: Console
})
```

- **Options:**
  - `storedKeyHash` - Pre-computed BLAKE3 hash of valid API key
  - `environment` - Runtime environment (default: `process.env.NODE_ENV`)
  - `requireInDev` - Require API key even in development (default: `false`)
  - `logger` - Logger instance (default: `console`)

#### Methods

##### `initialize(): Promise<{key: string, hash: string}>`

Generate and store new API key pair.

```javascript
const authenticator = new ApiKeyAuthenticator();
const { key, hash } = await authenticator.initialize();
```

##### `authenticate(context): Promise<AuthContext>`

Authenticate request based on API key.

- **Parameters:**
  - `context.headers` - Request headers object
  - `context.env` - Environment variables (default: `process.env`)
- **Returns:** Promise resolving to authentication context
- **Throws:** Error if authentication fails in production

```javascript
const result = await authenticator.authenticate({
  headers: { 'x-api-key': 'user-key' }
});

// result = {
//   authenticated: boolean,
//   keyHash?: string,
//   environment: 'development' | 'production' | 'test',
//   timestamp: Date,
//   source: 'header' | 'env' | 'none'
// }
```

##### `setStoredHash(hash: string): void`

Update the stored API key hash.

##### `getAuditLog(): Array`

Retrieve authentication attempt audit log.

```javascript
const log = authenticator.getAuditLog();
// Returns array of: { timestamp, environment, source, success, reason }
```

##### `clearAuditLog(): void`

Clear the audit log.

### Helper Functions

#### `createAuthMiddleware(authenticator): Function`

Create authentication middleware function.

```javascript
const authMiddleware = createAuthMiddleware(authenticator);

// Use in request handler
const authResult = await authMiddleware(context);
```

#### `createAuthenticator(options): Promise<{authenticator, key}>`

Convenience function to create and initialize authenticator.

```javascript
const { authenticator, key } = await createAuthenticator({
  environment: 'production'
});

console.log('Save this key:', key);
```

## Environment Configuration

### Environment Variables

- `UNRDF_API_KEY` - API key for authentication
- `NODE_ENV` - Environment mode (`development`, `production`, `test`)

### Production Setup

```bash
# Generate API key
export UNRDF_API_KEY="your-generated-key-here"

# Start daemon
NODE_ENV=production node daemon.mjs
```

### Development Setup

```bash
# Optional in development
# Warning will be shown if not provided
node daemon.mjs
```

## Security Best Practices

### 1. Key Generation

Always use `generateSecureApiKey()` for key generation. Never use predictable values.

```javascript
// ✅ CORRECT
const key = generateSecureApiKey();

// ❌ WRONG
const key = 'my-api-key'; // Predictable, insecure
```

### 2. Key Storage

Store only the hash, never the plaintext key.

```javascript
// ✅ CORRECT
const { key, hash } = await generateApiKeyPair();
database.store({ userId, keyHash: hash }); // Store hash
sendToUser(key); // Send key once

// ❌ WRONG
database.store({ userId, apiKey: key }); // Never store plaintext
```

### 3. Key Transmission

Transmit keys only over HTTPS and show once during generation.

```javascript
// ✅ CORRECT
app.post('/api/keys', async (req, res) => {
  const { key, hash } = await generateApiKeyPair();
  await db.storeHash(hash);
  res.json({ key }); // Shown once
});

// ❌ WRONG
app.get('/api/keys/:id', (req, res) => {
  const key = db.getPlaintextKey(id); // Never retrieve plaintext
  res.json({ key });
});
```

### 4. Environment Variables

Use environment variables for server-side authentication, not client-side.

```bash
# ✅ CORRECT - Server environment
export UNRDF_API_KEY="server-key"

# ❌ WRONG - Client bundle
REACT_APP_API_KEY="exposed-key" # Will be in client bundle!
```

### 5. Production Requirements

Always require API keys in production.

```javascript
// ✅ CORRECT
const auth = new ApiKeyAuthenticator({
  environment: 'production', // Blocks missing keys
  storedKeyHash: hash
});

// ❌ WRONG
const auth = new ApiKeyAuthenticator({
  environment: 'development' // Allows missing keys
});
```

## Testing

### Unit Tests

Run the comprehensive test suite:

```bash
# Run all tests
pnpm test auth-api-key.test.mjs

# Manual verification
node test-auth-manual.mjs
```

### Test Coverage

- **62 test cases** covering:
  - Key generation (5 tests)
  - Hashing (5 tests)
  - Verification (5 tests)
  - Key pairs (3 tests)
  - Schema validation (4 tests)
  - Authenticator (20+ tests)
  - Middleware (3 tests)
  - Helpers (3 tests)
  - Environment handling (8+ tests)
  - Audit logging (6+ tests)

## Performance

### Benchmarks

- **Key Generation:** <1ms
- **BLAKE3 Hashing:** <1ms
- **Verification:** <2ms (constant-time)
- **Authentication:** <5ms end-to-end

### Memory Footprint

- Audit log: Auto-limited to 1000 entries (~100KB)
- No memory leaks in long-running daemons

## Error Handling

All functions throw descriptive errors:

```javascript
try {
  await authenticator.authenticate(context);
} catch (err) {
  if (err.message.includes('Invalid API key')) {
    // Wrong key provided
  } else if (err.message.includes('required in production')) {
    // Missing key in production
  } else if (err.message.includes('Invalid API key format')) {
    // Malformed key
  }
}
```

## Integration Examples

See `/examples/06-api-key-authentication.mjs` for:

1. Basic authentication
2. Environment variable usage
3. Middleware integration
4. Development vs production modes
5. Audit logging
6. Daemon integration pattern

## Migration Guide

### From No Authentication

```javascript
// Before
const daemon = new Daemon({ id: 'my-daemon' });

// After
import { createAuthenticator } from '@unrdf/daemon';

const { authenticator, key } = await createAuthenticator({
  environment: 'production'
});

console.log('API Key (save this):', key);

// Add authentication to all operations
const daemon = new Daemon({
  id: 'my-daemon',
  authenticator
});
```

### From Basic Auth

Replace Basic Auth with API key authentication for better security and performance.

## Troubleshooting

### "Invalid API key format"

Ensure key is hexadecimal and 32-128 characters.

```javascript
// Valid formats
const key = generateSecureApiKey(); // 64 hex chars
const key = 'a'.repeat(64); // Valid hex
```

### "No stored API key hash configured"

Initialize the authenticator with a hash.

```javascript
const { authenticator, key } = await createAuthenticator();
// OR
authenticator.setStoredHash(existingHash);
```

### "API key required in production environment"

Provide API key via header or environment variable.

```javascript
// Via header
context = { headers: { 'x-api-key': key } };

// Via environment
process.env.UNRDF_API_KEY = key;
```

## License

MIT
