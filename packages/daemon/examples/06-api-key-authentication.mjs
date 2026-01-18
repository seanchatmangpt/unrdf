/**
 * @file API Key Authentication Example
 * @description Demonstrates API key authentication for daemon operations
 */

import {
  ApiKeyAuthenticator,
  createAuthenticator,
  createAuthMiddleware,
} from '../src/auth/api-key-auth.mjs';

import {
  generateApiKeyPair,
} from '../src/auth/crypto-utils.mjs';

async function example1_BasicAuthentication() {
  console.log('Example 1: Basic API Key Authentication\n');

  // Generate a new API key pair
  const { key, hash } = await generateApiKeyPair();

  console.log('Generated API Key:', key);
  console.log('Key Hash (store this):', hash);
  console.log('');

  // Create authenticator with the hash
  const authenticator = new ApiKeyAuthenticator({
    storedKeyHash: hash,
    environment: 'production',
  });

  // Simulate request with API key in header
  const result = await authenticator.authenticate({
    headers: { 'x-api-key': key },
  });

  console.log('Authentication Result:', result);
  console.log('Authenticated:', result.authenticated);
  console.log('Source:', result.source);
  console.log('');
}

async function example2_EnvironmentVariable() {
  console.log('Example 2: Environment Variable Authentication\n');

  // Create authenticator
  const { authenticator, key } = await createAuthenticator({
    environment: 'production',
  });

  console.log('API Key (set as UNRDF_API_KEY):', key);
  console.log('');

  // Authenticate using environment variable
  const result = await authenticator.authenticate({
    headers: {},
    env: { UNRDF_API_KEY: key },
  });

  console.log('Authenticated via env:', result.authenticated);
  console.log('Source:', result.source);
  console.log('');
}

async function example3_Middleware() {
  console.log('Example 3: Authentication Middleware\n');

  // Setup
  const { authenticator, key } = await createAuthenticator();

  // Create middleware
  const authMiddleware = createAuthMiddleware(authenticator);

  // Use in request handler
  try {
    const authResult = await authMiddleware({
      headers: { 'x-api-key': key },
    });

    console.log('Middleware authenticated request');
    console.log('Auth context:', authResult);
  } catch (err) {
    console.error('Middleware rejected request:', err.message);
  }

  console.log('');
}

async function example4_DevelopmentMode() {
  console.log('Example 4: Development vs Production Mode\n');

  const { hash } = await generateApiKeyPair();

  // Development mode - allows missing key with warning
  const devAuth = new ApiKeyAuthenticator({
    storedKeyHash: hash,
    environment: 'development',
    logger: {
      warn: (msg) => console.log('⚠️  Dev Warning:', msg),
    },
  });

  const devResult = await devAuth.authenticate({ headers: {} });
  console.log('Development mode result:', devResult.authenticated);
  console.log('');

  // Production mode - blocks missing key
  const prodAuth = new ApiKeyAuthenticator({
    storedKeyHash: hash,
    environment: 'production',
  });

  try {
    await prodAuth.authenticate({ headers: {} });
    console.log('Production mode accepted missing key (UNEXPECTED)');
  } catch (err) {
    console.log('✅ Production mode rejected missing key');
    console.log('Error:', err.message);
  }

  console.log('');
}

async function example5_AuditLog() {
  console.log('Example 5: Audit Log\n');

  const { authenticator, key } = await createAuthenticator();
  const wrongKey = 'a'.repeat(64); // Valid format but wrong key

  // Successful authentication
  await authenticator.authenticate({
    headers: { 'x-api-key': key },
  });

  // Failed authentication
  try {
    await authenticator.authenticate({
      headers: { 'x-api-key': wrongKey },
    });
  } catch {
    // Expected
  }

  // Get audit log
  const log = authenticator.getAuditLog();

  console.log('Audit Log Entries:', log.length);
  log.forEach((entry, i) => {
    console.log(`  ${i + 1}. ${entry.success ? '✅' : '❌'} ${entry.reason} (${entry.source})`);
  });

  console.log('');
}

async function example6_DaemonIntegration() {
  console.log('Example 6: Daemon Integration Pattern\n');

  // Initialize daemon with authentication
  const { authenticator, key } = await createAuthenticator({
    environment: 'production',
  });

  console.log('Daemon API Key:', key);
  console.log('Store this key securely and provide to clients');
  console.log('');

  // Simulate daemon operation request
  class DaemonOperationHandler {
    constructor(authenticator) {
      this.authenticator = authenticator;
    }

    async executeOperation(context, operation) {
      // Authenticate first
      const authResult = await this.authenticator.authenticate(context);

      if (!authResult.authenticated) {
        throw new Error('Operation requires authentication');
      }

      // Execute operation
      console.log(`Executing ${operation} (authenticated via ${authResult.source})`);
      return { success: true, operation };
    }
  }

  const handler = new DaemonOperationHandler(authenticator);

  // Client request with API key
  const result = await handler.executeOperation(
    { headers: { 'x-api-key': key } },
    'backup-knowledge-graph'
  );

  console.log('Operation result:', result);
  console.log('');
}

async function runAllExamples() {
  console.log('='.repeat(60));
  console.log('API KEY AUTHENTICATION EXAMPLES');
  console.log('='.repeat(60));
  console.log('');

  await example1_BasicAuthentication();
  await example2_EnvironmentVariable();
  await example3_Middleware();
  await example4_DevelopmentMode();
  await example5_AuditLog();
  await example6_DaemonIntegration();

  console.log('='.repeat(60));
  console.log('All examples completed successfully');
  console.log('='.repeat(60));
}

runAllExamples().catch(console.error);
