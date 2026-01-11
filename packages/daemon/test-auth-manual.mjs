/**
 * Manual test for authentication system
 * Run with: node test-auth-manual.mjs
 */

import {
  generateSecureApiKey,
  hashApiKey,
  verifyApiKey,
  generateApiKeyPair,
} from './src/auth/crypto-utils.mjs';

import {
  ApiKeyAuthenticator,
  createAuthenticator,
} from './src/auth/api-key-auth.mjs';

async function runTests() {
  console.log('🧪 Starting manual authentication tests...\n');

  let passed = 0;
  let failed = 0;

  // Test 1: Generate API key
  console.log('Test 1: Generate API key');
  try {
    const key = generateSecureApiKey();
    if (key.length === 64 && /^[a-f0-9]+$/i.test(key)) {
      console.log('✅ PASS - Generated 64-char hex key');
      passed++;
    } else {
      console.log('❌ FAIL - Invalid key format');
      failed++;
    }
  } catch (err) {
    console.log('❌ FAIL -', err.message);
    failed++;
  }

  // Test 2: Hash API key
  console.log('\nTest 2: Hash API key');
  try {
    const key = 'test-api-key-12345678abcdef';
    const hash = await hashApiKey(key);
    if (hash.length === 64 && /^[a-f0-9]+$/i.test(hash)) {
      console.log('✅ PASS - Generated BLAKE3 hash');
      passed++;
    } else {
      console.log('❌ FAIL - Invalid hash format');
      failed++;
    }
  } catch (err) {
    console.log('❌ FAIL -', err.message);
    failed++;
  }

  // Test 3: Verify API key
  console.log('\nTest 3: Verify API key');
  try {
    const key = 'my-secret-key-123456';
    const hash = await hashApiKey(key);
    const isValid = await verifyApiKey(key, hash);
    if (isValid === true) {
      console.log('✅ PASS - Valid key verified successfully');
      passed++;
    } else {
      console.log('❌ FAIL - Valid key not verified');
      failed++;
    }
  } catch (err) {
    console.log('❌ FAIL -', err.message);
    failed++;
  }

  // Test 4: Reject wrong key
  console.log('\nTest 4: Reject wrong API key');
  try {
    const correctKey = 'correct-key-abcdef';
    const wrongKey = 'wrong-key-fedcba';
    const hash = await hashApiKey(correctKey);
    const isValid = await verifyApiKey(wrongKey, hash);
    if (isValid === false) {
      console.log('✅ PASS - Wrong key rejected');
      passed++;
    } else {
      console.log('❌ FAIL - Wrong key accepted');
      failed++;
    }
  } catch (err) {
    console.log('❌ FAIL -', err.message);
    failed++;
  }

  // Test 5: Generate key pair
  console.log('\nTest 5: Generate API key pair');
  try {
    const { key, hash } = await generateApiKeyPair();
    const isValid = await verifyApiKey(key, hash);
    if (isValid === true && key.length === 64 && hash.length === 64) {
      console.log('✅ PASS - Key pair generated and verified');
      passed++;
    } else {
      console.log('❌ FAIL - Invalid key pair');
      failed++;
    }
  } catch (err) {
    console.log('❌ FAIL -', err.message);
    failed++;
  }

  // Test 6: Authenticator - valid key
  console.log('\nTest 6: Authenticator with valid key');
  try {
    const { key, hash } = await generateApiKeyPair();
    const auth = new ApiKeyAuthenticator({
      storedKeyHash: hash,
      environment: 'development',
    });

    const result = await auth.authenticate({
      headers: { 'x-api-key': key },
    });

    if (result.authenticated === true && result.source === 'header') {
      console.log('✅ PASS - Authenticated with valid key');
      passed++;
    } else {
      console.log('❌ FAIL - Authentication failed');
      failed++;
    }
  } catch (err) {
    console.log('❌ FAIL -', err.message);
    failed++;
  }

  // Test 7: Authenticator - invalid key
  console.log('\nTest 7: Authenticator with invalid key');
  try {
    const { hash } = await generateApiKeyPair();
    const wrongKey = await generateSecureApiKey();
    const auth = new ApiKeyAuthenticator({
      storedKeyHash: hash,
      environment: 'development',
    });

    await auth.authenticate({
      headers: { 'x-api-key': wrongKey },
    });

    console.log('❌ FAIL - Invalid key was accepted');
    failed++;
  } catch (err) {
    if (err.message.includes('Invalid API key')) {
      console.log('✅ PASS - Invalid key rejected');
      passed++;
    } else {
      console.log('❌ FAIL - Wrong error:', err.message);
      failed++;
    }
  }

  // Test 8: Environment variable support
  console.log('\nTest 8: Environment variable support');
  try {
    const { key, hash } = await generateApiKeyPair();
    const auth = new ApiKeyAuthenticator({
      storedKeyHash: hash,
    });

    const result = await auth.authenticate({
      headers: {},
      env: { UNRDF_API_KEY: key },
    });

    if (result.authenticated === true && result.source === 'env') {
      console.log('✅ PASS - Authenticated via environment variable');
      passed++;
    } else {
      console.log('❌ FAIL - Environment auth failed');
      failed++;
    }
  } catch (err) {
    console.log('❌ FAIL -', err.message);
    failed++;
  }

  // Test 9: Development mode graceful degradation
  console.log('\nTest 9: Development mode allows missing key');
  try {
    const { hash } = await generateApiKeyPair();
    const mockLogger = { warn: () => {} };
    const auth = new ApiKeyAuthenticator({
      storedKeyHash: hash,
      environment: 'development',
      logger: mockLogger,
    });

    const result = await auth.authenticate({ headers: {} });

    if (result.authenticated === false) {
      console.log('✅ PASS - Development mode allows missing key');
      passed++;
    } else {
      console.log('❌ FAIL - Unexpected authentication');
      failed++;
    }
  } catch (err) {
    console.log('❌ FAIL -', err.message);
    failed++;
  }

  // Test 10: Production mode blocks missing key
  console.log('\nTest 10: Production mode blocks missing key');
  try {
    const { hash } = await generateApiKeyPair();
    const auth = new ApiKeyAuthenticator({
      storedKeyHash: hash,
      environment: 'production',
    });

    await auth.authenticate({ headers: {} });

    console.log('❌ FAIL - Production mode accepted missing key');
    failed++;
  } catch (err) {
    if (err.message.includes('required in production')) {
      console.log('✅ PASS - Production mode blocked missing key');
      passed++;
    } else {
      console.log('❌ FAIL - Wrong error:', err.message);
      failed++;
    }
  }

  // Test 11: Audit log
  console.log('\nTest 11: Audit log functionality');
  try {
    const { key, hash } = await generateApiKeyPair();
    const auth = new ApiKeyAuthenticator({
      storedKeyHash: hash,
    });

    await auth.authenticate({ headers: { 'x-api-key': key } });
    const log = auth.getAuditLog();

    if (log.length === 1 && log[0].success === true) {
      console.log('✅ PASS - Audit log records attempts');
      passed++;
    } else {
      console.log('❌ FAIL - Audit log not working');
      failed++;
    }
  } catch (err) {
    console.log('❌ FAIL -', err.message);
    failed++;
  }

  // Test 12: createAuthenticator helper
  console.log('\nTest 12: createAuthenticator helper');
  try {
    const { authenticator, key } = await createAuthenticator({
      environment: 'development',
    });

    const result = await authenticator.authenticate({
      headers: { 'x-api-key': key },
    });

    if (result.authenticated === true) {
      console.log('✅ PASS - createAuthenticator works');
      passed++;
    } else {
      console.log('❌ FAIL - Helper function failed');
      failed++;
    }
  } catch (err) {
    console.log('❌ FAIL -', err.message);
    failed++;
  }

  // Summary
  console.log('\n' + '='.repeat(50));
  console.log(`Total: ${passed + failed} tests`);
  console.log(`✅ Passed: ${passed}`);
  console.log(`❌ Failed: ${failed}`);
  console.log(`Pass Rate: ${((passed / (passed + failed)) * 100).toFixed(1)}%`);
  console.log('='.repeat(50));

  if (failed === 0) {
    console.log('\n🎉 All tests passed!');
    process.exit(0);
  } else {
    console.log('\n⚠️  Some tests failed');
    process.exit(1);
  }
}

runTests().catch((err) => {
  console.error('Fatal error:', err);
  process.exit(1);
});
