/**
 * @fileoverview Security Best Practices Examples for UNRDF
 * Demonstrates secure coding patterns following OWASP Top 10 guidelines
 */

import {
  sanitizeHTML,
  sanitizeURL,
  isPathSafe,
  RateLimiter,
  CSRFTokenManager,
  hashPassword,
  verifyPassword,
  getSecurityHeaders,
  validateInput
} from '../packages/core/src/security.mjs';

import {
  EmailSchema,
  SafeStringSchema,
  SPARQLQuerySchema,
  PasswordSchema,
  URLSchema,
  validateFields
} from '../packages/core/src/security-schemas.mjs';

// ============================================================================
// 1. INPUT VALIDATION - Always validate user input
// ============================================================================

/**
 * Example: Validating user registration data
 */
export function secureUserRegistration(userData) {
  const result = validateFields(userData, {
    email: EmailSchema,
    username: SafeStringSchema,
    password: PasswordSchema
  });

  if (!result.valid) {
    throw new Error(`Validation failed: ${JSON.stringify(result.errors)}`);
  }

  return {
    email: userData.email,
    username: userData.username,
    // Password will be hashed separately
  };
}

/**
 * Example: Validating SPARQL query input
 */
export function secureSPARQLQuery(queryString) {
  // Validate query format
  const result = SPARQLQuerySchema.safeParse(queryString);

  if (!result.success) {
    throw new Error('Invalid SPARQL query');
  }

  // Additional validation for dangerous patterns
  const { valid, issues } = validateInput(queryString);
  if (!valid) {
    throw new Error(`Query validation failed: ${issues.join(', ')}`);
  }

  return result.data;
}

// ============================================================================
// 2. OUTPUT SANITIZATION - Prevent XSS attacks
// ============================================================================

/**
 * Example: Rendering user-generated content safely
 */
export function renderUserComment(comment) {
  // Sanitize HTML to prevent XSS
  const safeHTML = sanitizeHTML(comment.text);

  return {
    ...comment,
    text: safeHTML,
    author: sanitizeHTML(comment.author)
  };
}

/**
 * Example: Safe URL redirection
 */
export function safeRedirect(url, allowedDomains = ['example.com']) {
  const safeURL = sanitizeURL(url, allowedDomains);

  if (!safeURL) {
    throw new Error('Invalid or disallowed redirect URL');
  }

  return safeURL;
}

// ============================================================================
// 3. AUTHENTICATION & AUTHORIZATION - Secure user authentication
// ============================================================================

/**
 * Example: Secure password hashing during registration
 */
export async function registerUser(username, email, password) {
  // Validate password strength
  PasswordSchema.parse(password);

  // Hash password securely
  const { hash, salt } = await hashPassword(password);

  // Store user (pseudo-code)
  const user = {
    username,
    email,
    passwordHash: hash,
    passwordSalt: salt,
    createdAt: new Date().toISOString()
  };

  // In real code: await db.users.insert(user)
  return user;
}

/**
 * Example: Secure password verification during login
 */
export async function loginUser(email, password, storedUser) {
  // Verify password
  const isValid = await verifyPassword(
    password,
    storedUser.passwordHash,
    storedUser.passwordSalt
  );

  if (!isValid) {
    throw new Error('Invalid credentials');
  }

  // Generate session token (simplified)
  return {
    userId: storedUser.id,
    sessionToken: crypto.randomBytes(32).toString('hex')
  };
}

// ============================================================================
// 4. RATE LIMITING - Prevent abuse and DoS attacks
// ============================================================================

/**
 * Example: Rate limiting API endpoint
 */
const apiLimiter = new RateLimiter({
  maxRequests: 100,
  windowMs: 60000 // 1 minute
});

export function handleAPIRequest(userId, handler) {
  // Check rate limit
  if (!apiLimiter.tryConsume(userId)) {
    const usage = apiLimiter.getUsage(userId);
    throw new Error(
      `Rate limit exceeded. Resets at ${new Date(usage.resetAt).toISOString()}`
    );
  }

  // Process request
  return handler();
}

/**
 * Example: Aggressive rate limiting for authentication
 */
const authLimiter = new RateLimiter({
  maxRequests: 5,
  windowMs: 300000 // 5 minutes
});

export function handleLogin(ipAddress, credentials) {
  if (!authLimiter.tryConsume(ipAddress)) {
    throw new Error('Too many login attempts. Please try again later.');
  }

  // Proceed with login
  return loginUser(credentials.email, credentials.password);
}

// ============================================================================
// 5. CSRF PROTECTION - Prevent Cross-Site Request Forgery
// ============================================================================

const csrfManager = new CSRFTokenManager({
  tokenLength: 32,
  expiryMs: 3600000 // 1 hour
});

/**
 * Example: Generating CSRF token for form
 */
export function renderSecureForm(sessionId) {
  const csrfToken = csrfManager.generate(sessionId);

  return {
    formHTML: `
      <form method="POST" action="/api/update">
        <input type="hidden" name="csrf_token" value="${csrfToken}">
        <!-- other form fields -->
      </form>
    `,
    csrfToken
  };
}

/**
 * Example: Verifying CSRF token on form submission
 */
export function handleFormSubmission(sessionId, submittedToken, formData) {
  // Verify CSRF token
  if (!csrfManager.verify(sessionId, submittedToken)) {
    throw new Error('Invalid CSRF token');
  }

  // Process form data
  return processForm(formData);
}

// ============================================================================
// 6. SECURE FILE HANDLING - Prevent path traversal
// ============================================================================

/**
 * Example: Secure file upload
 */
export function handleFileUpload(filename, baseDir) {
  // Validate filename
  if (!isPathSafe(filename, baseDir)) {
    throw new Error('Invalid filename - potential path traversal detected');
  }

  // Additional checks
  const allowedExtensions = ['.txt', '.md', '.json', '.ttl'];
  const ext = filename.substring(filename.lastIndexOf('.'));

  if (!allowedExtensions.includes(ext)) {
    throw new Error(`File type ${ext} not allowed`);
  }

  // Safe to process
  const safePath = `${baseDir}/${filename.replace(/[^a-zA-Z0-9._-]/g, '_')}`;
  return safePath;
}

/**
 * Example: Secure file download
 */
export function handleFileDownload(requestedFile, baseDir) {
  // Prevent directory traversal
  if (!isPathSafe(requestedFile, baseDir)) {
    throw new Error('Access denied');
  }

  // Resolve to absolute path and verify it's within baseDir
  const safePath = `${baseDir}/${requestedFile}`;

  // In real code: verify file exists and is within baseDir
  return safePath;
}

// ============================================================================
// 7. SECURITY HEADERS - Protect against common attacks
// ============================================================================

/**
 * Example: Setting security headers on HTTP response
 */
export function applySecurityHeaders(response) {
  const headers = getSecurityHeaders();

  // Apply all security headers
  for (const [name, value] of Object.entries(headers)) {
    response.setHeader(name, value);
  }

  return response;
}

/**
 * Example: Custom CSP for specific route
 */
export function applyCustomCSP(response, options = {}) {
  const baseHeaders = getSecurityHeaders();

  // Customize CSP for specific needs
  const csp = options.allowInlineScripts
    ? "default-src 'self'; script-src 'self' 'unsafe-inline'"
    : baseHeaders['Content-Security-Policy'];

  response.setHeader('Content-Security-Policy', csp);

  return response;
}

// ============================================================================
// 8. SECURE API DESIGN - RESTful API best practices
// ============================================================================

/**
 * Example: Secure API endpoint handler
 */
export async function secureAPIEndpoint(request) {
  // 1. Apply security headers
  const response = { headers: getSecurityHeaders() };

  // 2. Rate limiting
  const clientIP = request.ip;
  if (!apiLimiter.tryConsume(clientIP)) {
    return {
      ...response,
      status: 429,
      body: { error: 'Rate limit exceeded' }
    };
  }

  // 3. Validate authentication token
  const token = request.headers.authorization?.replace('Bearer ', '');
  if (!token) {
    return {
      ...response,
      status: 401,
      body: { error: 'Unauthorized' }
    };
  }

  // 4. Validate request body
  const bodyValidation = SafeStringSchema.safeParse(request.body);
  if (!bodyValidation.success) {
    return {
      ...response,
      status: 400,
      body: { error: 'Invalid request body', details: bodyValidation.error }
    };
  }

  // 5. Process request
  const result = await processRequest(bodyValidation.data);

  return {
    ...response,
    status: 200,
    body: result
  };
}

// ============================================================================
// 9. SECURE ERROR HANDLING - Don't leak sensitive information
// ============================================================================

/**
 * Example: Secure error handling
 */
export function handleError(error, isProduction = true) {
  // Log full error internally (with sensitive data redacted)
  console.error('[ERROR]', {
    message: error.message,
    stack: isProduction ? 'REDACTED' : error.stack,
    timestamp: new Date().toISOString()
  });

  // Return sanitized error to client
  if (isProduction) {
    return {
      error: 'An error occurred',
      code: error.code || 'INTERNAL_ERROR',
      // Don't expose stack traces or implementation details
    };
  } else {
    return {
      error: error.message,
      code: error.code,
      stack: error.stack
    };
  }
}

// ============================================================================
// 10. SECURE SPARQL EXECUTION - RDF-specific security
// ============================================================================

/**
 * Example: Execute SPARQL query with security measures
 */
export async function executeSPARQL(query, store, userId) {
  // 1. Validate query syntax
  const validatedQuery = secureSPARQLQuery(query);

  // 2. Rate limit per user
  if (!apiLimiter.tryConsume(`sparql:${userId}`)) {
    throw new Error('SPARQL query rate limit exceeded');
  }

  // 3. Set timeout to prevent DoS
  const timeout = 5000; // 5 seconds

  // 4. Execute with safety measures (pseudo-code)
  const promise = store.query(validatedQuery);
  const timeoutPromise = new Promise((_, reject) =>
    setTimeout(() => reject(new Error('Query timeout')), timeout)
  );

  try {
    const results = await Promise.race([promise, timeoutPromise]);
    return results;
  } catch (error) {
    // Handle error securely
    throw new Error('Query execution failed');
  }
}

// ============================================================================
// HELPER FUNCTIONS (for examples)
// ============================================================================

function processForm(formData) {
  // Placeholder for form processing
  return { success: true, data: formData };
}

async function processRequest(data) {
  // Placeholder for request processing
  return { success: true, data };
}

// ============================================================================
// USAGE EXAMPLES
// ============================================================================

/**
 * Example: Complete secure user flow
 */
export async function exampleSecureUserFlow() {
  try {
    // 1. Register user with strong password
    const user = await registerUser(
      'johndoe',
      'john@example.com',
      'MyStr0ng!Pass@2024'
    );

    console.log('User registered:', user.username);

    // 2. Generate CSRF token for session
    const sessionId = 'session_123';
    const form = renderSecureForm(sessionId);
    console.log('CSRF token generated:', form.csrfToken);

    // 3. Handle form submission
    const formData = { name: 'John Doe', bio: 'Hello <script>alert("xss")</script>' };
    const result = handleFormSubmission(sessionId, form.csrfToken, formData);
    console.log('Form processed:', result);

    // 4. Render comment safely
    const comment = { text: '<b>Great post!</b>', author: 'John' };
    const safeComment = renderUserComment(comment);
    console.log('Safe comment:', safeComment);

  } catch (error) {
    const safeError = handleError(error, process.env.NODE_ENV === 'production');
    console.error('Error:', safeError);
  }
}

// Run example if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  exampleSecureUserFlow();
}
