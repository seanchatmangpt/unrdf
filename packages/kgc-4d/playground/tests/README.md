# KGC-4D Playground Tests

Comprehensive Playwright E2E test suite for the KGC-4D Shard-Based Architecture playground.

## Test Coverage

### 1. Connection Tests (`connection.spec.js`)
- ✅ Initial connection establishment
- ✅ Receiving initial Shard
- ✅ Heartbeat keep-alive
- ✅ Disconnection and reconnection
- ✅ Error handling
- ✅ Connection status display

**What it validates**: The Tether (SSE connection) between Browser and Server is established, maintained, and recoverable.

### 2. Shard Projection Tests (`shard-projection.spec.js`)
- ✅ Loading Shard with project data
- ✅ Displaying quads in Shard Viewer
- ✅ Filtering quads by search
- ✅ Exporting as N-Quads
- ✅ Shard metadata accuracy
- ✅ Reflecting universe changes

**What it validates**: Check-Out operation (Server → Client) correctly projects Universe to Shard.

### 3. Entity Operations Tests (`entity-operations.spec.js`)
- ✅ Loading entity properties
- ✅ Selecting entities
- ✅ Editing valid properties
- ✅ Validation: Rejecting invalid budget (>100k)
- ✅ Validation: Rejecting empty name
- ✅ Validation: Enforcing status enum
- ✅ Browsing by entity type

**What it validates**: Client-side UI correctly displays entities and prepares deltas for submission.

### 4. Delta Validation Tests (`delta-validation.spec.js`)
- ✅ Valid delta submission (ACK)
- ✅ Rejecting invalid budget
- ✅ Rejecting invalid status
- ✅ Rejecting empty name
- ✅ Recording ACK events
- ✅ Vector clock inclusion
- ✅ Syncing state display

**What it validates**: Check-In operation (Client → Server) with Knowledge Hooks validation.

### 5. UI Interaction Tests (`ui-interactions.spec.js`)
- ✅ Tab navigation
- ✅ Universe Explorer display
- ✅ Event Timeline display
- ✅ Connection Status display
- ✅ Edit button hover behavior
- ✅ Hero section metrics
- ✅ Responsive design (mobile/tablet/desktop)
- ✅ Rapid tab switching
- ✅ Footer branding

**What it validates**: UI is responsive, interactive, and handles rapid interactions.

### 6. API Endpoint Tests (`api-endpoints.spec.js`)
- ✅ `GET /api/shard` returns valid shard
- ✅ `GET /api/shard?stats=true` returns statistics
- ✅ `GET /api/shard` with filters (subject, type)
- ✅ `POST /api/delta` with valid operations (ACK)
- ✅ `POST /api/delta` with invalid budget (REJECT)
- ✅ `POST /api/delta` with invalid status (REJECT)
- ✅ `POST /api/delta` with empty name (REJECT)
- ✅ `POST /api/delta` error handling
- ✅ `GET /api/tether` event stream
- ✅ Vector clock inclusion
- ✅ Timestamp accuracy
- ✅ Concurrent request handling

**What it validates**: API endpoints correctly implement Shard-Based Architecture with proper validation.

## Running Tests

### Install Dependencies
```bash
cd packages/kgc-4d/playground
pnpm install
```

### Run All Tests
```bash
# Headless (default)
pnpm test:e2e

# With UI mode
pnpm test:e2e:ui

# With debug mode
pnpm test:e2e:debug

# With visible browser
pnpm test:e2e:headed
```

### Run Specific Test Suite
```bash
# Connection tests only
pnpm test:e2e connection.spec.js

# API tests only
pnpm test:e2e api-endpoints.spec.js

# Entity operations only
pnpm test:e2e entity-operations.spec.js
```

### Run with Specific Browser
```bash
# Just Chromium
pnpm test:e2e -- --project=chromium

# Just Firefox
pnpm test:e2e -- --project=firefox

# Just WebKit
pnpm test:e2e -- --project=webkit
```

### Generate Report
```bash
# After tests complete
pnpm test:e2e
# Then open report
npx playwright show-report test-results
```

## Test Architecture

### Fixtures (`fixtures.js`)
Custom Playwright fixtures for KGC-4D:

- `waitForSSE` - Wait for specific SSE event type
- `submitAndWaitForResult` - Submit delta and get ACK/REJECT
- `getCurrentShard` - Fetch current Shard from API
- `getUniverseStats` - Fetch Universe statistics

### Test Flow

```
1. Navigate to playground
2. Wait for connection (1-2s)
3. Verify Shard received
4. Interact with UI or API
5. Validate response
6. Check event timeline
7. Verify state consistency
```

## Key Validation Points

| Component | Validates |
|-----------|-----------|
| **Connection** | Tether SSE establishes and maintains |
| **Projection** | Server correctly filters Universe → Shard |
| **Validation** | Knowledge Hooks reject invalid deltas |
| **ACK/REJECT** | Server returns proper status and metadata |
| **Vector Clock** | Causality tracking is included |
| **UI State** | Optimistic updates work correctly |
| **Error Handling** | Invalid operations fail gracefully |

## Success Criteria

All tests pass when:

- ✅ Connection established (< 2 seconds)
- ✅ Shard received with valid quads
- ✅ Valid operations return ACK
- ✅ Invalid operations return REJECT with reason
- ✅ Vector clock included in all responses
- ✅ UI renders without errors
- ✅ Event timeline records operations
- ✅ Timestamps in ISO 8601 format
- ✅ Concurrent requests handled correctly

## Debugging Tests

### UI Mode (Interactive)
```bash
pnpm test:e2e:ui
```
Click tests to run individually, inspect elements, see live logs.

### Debug Mode (Browser Tools)
```bash
pnpm test:e2e:debug
```
Opens Playwright Inspector with step-through debugging.

### Headed Mode (Visible Browser)
```bash
pnpm test:e2e:headed
```
Runs tests with visible browser for visual validation.

### Screenshots and Videos
Tests automatically capture:
- Screenshots on failure (saved to `test-results/`)
- Videos on failure (saved to `test-results/`)

View with:
```bash
npx playwright show-report test-results
```

## Performance Benchmarks

Expected timings:
- Connection establishment: < 2 seconds
- Shard projection: < 500ms
- Delta submission: < 1 second
- UI interaction response: < 300ms
- Event propagation: < 1 second

## Continuous Integration

Configure in your CI (GitHub Actions, etc.):

```yaml
- name: Install Playwright
  run: pnpm install

- name: Run E2E Tests
  run: pnpm test:e2e

- name: Upload Report
  if: always()
  uses: actions/upload-artifact@v3
  with:
    name: playwright-report
    path: test-results/
```

## Troubleshooting

### Tests timeout
- Increase timeout in `playwright.config.js`
- Check if server is running on port 3001
- Verify network connectivity

### Connection not established
- Ensure playground is running: `pnpm dev`
- Check for port conflicts
- Verify environment (should use `http://localhost:3001`)

### Validation tests fail
- Knowledge Hooks may have changed
- Verify server validation rules in `lib/server/delta.mjs`
- Check test expectations match actual rules

### Flaky tests
- Increase wait times in tests
- Ensure server is stable before running tests
- Check for race conditions in test setup
