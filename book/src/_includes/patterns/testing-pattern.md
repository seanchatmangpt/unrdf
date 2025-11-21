# Testing Pattern (Shared)

Standard testing approach used across UNRDF features.

## Basic Test Structure

```typescript
import { describe, it, expect, beforeEach } from 'vitest';
import { createKnowledgeEngine } from 'unrdf';

describe('Feature Name', () => {
  let engine;

  beforeEach(async () => {
    engine = await createKnowledgeEngine();
  });

  it('should handle valid input', async () => {
    // Arrange: Set up test data
    const input = createTestInput();

    // Act: Execute feature
    const result = await engine.execute(input);

    // Assert: Verify outcome
    expect(result).toBeDefined();
    expect(result.status).toBe('success');
  });

  it('should reject invalid input', async () => {
    // Arrange: Invalid data
    const invalidInput = createInvalidInput();

    // Act & Assert: Expect error
    await expect(
      engine.execute(invalidInput)
    ).rejects.toThrow('Validation failed');
  });
});
```

## Mocking External Dependencies

```typescript
import { vi } from 'vitest';

// Mock external API
vi.mock('@/lib/external-api', () => ({
  fetchData: vi.fn(() => Promise.resolve({ success: true }))
}));
```

## Performance Testing

```typescript
it('should meet performance targets', async () => {
  const start = performance.now();

  await engine.execute(largeInput);

  const duration = performance.now() - start;
  expect(duration).toBeLessThan(100); // < 100ms
});
```

See specific chapters for feature-specific test patterns.
