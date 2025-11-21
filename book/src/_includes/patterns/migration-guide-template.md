# Migration Guide Template

Standard migration approach for adopting UNRDF features.

## Step 1: Assess Current Implementation

**Identify existing code:**
- [ ] Locate feature implementation
- [ ] Document current behavior
- [ ] List dependencies
- [ ] Measure baseline performance

**Example:**
```typescript
// BEFORE: Traditional approach
const result = await traditionalImplementation(input);
```

## Step 2: Plan Migration

**Create migration checklist:**
- [ ] Choose UNRDF equivalent feature
- [ ] Map old patterns to new patterns
- [ ] Identify breaking changes
- [ ] Plan rollback strategy

## Step 3: Implement Side-by-Side

**Run both implementations:**
```typescript
// Run old + new in parallel (dark launch)
const oldResult = await traditionalImplementation(input);
const newResult = await unrdfImplementation(input);

// Compare results
if (JSON.stringify(oldResult) !== JSON.stringify(newResult)) {
  console.warn('Results differ:', { oldResult, newResult });
}

// Return old result (safe)
return oldResult;
```

## Step 4: Validate Parity

**Test thoroughly:**
- [ ] Unit tests pass
- [ ] Integration tests pass
- [ ] Performance meets targets
- [ ] No regressions

## Step 5: Gradual Rollout

**Incremental migration:**
```typescript
// Feature flag-based rollout
const useUnrdf = getFeatureFlag('unrdf-migration', userId);

if (useUnrdf) {
  return await unrdfImplementation(input);
} else {
  return await traditionalImplementation(input);
}
```

**Rollout schedule:**
- Week 1: 5% of traffic
- Week 2: 25% of traffic
- Week 3: 50% of traffic
- Week 4: 100% of traffic

## Step 6: Cleanup

**Remove old implementation:**
```typescript
// AFTER: UNRDF only
const result = await unrdfImplementation(input);
```

**Final checklist:**
- [ ] Remove traditional code
- [ ] Update documentation
- [ ] Train team on new approach
- [ ] Monitor production metrics

See specific chapters for feature-specific migration paths.
