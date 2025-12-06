# Deprecated Features

## v5.0.0-beta.2

### knowledge-hooks-api OTEL Validation

**Status**: Deprecated (Non-Functional)

**Description**: The knowledge-hooks-api OTEL validation suite was designed to validate hook definition, registration, execution, and batching through OpenTelemetry span analysis.

**Issue**: No OTEL spans are collected during validation execution, resulting in a 0/100 score.

**Root Cause**: The hooks implementation in `packages/hooks/` exists but doesn't integrate with the OTEL validation runner. The runner expects spans like:
- `hook.define`
- `hook.validate`
- `hook.execute`
- `hook.batch.execute`

These spans are never generated because the hooks code doesn't run during validation.

**Impact**:
- Lowers overall OTEL score from 100/100 to 83/100
- 5 of 6 features pass validation
- knowledge-hooks-api contributes 0/100 points

**Workaround**: Accept the 83/100 score. The failing feature doesn't impact core RDF functionality.

**Future Options**:
1. Fix OTEL integration to generate required spans during validation
2. Remove knowledge-hooks-api validation entirely
3. Implement hooks with proper OTEL instrumentation
4. Mark as experimental feature not subject to validation

**Decision**: For v5.0.0-beta.2, accept the 83/100 score and document this as a known limitation.

**Recovery**: If hooks functionality is needed in the future, the implementation exists in:
- `packages/hooks/src/hooks/knowledge-hook-engine.mjs`
- `packages/hooks/src/hooks/knowledge-hook-manager.mjs`
- `packages/hooks/src/hooks/hook-executor-batching.mjs`
- `packages/hooks/src/hooks/policy-pack.mjs`

---

**Last Updated**: 2025-12-06
**Deprecated In**: v5.0.0-beta.2
**Reason**: Non-functional OTEL validation, not core to RDF functionality
