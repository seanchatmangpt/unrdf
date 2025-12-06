# Errors: @unrdf/streaming

Complete reference for error types and handling.

---

## Error Handling Overview

@unrdf/streaming uses consistent error handling with clear error codes and messages.

---

## Error Categories

### Initialization Errors
Errors that occur during initialization.

### Configuration Errors
Invalid configuration values or conflicts.

### Operation Errors
Errors during normal operations.

### Resource Errors
Out of memory, file not found, permission denied.

### Timeout Errors
Operations exceeding timeout limits.

---

## Common Error Codes

Each error has a standardized code for programmatic handling:

- INIT_ERROR: Initialization failed
- CONFIG_ERROR: Invalid configuration
- TIMEOUT_ERROR: Operation timeout
- RESOURCE_ERROR: Resource unavailable
- PERMISSION_ERROR: Access denied

---

## Error Structure

All errors include:
- Code: For programmatic handling
- Message: Human-readable explanation
- Details: Additional context
- Timestamp: When error occurred
- Stack: For debugging

---

## Error Handling Patterns

### Try-Catch Pattern
Wrap operations in try-catch blocks.

### Promise Rejection Pattern
Handle promise rejections with .catch().

### Error Event Pattern
Listen to error events on streams.

### Global Error Handler
Catch uncaught errors at application level.

---

## Troubleshooting Guide

### Problem: Operation Timeout
Cause: Operation exceeds timeout limit
Solution: Increase timeout or optimize operation

### Problem: Out of Memory
Cause: Too much data in memory
Solution: Use streaming mode or reduce batch size

### Problem: Permission Denied
Cause: Insufficient access rights
Solution: Check file/directory permissions

---

## Error Recovery

Recovery strategies for different error types:

- Transient errors: Retry with backoff
- Permanent errors: Fail fast and log
- Resource errors: Free resources and retry
- Configuration errors: Fix and restart

---

## Logging and Debugging

Enable detailed error logging to help debug issues:
- Error codes and messages
- Stack traces
- Context information
- Timestamp and duration

---

## See Also

- How-To guide: Troubleshooting common issues
- Reference: API error codes
- Explanation: Error handling design
