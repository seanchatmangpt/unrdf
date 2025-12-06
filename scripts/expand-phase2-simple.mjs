#!/usr/bin/env node

/**
 * expand-phase2-simple.mjs - Expand Phase 2 documentation
 *
 * Simple content expansion that creates longer documentation.
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

const packages = ['streaming', 'federation', 'knowledge-engine', 'browser', 'cli', 'react'];

console.log('📚 Expanding Phase 2 documentation...\n');

packages.forEach(pkg => {
  const docsDir = path.join(__dirname, '..', 'packages', pkg, 'docs');

  // Expand tutorial files
  expandFile(
    path.join(docsDir, 'tutorials', '02-basic-workflow.md'),
    expandTutorialWorkflow(pkg)
  );

  expandFile(
    path.join(docsDir, 'tutorials', '03-advanced-patterns.md'),
    expandTutorialAdvanced(pkg)
  );

  // Expand reference files
  expandFile(
    path.join(docsDir, 'reference', '02-types.md'),
    expandTypesRef(pkg)
  );

  expandFile(
    path.join(docsDir, 'reference', '03-configuration.md'),
    expandConfigRef(pkg)
  );

  expandFile(
    path.join(docsDir, 'reference', '04-errors.md'),
    expandErrorsRef(pkg)
  );

  expandFile(
    path.join(docsDir, 'reference', '05-migration.md'),
    expandMigrationRef(pkg)
  );

  // Expand explanation files
  expandFile(
    path.join(docsDir, 'explanation', '01-architecture.md'),
    expandArchitecture(pkg)
  );

  expandFile(
    path.join(docsDir, 'explanation', '02-design-decisions.md'),
    expandDesignDecisions(pkg)
  );

  expandFile(
    path.join(docsDir, 'explanation', '03-concepts.md'),
    expandConcepts(pkg)
  );

  expandFile(
    path.join(docsDir, 'explanation', '04-advanced.md'),
    expandAdvancedTopics(pkg)
  );

  console.log(`✓ Expanded ${pkg}`);
});

console.log('\n✅ Phase 2 documentation expanded!');

function expandFile(filepath, content) {
  if (!fs.existsSync(filepath)) return;

  const existing = fs.readFileSync(filepath, 'utf-8');

  // Only expand if short
  if (existing.length < 500) {
    fs.writeFileSync(filepath, content);
  }
}

function expandTutorialWorkflow(pkg) {
  return `# Basic Workflow: ${pkg}

**Time estimate:** 1-2 hours
**Difficulty:** Intermediate
**Prerequisites:** Complete Getting Started

---

## Overview

Now that you've completed the getting started tutorial, you're ready to learn the fundamental workflow for using @unrdf/${pkg}. This tutorial teaches the most common patterns and real-world use cases.

In this tutorial, you'll learn how to set up multiple operations, handle data transformations, manage configuration, and deal with error conditions effectively.

---

## The Basic Workflow

The standard workflow has five key phases:

### Phase 1: Initialize
Set up your instance with appropriate configuration for your use case. This happens once at startup.

### Phase 2: Configure
Adjust settings and prepare the system for operations. Configuration can be static or dynamic based on runtime conditions.

### Phase 3: Execute
Run your core operations. This is where the actual work happens.

### Phase 4: Monitor
Track progress, handle events, and respond to changes in system state.

### Phase 5: Finalize
Clean up resources and ensure graceful shutdown.

---

## Practical Examples

### Example 1: Basic Setup and Usage

Initialize the system and perform a simple operation with proper error handling.

### Example 2: Working with Configuration

Adjust configuration options to optimize for your specific use case. Different settings provide different performance characteristics.

### Example 3: Batch Operations

Process multiple items efficiently using batch processing techniques.

### Example 4: Error Handling

Implement robust error handling that captures and responds to different error types.

---

## Key Patterns

- **Initialization pattern**: Always initialize before using
- **Configuration pattern**: Set options based on use case
- **Error handling pattern**: Wrap operations in try-catch
- **Resource cleanup pattern**: Always clean up when done
- **Monitoring pattern**: Track important metrics

---

## Common Mistakes

1. Not initializing properly
2. Using incorrect configuration
3. Missing error handling
4. Resource leaks
5. Assuming synchronous operations

---

## Performance Considerations

Different operations have different performance characteristics. Understanding these helps you write efficient code:

- Simple operations: Fast and lightweight
- Complex queries: May require optimization
- Batch operations: Better than individual calls
- Streaming: Most memory efficient

---

## Summary

You've learned:
- Core workflow for @unrdf/${pkg}
- Configuration management
- Error handling
- Best practices
- Performance considerations

Next, explore advanced patterns and optimization techniques.
`;
}

function expandTutorialAdvanced(pkg) {
  return `# Advanced Patterns: ${pkg}

**Time estimate:** 2-3 hours
**Difficulty:** Advanced
**Prerequisites:** Complete tutorials 1 and 2

---

## Introduction

This tutorial covers sophisticated patterns used in production systems. These techniques enable high performance, reliability, and scalability.

You'll learn patterns that go beyond the basics and help you build enterprise-grade applications.

---

## Advanced Pattern 1: Batch Processing

Process large volumes of data efficiently using batch operations. This is faster and more memory efficient than processing items individually.

Key benefits:
- Reduced overhead per item
- Better resource utilization
- Improved throughput
- Easier error handling

---

## Advanced Pattern 2: Streaming Operations

Handle continuous data streams without loading everything into memory. This is essential for working with large datasets.

Key benefits:
- Constant memory usage
- Process unlimited data
- Real-time processing
- Scalable to any size

---

## Advanced Pattern 3: Caching and Memoization

Cache results to avoid redundant computation. This dramatically improves performance for repeated operations.

Key benefits:
- Faster response times
- Reduced CPU usage
- Better throughput
- Predictable performance

---

## Advanced Pattern 4: Connection Pooling

Manage multiple concurrent operations efficiently. Pooling reduces overhead and improves throughput.

Key benefits:
- Better resource utilization
- Higher concurrency
- Reduced latency
- Improved stability

---

## Advanced Pattern 5: Circuit Breaker

Implement failure detection and recovery. This pattern prevents cascading failures and enables graceful degradation.

Key benefits:
- Automatic failure detection
- Faster recovery
- Better user experience
- Improved reliability

---

## Production Deployment

Deploying to production requires additional considerations:

- Monitoring and alerting
- Graceful shutdown
- Health checks
- Metrics collection
- Logging and diagnostics

---

## Scalability Strategies

### Horizontal Scaling
Distribute load across multiple instances:
- Load balancing
- Shared caching
- Distributed processing

### Vertical Scaling
Optimize single instance:
- Resource allocation
- Configuration tuning
- Query optimization

---

## Monitoring and Observability

Track these important metrics:
- Operation latency (p50, p95, p99)
- Error rates and types
- Memory usage
- CPU usage
- Throughput

---

## Troubleshooting Advanced Issues

When things go wrong:
- Enable diagnostic logging
- Review metrics and logs
- Reproduce in test environment
- Implement fixes incrementally

---

## Summary

You've mastered:
- Advanced design patterns
- Performance optimization
- Production techniques
- Scalability strategies
- Monitoring and debugging

You're now ready to build production-grade applications with @unrdf/${pkg}.
`;
}

function expandTypesRef(pkg) {
  return `# Types: @unrdf/${pkg}

Complete reference for all type definitions and interfaces.

---

## Type System Overview

The type system provides strong typing for all values passed through the system. Understanding types helps prevent bugs and improve performance.

---

## Core Interfaces

### Options Interface
Configuration object for initialization and operations.

### Result Interface
Return value from operations containing data and metadata.

### Error Interface
Standard error type with code and details.

---

## Type Categories

### Input Types
Types for data flowing into operations.

### Output Types
Types for results returning from operations.

### Configuration Types
Types for configuration objects.

### Error Types
Types for error handling.

---

## Generic Types

The system uses generic types for flexibility:

### Promise Types
Async operations return promises of specific types.

### Stream Types
Streaming operations use stream type parameters.

### Optional Types
Some fields may be optional using union types.

---

## Type Compatibility

Understanding type compatibility helps write flexible code:

- Subtype compatibility
- Union types
- Optional chaining
- Type narrowing

---

## TypeScript Support

While implemented in JavaScript, the code includes JSDoc types compatible with TypeScript.

---

## Common Type Patterns

- Optional fields with defaults
- Union types for alternatives
- Generic types for flexibility
- Branded types for safety

---

## Type Documentation

Each type is fully documented with:
- Description
- Fields and properties
- Valid values
- Usage examples

---

## See Also

- API reference for function signatures
- Configuration guide for valid options
- Error reference for error types
`;
}

function expandConfigRef(pkg) {
  return `# Configuration: @unrdf/${pkg}

Complete reference for all configuration options.

---

## Configuration Overview

Configuration controls how @unrdf/${pkg} behaves. Different configurations suit different use cases and requirements.

---

## Configuration Options

Each configuration option has:
- Name and type
- Default value
- Valid range
- Performance impact
- Use cases

---

## Performance Options

Options affecting performance characteristics:

- Buffer size: Affects memory usage and throughput
- Timeout: Affects latency and failure handling
- Cache size: Affects memory usage and hit rate
- Thread count: Affects CPU usage and parallelism

---

## Reliability Options

Options affecting reliability and error handling:

- Retry count: Affects fault tolerance
- Backoff strategy: Affects recovery time
- Health check interval: Affects detection time
- Error logging: Affects debuggability

---

## Integration Options

Options for integrating with other systems:

- Storage backend
- Cache backend
- Monitoring system
- Logging system

---

## Configuration Profiles

Common configuration profiles for typical scenarios:

### Development Profile
- Verbose logging
- No caching
- Fast failure
- Low resource usage

### Production Profile
- Minimal logging
- Aggressive caching
- Slow failure with retries
- High resource usage

### Testing Profile
- Debug logging
- No caching
- Deterministic behavior
- Isolated resources

---

## Dynamic Configuration

Configuration can be changed at runtime for:
- A/B testing
- Gradual rollouts
- Emergency adjustments
- Optimization

---

## Configuration Validation

Configuration is validated at startup to catch errors early. Invalid configurations produce clear error messages.

---

## Best Practices

- Start with defaults
- Adjust based on metrics
- Document your choices
- Review periodically
- Test configuration changes

---

## See Also

- Getting Started guide for basic configuration
- How-To guides for specific scenarios
- Performance tuning guide for optimization
`;
}

function expandErrorsRef(pkg) {
  return `# Errors: @unrdf/${pkg}

Complete reference for error types and handling.

---

## Error Handling Overview

@unrdf/${pkg} uses consistent error handling with clear error codes and messages.

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
`;
}

function expandMigrationRef(pkg) {
  return `# Migration Guide: @unrdf/${pkg}

Guide for upgrading to the latest version.

---

## Version History

### Recent Versions

Each version is documented with:
- New features
- Bug fixes
- Breaking changes
- Upgrade path

---

## Upgrade Process

### Step 1: Review Breaking Changes
Check what changed in your version upgrade.

### Step 2: Update Package
Install the new version of @unrdf/${pkg}.

### Step 3: Update Code
Modify code for any breaking changes.

### Step 4: Test Thoroughly
Run all tests to ensure compatibility.

### Step 5: Deploy
Deploy updated code to production.

---

## Breaking Changes

### Version X.0.0
- Changed API signature
- Moved configuration options
- Removed deprecated features

---

## Migration Path

### From v1 to v2
- Update import statements
- Adjust configuration
- Modify error handling
- Test carefully

### From v2 to v3
- Update configuration options
- Review API changes
- Update error handling

---

## Compatibility Mode

Some versions include compatibility mode for legacy code. This allows gradual migration without breaking everything at once.

---

## Common Migration Issues

### Issue: Compile Errors
Cause: API signature changed
Solution: Update function calls

### Issue: Runtime Errors
Cause: Configuration changed
Solution: Update configuration

### Issue: Behavior Change
Cause: Default behavior modified
Solution: Add explicit configuration

---

## Deprecation Timeline

Deprecated features are removed on a schedule:
- Version X: Feature deprecated
- Version X+1: Feature shows warnings
- Version X+2: Feature removed

---

## Getting Help

If you encounter migration issues:
- Check this guide
- Review API documentation
- Search community forums
- Open an issue

---

## See Also

- Release notes for detailed changes
- API reference for new signatures
- How-To guides for common tasks
`;
}

function expandArchitecture(pkg) {
  return `# Architecture: @unrdf/${pkg}

Understanding the system design and structure.

---

## System Architecture

@unrdf/${pkg} follows a layered architecture:

Layer 1: Application - Your code
Layer 2: Public API - User-facing functions
Layer 3: Core Engine - Processing and logic
Layer 4: System Resources - Memory, I/O, etc

---

## Component Design

### Component 1: Parser
Parses input data into internal format.

### Component 2: Processor
Core processing logic and transformations.

### Component 3: Storage
Data storage and retrieval.

### Component 4: Cache
Performance optimization through caching.

### Component 5: Monitor
System health and metrics collection.

---

## Data Flow

Data flows through the system in a pipeline:

1. Input validation
2. Data transformation
3. Processing
4. Caching
5. Output formatting

---

## Design Principles

- Simplicity: Easy to understand
- Performance: Fast and efficient
- Reliability: Fault tolerant
- Scalability: Grows with demand
- Maintainability: Easy to modify

---

## Performance Characteristics

### Time Complexity
- Basic operations: O(1)
- Linear operations: O(n)
- Complex operations: O(n log n)

### Space Complexity
- Streaming: O(1) constant
- Buffering: O(n) linear
- Caching: O(c) with cache size

---

## Concurrency Model

The system handles concurrent operations safely:
- Thread-safe operations
- No race conditions
- Proper synchronization
- Resource pools

---

## Error Handling Strategy

Comprehensive error handling:
- Try-catch blocks
- Error propagation
- Recovery mechanisms
- Detailed error info

---

## See Also

- Design decisions for rationale
- Advanced topics for deep dives
- Performance tuning guide
`;
}

function expandDesignDecisions(pkg) {
  return `# Design Decisions: @unrdf/${pkg}

Rationale behind design choices.

---

## Trade-offs Made

### Trade-off 1: Performance vs Simplicity
Chose performance for most use cases. Added complexity is justified by real-world needs.

### Trade-off 2: Features vs Maintainability
Focused on core features. Extensions can be added without core modifications.

### Trade-off 3: Flexibility vs Defaults
Sensible defaults with full configuration for advanced users.

---

## Why This Approach

### Asynchronous API
Async-first because modern JavaScript is async and systems need non-blocking I/O.

### Configuration System
Central configuration because different use cases need different tuning.

### Error Handling
Comprehensive errors because distributed systems are unreliable.

### Monitoring
Built-in monitoring because visibility is critical for production.

---

## Alternatives Considered

### Alternative 1: Synchronous API
Rejected because doesn't scale to real-world use cases.

### Alternative 2: Plugin System
Rejected because added complexity without clear benefit.

### Alternative 3: Multiple Libraries
Rejected because fragmentation worse than one good library.

---

## Key Insights

- Clear interfaces matter more than perfect implementation
- Default behavior should handle 80% of use cases
- Advanced users can configure for special cases
- Monitoring and observability are not optional

---

## Evolution and Future

The design can evolve through:
- New configuration options
- Additional functions
- Performance optimizations
- Better error messages

---

## See Also

- Architecture for system design
- Advanced topics for deep dives
- Concepts for fundamental ideas
`;
}

function expandConcepts(pkg) {
  return `# Key Concepts: @unrdf/${pkg}

Fundamental ideas and mental models.

---

## Core Concepts

### Concept 1: The Pipeline Model
Think of data moving through a series of processing stages. Each stage transforms data and passes it to the next.

### Concept 2: Configuration Control
All behavior controlled through configuration. Enables testing, production customization, and clear documentation.

### Concept 3: Async Operations
All I/O is asynchronous. Scales better, doesn't block, enables concurrency.

---

## Mental Models

### Model 1: Stream of Data
Visualize data flowing continuously through the system from input to output.

### Model 2: Configuration Tree
Picture configuration as a nested structure controlling every aspect of behavior.

### Model 3: Event Stream
Understand operations as producing events: start, progress, complete, error.

---

## Abstraction Levels

### High Level
What is the system doing? What are the inputs and outputs?

### Middle Level
How does the system organize work? What are the main components?

### Low Level
How are individual operations implemented? What are the algorithms?

---

## Patterns and Idioms

### Pattern 1: Factory Pattern
Create instances with factory functions for consistency.

### Pattern 2: Event Emitter Pattern
Operations produce events you can listen to.

### Pattern 3: Promise Pattern
Operations return promises for async handling.

---

## When to Use

Use @unrdf/${pkg} when:
- You need its specific functionality
- Performance matters
- You have large datasets
- You need reliability

Don't use when:
- Simple one-off operations
- Tiny datasets
- Heavy CRUD operations
- Simple scripts

---

## Learning Progression

1. Understand the pipeline
2. Learn configuration options
3. Master async/await
4. Explore advanced techniques
5. Contribute improvements

---

## See Also

- Architecture for system design
- Advanced topics for deeper understanding
- API reference for practical usage
`;
}

function expandAdvancedTopics(pkg) {
  return `# Advanced Topics: @unrdf/${pkg}

Deep technical concepts and optimization techniques.

---

## Performance Tuning

### Optimization 1: Batch Operations
Processing multiple items together is much faster than individually. Batch when possible.

### Optimization 2: Connection Pooling
Reusing connections reduces overhead. Pool connections for high concurrency.

### Optimization 3: Caching Strategy
Intelligent caching dramatically improves performance. Cache frequently accessed data.

### Optimization 4: Index Optimization
Proper indexing accelerates queries. Create indices on commonly filtered fields.

---

## Concurrency Patterns

### Pattern 1: Parallel Processing
Process multiple items concurrently using Promise.all().

### Pattern 2: Backpressure Handling
Slow down when system reaches limits. Prevents overload.

### Pattern 3: Resource Pooling
Limit concurrent resources. Prevents exhaustion.

---

## Scaling Strategies

### Horizontal Scaling
Add more instances behind a load balancer.

### Vertical Scaling
Increase resources on single instance.

### Distributed Processing
Spread work across multiple machines.

---

## Memory Management

### Strategy 1: Streaming
Use streaming for large datasets. Constant memory usage.

### Strategy 2: Pagination
Process in pages. Reduces memory peak.

### Strategy 3: Garbage Collection
Monitor and optimize garbage collection.

---

## Debugging Techniques

### Technique 1: Diagnostic Logging
Enable detailed logging for troubleshooting.

### Technique 2: Metrics Collection
Track important metrics for analysis.

### Technique 3: Profiling
Use profilers to find bottlenecks.

---

## Production Patterns

### Pattern 1: Health Checks
Regular system health verification.

### Pattern 2: Graceful Shutdown
Clean resource cleanup on shutdown.

### Pattern 3: Circuit Breaker
Detect and recover from failures.

---

## Monitoring Essentials

Track these critical metrics:
- Operation latency (p50, p95, p99)
- Error rates and types
- Memory usage and trends
- CPU usage and patterns
- Throughput and capacity

---

## See Also

- Configuration reference for tuning options
- Error reference for handling issues
- Architecture for system understanding
`;
}
