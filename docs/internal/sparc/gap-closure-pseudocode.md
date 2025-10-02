# Gap Closure Pseudocode

## Algorithm Design for Gap Closure Implementation

This document provides the algorithmic foundation for closing the remaining gaps in the KGC JavaScript Sidecar implementation.

---

## 1. Sidecar Communication with Circuit Breaker

### ALGORITHM: SidecarCommunicationV2
**Purpose:** Handle KGC sidecar unavailability with circuit breaker pattern

```
ALGORITHM: SidecarCommunicationV2
INPUT: hook_operation, timeout_ms, fallback_mode
OUTPUT: operation_result OR local_fallback_result

// State management
STATE circuit_breaker = {
  state: 'closed',  // closed, open, half-open
  failure_count: 0,
  failure_threshold: 3,
  success_threshold: 2,  // for half-open → closed
  reset_timeout: 30000,   // 30s
  last_failure_time: null,
  consecutive_successes: 0
}

// Main execution flow
1. CHECK circuit_breaker.state

   IF state == 'open':
     2a. CHECK if reset_timeout elapsed
         IF current_time - last_failure_time > reset_timeout:
           SET state = 'half-open'
           SET consecutive_successes = 0
         ELSE:
           GOTO 5 (execute fallback)

   IF state == 'half-open':
     2b. ATTEMPT single request to test recovery
         // Allow limited traffic through

   IF state == 'closed':
     2c. PROCEED with normal operation

3. EXECUTE health_check(timeout=100ms)

   IF health_check_succeeds:
     4a. EXECUTE hook_operation via gRPC/HTTP
     4b. IF operation_succeeds:
           CALL on_success()
           RETURN operation_result
     4c. ELSE IF timeout OR error:
           CALL on_failure(error)
           GOTO 5 (execute fallback)
   ELSE:
     CALL on_failure(health_check_error)
     GOTO 5 (execute fallback)

5. EXECUTE local_fallback_mode
   5a. LOG fallback activation with reason
   5b. EXECUTE local_operation
   5c. RECORD metrics for fallback execution
   5d. RETURN local_operation_result

// Success handler
FUNCTION on_success():
  IF state == 'half-open':
    INCREMENT consecutive_successes
    IF consecutive_successes >= success_threshold:
      SET state = 'closed'
      SET failure_count = 0
      LOG "Circuit breaker closed - service recovered"

  IF state == 'closed':
    SET failure_count = 0  // Reset on any success

// Failure handler
FUNCTION on_failure(error):
  INCREMENT failure_count
  SET last_failure_time = current_time

  IF state == 'closed':
    IF failure_count >= failure_threshold:
      SET state = 'open'
      LOG "Circuit breaker opened - service unavailable"

  IF state == 'half-open':
    SET state = 'open'
    SET consecutive_successes = 0
    LOG "Circuit breaker reopened - recovery failed"
```

**Time Complexity:** O(1) - constant time operations
**Space Complexity:** O(1) - fixed state size
**Error Handling:** All errors trapped and trigger fallback

---

## 2. Hook Evaluation Optimization

### ALGORITHM: HookEvaluationOptimization
**Purpose:** Optimize hook evaluation to meet p99 ≤ 2ms target

```
ALGORITHM: HookEvaluationOptimization
INPUT: hook_definition, rdf_data, options
OUTPUT: evaluation_result, execution_metrics

// Caching layer
GLOBAL hook_cache = LRUCache(max_size=10000)
GLOBAL query_cache = LRUCache(max_size=5000)
GLOBAL validation_cache = ContentHashMap()

1. GENERATE cache_key = hash(hook_definition.when.ref.sha256, rdf_data.hash)

   IF hook_cache.has(cache_key) AND cache_valid(cache_key):
     RETURN hook_cache.get(cache_key)

2. START performance_timer
   SET execution_budget = 2000  // 2ms in microseconds

3. PARSE hook_definition ONCE

   IF hook_definition.when.kind == 'sparql-ask':
     4a. COMPILE query with optimization
         query_cache_key = hash(hook_definition.when.ref.sha256)

         IF query_cache.has(query_cache_key):
           compiled_query = query_cache.get(query_cache_key)
         ELSE:
           compiled_query = compile_sparql(query)
           query_cache.set(query_cache_key, compiled_query)

     4b. USE streaming query execution
         result = execute_streaming(compiled_query, rdf_data, {
           limit: 1,  // Short-circuit for ASK queries
           timeout: execution_budget
         })

     4c. SHORT-CIRCUIT on first result
         IF result.length > 0:
           evaluation_result = { matched: true, result: result[0] }
           GOTO 8 (cache and return)

   IF hook_definition.when.kind == 'shacl':
     5a. CALCULATE content_hash = hash(rdf_data)
         shape_hash = hash(hook_definition.when.ref.sha256)
         validation_key = (content_hash, shape_hash)

     5b. IF validation_cache.has(validation_key):
           cached_result = validation_cache.get(validation_key)
           IF cached_result.timestamp > (current_time - 60000):  // 1min TTL
             evaluation_result = cached_result.value
             GOTO 8 (cache and return)

     5c. USE incremental validation
         // Only validate changed triples since last validation
         changed_triples = diff(rdf_data, previous_rdf_data)

         IF changed_triples.length < rdf_data.length * 0.1:  // < 10% changed
           validation_result = incremental_validate(
             shape,
             changed_triples,
             previous_validation_result
           )
         ELSE:
           validation_result = full_validate(shape, rdf_data)

     5d. CACHE validation results
         validation_cache.set(validation_key, {
           value: validation_result,
           timestamp: current_time
         })

         evaluation_result = validation_result
         GOTO 8 (cache and return)

   IF hook_definition.when.kind == 'delta':
     6a. CALCULATE row_digest = hash(rdf_data)
         previous_digest = get_previous_digest(hook_definition.name)

     6b. COMPARE digests
         changed = (row_digest != previous_digest)

         evaluation_result = {
           matched: changed,
           previous: previous_digest,
           current: row_digest
         }

         STORE_digest(hook_definition.name, row_digest)
         GOTO 8 (cache and return)

   IF hook_definition.when.kind IN ['threshold', 'count', 'window']:
     7a. EXECUTE optimized aggregation query
         result = execute_aggregation(
           hook_definition.when,
           rdf_data,
           { timeout: execution_budget }
         )

     7b. COMPARE result with threshold
         evaluation_result = {
           matched: compare(result, hook_definition.when.threshold),
           value: result
         }
         GOTO 8 (cache and return)

8. STOP performance_timer
   execution_time = performance_timer.elapsed()

   IF execution_time <= execution_budget:
     // Cache successful result
     hook_cache.set(cache_key, evaluation_result, {
       ttl: calculate_ttl(hook_definition),
       priority: calculate_priority(execution_time)
     })

   RECORD metrics {
     execution_time,
     cache_hit: false,
     hook_type: hook_definition.when.kind,
     result: evaluation_result.matched
   }

   RETURN evaluation_result, execution_metrics
```

**Optimization Strategies:**
1. **Query Caching:** Compiled queries cached by content hash
2. **Short-Circuit Evaluation:** ASK queries exit on first result
3. **Incremental Validation:** SHACL only validates changed triples
4. **Streaming Execution:** Results processed as they arrive
5. **Parallel Validation:** Independent shapes validated concurrently
6. **Content-Addressed Caching:** Results cached by RDF content hash

**Performance Targets:**
- p50: ≤ 200µs (0.2ms)
- p99: ≤ 2ms
- Cache hit rate: ≥ 80% for repeated queries

---

## 3. Test Fixing Algorithm

### ALGORITHM: TestFixingStrategy
**Purpose:** Systematically fix failing tests to achieve ≥95% pass rate

```
ALGORITHM: TestFixingStrategy
INPUT: test_suite, failure_analysis
OUTPUT: fixed_tests, pass_rate

1. CATEGORIZE test failures by root cause
   categories = {
     'parse_errors': [],
     'mock_failures': [],
     'assertion_errors': [],
     'timeout_errors': [],
     'configuration_errors': []
   }

   FOR EACH failed_test IN test_suite.failures:
     category = classify_failure(failed_test)
     categories[category].append(failed_test)

2. PRIORITIZE by impact and complexity
   priority_queue = PriorityQueue()

   // P0: Parse errors (blocks test execution)
   FOR EACH test IN categories['parse_errors']:
     priority_queue.add(test, priority=0, complexity=1)

   // P1: Mock failures (affects multiple tests)
   FOR EACH test IN categories['mock_failures']:
     affected_count = count_dependent_tests(test)
     priority_queue.add(test, priority=1, complexity=affected_count)

   // P2: Assertion errors (test logic issues)
   FOR EACH test IN categories['assertion_errors']:
     priority_queue.add(test, priority=2, complexity=2)

   // P3: Configuration errors (setup issues)
   FOR EACH test IN categories['configuration_errors']:
     priority_queue.add(test, priority=3, complexity=3)

3. FIX tests in priority order
   fixed_count = 0
   target_pass_rate = 0.95  // 95%

   WHILE NOT priority_queue.empty() AND pass_rate < target_pass_rate:
     test = priority_queue.pop()

     IF test.category == 'parse_errors':
       fix_parse_error(test)

     IF test.category == 'mock_failures':
       implement_proper_mock(test)

     IF test.category == 'assertion_errors':
       fix_test_logic(test)

     IF test.category == 'configuration_errors':
       fix_test_configuration(test)

     INCREMENT fixed_count
     pass_rate = calculate_pass_rate(test_suite)

     LOG "Fixed test: {test.name}, Pass rate: {pass_rate}"

4. VALIDATE fixes
   FOR EACH fixed_test IN fixed_tests:
     RUN test 10 times

     IF any_run_fails:
       MARK as flaky
       ANALYZE flakiness_pattern
       APPLY stability_fix

5. RETURN fixed_tests, final_pass_rate
```

**Fixing Strategies by Category:**

#### Parse Error Fixes
```
FUNCTION fix_parse_error(test):
  1. READ test file
  2. RUN ESLint with --fix
  3. VALIDATE syntax with Vitest
  4. IF still failing:
       MANUAL review and correction
```

#### Mock Failure Fixes
```
FUNCTION implement_proper_mock(test):
  1. IDENTIFY external dependency
  2. CREATE comprehensive mock implementation
  3. DEFINE mock responses for all test scenarios
  4. VALIDATE mock behavior matches real service
```

#### Assertion Error Fixes
```
FUNCTION fix_test_logic(test):
  1. ANALYZE expected vs actual values
  2. DETERMINE if test expectation is correct
  3. IF expectation wrong:
       UPDATE test assertion
     ELSE:
       FIX implementation to meet expectation
  4. VERIFY fix with multiple test runs
```

---

## 4. OTEL Trace Export

### ALGORITHM: OTELTraceExport
**Purpose:** Export OpenTelemetry traces to Jaeger

```
ALGORITHM: OTELTraceExport
INPUT: trace_data, jaeger_endpoint, configuration
OUTPUT: export_result, metrics

1. INITIALIZE OTEL exporter
   exporter = new JaegerExporter({
     endpoint: jaeger_endpoint,
     serviceName: 'unrdf-kgc',
     tags: {
       'service.version': package.version,
       'deployment.environment': process.env.NODE_ENV
     }
   })

2. CONFIGURE trace provider
   provider = new TracerProvider({
     resource: new Resource({
       'service.name': 'unrdf-kgc',
       'service.namespace': 'knowledge-engine'
     }),
     sampler: new AlwaysOnSampler(),  // or ProbabilitySampler(0.1)
     plugins: {
       http: { enabled: true },
       https: { enabled: true }
     }
   })

   provider.addSpanProcessor(
     new BatchSpanProcessor(exporter, {
       maxQueueSize: 2048,
       maxExportBatchSize: 512,
       scheduledDelayMillis: 5000,
       exportTimeoutMillis: 30000
     })
   )

3. CREATE trace spans for transaction lifecycle

   FUNCTION create_transaction_span(transaction_id, delta, options):
     span = tracer.startSpan('transaction.apply', {
       attributes: {
         'transaction.id': transaction_id,
         'transaction.delta.additions': delta.additions.length,
         'transaction.delta.deletions': delta.deletions.length,
         'transaction.actor': options.actor,
         'transaction.correlationId': options.correlationId
       }
     })

     RETURN span

4. CREATE child spans for hook execution

   FUNCTION create_hook_span(parent_span, hook_name, hook_type):
     hook_span = tracer.startSpan('hook.execute', {
       parent: parent_span,
       attributes: {
         'hook.name': hook_name,
         'hook.type': hook_type,
         'hook.phase': 'pre' OR 'post'
       }
     })

     RETURN hook_span

5. MARK error spans appropriately

   FUNCTION mark_error_span(span, error):
     span.setStatus({
       code: SpanStatusCode.ERROR,
       message: error.message
     })

     span.recordException(error, {
       'error.type': error.constructor.name,
       'error.stack': error.stack
     })

6. EXPORT traces with retry logic

   FUNCTION export_traces(spans):
     MAX_RETRIES = 3
     retry_count = 0

     WHILE retry_count < MAX_RETRIES:
       TRY:
         result = await exporter.export(spans)

         IF result.code == ExportResultCode.SUCCESS:
           RECORD metric {
             'otel.export.success': true,
             'otel.export.span_count': spans.length
           }
           RETURN result

       CATCH error:
         retry_count++

         IF retry_count < MAX_RETRIES:
           WAIT exponential_backoff(retry_count)
         ELSE:
           RECORD metric {
             'otel.export.failure': true,
             'otel.export.error': error.message
           }
           THROW error

7. CLEANUP and flush on shutdown

   FUNCTION shutdown_otel():
     await provider.shutdown()
     await exporter.shutdown()
     LOG "OTEL exporter shutdown complete"
```

**Trace Correlation:**
```
// Maintain correlation IDs throughout transaction lifecycle
correlation_id = generate_uuid()

transaction_span.setAttribute('correlationId', correlation_id)
hook_span_1.setAttribute('correlationId', correlation_id)
hook_span_2.setAttribute('correlationId', correlation_id)
effect_span.setAttribute('correlationId', correlation_id)
```

---

## 5. Performance Optimization Pipeline

### ALGORITHM: PerformanceOptimizationPipeline
**Purpose:** Systematic performance optimization to meet KGC PRD targets

```
ALGORITHM: PerformanceOptimizationPipeline
INPUT: current_metrics, target_metrics
OUTPUT: optimized_system, performance_report

1. PROFILE current performance
   profile = {
     latency: { p50, p95, p99 },
     throughput: { hooks_per_min },
     memory: { heap_used, external },
     cpu: { user_time, system_time }
   }

2. IDENTIFY bottlenecks
   bottlenecks = []

   IF profile.latency.p99 > target_metrics.latency.p99:
     bottlenecks.append('latency_p99')

   IF profile.throughput.hooks_per_min < target_metrics.throughput:
     bottlenecks.append('throughput')

   IF profile.memory.heap_used > target_metrics.memory:
     bottlenecks.append('memory')

3. APPLY optimizations in priority order

   FOR EACH bottleneck IN bottlenecks:
     IF bottleneck == 'latency_p99':
       APPLY fast_path_optimization()
       APPLY query_caching()
       APPLY streaming_execution()

     IF bottleneck == 'throughput':
       APPLY parallel_execution()
       APPLY batch_processing()
       APPLY connection_pooling()

     IF bottleneck == 'memory':
       APPLY memory_pooling()
       APPLY incremental_processing()
       APPLY garbage_collection_tuning()

4. VALIDATE optimizations
   new_profile = benchmark(optimized_system)

   improvements = {
     latency_reduction: calculate_reduction(profile.latency, new_profile.latency),
     throughput_increase: calculate_increase(profile.throughput, new_profile.throughput),
     memory_reduction: calculate_reduction(profile.memory, new_profile.memory)
   }

   IF all_targets_met(new_profile, target_metrics):
     RETURN optimized_system, improvements
   ELSE:
     GOTO step 2 (iterate optimization)
```

---

## Complexity Analysis

### Circuit Breaker Pattern
- **Time Complexity:** O(1) - constant time state checks
- **Space Complexity:** O(1) - fixed state size
- **Worst Case:** Immediate fallback when circuit open

### Hook Evaluation Optimization
- **Time Complexity:** O(log n) with caching, O(n) without
- **Space Complexity:** O(m) where m = cache size
- **Best Case:** O(1) cache hit
- **Worst Case:** O(n) full scan for complex queries

### Test Fixing Strategy
- **Time Complexity:** O(n log n) for priority queue operations
- **Space Complexity:** O(n) for test categorization
- **Expected:** 51 tests fixed in prioritized order

### OTEL Trace Export
- **Time Complexity:** O(k) where k = number of spans
- **Space Complexity:** O(k) for span buffering
- **Batch Processing:** 512 spans per batch

---

## Validation Strategy

All algorithms will be validated through:
1. **Unit Tests:** Individual algorithm correctness
2. **Benchmark Tests:** Performance target validation
3. **Integration Tests:** End-to-end workflow validation
4. **Property Tests:** Invariant verification (idempotence, determinism)
5. **Stress Tests:** Sustained load performance validation

**Success Criteria:**
- All algorithms meet time/space complexity targets
- Performance SLOs achieved under load
- Zero regressions from baseline
- Code coverage ≥ 95% for all optimizations
