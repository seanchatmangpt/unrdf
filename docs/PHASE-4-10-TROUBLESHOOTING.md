# Phase 4.10: Troubleshooting - Common Issues and Solutions

## Runtime Mismatch Issues

### Problem: "Works in browser, fails in production"

**Symptoms:**
- Code works fine with AtomVM in browser
- Same code fails on real OTP node
- Error is not reproducible locally

**Root Causes:**
1. **Timing differences** - Browser may be slower/faster
2. **Feature differences** - Using something AtomVM stubs
3. **Data size** - Production has more data than local testing

**Diagnosis:**

```erlang
% Enable tracing in production
erlang:trace_pattern({'_', '_', '_}, true, [call]),
erlang:trace(all, true, [call, return_to, {tracer, dbg, ip}]).

% Grep logs for failures
grep -i "error\|exception" logs/erlang.log
```

**Solutions:**

```typescript
// 1. Run golden tests against real OTP
npm run test:golden -- --target=production

// 2. Check for test-specific code
if (process.env.NODE_ENV === 'test') {
  // ✗ Test-specific code paths
}

// 3. Verify feature availability
if (!client.supportsFeature('ets')) {
  // Fallback to maps
}
```

---

## Protocol Mismatch Issues

### Problem: "Protocol error: unexpected message format"

**Symptoms:**
```
Protocol error: Cannot read property 'type' of undefined
ReferenceError: Message field missing
Error: Unknown message type 'unknown'
```

**Root Cause:**
Protocol layer producing different output on two runtimes.

**Diagnosis:**

```typescript
// Log raw messages from both
class DebugAdapter implements ProtocolAdapter {
  async send(command: Command): Promise<Response> {
    const json = JSON.stringify(command);
    console.log('→ Raw:', json);
    const response = await this.delegate.send(command);
    console.log('← Raw:', JSON.stringify(response));
    return response;
  }
}

// Compare outputs
$ npm run test:golden -- --target=production --debug
$ npm run test:golden -- --target=development --debug
$ diff <(grep "Raw:" prod.log | sort) <(grep "Raw:" dev.log | sort)
```

**Solutions:**

1. **Protocol version mismatch**
   ```erlang
   % Check version negotiation
   Version = maps:get(<<"version">>, Message),
   case Version of
       <<"1.0.0">> -> ok;
       Other ->
           error_logger:error_msg(
               "Version mismatch: expected 1.0.0, got ~p~n", [Other])
   end.
   ```

2. **Encoding difference**
   ```erlang
   % Ensure identical JSON encoding
   encode(Term) ->
       % Same encoder on both runtimes
       jsx:encode(Term, [strict_format]).
   ```

3. **Unknown fields**
   ```erlang
   % Strip unknown fields for comparison
   normalize_message(Msg) ->
       maps:with([type, id, payload, meta], Msg).
   ```

---

## Connection Issues

### Problem: "Cannot connect to AtomVM in browser"

**Symptoms:**
```
WebSocket is closed
Error: AtomVM not loaded
Error: No port found
```

**Root Cause:**
AtomVM WASM module not loaded before connecting.

**Diagnosis:**

```typescript
// Check if AtomVM is loaded
if (typeof window.atomvm_port === 'undefined') {
  console.error('AtomVM not loaded');
  console.log('Available:', Object.keys(window));
}
```

**Solutions:**

1. **Load WASM before app starts**
   ```html
   <!-- Correct order -->
   <script src="atomvm.js"></script>
   <script>
     // Wait for AtomVM to initialize
     window.addEventListener('atomvm:ready', () => {
       // Now start app
       import('./app.js');
     });
   </script>
   ```

2. **Check browser compatibility**
   ```typescript
   // WebAssembly required
   if (typeof WebAssembly === 'undefined') {
     console.error('WebAssembly not supported');
   }

   // WebSocket required
   if (typeof WebSocket === 'undefined') {
     console.error('WebSocket not supported');
   }
   ```

3. **Verify network**
   ```bash
   # Check if AtomVM app is listening
   curl -i http://localhost:9000/health
   ```

---

## Performance Issues

### Problem: "AtomVM is much slower than expected"

**Symptoms:**
- Single query takes 5+ seconds
- Garbage collection pauses visible
- Memory usage growing unbounded

**Root Causes:**
1. **Large result sets** - AtomVM slower for big data
2. **Inefficient queries** - No query optimization
3. **Memory leaks** - Circular references

**Diagnosis:**

```erlang
% Measure performance
measure_query(Query) ->
    Start = erlang:monotonic_time(millisecond),
    Result = execute_query(Query),
    Elapsed = erlang:monotonic_time(millisecond) - Start,
    io:format("Query took ~pms~n", [Elapsed]),
    Result.

% Profile memory
erl -s eprof start -s cerl start -s erl_eval eval
profiler:profile([]) % Enable profiling
```

**Solutions:**

1. **Use limits for large datasets**
   ```erlang
   % Paginate results
   execute_query(Query) ->
       execute_query(Query, #{limit => 1000, offset => 0}).

   % In AtomVM, keep result sets small
   -ifdef(ATOMVM).
       DEFAULT_LIMIT = 100;
   -else.
       DEFAULT_LIMIT = 10000.
   -endif.
   ```

2. **Optimize queries**
   ```erlang
   % ✗ Inefficient: Full scan
   execute_slow(Query) ->
       filter_all_triples(Query).

   % ✓ Efficient: Use indices
   execute_fast(Query) ->
       use_index(Query).
   ```

3. **Fix memory leaks**
   ```erlang
   % Ensure processes don't accumulate state
   handle_call(Request, State) ->
       Result = process_request(Request),
       % Return NEW state, not accumulated state
       {reply, Result, fresh_state()}.
   ```

---

## Error Handling Issues

### Problem: "Error codes don't match between runtimes"

**Symptoms:**
```
OTP returns: QUERY_TIMEOUT
AtomVM returns: TIMEOUT
Browser retries, production doesn't
```

**Root Cause:**
Different error code definitions on each runtime.

**Solutions:**

1. **Centralize error definitions**
   ```erlang
   % lib/unrdf_protocol/include/errors.hrl
   -define(ERROR_QUERY_TIMEOUT, <<"QUERY_TIMEOUT">>).
   -define(ERROR_INTERNAL, <<"INTERNAL_ERROR">>).

   % Both runtimes include the same file
   ```

2. **Verify error codes match**
   ```bash
   # Extract all error codes from both runtimes
   grep -r "error_code" lib/unrdf_protocol/src/*.erl | sort | uniq
   grep -r "QUERY_TIMEOUT\|INTERNAL" lib/unrdf_*/src/*.erl

   # Should show same codes
   ```

3. **Test error consistency**
   ```typescript
   // Test that errors are consistent
   it('returns same error code on both runtimes', async () => {
     const clients = [
       new ProtocolClient('production'),
       new ProtocolClient('development')
     ];

     for (const client of clients) {
       try {
         await client.executeQuery('INVALID');
       } catch (e) {
         expect(e.code).toBe('VALIDATION_ERROR');
       }
     }
   });
   ```

---

## Timeout Issues

### Problem: "Operations timeout unexpectedly"

**Symptoms:**
```
QUERY_TIMEOUT on every operation
Client timeout doesn't match server timeout
Timeout duration is wrong
```

**Root Causes:**
1. **Timeout values differ** between client and server
2. **Slow operations** on one runtime
3. **Network latency** not accounted for

**Solutions:**

1. **Use consistent timeout values**
   ```typescript
   // Both runtimes use same timeout config
   const QUERY_TIMEOUT_MS = 5000;

   const response = await client.send({
     type: 'command',
     payload: { action: 'query' },
     meta: { timeout_ms: QUERY_TIMEOUT_MS }
   });
   ```

2. **Measure actual latency**
   ```typescript
   // Log latencies
   const start = Date.now();
   await operation();
   const latency = Date.now() - start;

   console.log(`Operation took ${latency}ms (limit: ${timeout}ms)`);

   if (latency > timeout * 0.8) {
     console.warn('Approaching timeout threshold');
   }
   ```

3. **Add headroom for slow operations**
   ```typescript
   // Request has internal timeout of 5s
   // Client timeout should be larger
   const OPERATION_TIMEOUT = 5000;
   const NETWORK_LATENCY = 500;
   const CLIENT_TIMEOUT = OPERATION_TIMEOUT + NETWORK_LATENCY + 1000; // 6.5s
   ```

---

## WebSocket Issues

### Problem: "WebSocket connection drops intermittently"

**Symptoms:**
```
Connection lost
Reconnecting...
Message queue overflow
```

**Root Causes:**
1. **Network instability** - Connection flakes
2. **Server crash** - OTP or AtomVM process dies
3. **Heartbeat failure** - Keep-alive not working

**Solutions:**

1. **Implement heartbeat**
   ```typescript
   // Send periodic ping
   setInterval(async () => {
     try {
       await client.send({
         type: 'command',
         payload: { action: 'ping' }
       });
     } catch (e) {
       console.warn('Heartbeat failed:', e);
     }
   }, 30000);  // Every 30 seconds
   ```

2. **Implement automatic reconnect**
   ```typescript
   // Built into RealAdapter, but verify it works
   ws.onclose = () => {
     console.log('WebSocket closed, attempting reconnect...');
     setTimeout(() => attemptReconnect(), 1000);
   };
   ```

3. **Check message queue**
   ```typescript
   // Monitor backlog
   if (pendingRequests.size > 100) {
     console.warn('Message queue backlog:', pendingRequests.size);
   }
   ```

---

## Data Consistency Issues

### Problem: "Data is different between client and server"

**Symptoms:**
```
Query returns different results
Client cache doesn't match server
Add operation succeeds locally but fails on server
```

**Root Causes:**
1. **Stale cache** - Client cache not invalidated
2. **Concurrency** - Race condition in updates
3. **Sync failure** - Data sync didn't complete

**Solutions:**

1. **Invalidate cache on updates**
   ```typescript
   async addTriple(s, p, o) {
     await this.client.send({
       type: 'command',
       payload: { action: 'addTriple', params: { s, p, o } }
     });

     // Clear cache
     this.cache.clear();
   }
   ```

2. **Verify with server**
   ```typescript
   // After update, query server to confirm
   const result = await client.executeQuery(
     `SELECT * WHERE { <${subject}> <${predicate}> ?o }`
   );

   if (!result.length) {
     throw new Error('Update verification failed');
   }
   ```

3. **Monitor sync events**
   ```typescript
   client.subscribe('dataChanged', (event) => {
     console.log('Server data changed:', event.payload.changes);
   });
   ```

---

## Browser Compatibility Issues

### Problem: "Works in Chrome, fails in Firefox"

**Symptoms:**
```
WebAssembly not supported
WebSocket not available
Typed arrays not working
```

**Solutions:**

1. **Check browser support**
   ```typescript
   const canRun = () => {
     return (
       typeof WebAssembly !== 'undefined' &&
       typeof WebSocket !== 'undefined'
     );
   };

   if (!canRun()) {
     console.error('Browser not supported');
     // Show fallback UI
   }
   ```

2. **Use polyfills**
   ```html
   <script src="https://cdn.jsdelivr.net/npm/wasm-polyfill"></script>
   <script src="https://cdn.jsdelivr.net/npm/websocket-polyfill"></script>
   ```

---

## Getting Help

### Debug Checklist

1. [ ] Enable debug logging on both runtimes
2. [ ] Run golden tests against both
3. [ ] Compare protocol messages (should match)
4. [ ] Check error codes (should match)
5. [ ] Measure performance (document expected)
6. [ ] Review recent changes (what changed?)
7. [ ] Check CI/CD logs (did tests pass?)

### Enabling Verbose Logging

```bash
# Erlang side
erl -s unrdf_app start -kernel logger_level debug

# JavaScript side
export DEBUG=unrdf:*
npm run dev
```

### Get the logs

```bash
# Erlang logs
cat logs/erlang.log

# Browser console
open DevTools → Console → save as HTML

# JavaScript logs
tail -f logs/app.log
```

---

## See Also

- **01-PROTOCOL-DESIGN.md** - Protocol details
- **03-BROWSER-SIMULATION.md** - AtomVM limitations
- **04-DUAL-ADAPTERS.md** - Adapter implementation
