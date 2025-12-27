/**
 * OTEL Instrumentation Examples
 * 
 * Shows how to add missing OTEL spans to performance-critical operations
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf');

/**
 * Example 1: Instrument freezeUniverse
 * 
 * Location: packages/kgc-4d/src/freeze.mjs:35
 * Priority: P0 (High latency operation)
 */
export async function freezeUniverseInstrumented(_store, _gitBackbone) {
  return tracer.startActiveSpan('kgc.freeze', async (span) => {
    const startTime = Date.now();
    
    try {
      // Existing freeze logic...
      const universeQuads = [];
      
      span.setAttributes({
        'kgc.quad_count': universeQuads.length,
        'kgc.hash_algorithm': 'blake3',
      });
      
      // Checkpoint 1: Serialization
      const serializeStart = Date.now();
      const _nquads = 'serialized data';
      const serializeDuration = Date.now() - serializeStart;
      
      span.setAttributes({
        'kgc.serialize_duration_ms': serializeDuration,
      });
      
      // Checkpoint 2: Hash computation
      const hashStart = Date.now();
      const universeHash = 'computed-hash';
      const hashDuration = Date.now() - hashStart;
      
      span.setAttributes({
        'kgc.hash_duration_ms': hashDuration,
      });
      
      // Checkpoint 3: Git commit
      const gitStart = Date.now();
      const gitRef = 'git-ref-123';
      const gitDuration = Date.now() - gitStart;
      
      span.setAttributes({
        'kgc.git_duration_ms': gitDuration,
        'kgc.git_ref': gitRef,
      });
      
      span.setAttributes({
        'kgc.total_duration_ms': Date.now() - startTime,
      });
      
      span.setStatus({ code: SpanStatusCode.OK });
      
      return {
        id: 'receipt-id',
        universe_hash: universeHash,
        git_ref: gitRef,
      };
    } catch (error) {
      span.recordException(error);
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message,
      });
      throw error;
    } finally {
      span.end();
    }
  });
}

/**
 * Example 2: Instrument reconstructState
 * 
 * Location: packages/kgc-4d/src/freeze.mjs:214
 * Priority: P0 (High latency operation)
 */
export async function reconstructStateInstrumented(store, gitBackbone, targetTime) {
  return tracer.startActiveSpan('kgc.reconstruct', async (span) => {
    const startTime = Date.now();
    
    try {
      span.setAttributes({
        'kgc.target_time': targetTime.toString(),
      });
      
      // Find best snapshot
      const findStart = Date.now();
      const bestSnapshot = { t_ns: 0n, git_ref: 'ref' };
      const findDuration = Date.now() - findStart;
      
      span.setAttributes({
        'kgc.find_snapshot_duration_ms': findDuration,
        'kgc.snapshot_time': bestSnapshot.t_ns.toString(),
      });
      
      // Load snapshot from Git
      const loadStart = Date.now();
      const snapshotNQuads = 'snapshot-data';
      const loadDuration = Date.now() - loadStart;
      
      span.setAttributes({
        'kgc.load_snapshot_duration_ms': loadDuration,
        'kgc.snapshot_size_bytes': snapshotNQuads.length,
      });
      
      // Replay events
      const replayStart = Date.now();
      const eventsToReplay = [];
      const replayDuration = Date.now() - replayStart;
      
      span.setAttributes({
        'kgc.events_replayed': eventsToReplay.length,
        'kgc.replay_duration_ms': replayDuration,
        'kgc.total_duration_ms': Date.now() - startTime,
      });
      
      span.setStatus({ code: SpanStatusCode.OK });
      
      return {};
    } catch (error) {
      span.recordException(error);
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message,
      });
      throw error;
    } finally {
      span.end();
    }
  });
}

/**
 * Example 3: Instrument SPARQL queries
 * 
 * Location: Oxigraph wrapper
 * Priority: P0 (High frequency operation)
 */
export function wrapStoreWithTracing(store) {
  const originalQuery = store.query.bind(store);
  
  store.query = function(queryString) {
    return tracer.startActiveSpan('query.sparql', (span) => {
      const startTime = Date.now();
      
      try {
        const queryType = queryString.trim().split(' ')[0].toUpperCase();
        
        span.setAttributes({
          'query.type': queryType,
          'query.length': queryString.length,
        });
        
        const result = originalQuery(queryString);
        
        const resultCount = Array.isArray(result) ? result.length : 
                           typeof result === 'boolean' ? 1 : 0;
        
        span.setAttributes({
          'query.result_count': resultCount,
          'query.duration_ms': Date.now() - startTime,
        });
        
        span.setStatus({ code: SpanStatusCode.OK });
        
        return result;
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message,
        });
        throw error;
      } finally {
        span.end();
      }
    });
  };
  
  return store;
}
