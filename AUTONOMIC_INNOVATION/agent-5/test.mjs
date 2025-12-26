/**
 * @file Tests for Agent 5 - Commutativity analysis
 */

import { strict as assert } from 'node:assert';
import { canReorder, conflictCertificate } from './commutativity.mjs';
import { analyzeWriteSet, analyzeReadSet, dependencyGraph } from './analysis.mjs';

/**
 * Test: Commutative capsules (disjoint subjects)
 */
function testCommutativeCapsules() {
  const capsuleA = {
    id: 'capsule-a',
    delta: {
      add: [
        { subject: 'http://example.org/subject-x', predicate: 'http://example.org/prop', object: 'value1' }
      ]
    }
  };

  const capsuleB = {
    id: 'capsule-b',
    delta: {
      add: [
        { subject: 'http://example.org/subject-y', predicate: 'http://example.org/prop', object: 'value2' }
      ]
    }
  };

  const result = canReorder(capsuleA, capsuleB);
  assert.strictEqual(result.ok, true, 'Capsules with disjoint subjects should commute');
  assert.strictEqual(result.witness, undefined, 'No witness should be present for commutative capsules');

  console.log('✅ Test passed: Commutative capsules');
}

/**
 * Test: Conflicting write-write (same subject, predicate, different objects)
 */
function testConflictingWriteWrite() {
  const capsuleA = {
    id: 'capsule-a',
    delta: {
      add: [
        { subject: 'http://example.org/subject-z', predicate: 'http://example.org/prop', object: 'value1' }
      ]
    }
  };

  const capsuleB = {
    id: 'capsule-b',
    delta: {
      add: [
        { subject: 'http://example.org/subject-z', predicate: 'http://example.org/prop', object: 'value2' }
      ]
    }
  };

  const result = canReorder(capsuleA, capsuleB);
  assert.strictEqual(result.ok, false, 'Capsules with write-write conflict should not commute');
  assert.ok(result.witness, 'Witness should be present');
  assert.strictEqual(result.witness.conflictType, 'write-write', 'Conflict type should be write-write');
  assert.strictEqual(result.witness.quadA.subject, 'http://example.org/subject-z');
  assert.strictEqual(result.witness.quadB.subject, 'http://example.org/subject-z');

  console.log('✅ Test passed: Conflicting write-write');
}

/**
 * Test: Conflict certificate determinism (same conflict → same hash)
 */
function testCertificateDeterminism() {
  const capsuleA = {
    id: 'capsule-a',
    delta: {
      add: [
        { subject: 'http://example.org/subject-z', predicate: 'http://example.org/prop', object: 'value1' }
      ]
    }
  };

  const capsuleB = {
    id: 'capsule-b',
    delta: {
      add: [
        { subject: 'http://example.org/subject-z', predicate: 'http://example.org/prop', object: 'value2' }
      ]
    }
  };

  const certificates = [];
  const signatures = new Set();

  for (let i = 0; i < 100; i++) {
    const cert = conflictCertificate(capsuleA, capsuleB);
    certificates.push(cert);

    // Extract signature from certificate
    const sigMatch = cert.match(/signature: ([a-f0-9]+)/);
    assert.ok(sigMatch, 'Certificate should contain signature');
    signatures.add(sigMatch[1]);
  }

  assert.strictEqual(signatures.size, 1, 'All 100 certificates should have identical signatures');
  assert.ok(certificates[0].includes('CONFLICT_CERT_V1'), 'Certificate should have version header');
  assert.ok(certificates[0].includes('capsuleA_id: capsule-a'), 'Certificate should include capsule A ID');
  assert.ok(certificates[0].includes('capsuleB_id: capsule-b'), 'Certificate should include capsule B ID');

  console.log('✅ Test passed: Certificate determinism (100x)');
}

/**
 * Test: Dependency graph (3 capsules with known dependencies)
 */
function testDependencyGraph() {
  const capsule1 = {
    id: 'capsule-1',
    delta: {
      add: [
        { subject: 'http://example.org/s1', predicate: 'http://example.org/p1', object: 'v1' }
      ]
    }
  };

  const capsule2 = {
    id: 'capsule-2',
    delta: {
      add: [
        { subject: 'http://example.org/s1', predicate: 'http://example.org/p2', object: 'v2' }
      ]
    }
  };

  const capsule3 = {
    id: 'capsule-3',
    delta: {
      add: [
        { subject: 'http://example.org/s2', predicate: 'http://example.org/p3', object: 'v3' }
      ]
    }
  };

  const graph = dependencyGraph([capsule1, capsule2, capsule3]);

  assert.strictEqual(graph.nodes.length, 3, 'Graph should have 3 nodes');
  assert.ok(Array.isArray(graph.edges), 'Graph should have edges array');

  // Capsule 1 and 2 both modify subject s1, so there should be dependencies
  const hasDependency = graph.edges.some(e =>
    (e.from === 'capsule-1' && e.to === 'capsule-2') ||
    (e.from === 'capsule-2' && e.to === 'capsule-1')
  );
  assert.ok(hasDependency, 'Capsules 1 and 2 should have dependency due to shared subject');

  console.log('✅ Test passed: Dependency graph');
}

/**
 * Test: Large capsule array performance (<200ms)
 */
function testLargeCapsulePerformance() {
  const capsules = [];

  for (let i = 0; i < 100; i++) {
    capsules.push({
      id: `capsule-${i}`,
      delta: {
        add: [
          {
            subject: `http://example.org/subject-${i % 20}`, // 20 unique subjects, some overlap
            predicate: `http://example.org/prop-${i % 10}`,
            object: `value-${i}`
          }
        ]
      }
    });
  }

  const startTime = performance.now();
  const graph = dependencyGraph(capsules);
  const endTime = performance.now();
  const duration = endTime - startTime;

  assert.strictEqual(graph.nodes.length, 100, 'Graph should have 100 nodes');
  assert.ok(duration < 200, `Dependency graph should compute in <200ms, took ${duration.toFixed(2)}ms`);

  console.log(`✅ Test passed: Large capsule performance (${duration.toFixed(2)}ms for 100 capsules)`);
}

/**
 * Test: Contradiction (add + delete same quad)
 */
function testContradiction() {
  const capsuleA = {
    id: 'capsule-a',
    delta: {
      add: [
        { subject: 'http://example.org/s', predicate: 'http://example.org/p', object: 'value' }
      ]
    }
  };

  const capsuleB = {
    id: 'capsule-b',
    delta: {
      del: [
        { subject: 'http://example.org/s', predicate: 'http://example.org/p', object: 'value' }
      ]
    }
  };

  const result = canReorder(capsuleA, capsuleB);
  assert.strictEqual(result.ok, false, 'Add + delete on same quad should conflict');
  assert.strictEqual(result.witness.conflictType, 'contradiction', 'Conflict type should be contradiction');

  console.log('✅ Test passed: Contradiction detection');
}

/**
 * Test: Idempotent operations (same add twice)
 */
function testIdempotentOperations() {
  const capsuleA = {
    id: 'capsule-a',
    delta: {
      add: [
        { subject: 'http://example.org/s', predicate: 'http://example.org/p', object: 'value' }
      ]
    }
  };

  const capsuleB = {
    id: 'capsule-b',
    delta: {
      add: [
        { subject: 'http://example.org/s', predicate: 'http://example.org/p', object: 'value' }
      ]
    }
  };

  const result = canReorder(capsuleA, capsuleB);
  assert.strictEqual(result.ok, true, 'Identical operations should be commutative (idempotent)');

  console.log('✅ Test passed: Idempotent operations');
}

/**
 * Test: Write set analysis
 */
function testWriteSetAnalysis() {
  const capsule = {
    id: 'capsule-test',
    delta: {
      add: [
        { subject: 'http://example.org/s1', predicate: 'http://example.org/p1', object: 'v1' },
        { subject: 'http://example.org/s1', predicate: 'http://example.org/p2', object: 'v2' }
      ],
      del: [
        { subject: 'http://example.org/s2', predicate: 'http://example.org/p1', object: 'v3' }
      ]
    }
  };

  const writeSet = analyzeWriteSet(capsule);
  assert.strictEqual(writeSet.size, 3, 'Write set should have 3 unique (s,p) pairs');
  assert.ok(writeSet.has('http://example.org/s1|http://example.org/p1'));
  assert.ok(writeSet.has('http://example.org/s1|http://example.org/p2'));
  assert.ok(writeSet.has('http://example.org/s2|http://example.org/p1'));

  console.log('✅ Test passed: Write set analysis');
}

/**
 * Test: Read set analysis
 */
function testReadSetAnalysis() {
  const capsule = {
    id: 'capsule-test',
    delta: {
      add: [
        { subject: 'http://example.org/s1', predicate: 'http://example.org/p1', object: 'v1' }
      ]
    },
    intent: {
      guards: ['require http://example.org/guard-subject to exist'],
      profiles: ['profile http://example.org/profile-subject active']
    }
  };

  const readSet = analyzeReadSet(capsule);
  assert.ok(readSet.size >= 1, 'Read set should contain at least subjects from delta');
  assert.ok(readSet.has('http://example.org/s1'), 'Read set should include subject from delta');

  console.log('✅ Test passed: Read set analysis');
}

// Run all tests
console.log('\n🧪 Running Agent 5 Tests\n');

try {
  testCommutativeCapsules();
  testConflictingWriteWrite();
  testCertificateDeterminism();
  testDependencyGraph();
  testLargeCapsulePerformance();
  testContradiction();
  testIdempotentOperations();
  testWriteSetAnalysis();
  testReadSetAnalysis();

  console.log('\n✅ All tests passed (9/9)\n');
  process.exit(0);
} catch (error) {
  console.error('\n❌ Test failed:', error.message);
  console.error(error.stack);
  process.exit(1);
}
