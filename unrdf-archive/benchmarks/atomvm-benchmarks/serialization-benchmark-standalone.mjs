/**
 * Serialization Benchmark (Standalone Version)
 *
 * Demonstrates serialization/deserialization measurement methodology.
 * Uses simple mock RDF terms instead of actual Oxigraph terms.
 */

/**
 * Format number with thousands separator
 */
function formatNumber(num) {
  return num.toLocaleString('en-US', { maximumFractionDigits: 2 });
}

/**
 * Simple mock triple structure
 */
function createTriple(s, p, o) {
  return {
    subject: { termType: 'NamedNode', value: s },
    predicate: { termType: 'NamedNode', value: p },
    object: { termType: 'Literal', value: o }
  };
}

/**
 * Serialize triple to JSON
 */
function serializeToJSON(triple) {
  return JSON.stringify(triple);
}

/**
 * Deserialize triple from JSON
 */
function deserializeFromJSON(json) {
  return JSON.parse(json);
}

/**
 * Simulate BEAM binary serialization
 */
function serializeToBEAM(triple) {
  const s = `${triple.subject.termType}:${triple.subject.value}`;
  const p = `${triple.predicate.termType}:${triple.predicate.value}`;
  const o = `${triple.object.termType}:${triple.object.value}`;
  const beamStr = `${s.length}:${s}|${p.length}:${p}|${o.length}:${o}`;
  return Buffer.from(beamStr, 'utf-8');
}

/**
 * Deserialize from simulated BEAM format
 */
function deserializeFromBEAM(buffer) {
  const str = buffer.toString('utf-8');
  const parts = str.split('|');

  const parseComponent = (component) => {
    const colonIdx = component.indexOf(':');
    const len = parseInt(component.slice(0, colonIdx), 10);
    const rest = component.slice(colonIdx + 1);
    const secondColonIdx = rest.indexOf(':');
    const termType = rest.slice(0, secondColonIdx);
    const value = rest.slice(secondColonIdx + 1);
    return { termType, value };
  };

  const s = parseComponent(parts[0]);
  const p = parseComponent(parts[1]);
  const o = parseComponent(parts[2]);

  return {
    subject: s,
    predicate: p,
    object: o
  };
}

/**
 * Main benchmark
 */
async function runBenchmark() {
  console.log('\n=== Serialization Benchmark (Standalone Demo) ===\n');

  // Create 1000 test triples
  console.log('Creating 1000 test triples...');
  const setupStart = performance.now();

  const triples = [];
  for (let i = 0; i < 1000; i++) {
    triples.push(createTriple(
      `http://example.org/subject${i}`,
      `http://example.org/predicate${i % 10}`,
      `value_${i}`
    ));
  }

  const setupTime = performance.now() - setupStart;
  console.log(`Setup completed in ${setupTime.toFixed(2)}ms\n`);

  // Benchmark 1: JSON Serialization
  console.log('Benchmark 1: JSON Serialization');
  const jsonSerStart = performance.now();
  const jsonStrings = [];

  for (const triple of triples) {
    const json = serializeToJSON(triple);
    jsonStrings.push(json);
  }

  const jsonSerEnd = performance.now();
  const jsonSerTime = jsonSerEnd - jsonSerStart;
  const jsonSerThroughput = (triples.length / jsonSerTime) * 1000;

  const totalJsonSize = jsonStrings.reduce((sum, s) => sum + s.length, 0);
  const avgJsonSize = totalJsonSize / jsonStrings.length;

  console.log(`  Serialized ${triples.length} triples in ${jsonSerTime.toFixed(2)}ms`);
  console.log(`  Throughput: ${formatNumber(jsonSerThroughput)} triples/sec`);
  console.log(`  Average size: ${avgJsonSize.toFixed(0)} bytes/triple`);

  // Benchmark 2: JSON Deserialization
  console.log('\nBenchmark 2: JSON Deserialization');
  const jsonDeserStart = performance.now();
  const jsonDeserialized = [];

  for (const json of jsonStrings) {
    const triple = deserializeFromJSON(json);
    jsonDeserialized.push(triple);
  }

  const jsonDeserEnd = performance.now();
  const jsonDeserTime = jsonDeserEnd - jsonDeserStart;
  const jsonDeserThroughput = (jsonStrings.length / jsonDeserTime) * 1000;

  console.log(`  Deserialized ${jsonStrings.length} triples in ${jsonDeserTime.toFixed(2)}ms`);
  console.log(`  Throughput: ${formatNumber(jsonDeserThroughput)} triples/sec`);

  // Benchmark 3: JSON Roundtrip
  console.log('\nBenchmark 3: JSON Roundtrip');
  const jsonRoundtripStart = performance.now();

  for (const triple of triples) {
    const json = serializeToJSON(triple);
    const restored = deserializeFromJSON(json);
    if (restored.subject.value !== triple.subject.value) {
      throw new Error('Roundtrip failed');
    }
  }

  const jsonRoundtripEnd = performance.now();
  const jsonRoundtripTime = jsonRoundtripEnd - jsonRoundtripStart;
  const jsonRoundtripThroughput = (triples.length / jsonRoundtripTime) * 1000;

  console.log(`  Roundtripped ${triples.length} triples in ${jsonRoundtripTime.toFixed(2)}ms`);
  console.log(`  Throughput: ${formatNumber(jsonRoundtripThroughput)} roundtrips/sec`);

  // Benchmark 4: BEAM Serialization
  console.log('\nBenchmark 4: BEAM Binary Serialization (simulated)');
  const beamSerStart = performance.now();
  const beamBuffers = [];

  for (const triple of triples) {
    const buffer = serializeToBEAM(triple);
    beamBuffers.push(buffer);
  }

  const beamSerEnd = performance.now();
  const beamSerTime = beamSerEnd - beamSerStart;
  const beamSerThroughput = (triples.length / beamSerTime) * 1000;

  const totalBeamSize = beamBuffers.reduce((sum, b) => sum + b.length, 0);
  const avgBeamSize = totalBeamSize / beamBuffers.length;

  console.log(`  Serialized ${triples.length} triples in ${beamSerTime.toFixed(2)}ms`);
  console.log(`  Throughput: ${formatNumber(beamSerThroughput)} triples/sec`);
  console.log(`  Average size: ${avgBeamSize.toFixed(0)} bytes/triple`);

  // Benchmark 5: BEAM Deserialization
  console.log('\nBenchmark 5: BEAM Binary Deserialization (simulated)');
  const beamDeserStart = performance.now();
  const beamDeserialized = [];

  for (const buffer of beamBuffers) {
    const triple = deserializeFromBEAM(buffer);
    beamDeserialized.push(triple);
  }

  const beamDeserEnd = performance.now();
  const beamDeserTime = beamDeserEnd - beamDeserStart;
  const beamDeserThroughput = (beamBuffers.length / beamDeserTime) * 1000;

  console.log(`  Deserialized ${beamBuffers.length} triples in ${beamDeserTime.toFixed(2)}ms`);
  console.log(`  Throughput: ${formatNumber(beamDeserThroughput)} triples/sec`);

  // Benchmark 6: BEAM Roundtrip
  console.log('\nBenchmark 6: BEAM Binary Roundtrip (simulated)');
  const beamRoundtripStart = performance.now();

  for (const triple of triples) {
    const buffer = serializeToBEAM(triple);
    const restored = deserializeFromBEAM(buffer);
    if (restored.subject.value !== triple.subject.value) {
      throw new Error('BEAM roundtrip failed');
    }
  }

  const beamRoundtripEnd = performance.now();
  const beamRoundtripTime = beamRoundtripEnd - beamRoundtripStart;
  const beamRoundtripThroughput = (triples.length / beamRoundtripTime) * 1000;

  console.log(`  Roundtripped ${triples.length} triples in ${beamRoundtripTime.toFixed(2)}ms`);
  console.log(`  Throughput: ${formatNumber(beamRoundtripThroughput)} roundtrips/sec`);

  // Results summary
  console.log('\n=== RESULTS ===\n');

  console.log('JSON Format:');
  console.log(`- Serialization: ${formatNumber(jsonSerThroughput)} triples/sec`);
  console.log(`- Deserialization: ${formatNumber(jsonDeserThroughput)} triples/sec`);
  console.log(`- Roundtrip: ${formatNumber(jsonRoundtripThroughput)} roundtrips/sec`);
  console.log(`- Average Size: ${avgJsonSize.toFixed(0)} bytes/triple`);

  console.log('\nBEAM Binary Format (simulated):');
  console.log(`- Serialization: ${formatNumber(beamSerThroughput)} triples/sec`);
  console.log(`- Deserialization: ${formatNumber(beamDeserThroughput)} triples/sec`);
  console.log(`- Roundtrip: ${formatNumber(beamRoundtripThroughput)} roundtrips/sec`);
  console.log(`- Average Size: ${avgBeamSize.toFixed(0)} bytes/triple`);

  console.log('\nFormat Comparison:');
  const beamSpeedupRoundtrip = beamRoundtripThroughput / jsonRoundtripThroughput;
  const beamSizeRatio = avgBeamSize / avgJsonSize;

  console.log(`- BEAM roundtrip speedup: ${beamSpeedupRoundtrip.toFixed(2)}x`);
  console.log(`- BEAM size ratio: ${(beamSizeRatio * 100).toFixed(1)}% of JSON`);

  console.log('\n=== Benchmark Methodology Demonstrated ===');
  console.log('✅ Using performance.now() for timing');
  console.log('✅ Measuring throughput (items/sec)');
  console.log('✅ Measuring size overhead (bytes/triple)');
  console.log('✅ Format comparison (JSON vs BEAM)');
  console.log('✅ Roundtrip validation (correctness check)');

  console.log('\n===============================\n');
}

runBenchmark().catch(error => {
  console.error('Benchmark failed:', error);
  process.exit(1);
});
