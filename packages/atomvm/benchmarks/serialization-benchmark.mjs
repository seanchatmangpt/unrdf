/**
 * Serialization Benchmark
 *
 * Measures triple serialization/deserialization performance.
 *
 * Method:
 * - Serializes/deserializes 1000 triples
 * - Measures roundtrip time
 * - Compares JSON vs simulated BEAM format
 * - Outputs throughput numbers
 */

import { createStore, dataFactory } from '../../oxigraph/src/index.mjs';

const { namedNode, literal } = dataFactory;

/**
 * Format number with thousands separator
 * @param {number} num - Number to format
 * @returns {string} Formatted number
 */
function formatNumber(num) {
  return num.toLocaleString('en-US', { maximumFractionDigits: 2 });
}

/**
 * Serialize triple to JSON
 * @param {object} triple - RDF triple
 * @returns {string} JSON string
 */
function serializeToJSON(triple) {
  return JSON.stringify({
    subject: {
      termType: triple.subject.termType,
      value: triple.subject.value
    },
    predicate: {
      termType: triple.predicate.termType,
      value: triple.predicate.value
    },
    object: {
      termType: triple.object.termType,
      value: triple.object.value,
      language: triple.object.language || '',
      datatype: triple.object.datatype?.value || ''
    }
  });
}

/**
 * Deserialize triple from JSON
 * @param {string} json - JSON string
 * @returns {object} RDF triple
 */
function deserializeFromJSON(json) {
  const obj = JSON.parse(json);

  let subject;
  if (obj.subject.termType === 'NamedNode') {
    subject = namedNode(obj.subject.value);
  } else {
    subject = { termType: obj.subject.termType, value: obj.subject.value };
  }

  let predicate;
  if (obj.predicate.termType === 'NamedNode') {
    predicate = namedNode(obj.predicate.value);
  } else {
    predicate = { termType: obj.predicate.termType, value: obj.predicate.value };
  }

  let object;
  if (obj.object.termType === 'Literal') {
    object = literal(obj.object.value);
  } else if (obj.object.termType === 'NamedNode') {
    object = namedNode(obj.object.value);
  } else {
    object = { termType: obj.object.termType, value: obj.object.value };
  }

  return { subject, predicate, object };
}

/**
 * Simulate BEAM binary serialization
 * (In reality, this would use Erlang External Term Format)
 * @param {object} triple - RDF triple
 * @returns {Buffer} Binary representation
 */
function serializeToBEAM(triple) {
  // Simulated BEAM format: compact binary encoding
  const s = `${triple.subject.termType}:${triple.subject.value}`;
  const p = `${triple.predicate.termType}:${triple.predicate.value}`;
  const o = `${triple.object.termType}:${triple.object.value}`;
  const beamStr = `${s.length}:${s}|${p.length}:${p}|${o.length}:${o}`;
  return Buffer.from(beamStr, 'utf-8');
}

/**
 * Deserialize from simulated BEAM format
 * @param {Buffer} buffer - Binary representation
 * @returns {object} RDF triple
 */
function deserializeFromBEAM(buffer) {
  // Parse simulated BEAM format
  const str = buffer.toString('utf-8');
  const parts = str.split('|');

  const parseComponent = (component) => {
    const colonIdx = component.indexOf(':');
    const len = parseInt(component.slice(0, colonIdx), 10);
    const rest = component.slice(colonIdx + 1);
    const [termType, value] = rest.split(':');
    return { termType, value };
  };

  const s = parseComponent(parts[0]);
  const p = parseComponent(parts[1]);
  const o = parseComponent(parts[2]);

  return {
    subject: s.termType === 'NamedNode' ? namedNode(s.value) : s,
    predicate: p.termType === 'NamedNode' ? namedNode(p.value) : p,
    object: o.termType === 'Literal' ? literal(o.value) :
            o.termType === 'NamedNode' ? namedNode(o.value) : o
  };
}

/**
 * Main benchmark function
 */
async function runBenchmark() {
  console.log('\n=== Serialization Benchmark ===\n');

  // Setup: Create 1000 test triples
  console.log('Setting up: Creating 1000 triples...');
  const setupStart = performance.now();

  const triples = [];
  for (let i = 0; i < 1000; i++) {
    const subject = namedNode(`http://example.org/subject${i}`);
    const predicate = namedNode(`http://example.org/predicate${i % 10}`);
    const object = literal(`value_${i}`);
    triples.push({ subject, predicate, object });
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

  console.log(`  Serialized ${triples.length} triples in ${jsonSerTime.toFixed(2)}ms`);
  console.log(`  Throughput: ${formatNumber(jsonSerThroughput)} triples/sec`);

  // Calculate average JSON size
  const totalJsonSize = jsonStrings.reduce((sum, s) => sum + s.length, 0);
  const avgJsonSize = totalJsonSize / jsonStrings.length;
  console.log(`  Average JSON size: ${avgJsonSize.toFixed(0)} bytes/triple`);

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
  console.log('\nBenchmark 3: JSON Roundtrip (Serialize + Deserialize)');
  const jsonRoundtripStart = performance.now();

  for (const triple of triples) {
    const json = serializeToJSON(triple);
    const restored = deserializeFromJSON(json);
    // Verify restoration
    if (restored.subject.value !== triple.subject.value) {
      throw new Error('Roundtrip failed');
    }
  }

  const jsonRoundtripEnd = performance.now();
  const jsonRoundtripTime = jsonRoundtripEnd - jsonRoundtripStart;
  const jsonRoundtripThroughput = (triples.length / jsonRoundtripTime) * 1000;

  console.log(`  Roundtripped ${triples.length} triples in ${jsonRoundtripTime.toFixed(2)}ms`);
  console.log(`  Throughput: ${formatNumber(jsonRoundtripThroughput)} roundtrips/sec`);

  // Benchmark 4: BEAM Serialization (simulated)
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

  console.log(`  Serialized ${triples.length} triples in ${beamSerTime.toFixed(2)}ms`);
  console.log(`  Throughput: ${formatNumber(beamSerThroughput)} triples/sec`);

  // Calculate average BEAM size
  const totalBeamSize = beamBuffers.reduce((sum, b) => sum + b.length, 0);
  const avgBeamSize = totalBeamSize / beamBuffers.length;
  console.log(`  Average BEAM size: ${avgBeamSize.toFixed(0)} bytes/triple`);

  // Benchmark 5: BEAM Deserialization (simulated)
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

  // Benchmark 6: BEAM Roundtrip (simulated)
  console.log('\nBenchmark 6: BEAM Binary Roundtrip (simulated)');
  const beamRoundtripStart = performance.now();

  for (const triple of triples) {
    const buffer = serializeToBEAM(triple);
    const restored = deserializeFromBEAM(buffer);
    // Verify restoration
    if (restored.subject.value !== triple.subject.value) {
      throw new Error('BEAM roundtrip failed');
    }
  }

  const beamRoundtripEnd = performance.now();
  const beamRoundtripTime = beamRoundtripEnd - beamRoundtripStart;
  const beamRoundtripThroughput = (triples.length / beamRoundtripTime) * 1000;

  console.log(`  Roundtripped ${triples.length} triples in ${beamRoundtripTime.toFixed(2)}ms`);
  console.log(`  Throughput: ${formatNumber(beamRoundtripThroughput)} roundtrips/sec`);

  // Print summary results
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
  const beamSpeedupSer = beamSerThroughput / jsonSerThroughput;
  const beamSpeedupDeser = beamDeserThroughput / jsonDeserThroughput;
  const beamSpeedupRoundtrip = beamRoundtripThroughput / jsonRoundtripThroughput;
  const beamSizeRatio = avgBeamSize / avgJsonSize;

  console.log(`- BEAM serialization speedup: ${beamSpeedupSer.toFixed(2)}x`);
  console.log(`- BEAM deserialization speedup: ${beamSpeedupDeser.toFixed(2)}x`);
  console.log(`- BEAM roundtrip speedup: ${beamSpeedupRoundtrip.toFixed(2)}x`);
  console.log(`- BEAM size ratio: ${(beamSizeRatio * 100).toFixed(1)}% of JSON`);

  console.log('\n=== Performance Target Validation ===');
  const targetRoundtrips = 5000;
  const bestRoundtrip = Math.max(jsonRoundtripThroughput, beamRoundtripThroughput);
  const meetsTarget = bestRoundtrip >= targetRoundtrips;
  console.log(`- Target: >= ${formatNumber(targetRoundtrips)} roundtrips/sec`);
  console.log(`- Best Actual: ${formatNumber(bestRoundtrip)} roundtrips/sec`);
  console.log(`- Status: ${meetsTarget ? '✅ PASS' : '❌ FAIL'}`);

  console.log('\n===============================\n');
}

// Run benchmark
runBenchmark().catch(error => {
  console.error('Benchmark failed:', error);
  process.exit(1);
});
