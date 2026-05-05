/**
 * @file Write set, read set, and dependency graph analysis
 * @module agent-5/analysis
 */

/**
 * @typedef {Object} Quad
 * @property {string} subject
 * @property {string} predicate
 * @property {string} object
 */

/**
 * @typedef {Object} Capsule
 * @property {string} id
 * @property {Object} delta
 * @property {Quad[]} [delta.add]
 * @property {Quad[]} [delta.del]
 * @property {Object} [intent]
 * @property {string[]} [intent.guards]
 * @property {string[]} [intent.profiles]
 */

/**
 * @typedef {Object} DependencyGraph
 * @property {string[]} nodes - Capsule IDs
 * @property {Array<{from: string, to: string, reason: string}>} edges
 */

/**
 * Analyze write set of a capsule
 * @param {Capsule} capsule
 * @returns {Set<string>} Set of (subject|predicate) pairs
 */
export function analyzeWriteSet(capsule) {
  const writeSet = new Set();

  const addQuads = capsule.delta?.add || [];
  const delQuads = capsule.delta?.del || [];

  for (const quad of addQuads) {
    writeSet.add(`${quad.subject}|${quad.predicate}`);
  }

  for (const quad of delQuads) {
    writeSet.add(`${quad.subject}|${quad.predicate}`);
  }

  return writeSet;
}

/**
 * Analyze read set of a capsule
 * @param {Capsule} capsule
 * @returns {Set<string>} Set of subjects
 */
export function analyzeReadSet(capsule) {
  const readSet = new Set();

  // Extract subjects from intent guards
  if (capsule.intent?.guards) {
    for (const guard of capsule.intent.guards) {
      // Guards reference subjects - extract them
      // Simple heuristic: look for URIs in guard strings
      const uriMatches = guard.match(/(?:https?:\/\/|urn:)[^\s,)]+/g);
      if (uriMatches) {
        for (const uri of uriMatches) {
          readSet.add(uri);
        }
      }
    }
  }

  // Extract subjects from intent profiles
  if (capsule.intent?.profiles) {
    for (const profile of capsule.intent.profiles) {
      // Profiles may reference subjects
      const uriMatches = profile.match(/(?:https?:\/\/|urn:)[^\s,)]+/g);
      if (uriMatches) {
        for (const uri of uriMatches) {
          readSet.add(uri);
        }
      }
    }
  }

  // Also consider subjects from delta operations as potential reads
  // (operations may depend on current state of subjects)
  const addQuads = capsule.delta?.add || [];
  const delQuads = capsule.delta?.del || [];

  for (const quad of [...addQuads, ...delQuads]) {
    readSet.add(quad.subject);
  }

  return readSet;
}

/**
 * Build dependency graph from array of capsules
 * @param {Capsule[]} capsules
 * @returns {DependencyGraph}
 */
export function dependencyGraph(capsules) {
  const nodes = capsules.map(c => c.id || `capsule-${capsules.indexOf(c)}`);
  const edges = [];

  // Build write and read sets for all capsules
  const writeSets = new Map();
  const readSets = new Map();

  for (const capsule of capsules) {
    const id = capsule.id || `capsule-${capsules.indexOf(capsule)}`;
    writeSets.set(id, analyzeWriteSet(capsule));
    readSets.set(id, analyzeReadSet(capsule));
  }

  // Check all pairs for dependencies
  for (let i = 0; i < capsules.length; i++) {
    const capsuleA = capsules[i];
    const idA = capsuleA.id || `capsule-${i}`;
    const writeSetA = writeSets.get(idA);

    for (let j = 0; j < capsules.length; j++) {
      if (i === j) continue;

      const capsuleB = capsules[j];
      const idB = capsuleB.id || `capsule-${j}`;
      const readSetB = readSets.get(idB);
      const writeSetB = writeSets.get(idB);

      // Check if B reads what A writes
      let hasReadWriteDep = false;
      for (const writeKey of writeSetA) {
        const subject = writeKey.split('|')[0];
        if (readSetB.has(subject)) {
          hasReadWriteDep = true;
          break;
        }
      }

      if (hasReadWriteDep) {
        edges.push({
          from: idA,
          to: idB,
          reason: 'read-write dependency'
        });
        continue;
      }

      // Check if both write to same (subject, predicate)
      let hasWriteWriteDep = false;
      for (const writeKey of writeSetA) {
        if (writeSetB.has(writeKey)) {
          hasWriteWriteDep = true;
          break;
        }
      }

      if (hasWriteWriteDep) {
        edges.push({
          from: idA,
          to: idB,
          reason: 'write-write conflict (order required)'
        });
      }
    }
  }

  return { nodes, edges };
}
