/**
 * @file Process Mining Verifier
 * @module manufacturing/causality/process-verifier
 * @description Van der Aalst-style process conformance checking using OCEL event logs
 */

/**
 * Process verification result
 * @typedef {object} ProcessVerificationResult
 * @property {boolean} conformant - Whether observed process conforms to intended model
 * @property {number} fitness - 0-1 score: can model explain the log?
 * @property {number} precision - 0-1 score: is model too permissive?
 * @property {string[]} deviations - Specific conformance violations found
 * @property {object} diagnostics - Detailed diagnostic information
 */

export const DEVIATION_TYPES = {
  SKIPPED_STAGE: 'SKIPPED_STAGE',
  IMPOSSIBLE_ORDERING: 'IMPOSSIBLE_ORDERING',
  DUPLICATE_TERMINAL: 'DUPLICATE_TERMINAL_STATE',
  ORPHAN_OBJECT: 'ORPHAN_OBJECT',
  MISSING_PREDECESSOR: 'MISSING_CAUSAL_PREDECESSOR',
  VARIANT_EXPLOSION: 'UNEXPLAINED_VARIANT_EXPLOSION',
  TEMPORAL_VIOLATION: 'TEMPORAL_LAWFULNESS_VIOLATION',
};

/**
 * Verify process conformance against intended manufacturing model
 * @param {object} ocelLog - OCEL 2.0 event log
 * @param {object} intendedModel - Intended process model (stage transitions, gates)
 * @returns {ProcessVerificationResult}
 */
export function verifyProcessConformance(ocelLog, intendedModel) {
  const deviations = [];
  let fitness = 1.0;
  let precision = 1.0;

  // 1. Check stage ordering conformance
  const stageDeviations = verifyStageOrdering(ocelLog, intendedModel.stages);
  deviations.push(...stageDeviations);

  // 2. Check object lifecycle completeness
  const lifecycleDeviations = verifyObjectLifecycles(ocelLog);
  deviations.push(...lifecycleDeviations);

  // 3. Check temporal lawfulness
  const temporalDeviations = verifyTemporalLawfulness(ocelLog);
  deviations.push(...temporalDeviations);

  // 4. Check causal consistency
  const causalDeviations = verifyCausalConsistency(ocelLog);
  deviations.push(...causalDeviations);

  // 5. Check for orphan objects
  const orphanDeviations = verifyOrphanObjects(ocelLog);
  deviations.push(...orphanDeviations);

  // 6. Check variant explosion (deterministic system should have limited variants)
  const variantDeviations = verifyVariantExplosion(ocelLog, intendedModel);
  deviations.push(...variantDeviations);

  // Calculate fitness and precision
  if (deviations.length > 0) {
    fitness = Math.max(0, 1 - (deviations.length / 20)); // Penalize each deviation
    precision = deviations.some(d => d.type === DEVIATION_TYPES.SKIPPED_STAGE) ? 0.8 : 1.0;
  }

  return {
    conformant: deviations.length === 0,
    fitness,
    precision,
    deviations,
    diagnostics: {
      totalEvents: countTotalEvents(ocelLog),
      uniqueArtifacts: countUniqueArtifacts(ocelLog),
      stageDistribution: getStageDistribution(ocelLog),
    },
  };
}

/**
 * Verify stage ordering: seeded → bred → validated → ... → receipted
 */
function verifyStageOrdering(ocelLog, intendedStages) {
  const deviations = [];
  const stageOrder = intendedStages || [
    'seeded', 'bred', 'validated', 'projected', 'compiled',
    'benchmarked', 'released', 'observed', 'receipted'
  ];

  // For each artifact, verify stages occurred in order
  const artifacts = getArtifacts(ocelLog);
  for (const artifact of artifacts) {
    const stages = getStagesForArtifact(ocelLog, artifact.id);

    for (let i = 0; i < stages.length - 1; i++) {
      const current = stages[i];
      const next = stages[i + 1];
      const currentIndex = stageOrder.indexOf(current);
      const nextIndex = stageOrder.indexOf(next);

      if (currentIndex === -1 || nextIndex === -1) {
        deviations.push({
          type: DEVIATION_TYPES.IMPOSSIBLE_ORDERING,
          artifact,
          message: `Unknown stage in sequence: ${current} → ${next}`,
        });
        continue;
      }

      if (nextIndex < currentIndex) {
        deviations.push({
          type: DEVIATION_TYPES.IMPOSSIBLE_ORDERING,
          artifact,
          message: `Backward stage transition: ${current} → ${next}`,
        });
      }

      // Flag SKIPPED_STAGE only when critical stages are skipped (e.g., released without validated)
      // Optional stages like 'bred' may be skipped without penalty
      const criticalStages = ['validated', 'compiled', 'benchmarked'];
      const skipped = stageOrder.slice(currentIndex + 1, nextIndex);
      const hasCriticalSkip = skipped.some(s => criticalStages.includes(s));
      if (skipped.length > 1 && hasCriticalSkip) {
        deviations.push({
          type: DEVIATION_TYPES.SKIPPED_STAGE,
          artifact,
          message: `Skipped critical stages between ${current} and ${next}: ${skipped.join(', ')}`,
        });
      }
    }

    // Check for impossible ordering: release before validate
    if (stages.includes('released') && !stages.includes('validated')) {
      deviations.push({
        type: DEVIATION_TYPES.SKIPPED_STAGE,
        artifact,
        message: 'Artifact released without validation',
      });
    }
  }

  return deviations;
}

/**
 * Verify each artifact has complete lifecycle
 */
function verifyObjectLifecycles(ocelLog) {
  const deviations = [];
  const artifacts = getArtifacts(ocelLog);

  for (const artifact of artifacts) {
    const events = getEventsForObject(ocelLog, artifact.id);
    const eventTypes = new Set(events.map(e => e.type));

    // Must have creation event
    if (!eventTypes.has('ArtifactCreated')) {
      deviations.push({
        type: DEVIATION_TYPES.ORPHAN_OBJECT,
        artifact,
        message: 'Artifact has no creation event',
      });
      continue;
    }

    // Check for duplicate terminal states
    const terminalEvents = events.filter(e =>
      e.type === 'ArtifactReleased' || e.type === 'ArtifactReceipted'
    );
    if (terminalEvents.length > 1) {
      deviations.push({
        type: DEVIATION_TYPES.DUPLICATE_TERMINAL,
        artifact,
        message: `Artifact has ${terminalEvents.length} terminal events`,
      });
    }

    // Verify receipt chain
    const receipts = events.filter(e => e.type === 'ReceiptGenerated');
    for (let i = 0; i < receipts.length; i++) {
      const receipt = receipts[i];
      const nextReceipt = receipts[i + 1];

      if (nextReceipt &&
          receipt.attributes.previousHash !== nextReceipt.attributes.receiptHash) {
        deviations.push({
          type: DEVIATION_TYPES.MISSING_PREDECESSOR,
          artifact,
          message: `Receipt chain broken at position ${i}`,
        });
      }
    }
  }

  return deviations;
}

/**
 * Verify temporal lawfulness (timestamps monotonic per object)
 */
function verifyTemporalLawfulness(ocelLog) {
  const deviations = [];
  const artifacts = getArtifacts(ocelLog);

  for (const artifact of artifacts) {
    const events = getEventsForObject(ocelLog, artifact.id);

    for (let i = 0; i < events.length - 1; i++) {
      const current = events[i];
      const next = events[i + 1];

      if (new Date(current.timestamp) > new Date(next.timestamp)) {
        deviations.push({
          type: DEVIATION_TYPES.TEMPORAL_VIOLATION,
          artifact,
          message: `Non-monotonic timestamps: ${current.timestamp} > ${next.timestamp}`,
        });
      }
    }
  }

  return deviations;
}

/**
 * Verify causal consistency (receipts reference valid artifact states)
 */
function verifyCausalConsistency(ocelLog) {
  const deviations = [];
  const receipts = getObjectsByType(ocelLog, 'receipt');

  for (const receipt of receipts) {
    const artifactId = receipt.artifact;
    const artifact = getObject(ocelLog, artifactId);

    if (!artifact) {
      deviations.push({
        type: DEVIATION_TYPES.ORPHAN_OBJECT,
        object: receipt.id,
        message: `Receipt references non-existent artifact: ${artifactId}`,
      });
      continue;
    }

    // Verify artifact was in valid state when receipt emitted
    const receiptEvent = getEventForObject(ocelLog, receipt.id, 'ReceiptGenerated');
    if (!receiptEvent) continue;

    const artifactEvents = getEventsForObject(ocelLog, artifactId);
    const artifactStateAtReceipt = getLastEventBefore(receiptEvent.timestamp, artifactEvents);

    if (!artifactStateAtReceipt ||
        !['validated', 'compiled', 'benchmarked', 'released', 'receipted'].includes(artifactStateAtReceipt.activity)) {
      deviations.push({
        type: DEVIATION_TYPES.MISSING_PREDECESSOR,
        object: receipt.id,
        message: `Receipt emitted for artifact in invalid state: ${artifactStateAtReceipt?.activity || 'unknown'}`,
      });
    }
  }

  return deviations;
}

/**
 * Verify no orphan objects (objects without lawful lifecycle)
 */
function verifyOrphanObjects(ocelLog) {
  const deviations = [];
  const objects = getAllObjects(ocelLog);

  for (const obj of objects) {
    if (obj.type === 'artifact') {
      const events = getEventsForObject(ocelLog, obj.id);
      if (events.length === 0) {
        deviations.push({
          type: DEVIATION_TYPES.ORPHAN_OBJECT,
          object: obj.id,
          message: 'Artifact has no events',
        });
      }
    }
  }

  return deviations;
}

/**
 * Check for variant explosion (too many execution paths)
 */
function verifyVariantExplosion(ocelLog, intendedModel) {
  const deviations = [];

  // Count distinct event sequences per artifact type
  const artifactTypes = {};
  for (const artifact of getArtifacts(ocelLog)) {
    const kind = artifact.kind || 'unknown';
    const stages = getStagesForArtifact(ocelLog, artifact.id);
    const signature = stages.join('→');

    if (!artifactTypes[kind]) {
      artifactTypes[kind] = new Set();
    }
    artifactTypes[kind].add(signature);
  }

  // Flag types with too many variants (deterministic system should have limited variants)
  for (const [kind, variants] of Object.entries(artifactTypes)) {
    const variantCount = variants.size;
    const expectedVariants = intendedModel.expectedVariants?.[kind] || 3;

    if (variantCount > expectedVariants) {
      deviations.push({
        type: DEVIATION_TYPES.VARIANT_EXPLOSION,
        kind,
        message: `${variantCount} variants detected for ${kind} (expected ≤${expectedVariants})`,
        diagnostics: { variants: Array.from(variants) },
      });
    }
  }

  return deviations;
}

// Helper functions

function getArtifacts(ocelLog) {
  return Object.values(ocelLog.objects || {}).filter(o => o.type === 'artifact');
}

function getObjectsByType(ocelLog, type) {
  return Object.values(ocelLog.objects || {}).filter(o => o.type === type);
}

function getAllObjects(ocelLog) {
  return Object.values(ocelLog.objects || {});
}

function getObject(ocelLog, id) {
  return (ocelLog.objects || {})[id];
}

function getEventsForObject(ocelLog, objectId) {
  return (ocelLog.events || []).filter(e => e.object === objectId);
}

function getEventForObject(ocelLog, objectId, type) {
  return (ocelLog.events || []).find(e => e.object === objectId && e.type === type);
}

function getStagesForArtifact(ocelLog, artifactId) {
  const events = getEventsForObject(ocelLog, artifactId);
  return events
    .filter(e => e.activity && ['seeded', 'bred', 'validated', 'projected', 'compiled', 'benchmarked', 'released', 'observed', 'receipted'].includes(e.activity))
    .map(e => e.activity);
}

function getLastEventBefore(timestamp, events) {
  return [...events].reverse().find(e => new Date(e.timestamp) <= new Date(timestamp));
}

function countTotalEvents(ocelLog) {
  return (ocelLog.events || []).length;
}

function countUniqueArtifacts(ocelLog) {
  return new Set((ocelLog.events || []).map(e => e.object)).size;
}

function getStageDistribution(ocelLog) {
  const distribution = {};
  for (const event of ocelLog.events || []) {
    distribution[event.activity] = (distribution[event.activity] || 0) + 1;
  }
  return distribution;
}
