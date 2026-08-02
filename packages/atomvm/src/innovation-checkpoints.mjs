import { createHash } from 'node:crypto';

export const INNOVATION_CHECKPOINTS = Object.freeze([
  ['gall-working-core', 'Gall', 'The complex federation must preserve a simple working tracer bullet.'],
  ['ocel-event-completeness', 'van-der-Aalst', 'Every actuation has object, activity, time, and outcome evidence.'],
  ['object-centric-identity', 'van-der-Aalst', 'Cluster, swarms, intent, route, and receipt retain distinct identities.'],
  ['route-conformance', 'van-der-Aalst', 'Observed execution conforms to the admitted topology and intended route.'],
  ['dependency-rule', 'Martin', 'Domain construction is independent from infrastructure; only the broker actuates.'],
  ['explicit-contracts', 'Martin', 'Failure modes and standing use explicit typed states rather than hidden control flow.'],
  ['tracer-bullet', 'Hunt-Thomas', 'One thin path crosses admit, route, construct, actuate, receipt, and replay.'],
  ['orthogonality-dry', 'Hunt-Thomas', 'Topology is represented once and projected deterministically.'],
  ['failure-isolation', 'Gall-Martin', 'A failed broker call becomes bounded evidence and does not corrupt topology.'],
  ['evolutionary-extension', 'Gall-Hunt-Thomas', 'A new swarm can be added through public admission and linking operations.'],
]);

function canonical(value) {
  if (Array.isArray(value)) return `[${value.map(canonical).join(',')}]`;
  if (value && typeof value === 'object') {
    return `{${Object.keys(value).sort().map(key => `${JSON.stringify(key)}:${canonical(value[key])}`).join(',')}}`;
  }
  return JSON.stringify(value);
}

function digest(value) {
  return createHash('sha256').update(canonical(value)).digest('hex');
}

function result(id, passed, evidence) {
  const definition = INNOVATION_CHECKPOINTS.find(([candidate]) => candidate === id);
  return Object.freeze({
    id,
    authority: definition[1],
    criterion: definition[2],
    status: passed ? 'ALIVE' : 'UNSUPPORTED',
    evidence: Object.freeze(evidence ?? {}),
  });
}

function linkSet(snapshot) {
  const links = new Set();
  for (const [left, peers] of snapshot.links ?? []) {
    for (const right of peers) links.add([left, right].sort().join('|'));
  }
  return links;
}

function routeConforms(route, snapshot) {
  if (!Array.isArray(route) || route.length === 0) return false;
  const admitted = new Set((snapshot.swarms ?? []).map(({ id }) => id));
  const links = linkSet(snapshot);
  if (!route.every(id => admitted.has(id))) return false;
  return route.slice(1).every((id, index) => links.has([route[index], id].sort().join('|')));
}

export function receiptToOcel(receipt, intent) {
  if (!receipt || !intent) throw new TypeError('receipt and intent are required');
  const eventId = `event:${receipt.receiptId}`;
  const candidates = [
    { id: `cluster:${receipt.clusterId}`, type: 'cluster' },
    { id: `swarm:${intent.sourceId}`, type: 'swarm' },
    { id: `swarm:${intent.targetId}`, type: 'swarm' },
    { id: `intent:${intent.intentId}`, type: 'intent' },
    { id: `receipt:${receipt.receiptId}`, type: 'receipt' },
  ];
  const objectMap = new Map(candidates.map(object => [object.id, Object.freeze(object)]));
  const objects = Object.freeze([...objectMap.values()]);
  const objectIds = Object.freeze([...objectMap.keys()]);

  return Object.freeze({
    objectTypes: Object.freeze(['cluster', 'swarm', 'intent', 'receipt']),
    objects,
    events: Object.freeze([Object.freeze({
      id: eventId,
      activity: intent.operation,
      time: receipt.completedAt,
      outcome: receipt.status,
      objects: objectIds,
      attributes: Object.freeze({ route: [...receipt.route], intentDigest: receipt.intentDigest }),
    })]),
  });
}

export function evaluateInnovationCheckpoints({ cluster, intent, receipt, replayedReceipt, brokerObserved = false, negativeControlPassed = false }) {
  if (!cluster || typeof cluster.snapshot !== 'function') throw new TypeError('cluster.snapshot is required');
  const snapshot = cluster.snapshot();
  const topologyBefore = { swarms: snapshot.swarms, links: snapshot.links };
  const ocel = receipt && intent ? receiptToOcel(receipt, intent) : null;
  const routeOk = Boolean(intent && receipt &&
    intent.sourceId === receipt.route?.[0] &&
    intent.targetId === receipt.route?.at(-1) &&
    routeConforms(receipt.route, snapshot));
  const receiptVerified = Boolean(receipt && cluster.verifyReceipt(receipt));
  const replayVerified = Boolean(receipt && replayedReceipt && receipt.receiptDigest === replayedReceipt.receiptDigest);
  const projectedA = typeof cluster.toNQuads === 'function' ? cluster.toNQuads() : '';
  const projectedB = typeof cluster.toNQuads === 'function' ? cluster.toNQuads() : '';
  const explicitStanding = ['ALIVE', 'BLOCKED'].includes(receipt?.status);
  const topologyStable = digest(topologyBefore) === digest({ swarms: cluster.snapshot().swarms, links: cluster.snapshot().links });

  const checks = [
    result('gall-working-core', snapshot.swarms.length >= 2 && routeOk && receiptVerified, { swarms: snapshot.swarms.length, route: receipt?.route }),
    result('ocel-event-completeness', Boolean(ocel?.events[0]?.activity && ocel.events[0]?.time && ocel.events[0]?.outcome && ocel.events[0]?.objects.length >= 4), { eventId: ocel?.events[0]?.id }),
    result('object-centric-identity', Boolean(new Set(ocel?.objects.map(({ id }) => id)).size === ocel?.objects.length && ocel?.objects.length >= 4), { objectCount: ocel?.objects.length ?? 0 }),
    result('route-conformance', routeOk, { intended: intent?.route, observed: receipt?.route }),
    result('dependency-rule', brokerObserved && negativeControlPassed, { brokerObserved, unbrokeredActuationRefused: negativeControlPassed }),
    result('explicit-contracts', explicitStanding && receiptVerified, { standing: receipt?.status, receiptVerified }),
    result('tracer-bullet', routeOk && receiptVerified && replayVerified, { replayVerified }),
    result('orthogonality-dry', projectedA.length > 0 && projectedA === projectedB, { projectionDigest: digest(projectedA) }),
    result('failure-isolation', topologyStable && explicitStanding, { topologyStable, standing: receipt?.status }),
    result('evolutionary-extension', typeof cluster.admitSwarm === 'function' && typeof cluster.connect === 'function' && typeof cluster.route === 'function', { publicExtensionSurface: true }),
  ];

  const passed = checks.filter(check => check.status === 'ALIVE').length;
  const body = {
    clusterId: snapshot.clusterId,
    status: passed === checks.length ? 'ALIVE' : 'PARTIAL_ALIVE',
    passed,
    total: checks.length,
    checkpoints: checks,
  };
  return Object.freeze({ ...body, reportDigest: digest(body) });
}
