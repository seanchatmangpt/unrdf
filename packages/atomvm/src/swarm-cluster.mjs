import { createHash, randomUUID } from 'node:crypto';

const NS = 'https://unrdf.dev/atomvm/swarm/';

export class SwarmClusterRefusal extends Error {
  constructor(code, message, details = {}) {
    super(message);
    this.name = 'SwarmClusterRefusal';
    this.code = code;
    this.details = Object.freeze({ ...details });
  }
}

function assertId(value, field) {
  if (typeof value !== 'string' || !/^[a-zA-Z0-9][a-zA-Z0-9._-]*$/.test(value)) {
    throw new SwarmClusterRefusal('INVALID_ID_REFUSED', `${field} must be a stable identifier`, { field, value });
  }
}

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

function escapeLiteral(value) {
  return String(value).replaceAll('\\', '\\\\').replaceAll('"', '\\"').replaceAll('\n', '\\n');
}

export class AtomVMSwarmCluster {
  #swarms = new Map();
  #links = new Map();
  #receipts = [];

  constructor({ clusterId, clock = () => new Date().toISOString(), idFactory = randomUUID } = {}) {
    assertId(clusterId, 'clusterId');
    this.clusterId = clusterId;
    this.clock = clock;
    this.idFactory = idFactory;
  }

  admitSwarm({ id, gatewayNode, cookieRef, endpoint, metadata = {} }) {
    assertId(id, 'swarm.id');
    assertId(gatewayNode, 'swarm.gatewayNode');
    if (this.#swarms.has(id)) {
      throw new SwarmClusterRefusal('DUPLICATE_SWARM_REFUSED', `Swarm ${id} is already admitted`, { id });
    }
    if (typeof endpoint !== 'string' || !endpoint.startsWith('atomvm://')) {
      throw new SwarmClusterRefusal('INVALID_ENDPOINT_REFUSED', 'endpoint must use atomvm://', { id, endpoint });
    }
    if (typeof cookieRef !== 'string' || cookieRef.length === 0) {
      throw new SwarmClusterRefusal('MISSING_AUTHORITY_REFUSED', 'cookieRef is required; raw cookies are not admitted', { id });
    }
    const swarm = Object.freeze({ id, gatewayNode, cookieRef, endpoint, metadata: Object.freeze({ ...metadata }) });
    this.#swarms.set(id, swarm);
    this.#links.set(id, new Set());
    return swarm;
  }

  connect(leftId, rightId) {
    if (leftId === rightId) {
      throw new SwarmClusterRefusal('SELF_LINK_REFUSED', 'A swarm cannot federate with itself', { leftId });
    }
    const left = this.#requireSwarm(leftId);
    const right = this.#requireSwarm(rightId);
    this.#links.get(left.id).add(right.id);
    this.#links.get(right.id).add(left.id);
    return Object.freeze({ left: left.id, right: right.id });
  }

  route(sourceId, targetId) {
    this.#requireSwarm(sourceId);
    this.#requireSwarm(targetId);
    const queue = [[sourceId]];
    const visited = new Set([sourceId]);
    while (queue.length) {
      const path = queue.shift();
      const current = path.at(-1);
      if (current === targetId) return Object.freeze([...path]);
      for (const next of [...this.#links.get(current)].sort()) {
        if (!visited.has(next)) {
          visited.add(next);
          queue.push([...path, next]);
        }
      }
    }
    throw new SwarmClusterRefusal('NO_ROUTE_REFUSED', `No admitted route from ${sourceId} to ${targetId}`, { sourceId, targetId });
  }

  constructIntent({ sourceId, targetId, operation, payload }) {
    if (typeof operation !== 'string' || operation.length === 0) {
      throw new SwarmClusterRefusal('INVALID_OPERATION_REFUSED', 'operation is required');
    }
    const route = this.route(sourceId, targetId);
    const intent = {
      intentId: this.idFactory(),
      clusterId: this.clusterId,
      sourceId,
      targetId,
      route,
      operation,
      payload,
      constructedAt: this.clock(),
      status: 'CONSTRUCTED'
    };
    return Object.freeze({ ...intent, intentDigest: digest(intent) });
  }

  async actuate(intent, broker) {
    if (!intent || intent.clusterId !== this.clusterId || intent.status !== 'CONSTRUCTED') {
      throw new SwarmClusterRefusal('UNADMITTED_INTENT_REFUSED', 'Only constructed intents for this cluster may actuate');
    }
    const expectedDigest = digest(Object.fromEntries(Object.entries(intent).filter(([key]) => key !== 'intentDigest')));
    if (expectedDigest !== intent.intentDigest) {
      throw new SwarmClusterRefusal('INTENT_DRIFT_REFUSED', 'Intent changed after construction');
    }
    if (!broker || typeof broker.execute !== 'function') {
      throw new SwarmClusterRefusal('BROKER_REQUIRED_REFUSED', 'Zero unreceipted actuation: broker.execute is required');
    }

    const startedAt = this.clock();
    let outcome;
    try {
      const target = this.#requireSwarm(intent.targetId);
      const result = await broker.execute({ intent, target, route: intent.route });
      outcome = { status: 'ALIVE', result };
    } catch (error) {
      outcome = { status: 'BLOCKED', error: { name: error?.name ?? 'Error', message: error?.message ?? String(error) } };
    }
    const receiptBody = {
      receiptId: this.idFactory(),
      clusterId: this.clusterId,
      intentDigest: intent.intentDigest,
      route: intent.route,
      startedAt,
      completedAt: this.clock(),
      ...outcome
    };
    const receipt = Object.freeze({ ...receiptBody, receiptDigest: digest(receiptBody) });
    this.#receipts.push(receipt);
    return receipt;
  }

  verifyReceipt(receipt) {
    const body = Object.fromEntries(Object.entries(receipt).filter(([key]) => key !== 'receiptDigest'));
    return digest(body) === receipt.receiptDigest && receipt.clusterId === this.clusterId;
  }

  replay(receiptId) {
    const receipt = this.#receipts.find(item => item.receiptId === receiptId);
    if (!receipt) throw new SwarmClusterRefusal('RECEIPT_NOT_FOUND_REFUSED', `Unknown receipt ${receiptId}`, { receiptId });
    if (!this.verifyReceipt(receipt)) throw new SwarmClusterRefusal('RECEIPT_DRIFT_REFUSED', 'Receipt failed digest verification', { receiptId });
    return receipt;
  }

  toNQuads() {
    const graph = `<${NS}graph/${this.clusterId}>`;
    const lines = [];
    const cluster = `<${NS}cluster/${this.clusterId}>`;
    lines.push(`${cluster} <${NS}type> <${NS}Cluster> ${graph} .`);
    for (const swarm of [...this.#swarms.values()].sort((a, b) => a.id.localeCompare(b.id))) {
      const subject = `<${NS}swarm/${swarm.id}>`;
      lines.push(`${cluster} <${NS}hasSwarm> ${subject} ${graph} .`);
      lines.push(`${subject} <${NS}gatewayNode> "${escapeLiteral(swarm.gatewayNode)}" ${graph} .`);
      lines.push(`${subject} <${NS}endpoint> "${escapeLiteral(swarm.endpoint)}" ${graph} .`);
    }
    const emitted = new Set();
    for (const [left, peers] of [...this.#links.entries()].sort()) {
      for (const right of [...peers].sort()) {
        const [source, target] = [left, right].sort();
        const key = `${source}|${target}`;
        if (emitted.has(key)) continue;
        emitted.add(key);
        lines.push(`<${NS}swarm/${source}> <${NS}federatedWith> <${NS}swarm/${target}> ${graph} .`);
      }
    }
    return `${lines.join('\n')}\n`;
  }

  snapshot() {
    return Object.freeze({
      clusterId: this.clusterId,
      swarms: [...this.#swarms.values()],
      links: [...this.#links.entries()].map(([id, peers]) => [id, [...peers].sort()]),
      receipts: [...this.#receipts]
    });
  }

  #requireSwarm(id) {
    const swarm = this.#swarms.get(id);
    if (!swarm) throw new SwarmClusterRefusal('UNKNOWN_SWARM_REFUSED', `Swarm ${id} is not admitted`, { id });
    return swarm;
  }
}

export function createAtomVMSwarmCluster(options) {
  return new AtomVMSwarmCluster(options);
}
