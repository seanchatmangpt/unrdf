const EPOCH = '1970-01-01T00:00:00.000Z';

function clone(value) {
  return structuredClone(value);
}

function assertString(value, field) {
  if (typeof value !== 'string' || value.length === 0) {
    throw new TypeError(`${field} must be a non-empty string`);
  }
  return value;
}

function attribute(name, value, time) {
  const item = { name: assertString(name, 'attribute.name'), value };
  if (time) item.time = time;
  return item;
}

function relationship(objectId, qualifier) {
  return {
    objectId: assertString(objectId, 'relationship.objectId'),
    qualifier: assertString(qualifier, 'relationship.qualifier'),
  };
}

export const ATOMVM_OCEL_OBJECT_TYPES = Object.freeze([
  {
    name: 'atomvm-runtime',
    attributes: [
      { name: 'peerId', type: 'string' },
      { name: 'runtime', type: 'string' },
      { name: 'sourceRef', type: 'string' },
      { name: 'standing', type: 'string' },
    ],
  },
  {
    name: 'p2p-session',
    attributes: [
      { name: 'transport', type: 'string' },
      { name: 'standing', type: 'string' },
    ],
  },
  {
    name: 'message',
    attributes: [
      { name: 'payload', type: 'string' },
      { name: 'sourceChecksum', type: 'integer' },
      { name: 'targetChecksum', type: 'integer' },
      { name: 'verified', type: 'boolean' },
    ],
  },
]);

export const ATOMVM_OCEL_EVENT_TYPES = Object.freeze([
  {
    name: 'peer.connected',
    attributes: [
      { name: 'transport', type: 'string' },
      { name: 'channel', type: 'string' },
    ],
  },
  {
    name: 'message.sent',
    attributes: [
      { name: 'runtimeSequence', type: 'integer' },
      { name: 'checksum', type: 'integer' },
    ],
  },
  {
    name: 'message.received',
    attributes: [
      { name: 'runtimeSequence', type: 'integer' },
      { name: 'checksum', type: 'integer' },
      { name: 'verified', type: 'boolean' },
    ],
  },
  {
    name: 'message.acknowledged',
    attributes: [
      { name: 'runtimeSequence', type: 'integer' },
      { name: 'checksum', type: 'integer' },
      { name: 'verified', type: 'boolean' },
    ],
  },
]);

export class AtomVMOcelV2Log extends EventTarget {
  #objects = new Map();
  #events = new Map();

  constructor({ peerId, sourceRef = 'atomvm/AtomVM@v0.6.6' }) {
    super();
    assertString(peerId, 'peerId');
    this.peerId = peerId;
    this.sourceRef = sourceRef;
    this.upsertRuntime(peerId, { sourceRef, standing: 'UNKNOWN' });
  }

  upsertRuntime(peerId, { sourceRef = this.sourceRef, standing = 'UNKNOWN' } = {}) {
    return this.upsertObject({
      id: `atomvm:${peerId}`,
      type: 'atomvm-runtime',
      attributes: [
        attribute('peerId', peerId, EPOCH),
        attribute('runtime', 'AtomVM/Emscripten/WASM', EPOCH),
        attribute('sourceRef', sourceRef, EPOCH),
        attribute('standing', standing, new Date().toISOString()),
      ],
      relationships: [],
    });
  }

  setRuntimeStanding(peerId, standing) {
    const id = `atomvm:${peerId}`;
    const existing = this.#objects.get(id);
    if (!existing) return this.upsertRuntime(peerId, { standing });
    const attributes = existing.attributes.filter(item => item.name !== 'standing');
    attributes.push(attribute('standing', standing, new Date().toISOString()));
    return this.upsertObject({ ...existing, attributes });
  }

  upsertSession(sessionId, localPeerId, remotePeerId, standing = 'ALIVE') {
    const id = `session:${sessionId}`;
    this.upsertRuntime(remotePeerId, { standing: 'UNKNOWN' });
    return this.upsertObject({
      id,
      type: 'p2p-session',
      attributes: [
        attribute('transport', 'WebRTC RTCDataChannel', EPOCH),
        attribute('standing', standing, new Date().toISOString()),
      ],
      relationships: [
        relationship(`atomvm:${localPeerId}`, 'local-runtime'),
        relationship(`atomvm:${remotePeerId}`, 'remote-runtime'),
      ],
    });
  }

  upsertMessage({ messageId, payload, sourcePeerId, targetPeerId, sourceChecksum, targetChecksum = null, verified = false }) {
    const existing = this.#objects.get(`message:${messageId}`);
    const attrs = [
      attribute('payload', payload, EPOCH),
      attribute('sourceChecksum', sourceChecksum, EPOCH),
    ];
    if (targetChecksum !== null && targetChecksum !== undefined) {
      attrs.push(attribute('targetChecksum', targetChecksum, new Date().toISOString()));
    }
    attrs.push(attribute('verified', Boolean(verified), new Date().toISOString()));
    return this.upsertObject({
      id: `message:${messageId}`,
      type: 'message',
      attributes: attrs,
      relationships: [
        relationship(`atomvm:${sourcePeerId}`, 'source-runtime'),
        relationship(`atomvm:${targetPeerId}`, 'target-runtime'),
      ],
      ...(existing ? { id: existing.id } : {}),
    });
  }

  addCommunicationEvent({ eventId, type, time = new Date().toISOString(), sessionId, messageId, sourcePeerId, targetPeerId, runtimeSequence, checksum, verified }) {
    const attributes = [
      attribute('runtimeSequence', runtimeSequence),
      attribute('checksum', checksum),
    ];
    if (typeof verified === 'boolean') attributes.push(attribute('verified', verified));
    return this.addEvent({
      id: eventId,
      type,
      time,
      attributes,
      relationships: [
        relationship(`session:${sessionId}`, 'session'),
        relationship(`message:${messageId}`, 'message'),
        relationship(`atomvm:${sourcePeerId}`, 'source-runtime'),
        relationship(`atomvm:${targetPeerId}`, 'target-runtime'),
      ],
    });
  }

  addConnectedEvent({ eventId, time = new Date().toISOString(), sessionId, localPeerId, remotePeerId, channel = 'unrdf-atomvm-ocel' }) {
    return this.addEvent({
      id: eventId,
      type: 'peer.connected',
      time,
      attributes: [attribute('transport', 'WebRTC RTCDataChannel'), attribute('channel', channel)],
      relationships: [
        relationship(`session:${sessionId}`, 'session'),
        relationship(`atomvm:${localPeerId}`, 'local-runtime'),
        relationship(`atomvm:${remotePeerId}`, 'remote-runtime'),
      ],
    });
  }

  upsertObject(object) {
    assertString(object?.id, 'object.id');
    assertString(object?.type, 'object.type');
    if (!ATOMVM_OCEL_OBJECT_TYPES.some(({ name }) => name === object.type)) {
      throw new TypeError(`Unsupported OCEL object type: ${object.type}`);
    }
    const normalized = {
      id: object.id,
      type: object.type,
      attributes: clone(object.attributes ?? []),
      relationships: clone(object.relationships ?? []),
    };
    this.#objects.set(normalized.id, normalized);
    this.#emit();
    return clone(normalized);
  }

  addEvent(event) {
    assertString(event?.id, 'event.id');
    assertString(event?.type, 'event.type');
    if (!ATOMVM_OCEL_EVENT_TYPES.some(({ name }) => name === event.type)) {
      throw new TypeError(`Unsupported OCEL event type: ${event.type}`);
    }
    if (this.#events.has(event.id)) return clone(this.#events.get(event.id));
    const normalized = {
      id: event.id,
      type: event.type,
      time: assertString(event.time, 'event.time'),
      attributes: clone(event.attributes ?? []),
      relationships: clone(event.relationships ?? []),
    };
    this.#events.set(normalized.id, normalized);
    this.#emit();
    return clone(normalized);
  }

  merge(fragment) {
    if (!fragment || typeof fragment !== 'object') throw new TypeError('OCEL fragment required');
    for (const object of fragment.objects ?? []) this.upsertObject(object);
    for (const event of fragment.events ?? []) this.addEvent(event);
    return this.snapshot();
  }

  fragment({ objectIds = [], eventIds = [] } = {}) {
    return {
      objectTypes: clone(ATOMVM_OCEL_OBJECT_TYPES),
      eventTypes: clone(ATOMVM_OCEL_EVENT_TYPES),
      objects: objectIds.map(id => this.#objects.get(id)).filter(Boolean).map(clone),
      events: eventIds.map(id => this.#events.get(id)).filter(Boolean).map(clone),
    };
  }

  snapshot() {
    return {
      objectTypes: clone(ATOMVM_OCEL_OBJECT_TYPES),
      eventTypes: clone(ATOMVM_OCEL_EVENT_TYPES),
      objects: [...this.#objects.values()].map(clone),
      events: [...this.#events.values()].sort((a, b) => a.time.localeCompare(b.time) || a.id.localeCompare(b.id)).map(clone),
    };
  }

  #emit() {
    this.dispatchEvent(new CustomEvent('change', { detail: this.snapshot() }));
  }
}

export function decodeAtomVMReceipt(receipt) {
  if (typeof receipt !== 'string') {
    throw new TypeError(`AtomVM runtime receipt must be a string, got ${typeof receipt}`);
  }
  const match = /^(\d+):(\d+)$/.exec(receipt);
  if (!match) {
    throw new TypeError(`Invalid AtomVM runtime receipt: ${receipt}`);
  }
  const sequence = Number(match[1]);
  const checksum = Number(match[2]);
  if (!Number.isSafeInteger(sequence) || sequence < 1 || !Number.isSafeInteger(checksum) || checksum < 0 || checksum >= 1000000000) {
    throw new TypeError(`Out-of-range AtomVM runtime receipt: ${receipt}`);
  }
  return { sequence, checksum };
}
