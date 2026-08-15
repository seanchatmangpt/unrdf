import { AtomVMOcelV2Log, decodeAtomVMReceipt } from './ocel-v2.mjs';
import { AtomVMWebRtcLink } from './webrtc-link.mjs';

const SOURCE_REF = 'atomvm/AtomVM@v0.6.6';
const BASE = import.meta.env.BASE_URL;
const byId = id => document.getElementById(id);

function text(id, value) {
  const el = byId(id);
  if (el) el.textContent = String(value);
}

function setStanding(id, standing) {
  const el = byId(id);
  if (!el) return;
  el.textContent = standing;
  el.dataset.standing = standing;
}

function now() {
  return new Date().toISOString();
}

function eventId(type) {
  return `event:${type}:${crypto.randomUUID()}`;
}

function messageId() {
  return `msg-${crypto.randomUUID()}`;
}

function escapeHtml(value) {
  return String(value)
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;')
    .replaceAll("'", '&#039;');
}

async function ensureCrossOriginIsolation() {
  if (globalThis.crossOriginIsolated && typeof SharedArrayBuffer !== 'undefined') return true;
  if (!('serviceWorker' in navigator)) throw new Error('SERVICE_WORKER_UNSUPPORTED_REFUSED');

  const registration = await navigator.serviceWorker.register(`${BASE}coi-serviceworker.js`, { scope: BASE });
  await navigator.serviceWorker.ready;
  if (!navigator.serviceWorker.controller || !globalThis.crossOriginIsolated) {
    sessionStorage.setItem('atomvm-coi-reload', '1');
    location.reload();
    return false;
  }
  if (registration.active) return true;
  throw new Error('CROSS_ORIGIN_ISOLATION_BLOCKED');
}

async function waitForAtomVMProcess(timeoutMs = 15000) {
  const started = Date.now();
  let lastError;
  while (Date.now() - started < timeoutMs) {
    try {
      if (typeof window.Module?.call === 'function') {
        const receipt = await Promise.race([
          window.Module.call('peer', 'probe'),
          new Promise((_, reject) => setTimeout(() => reject(new Error('probe timeout')), 750)),
        ]);
        return decodeAtomVMReceipt(receipt);
      }
    } catch (error) {
      lastError = error;
    }
    await new Promise(resolve => setTimeout(resolve, 150));
  }
  throw new Error(`ATOMVM_PEER_PROCESS_BLOCKED: ${lastError?.message ?? 'timeout'}`);
}

async function loadAtomVMRuntime(peerId, onLog) {
  if (!globalThis.crossOriginIsolated || typeof SharedArrayBuffer === 'undefined') {
    throw new Error('ATOMVM_SHARED_ARRAY_BUFFER_BLOCKED');
  }

  const runtimeRoot = new URL(`${BASE}runtime/`, location.origin).href;
  const beamUrl = new URL('atomvm_ocel_peer.beam', runtimeRoot).href;

  window.Module = {
    arguments: [beamUrl],
    locateFile(path) {
      return new URL(path, runtimeRoot).href;
    },
    print(line) {
      onLog?.('stdout', String(line));
    },
    printErr(line) {
      onLog?.('stderr', String(line));
    },
  };

  await new Promise((resolve, reject) => {
    const script = document.createElement('script');
    script.src = new URL('AtomVM.js', runtimeRoot).href;
    script.async = true;
    script.dataset.atomvmRuntime = peerId;
    script.addEventListener('load', resolve, { once: true });
    script.addEventListener('error', () => reject(new Error(`ATOMVM_SCRIPT_LOAD_BLOCKED: ${script.src}`)), { once: true });
    document.head.appendChild(script);
  });

  const probe = await waitForAtomVMProcess();
  return {
    peerId,
    sourceRef: SOURCE_REF,
    probe,
    async observe(body) {
      const receipt = await window.Module.call('peer', body);
      return decodeAtomVMReceipt(receipt);
    },
  };
}

function ocelAttributeValue(item, name) {
  return item.attributes?.filter(attribute => attribute.name === name).at(-1)?.value;
}

function renderGraph(snapshot) {
  const svg = byId('ocelGraph');
  if (!svg) return;
  const width = 960;
  const height = 460;
  svg.setAttribute('viewBox', `0 0 ${width} ${height}`);
  svg.replaceChildren();

  const objects = snapshot.objects;
  const events = snapshot.events.slice(-10);
  const positions = new Map();
  const runtimeObjects = objects.filter(item => item.type === 'atomvm-runtime');
  const sessions = objects.filter(item => item.type === 'p2p-session');
  const messages = objects.filter(item => item.type === 'message').slice(-5);

  runtimeObjects.forEach((item, index) => positions.set(item.id, { x: 150 + index * 650, y: 90 }));
  sessions.forEach((item, index) => positions.set(item.id, { x: width / 2, y: 80 + index * 45 }));
  messages.forEach((item, index) => positions.set(item.id, { x: 180 + index * 145, y: 380 }));
  events.forEach((item, index) => positions.set(item.id, { x: 105 + index * 82, y: 235 }));

  const ns = 'http://www.w3.org/2000/svg';
  const make = (name, attrs = {}) => {
    const node = document.createElementNS(ns, name);
    for (const [key, value] of Object.entries(attrs)) node.setAttribute(key, String(value));
    return node;
  };

  for (const event of events) {
    const from = positions.get(event.id);
    for (const rel of event.relationships ?? []) {
      const to = positions.get(rel.objectId);
      if (!from || !to) continue;
      svg.appendChild(make('line', { x1: from.x, y1: from.y, x2: to.x, y2: to.y, class: 'ocel-edge', 'data-qualifier': rel.qualifier }));
    }
  }

  for (const object of [...runtimeObjects, ...sessions, ...messages]) {
    const pos = positions.get(object.id);
    if (!pos) continue;
    const group = make('g', { class: `ocel-node object ${object.type}`, 'data-ocel-id': object.id });
    group.appendChild(make('circle', { cx: pos.x, cy: pos.y, r: object.type === 'atomvm-runtime' ? 38 : 26 }));
    const label = make('text', { x: pos.x, y: pos.y + 4, 'text-anchor': 'middle' });
    label.textContent = object.type === 'atomvm-runtime' ? ocelAttributeValue(object, 'peerId') : object.id.split(':').at(-1).slice(0, 10);
    group.appendChild(label);
    svg.appendChild(group);
  }

  for (const event of events) {
    const pos = positions.get(event.id);
    const group = make('g', { class: `ocel-node event ${event.type.replaceAll('.', '-')}`, 'data-event-type': event.type, 'data-ocel-id': event.id });
    group.appendChild(make('rect', { x: pos.x - 30, y: pos.y - 18, width: 60, height: 36, rx: 8 }));
    const label = make('text', { x: pos.x, y: pos.y + 4, 'text-anchor': 'middle' });
    label.textContent = event.type.split('.').at(-1);
    group.appendChild(label);
    svg.appendChild(group);
  }
}

function render(snapshot) {
  text('objectCount', snapshot.objects.length);
  text('eventCount', snapshot.events.length);
  const raw = byId('rawOcel');
  if (raw) raw.textContent = JSON.stringify(snapshot, null, 2);

  const eventStream = byId('eventStream');
  if (eventStream) {
    eventStream.innerHTML = snapshot.events.slice().reverse().map(event => {
      const verified = event.attributes?.find(item => item.name === 'verified')?.value;
      return `<li data-event-type="${escapeHtml(event.type)}"><time>${escapeHtml(event.time)}</time><strong>${escapeHtml(event.type)}</strong>${typeof verified === 'boolean' ? `<span class="verification ${verified ? 'alive' : 'blocked'}">${verified ? 'VERIFIED' : 'MISMATCH'}</span>` : ''}</li>`;
    }).join('');
  }
  renderGraph(snapshot);
}

class Explorer {
  constructor(peerId) {
    this.peerId = peerId;
    this.ocel = new AtomVMOcelV2Log({ peerId, sourceRef: SOURCE_REF });
    this.link = new AtomVMWebRtcLink({ peerId });
    this.runtime = null;
    this.pending = new Map();
    this.logs = [];
    this.ocel.addEventListener('change', event => render(event.detail));
    this.link.addEventListener('connected', event => this.#onConnected(event.detail));
    this.link.addEventListener('message', event => this.#onWireMessage(event.detail));
    this.link.addEventListener('state', event => this.#onLinkState(event.detail));
    this.link.addEventListener('refused', event => this.#refused(event.detail.code));
  }

  async start() {
    render(this.ocel.snapshot());
    text('peerId', this.peerId);
    setStanding('runtimeStanding', 'UNKNOWN');
    setStanding('p2pStanding', 'UNKNOWN');
    const isolated = await ensureCrossOriginIsolation();
    if (!isolated) return;
    this.runtime = await loadAtomVMRuntime(this.peerId, (stream, line) => {
      this.logs.push({ stream, line, time: now() });
      this.logs = this.logs.slice(-30);
      const el = byId('runtimeLog');
      if (el) el.textContent = this.logs.map(item => `[${item.stream}] ${item.line}`).join('\n');
    });
    this.ocel.setRuntimeStanding(this.peerId, 'ALIVE');
    setStanding('runtimeStanding', 'ALIVE');
    document.body.dataset.runtimeStanding = 'ALIVE';
    text('runtimeReceipt', `seq=${this.runtime.probe.sequence} checksum=${this.runtime.probe.checksum}`);
  }

  async createOffer() {
    const offer = await this.link.createOffer();
    byId('localSignal').value = offer;
    return offer;
  }

  async acceptOffer(offer) {
    const answer = await this.link.acceptOffer(offer);
    byId('localSignal').value = answer;
    return answer;
  }

  async acceptAnswer(answer) {
    await this.link.acceptAnswer(answer);
  }

  async send(payload) {
    if (!this.runtime) throw new Error('LOCAL_ATOMVM_NOT_ALIVE_REFUSED');
    if (!this.link.remotePeerId || !this.link.sessionId) throw new Error('REMOTE_ATOMVM_NOT_CONNECTED_REFUSED');
    if (typeof payload !== 'string' || payload.length === 0) throw new Error('EMPTY_MESSAGE_REFUSED');

    const id = messageId();
    const body = JSON.stringify({ id, payload });
    const sourceReceipt = await this.runtime.observe(body);
    const sentAt = now();
    const sentEventId = eventId('sent');
    this.ocel.upsertMessage({
      messageId: id,
      payload,
      sourcePeerId: this.peerId,
      targetPeerId: this.link.remotePeerId,
      sourceChecksum: sourceReceipt.checksum,
      verified: false,
    });
    this.ocel.addCommunicationEvent({
      eventId: sentEventId,
      type: 'message.sent',
      time: sentAt,
      sessionId: this.link.sessionId,
      messageId: id,
      sourcePeerId: this.peerId,
      targetPeerId: this.link.remotePeerId,
      runtimeSequence: sourceReceipt.sequence,
      checksum: sourceReceipt.checksum,
    });

    const sentFragment = this.ocel.fragment({
      objectIds: [`atomvm:${this.peerId}`, `atomvm:${this.link.remotePeerId}`, `session:${this.link.sessionId}`, `message:${id}`],
      eventIds: [sentEventId],
    });

    const promise = new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        this.pending.delete(id);
        reject(new Error(`ATOMVM_ACK_TIMEOUT_BLOCKED:${id}`));
      }, 10000);
      this.pending.set(id, { resolve, reject, timeout });
    });

    this.link.send({
      kind: 'atomvm-message',
      protocol: 'unrdf-atomvm-ocel-v1',
      sessionId: this.link.sessionId,
      messageId: id,
      sourcePeerId: this.peerId,
      targetPeerId: this.link.remotePeerId,
      body,
      payload,
      sourceReceipt,
      sentFragment,
    });
    return promise;
  }

  snapshot() {
    return this.ocel.snapshot();
  }

  state() {
    return {
      peerId: this.peerId,
      runtimeStanding: byId('runtimeStanding')?.dataset.standing ?? 'UNKNOWN',
      p2pStanding: byId('p2pStanding')?.dataset.standing ?? 'UNKNOWN',
      link: this.link.state(),
      ocel: this.snapshot(),
    };
  }

  async #onConnected(state) {
    const remotePeerId = this.link.remotePeerId;
    if (!remotePeerId || !state.sessionId) return this.#refused('REMOTE_IDENTITY_UNKNOWN_REFUSED');
    this.ocel.upsertSession(state.sessionId, this.peerId, remotePeerId, 'ALIVE');
    this.ocel.setRuntimeStanding(remotePeerId, 'ALIVE');
    this.ocel.addConnectedEvent({
      eventId: eventId('connected'),
      sessionId: state.sessionId,
      localPeerId: this.peerId,
      remotePeerId,
    });
    setStanding('p2pStanding', 'ALIVE');
    document.body.dataset.p2pStanding = 'ALIVE';
    text('remotePeerId', remotePeerId);
  }

  #onLinkState(state) {
    if (state.connectionState === 'failed' || state.connectionState === 'closed') {
      setStanding('p2pStanding', 'BLOCKED');
      document.body.dataset.p2pStanding = 'BLOCKED';
    }
  }

  async #onWireMessage(wire) {
    try {
      if (wire?.sessionId !== this.link.sessionId) throw new Error('WIRE_SESSION_MISMATCH_REFUSED');
      if (wire.kind === 'atomvm-message') return await this.#receiveAtomVMMessage(wire);
      if (wire.kind === 'atomvm-ack') return await this.#receiveAck(wire);
      if (wire.kind === 'ocel-sync') {
        this.ocel.merge(wire.fragment);
        return;
      }
      throw new Error('UNKNOWN_WIRE_KIND_REFUSED');
    } catch (error) {
      this.#refused(error.message);
    }
  }

  async #receiveAtomVMMessage(wire) {
    if (!this.runtime) throw new Error('TARGET_ATOMVM_NOT_ALIVE_REFUSED');
    if (wire.targetPeerId !== this.peerId || wire.sourcePeerId !== this.link.remotePeerId) {
      throw new Error('WIRE_IDENTITY_MISMATCH_REFUSED');
    }
    this.ocel.merge(wire.sentFragment);
    const targetReceipt = await this.runtime.observe(wire.body);
    const verified = targetReceipt.checksum === wire.sourceReceipt.checksum;
    const receivedEventId = eventId('received');
    this.ocel.upsertMessage({
      messageId: wire.messageId,
      payload: wire.payload,
      sourcePeerId: wire.sourcePeerId,
      targetPeerId: wire.targetPeerId,
      sourceChecksum: wire.sourceReceipt.checksum,
      targetChecksum: targetReceipt.checksum,
      verified,
    });
    this.ocel.addCommunicationEvent({
      eventId: receivedEventId,
      type: 'message.received',
      sessionId: wire.sessionId,
      messageId: wire.messageId,
      sourcePeerId: wire.sourcePeerId,
      targetPeerId: wire.targetPeerId,
      runtimeSequence: targetReceipt.sequence,
      checksum: targetReceipt.checksum,
      verified,
    });

    const fragment = this.ocel.fragment({
      objectIds: [`message:${wire.messageId}`],
      eventIds: [receivedEventId],
    });
    this.link.send({
      kind: 'atomvm-ack',
      protocol: 'unrdf-atomvm-ocel-v1',
      sessionId: wire.sessionId,
      messageId: wire.messageId,
      sourcePeerId: wire.sourcePeerId,
      targetPeerId: wire.targetPeerId,
      sourceReceipt: wire.sourceReceipt,
      targetReceipt,
      verified,
      fragment,
    });
  }

  async #receiveAck(wire) {
    if (wire.sourcePeerId !== this.peerId || wire.targetPeerId !== this.link.remotePeerId) {
      throw new Error('ACK_IDENTITY_MISMATCH_REFUSED');
    }
    this.ocel.merge(wire.fragment);
    const verified = Boolean(wire.verified && wire.sourceReceipt.checksum === wire.targetReceipt.checksum);
    const ackBody = JSON.stringify({ id: wire.messageId, ack: wire.targetReceipt.checksum, verified });
    const ackReceipt = await this.runtime.observe(ackBody);
    const ackEventId = eventId('acknowledged');
    const message = this.ocel.snapshot().objects.find(item => item.id === `message:${wire.messageId}`);
    this.ocel.upsertMessage({
      messageId: wire.messageId,
      payload: ocelAttributeValue(message, 'payload') ?? '',
      sourcePeerId: this.peerId,
      targetPeerId: this.link.remotePeerId,
      sourceChecksum: wire.sourceReceipt.checksum,
      targetChecksum: wire.targetReceipt.checksum,
      verified,
    });
    this.ocel.addCommunicationEvent({
      eventId: ackEventId,
      type: 'message.acknowledged',
      sessionId: wire.sessionId,
      messageId: wire.messageId,
      sourcePeerId: this.peerId,
      targetPeerId: this.link.remotePeerId,
      runtimeSequence: ackReceipt.sequence,
      checksum: ackReceipt.checksum,
      verified,
    });
    this.link.send({
      kind: 'ocel-sync',
      protocol: 'unrdf-atomvm-ocel-v1',
      sessionId: wire.sessionId,
      fragment: this.ocel.fragment({
        objectIds: [`message:${wire.messageId}`],
        eventIds: [ackEventId],
      }),
    });
    const pending = this.pending.get(wire.messageId);
    if (pending) {
      clearTimeout(pending.timeout);
      this.pending.delete(wire.messageId);
      if (verified) pending.resolve({ status: 'ALIVE', messageId: wire.messageId, sourceReceipt: wire.sourceReceipt, targetReceipt: wire.targetReceipt, ackReceipt });
      else pending.reject(new Error(`ATOMVM_MESSAGE_CHECKSUM_MISMATCH_BLOCKED:${wire.messageId}`));
    }
  }

  #refused(code) {
    text('lastRefusal', code);
    const el = byId('lastRefusal');
    if (el) el.dataset.refusal = code;
  }
}

function bindUi(explorer) {
  byId('createOfferBtn')?.addEventListener('click', async () => explorer.createOffer());
  byId('acceptOfferBtn')?.addEventListener('click', async () => {
    const answer = await explorer.acceptOffer(byId('remoteSignal').value);
    byId('localSignal').value = answer;
  });
  byId('acceptAnswerBtn')?.addEventListener('click', async () => explorer.acceptAnswer(byId('remoteSignal').value));
  byId('sendBtn')?.addEventListener('click', async () => {
    const input = byId('messageInput');
    const result = await explorer.send(input.value);
    text('lastReceipt', JSON.stringify(result));
    input.value = '';
  });
  byId('downloadBtn')?.addEventListener('click', () => {
    const blob = new Blob([JSON.stringify(explorer.snapshot(), null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const anchor = document.createElement('a');
    anchor.href = url;
    anchor.download = `atomvm-ocel-${explorer.peerId}.json`;
    anchor.click();
    URL.revokeObjectURL(url);
  });
}

const params = new URLSearchParams(location.search);
const peerId = params.get('peer') || `peer-${crypto.randomUUID().slice(0, 8)}`;
const explorer = new Explorer(peerId);
window.__atomvmExplorer = explorer;
bindUi(explorer);
explorer.start().catch(error => {
  setStanding('runtimeStanding', 'BLOCKED');
  document.body.dataset.runtimeStanding = 'BLOCKED';
  text('lastRefusal', error.message);
  console.error(error);
});
