const PROTOCOL = 'unrdf-atomvm-ocel-v1';
const CHANNEL = 'unrdf-atomvm-ocel';

function assertString(value, field) {
  if (typeof value !== 'string' || value.length === 0) throw new TypeError(`${field} must be a non-empty string`);
  return value;
}

function parseSignal(text, expectedType) {
  let value;
  try {
    value = typeof text === 'string' ? JSON.parse(text) : text;
  } catch (error) {
    throw new TypeError(`Invalid WebRTC signal JSON: ${error.message}`);
  }
  if (value?.protocol !== PROTOCOL || value?.description?.type !== expectedType) {
    throw new TypeError(`Expected ${PROTOCOL} ${expectedType} signal`);
  }
  assertString(value.peerId, 'signal.peerId');
  assertString(value.sessionId, 'signal.sessionId');
  return value;
}

async function waitForIceGathering(pc, timeoutMs = 10000) {
  if (pc.iceGatheringState === 'complete') return;
  await new Promise((resolve, reject) => {
    const timeout = setTimeout(() => {
      pc.removeEventListener('icegatheringstatechange', onChange);
      reject(new Error('ICE_GATHERING_TIMEOUT'));
    }, timeoutMs);
    const onChange = () => {
      if (pc.iceGatheringState === 'complete') {
        clearTimeout(timeout);
        pc.removeEventListener('icegatheringstatechange', onChange);
        resolve();
      }
    };
    pc.addEventListener('icegatheringstatechange', onChange);
  });
}

export class AtomVMWebRtcLink extends EventTarget {
  constructor({ peerId, rtcConfig = { iceServers: [] } }) {
    super();
    this.peerId = assertString(peerId, 'peerId');
    this.remotePeerId = null;
    this.sessionId = null;
    this.pc = new RTCPeerConnection(rtcConfig);
    this.channel = null;
    this.pc.addEventListener('connectionstatechange', () => {
      this.dispatchEvent(new CustomEvent('state', { detail: this.state() }));
    });
    this.pc.addEventListener('datachannel', event => this.#bindChannel(event.channel));
  }

  async createOffer() {
    this.sessionId = crypto.randomUUID();
    this.#bindChannel(this.pc.createDataChannel(CHANNEL, { ordered: true }));
    await this.pc.setLocalDescription(await this.pc.createOffer());
    await waitForIceGathering(this.pc);
    return JSON.stringify({
      protocol: PROTOCOL,
      peerId: this.peerId,
      sessionId: this.sessionId,
      description: this.pc.localDescription,
    });
  }

  async acceptOffer(text) {
    const signal = parseSignal(text, 'offer');
    this.remotePeerId = signal.peerId;
    this.sessionId = signal.sessionId;
    await this.pc.setRemoteDescription(signal.description);
    await this.pc.setLocalDescription(await this.pc.createAnswer());
    await waitForIceGathering(this.pc);
    return JSON.stringify({
      protocol: PROTOCOL,
      peerId: this.peerId,
      sessionId: this.sessionId,
      description: this.pc.localDescription,
    });
  }

  async acceptAnswer(text) {
    const signal = parseSignal(text, 'answer');
    if (this.sessionId && signal.sessionId !== this.sessionId) {
      throw new Error('SESSION_ID_MISMATCH_REFUSED');
    }
    this.remotePeerId = signal.peerId;
    this.sessionId = signal.sessionId;
    await this.pc.setRemoteDescription(signal.description);
  }

  send(message) {
    if (!this.channel || this.channel.readyState !== 'open') {
      throw new Error('P2P_CHANNEL_NOT_OPEN_REFUSED');
    }
    this.channel.send(JSON.stringify(message));
  }

  state() {
    return Object.freeze({
      peerId: this.peerId,
      remotePeerId: this.remotePeerId,
      sessionId: this.sessionId,
      connectionState: this.pc.connectionState,
      channelState: this.channel?.readyState ?? 'absent',
    });
  }

  close() {
    this.channel?.close();
    this.pc.close();
  }

  #bindChannel(channel) {
    if (this.channel && this.channel !== channel) throw new Error('SECOND_DATA_CHANNEL_REFUSED');
    this.channel = channel;
    channel.addEventListener('open', () => {
      this.dispatchEvent(new CustomEvent('connected', { detail: this.state() }));
    });
    channel.addEventListener('close', () => {
      this.dispatchEvent(new CustomEvent('state', { detail: this.state() }));
    });
    channel.addEventListener('message', event => {
      let message;
      try {
        message = JSON.parse(event.data);
      } catch {
        this.dispatchEvent(new CustomEvent('refused', { detail: { code: 'INVALID_WIRE_JSON_REFUSED' } }));
        return;
      }
      this.dispatchEvent(new CustomEvent('message', { detail: message }));
    });
  }
}
