/**
 * @file Hypercore Transport for KGC-SWARM
 * @description P2P event log synchronization using Hypercore and Hyperswarm
 */

import crypto from 'node:crypto';
import { EventEmitter } from 'node:events';

/**
 * HypercoreTransport
 * 
 * Provides a decentralized, append-only log transport for KGCStore events.
 * Uses Hyperswarm for peer discovery and Corestore for managing multiple Hypercores.
 */
export class HypercoreTransport extends EventEmitter {
  /**
   * @param {Object} options
   * @param {string|Object} options.storage - Corestore storage path or object
   * @param {string|Buffer} options.topic - Hyperswarm topic (namespace)
   * @param {string} [options.name] - Optional name for the local core
   */
  constructor(options = {}) {
    super();
    this.storage = options.storage || './.kgc-swarm';
    this.topic = this._ensureBuffer(options.topic || this._generateDefaultTopic());
    this.name = options.name || 'default';
    
    // Lazy-loaded dependencies to avoid crash if not installed
    this._modules = {};
    this.corestore = null;
    this.swarm = null;
    this.localCore = null;
    this.cores = new Map(); // key -> core
    this.peers = new Set();
    this.initialized = false;
  }

  /**
   * Initialize the transport
   */
  async init() {
    if (this.initialized) return;

    try {
      // Dynamic imports to handle potentially missing peer dependencies
      const [Hypercore, Hyperswarm, Corestore, b4a] = await Promise.all([
        import('hypercore').catch(() => ({ default: class {} })),
        import('hyperswarm').catch(() => ({ default: class { join() {} ; on() {} ; flush() {} } })),
        import('corestore').catch(() => ({ default: class { get() {} ; replicate() {} } })),
        import('b4a').catch(() => ({ default: { from: (s) => Buffer.from(s), toString: (b) => b.toString() } }))
      ]);

      this._modules = { Hypercore: Hypercore.default, Hyperswarm: Hyperswarm.default, Corestore: Corestore.default, b4a: b4a.default };

      this.corestore = new this._modules.Corestore(this.storage);
      this.localCore = this.corestore.get({ name: this.name, valueEncoding: 'json' });
      await this.localCore.ready();

      this.swarm = new this._modules.Hyperswarm();
      this.swarm.on('connection', (conn) => {
        this.corestore.replicate(conn);
        this.emit('peer-connected', conn.remotePublicKey);
      });

      const discovery = this.swarm.join(this.topic, { client: true, server: true });
      await discovery.flushed();

      // Listen for updates on the local core
      this.localCore.on('append', () => {
        this.emit('local-append', this.localCore.length);
      });

      this.initialized = true;
      this.emit('ready', {
        publicKey: this.localCore.publicKey.toString('hex'),
        topic: this.topic.toString('hex')
      });

      // Begin discovery of other cores in the same corestore/swarm
      this._setupReplication();

    } catch (error) {
      console.error('[HypercoreTransport] Initialization failed:', error);
      this.emit('error', error);
      throw error;
    }
  }

  /**
   * Append an event to the local log
   * @param {Object} event - KGCStore event object
   */
  async append(event) {
    if (!this.initialized) await this.init();
    await this.localCore.append(event);
  }

  /**
   * Subscribe to a remote core
   * @param {Buffer|string} publicKey - Public key of the remote core
   */
  async addPeerCore(publicKey) {
    if (!this.initialized) await this.init();
    
    const key = typeof publicKey === 'string' ? Buffer.from(publicKey, 'hex') : publicKey;
    const keyString = key.toString('hex');

    if (this.cores.has(keyString)) return;

    const core = this.corestore.get({ key, valueEncoding: 'json' });
    await core.ready();
    
    core.on('append', async () => {
      const seq = core.length - 1;
      const block = await core.get(seq);
      this.emit('data', { peerId: keyString, seq, data: block });
    });

    // Initial sync of existing blocks
    for (let i = 0; i < core.length; i++) {
      const block = await core.get(i);
      this.emit('data', { peerId: keyString, seq: i, data: block });
    }

    this.cores.set(keyString, core);
  }

  /**
   * Broadcast a message to all peers via Hyperswarm (Gossip)
   * @param {string} channel - Extension channel name
   * @param {Object} message - Message payload
   */
  broadcast(channel, message) {
    if (!this.swarm) return;
    // Implementation of gossip via hypercore extensions or swarm broadcast
    // For now, we rely on Hypercore replication for state sync
    this.emit('broadcast', { channel, message });
  }

  _ensureBuffer(val) {
    if (Buffer.isBuffer(val)) return val;
    if (typeof val === 'string') return Buffer.from(val, 'hex').length === 32 ? Buffer.from(val, 'hex') : crypto.createHash('sha256').update(val).digest();
    return val;
  }

  _generateDefaultTopic() {
    return crypto.createHash('sha256').update('unrdf-kgc-swarm-v1').digest();
  }

  async close() {
    if (this.swarm) await this.swarm.destroy();
    if (this.corestore) await this.corestore.close();
    this.initialized = false;
  }
}

export function createHypercoreTransport(options) {
  return new HypercoreTransport(options);
}
