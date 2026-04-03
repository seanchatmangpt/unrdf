/**
 * Real-time Sync Layer
 * Distributed state using CRDT (Conflict-free Replicated Data Types)
 */

export class RealtimeSync {
  constructor() {
    this.documents = new Map();
    this.peers = new Map();
    this.awareness = new Map();
    this.eventBus = new Map();
  }

  /**
   * Create shared document (CRDT)
   */
  createDocument(docName) {
    const doc = {
      name: docName,
      content: '',
      versions: [{ v: 0, content: '', timestamp: Date.now() }],
      peers: new Set(),
      created: new Date(),
      lastModified: new Date()
    };
    this.documents.set(docName, doc);
    return doc;
  }

  /**
   * Update document (conflict-free)
   */
  async updateDocument(docName, update) {
    const doc = this.documents.get(docName);
    if (!doc) throw new Error(`Document not found: ${docName}`);

    const version = doc.versions.length;
    const newVersion = {
      v: version,
      content: update.content || doc.content,
      delta: update.delta,
      clientId: update.clientId,
      timestamp: Date.now()
    };

    doc.versions.push(newVersion);
    doc.content = newVersion.content;
    doc.lastModified = new Date();

    // Broadcast to peers
    await this._broadcastUpdate(docName, newVersion);

    return { docName, version: newVersion.v, timestamp: newVersion.timestamp };
  }

  async _broadcastUpdate(docName, version) {
    const doc = this.documents.get(docName);
    for (const peerId of doc.peers) {
      const peer = this.peers.get(peerId);
      if (peer?.onUpdate) {
        await peer.onUpdate(docName, version);
      }
    }
  }

  /**
   * Register peer for real-time sync
   */
  registerPeer(peerId, handlers = {}) {
    const peer = {
      id: peerId,
      connected: true,
      joined: new Date(),
      documents: new Set(),
      ...handlers
    };
    this.peers.set(peerId, peer);
    return peer;
  }

  /**
   * Join document as peer
   */
  async joinDocument(peerId, docName) {
    const doc = this.documents.get(docName);
    const peer = this.peers.get(peerId);

    if (!doc || !peer) throw new Error('Document or peer not found');

    doc.peers.add(peerId);
    peer.documents.add(docName);

    // Emit awareness update
    await this._broadcastAwareness(docName, { peerId, joined: true });

    return { peerId, docName, version: doc.versions.length };
  }

  async _broadcastAwareness(docName, update) {
    const doc = this.documents.get(docName);
    for (const peerId of doc.peers) {
      const peer = this.peers.get(peerId);
      if (peer?.onAwareness) {
        await peer.onAwareness(docName, update);
      }
    }
  }

  /**
   * Get document history
   */
  getHistory(docName, limit = 100) {
    const doc = this.documents.get(docName);
    if (!doc) return null;

    return {
      docName,
      totalVersions: doc.versions.length,
      versions: doc.versions.slice(-limit),
      peers: Array.from(doc.peers),
      lastModified: doc.lastModified
    };
  }
}

export default RealtimeSync;
