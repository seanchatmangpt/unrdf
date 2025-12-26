#!/usr/bin/env node
/**
 * @fileoverview WebSocket server for collaborative RDF editing
 *
 * Simple y-websocket server that coordinates CRDT synchronization
 * between multiple clients. Runs on port 1234 by default.
 *
 * Usage:
 *   node examples/collab-server.mjs [port]
 */

import { WebSocketServer } from 'ws';
import * as Y from 'yjs';
import * as syncProtocol from 'y-websocket/bin/utils';
import * as encoding from 'lib0/encoding';
import * as decoding from 'lib0/decoding';

const PORT = process.argv[2] || 1234;

// Store Yjs documents per room
const docs = new Map();

// Get or create Yjs document for a room
function getYDoc(roomName) {
  let doc = docs.get(roomName);
  if (!doc) {
    doc = new Y.Doc();
    docs.set(roomName, doc);
    console.log(`📄 Created room: ${roomName}`);
  }
  return doc;
}

// Setup WebSocket server
const wss = new WebSocketServer({ port: PORT });

console.log(`🚀 Collaborative RDF Server running on ws://localhost:${PORT}`);
console.log(`   Ready to sync RDF graphs across multiple clients\n`);

wss.on('connection', (ws, req) => {
  const roomName = new URL(req.url, `ws://localhost:${PORT}`).pathname.slice(1);

  if (!roomName) {
    ws.close(1008, 'Room name required');
    return;
  }

  const doc = getYDoc(roomName);

  console.log(`✅ Client connected to room: ${roomName}`);

  ws.on('message', (message) => {
    const encoder = encoding.createEncoder();
    const decoder = decoding.createDecoder(new Uint8Array(message));
    const messageType = decoding.readVarUint(decoder);

    switch (messageType) {
      case syncProtocol.messageSync:
        encoding.writeVarUint(encoder, syncProtocol.messageSync);
        syncProtocol.readSyncMessage(decoder, encoder, doc, ws);
        break;

      case syncProtocol.messageAwareness: {
        // Broadcast awareness to all clients in room
        wss.clients.forEach((client) => {
          if (client !== ws && client.readyState === 1) {
            client.send(message);
          }
        });
        break;
      }
    }

    if (encoding.length(encoder) > 1) {
      ws.send(encoding.toUint8Array(encoder));
    }
  });

  ws.on('close', () => {
    console.log(`❌ Client disconnected from room: ${roomName}`);
  });

  ws.on('error', (error) => {
    console.error(`⚠️  WebSocket error in room ${roomName}:`, error.message);
  });

  // Send sync step 1
  const encoder = encoding.createEncoder();
  encoding.writeVarUint(encoder, syncProtocol.messageSync);
  syncProtocol.writeSyncStep1(encoder, doc);
  ws.send(encoding.toUint8Array(encoder));
});

// Graceful shutdown
process.on('SIGINT', () => {
  console.log('\n🛑 Shutting down server...');
  wss.close(() => {
    console.log('✅ Server stopped');
    process.exit(0);
  });
});
