import { createServer } from 'node:http';
import { webcrypto } from 'node:crypto';
import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { AtomVMNodeRuntime } from '../src/node-runtime.mjs';
import { HotCodeLoader } from '../src/hot-code-loader.mjs';
import { RDFValidator } from '../src/rdf-validator.mjs';

class PolicyPack {
  constructor(name, version) {
    this.name = name;
    this.version = version;
    this.validator = new RDFValidator();
    this.rules = [];
  }

  registerRule(shapeName, rules, targetClass) {
    this.validator.registerShape(shapeName, rules, targetClass);
    this.rules.push({ shapeName, rules, targetClass });
    return this;
  }

  validate(triples) {
    return this.validator.validateGraph(triples, { shapes: this.rules.map(rule => rule.shapeName) });
  }

  bytes() {
    return Buffer.from(JSON.stringify({ name: this.name, version: this.version, rules: this.rules }));
  }
}

let server;
let baseUrl;
let currentBytes;

beforeAll(async () => {
  if (!globalThis.crypto?.subtle) globalThis.crypto = webcrypto;
  server = createServer((request, response) => {
    if (request.url !== '/fleet-policy.avm') {
      response.writeHead(404).end();
      return;
    }
    response.writeHead(200, { 'content-type': 'application/octet-stream' });
    response.end(currentBytes);
  });
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  const address = server.address();
  baseUrl = `http://127.0.0.1:${address.port}`;
});

afterAll(async () => {
  await new Promise((resolve, reject) => server.close(error => error ? reject(error) : resolve()));
});

describe('HotCodeLoader policy packs — real state transitions', () => {
  it('validates RDF state with a real policy and reloads its serialized bytes', async () => {
    const first = new PolicyPack('fleet-policy', 1).registerRule(
      'test:Vehicle',
      [{ property: 'test:id', required: true, datatype: 'xsd:string' }],
      'test:Vehicle',
    );
    const triples = [
      {
        subject: 'http://example.org/V-1',
        predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        value: 'test:Vehicle',
      },
      {
        subject: 'http://example.org/V-1',
        predicate: 'test:id',
        value: 'V-1',
        datatype: 'http://www.w3.org/2001/XMLSchema#string',
      },
    ];
    expect((await first.validate(triples)).valid).toBe(true);

    currentBytes = first.bytes();
    const runtime = new AtomVMNodeRuntime({ atomvmBinary: process.env.ATOMVM_BIN ?? process.execPath });
    const loader = new HotCodeLoader(runtime);
    const loaded = await loader.loadModule(`${baseUrl}/fleet-policy.avm`);
    expect(loaded.success).toBe(true);

    const second = new PolicyPack('fleet-policy', 2).registerRule(
      'test:Vehicle',
      [
        { property: 'test:id', required: true, datatype: 'xsd:string' },
        { property: 'test:status', required: true },
      ],
      'test:Vehicle',
    );
    currentBytes = second.bytes();
    const reloaded = await loader.reloadModule('fleet-policy');

    expect(reloaded.success).toBe(true);
    expect(reloaded.version).toBe(2);
    expect((await second.validate(triples)).valid).toBe(false);
    expect(loader.getActiveModules()[0].signature).not.toBe(loaded.signature);
  });
});
