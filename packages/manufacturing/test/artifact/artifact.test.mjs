/**
 * @file Artifact Module Tests
 * @description Chicago TDD — behavior verification for artifact creation and receipts
 */

import { describe, it, expect } from 'vitest';
import { Artifact, createArtifact, ARTIFACT_KINDS } from '../../src/artifact/index.mjs';

describe('ARTIFACT_KINDS', () => {
  it('contains exactly 15 artifact kinds', () => {
    expect(ARTIFACT_KINDS).toHaveLength(15);
  });

  it('includes powl, sparql, shacl, mcp-tool', () => {
    expect(ARTIFACT_KINDS).toContain('powl');
    expect(ARTIFACT_KINDS).toContain('sparql');
    expect(ARTIFACT_KINDS).toContain('shacl');
    expect(ARTIFACT_KINDS).toContain('mcp-tool');
  });

  it('includes documentation and code kinds', () => {
    expect(ARTIFACT_KINDS).toContain('documentation');
    expect(ARTIFACT_KINDS).toContain('python');
    expect(ARTIFACT_KINDS).toContain('typescript');
    expect(ARTIFACT_KINDS).toContain('go');
    expect(ARTIFACT_KINDS).toContain('rust');
  });
});

describe('Artifact', () => {
  it('stores type and content', () => {
    const a = new Artifact('sparql', 'SELECT * WHERE { ?s ?p ?o }');
    expect(a.type).toBe('sparql');
    expect(a.content).toBe('SELECT * WHERE { ?s ?p ?o }');
  });

  it('computes a receipt hash string', () => {
    const a = new Artifact('powl', 'workflow content');
    expect(typeof a.receipt).toBe('string');
    expect(a.receipt.length).toBeGreaterThan(0);
  });

  it('same content produces same receipt (deterministic)', () => {
    const a1 = new Artifact('python', 'print("hello")');
    const a2 = new Artifact('python', 'print("hello")');
    expect(a1.receipt).toBe(a2.receipt);
  });

  it('different content produces different receipt', () => {
    const a1 = new Artifact('python', 'print("hello")');
    const a2 = new Artifact('python', 'print("world")');
    expect(a1.receipt).not.toBe(a2.receipt);
  });

  it('records a timestamp', () => {
    const a = new Artifact('yaml', 'key: value');
    expect(typeof a.timestamp).toBe('string');
    expect(a.timestamp.length).toBeGreaterThan(0);
  });

  it('accepts object content (serializes to JSON for receipt)', () => {
    const a = new Artifact('json', { name: 'test', value: 42 });
    expect(a.receipt).toBeDefined();
    expect(a.content).toEqual({ name: 'test', value: 42 });
  });

  it('throws for unknown artifact kind', () => {
    expect(() => new Artifact('unknown-kind', 'content')).toThrow();
  });

  it('error message includes the unknown kind name', () => {
    expect(() => new Artifact('not-a-kind', 'x')).toThrow('not-a-kind');
  });

  it('toJSON includes type, receipt, and timestamp', () => {
    const a = new Artifact('ttl', '@prefix ex: <http://example.org/> .');
    const json = a.toJSON();
    expect(json.type).toBe('ttl');
    expect(json.receipt).toBe(a.receipt);
    expect(json.timestamp).toBe(a.timestamp);
    expect(json.contentLength).toBeGreaterThan(0);
  });

  it('toJSON does not include raw content (content is stored separately)', () => {
    const a = new Artifact('rust', 'fn main() {}');
    const json = a.toJSON();
    expect(json.content).toBeUndefined();
  });
});

describe('createArtifact', () => {
  it('creates an Artifact instance', () => {
    const a = createArtifact('documentation', '# Title');
    expect(a).toBeInstanceOf(Artifact);
  });

  it('passes type and content through', () => {
    const a = createArtifact('typescript', 'export const x = 1;');
    expect(a.type).toBe('typescript');
    expect(a.content).toBe('export const x = 1;');
  });

  it('throws for unknown kind', () => {
    expect(() => createArtifact('invalid', 'x')).toThrow();
  });

  it('receipt is hex string (sha256 or blake3 format)', () => {
    const a = createArtifact('java', 'public class Foo {}');
    // SHA-256 is 64 hex chars; BLAKE3 is also 64 hex chars
    expect(a.receipt).toMatch(/^[0-9a-f]{64}$/);
  });
});
