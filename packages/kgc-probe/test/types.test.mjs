/**
 * @fileoverview Comprehensive tests for KGC Probe types and Zod schemas
 *
 * Tests cover:
 * - All Zod schemas (pass + fail cases)
 * - All factory functions
 * - Validation helpers
 * - Edge cases and boundary conditions
 *
 * Target: 80+ tests, 80%+ coverage
 */

import { describe, it, expect } from 'vitest';
import {
  // Constants
  SHA256_REGEX,
  UUID_V4_REGEX,
  SEMVER_REGEX,

  // Frontmatter Schemas
  SourceSchema,
  BoundsSchema,
  AuthorSchema,
  DiatasisViewSchema,
  FrontmatterSchema,

  // Block Metadata Schemas
  BlockTypeSchema,
  OutputFormatSchema,
  DeterminismLevelSchema,
  BlockMetadataSchema,
  QueryTypeSchema,
  ResultBoundsSchema,
  QueryMetadataSchema,
  ExtractionTypeSchema,
  ExtractMetadataSchema,
  RenderMetadataSchema,
  ProofTypeSchema,
  ProofMetadataSchema,

  // Receipt Schemas
  DecisionSchema,
  MerkleProofSchema,
  ReceiptSchema,

  // Dynamic Section Schema
  DynamicSectionSchema,

  // Error Schemas
  ErrorSeveritySchema,
  ErrorTypeSchema,
  ProbeErrorSchema,
  ProbeWarningSchema,

  // Report Schemas
  DomainResultSchema,
  ProbeReportSchema,

  // Factory Functions
  createFrontmatter,
  createBounds,
  createSource,
  createAuthor,
  createBlockMetadata,
  createQueryMetadata,
  createExtractMetadata,
  createRenderMetadata,
  createProofMetadata,
  createReceipt,
  createMerkleProof,
  createDynamicSection,
  createProbeError,
  createProbeWarning,
  createDomainResult,
  createProbeReport,
  createProbeConfig,

  // Validation Functions
  validateFrontmatter,
  validateReceipt,
  tryValidateFrontmatter,
  tryValidateReceipt
} from '../src/types.mjs';

// ============================================================================
// CONSTANTS & REGEX TESTS
// ============================================================================

describe('SHA256_REGEX', () => {
  it('should match valid 64-char lowercase hex', () => {
    const valid = 'a'.repeat(64);
    expect(SHA256_REGEX.test(valid)).toBe(true);
  });

  it('should match mixed lowercase hex', () => {
    const valid = 'a1b2c3d4e5f6'.repeat(5) + 'a1b2';
    expect(SHA256_REGEX.test(valid)).toBe(true);
  });

  it('should reject uppercase hex', () => {
    const invalid = 'A'.repeat(64);
    expect(SHA256_REGEX.test(invalid)).toBe(false);
  });

  it('should reject too short', () => {
    expect(SHA256_REGEX.test('a'.repeat(63))).toBe(false);
  });

  it('should reject too long', () => {
    expect(SHA256_REGEX.test('a'.repeat(65))).toBe(false);
  });

  it('should reject non-hex characters', () => {
    expect(SHA256_REGEX.test('g'.repeat(64))).toBe(false);
  });
});

describe('UUID_V4_REGEX', () => {
  it('should match valid UUID v4', () => {
    expect(UUID_V4_REGEX.test('550e8400-e29b-41d4-a716-446655440000')).toBe(true);
  });

  it('should match case-insensitive UUID', () => {
    expect(UUID_V4_REGEX.test('550E8400-E29B-41D4-A716-446655440000')).toBe(true);
  });

  it('should reject non-v4 UUID (wrong version)', () => {
    expect(UUID_V4_REGEX.test('550e8400-e29b-31d4-a716-446655440000')).toBe(false);
  });

  it('should reject malformed UUID', () => {
    expect(UUID_V4_REGEX.test('not-a-uuid')).toBe(false);
  });
});

describe('SEMVER_REGEX', () => {
  it('should match basic semver', () => {
    expect(SEMVER_REGEX.test('1.0.0')).toBe(true);
    expect(SEMVER_REGEX.test('0.0.1')).toBe(true);
    expect(SEMVER_REGEX.test('10.20.30')).toBe(true);
  });

  it('should match semver with prerelease', () => {
    expect(SEMVER_REGEX.test('1.0.0-alpha')).toBe(true);
    expect(SEMVER_REGEX.test('1.0.0-beta.1')).toBe(true);
  });

  it('should match semver with build metadata', () => {
    expect(SEMVER_REGEX.test('1.0.0+build.123')).toBe(true);
  });

  it('should match semver with prerelease and build', () => {
    expect(SEMVER_REGEX.test('1.0.0-alpha+build')).toBe(true);
  });

  it('should reject invalid semver', () => {
    expect(SEMVER_REGEX.test('1.0')).toBe(false);
    expect(SEMVER_REGEX.test('v1.0.0')).toBe(false);
  });
});

// ============================================================================
// BOUNDS SCHEMA TESTS
// ============================================================================

describe('BoundsSchema', () => {
  it('should parse valid bounds', () => {
    const result = BoundsSchema.parse({
      maxQueries: 100,
      maxRuntime: 5000,
      maxFileScans: 50
    });
    expect(result.maxQueries).toBe(100);
    expect(result.maxRuntime).toBe(5000);
    expect(result.maxFileScans).toBe(50);
  });

  it('should accept minimum values', () => {
    const result = BoundsSchema.parse({
      maxQueries: 1,
      maxRuntime: 100,
      maxFileScans: 1
    });
    expect(result).toBeDefined();
  });

  it('should accept maximum values', () => {
    const result = BoundsSchema.parse({
      maxQueries: 10000,
      maxRuntime: 60000,
      maxFileScans: 1000
    });
    expect(result).toBeDefined();
  });

  it('should reject maxQueries below 1', () => {
    expect(() => BoundsSchema.parse({
      maxQueries: 0,
      maxRuntime: 5000,
      maxFileScans: 50
    })).toThrow();
  });

  it('should reject maxQueries above 10000', () => {
    expect(() => BoundsSchema.parse({
      maxQueries: 10001,
      maxRuntime: 5000,
      maxFileScans: 50
    })).toThrow();
  });

  it('should reject maxRuntime below 100', () => {
    expect(() => BoundsSchema.parse({
      maxQueries: 100,
      maxRuntime: 99,
      maxFileScans: 50
    })).toThrow();
  });

  it('should reject maxRuntime above 60000', () => {
    expect(() => BoundsSchema.parse({
      maxQueries: 100,
      maxRuntime: 60001,
      maxFileScans: 50
    })).toThrow();
  });
});

// ============================================================================
// SOURCE SCHEMA TESTS
// ============================================================================

describe('SourceSchema', () => {
  it('should parse valid source', () => {
    const result = SourceSchema.parse({
      path: 'src/index.mjs',
      lineStart: 1,
      lineEnd: 100,
      hash: 'a'.repeat(64)
    });
    expect(result.path).toBe('src/index.mjs');
    expect(result.lineStart).toBe(1);
    expect(result.lineEnd).toBe(100);
  });

  it('should accept lineEnd equal to lineStart', () => {
    const result = SourceSchema.parse({
      path: 'src/index.mjs',
      lineStart: 50,
      lineEnd: 50,
      hash: 'a'.repeat(64)
    });
    expect(result).toBeDefined();
  });

  it('should reject lineEnd less than lineStart', () => {
    expect(() => SourceSchema.parse({
      path: 'src/index.mjs',
      lineStart: 100,
      lineEnd: 50,
      hash: 'a'.repeat(64)
    })).toThrow(/lineEnd must be >= lineStart/);
  });

  it('should reject path with directory traversal', () => {
    expect(() => SourceSchema.parse({
      path: '../etc/passwd',
      lineStart: 1,
      lineEnd: 100,
      hash: 'a'.repeat(64)
    })).toThrow(/directory traversal/);
  });

  it('should reject invalid hash', () => {
    expect(() => SourceSchema.parse({
      path: 'src/index.mjs',
      lineStart: 1,
      lineEnd: 100,
      hash: 'invalid'
    })).toThrow();
  });
});

// ============================================================================
// AUTHOR SCHEMA TESTS
// ============================================================================

describe('AuthorSchema', () => {
  it('should parse author with name only', () => {
    const result = AuthorSchema.parse({ name: 'John Doe' });
    expect(result.name).toBe('John Doe');
    expect(result.role).toBeUndefined();
  });

  it('should parse author with name and role', () => {
    const result = AuthorSchema.parse({ name: 'John Doe', role: 'maintainer' });
    expect(result.name).toBe('John Doe');
    expect(result.role).toBe('maintainer');
  });

  it('should reject empty name', () => {
    expect(() => AuthorSchema.parse({ name: '' })).toThrow();
  });
});

// ============================================================================
// DIATASIS VIEW SCHEMA TESTS
// ============================================================================

describe('DiatasisViewSchema', () => {
  it('should accept all valid view types', () => {
    expect(DiatasisViewSchema.parse('tutorial')).toBe('tutorial');
    expect(DiatasisViewSchema.parse('how-to')).toBe('how-to');
    expect(DiatasisViewSchema.parse('reference')).toBe('reference');
    expect(DiatasisViewSchema.parse('explanation')).toBe('explanation');
  });

  it('should reject invalid view type', () => {
    expect(() => DiatasisViewSchema.parse('guide')).toThrow();
  });
});

// ============================================================================
// FRONTMATTER SCHEMA TESTS
// ============================================================================

describe('FrontmatterSchema', () => {
  const validFrontmatter = () => ({
    o_hash: 'a'.repeat(64),
    policy_id: '550e8400-e29b-41d4-a716-446655440000',
    receipts: [],
    bounds: { maxQueries: 100, maxRuntime: 5000, maxFileScans: 50 },
    views: ['reference'],
    sources: [],
    version: '1.0.0',
    createdAt: '2025-12-26T10:00:00Z',
    lastProved: '2025-12-26T14:00:00Z'
  });

  it('should parse valid frontmatter', () => {
    const result = FrontmatterSchema.parse(validFrontmatter());
    expect(result.o_hash).toBe('a'.repeat(64));
    expect(result.version).toBe('1.0.0');
  });

  it('should accept optional tags', () => {
    const fm = { ...validFrontmatter(), tags: ['api', 'user'] };
    const result = FrontmatterSchema.parse(fm);
    expect(result.tags).toEqual(['api', 'user']);
  });

  it('should accept optional authors', () => {
    const fm = { ...validFrontmatter(), authors: [{ name: 'John' }] };
    const result = FrontmatterSchema.parse(fm);
    expect(result.authors?.[0].name).toBe('John');
  });

  it('should reject invalid o_hash', () => {
    const fm = { ...validFrontmatter(), o_hash: 'invalid' };
    expect(() => FrontmatterSchema.parse(fm)).toThrow();
  });

  it('should reject invalid policy_id', () => {
    const fm = { ...validFrontmatter(), policy_id: 'not-a-uuid' };
    expect(() => FrontmatterSchema.parse(fm)).toThrow();
  });

  it('should reject lastProved before createdAt', () => {
    const fm = {
      ...validFrontmatter(),
      createdAt: '2025-12-26T14:00:00Z',
      lastProved: '2025-12-26T10:00:00Z'
    };
    expect(() => FrontmatterSchema.parse(fm)).toThrow(/lastProved must be >= createdAt/);
  });

  it('should reject more than 1000 receipts', () => {
    const fm = { ...validFrontmatter(), receipts: Array(1001).fill('a'.repeat(64)) };
    expect(() => FrontmatterSchema.parse(fm)).toThrow();
  });

  it('should reject empty views', () => {
    const fm = { ...validFrontmatter(), views: [] };
    expect(() => FrontmatterSchema.parse(fm)).toThrow();
  });

  it('should reject invalid semver', () => {
    const fm = { ...validFrontmatter(), version: 'not-semver' };
    expect(() => FrontmatterSchema.parse(fm)).toThrow();
  });
});

// ============================================================================
// BLOCK METADATA SCHEMA TESTS
// ============================================================================

describe('BlockMetadataSchema', () => {
  it('should parse valid block metadata', () => {
    const result = BlockMetadataSchema.parse({
      receiptId: 'a'.repeat(64),
      expectedOutputFormat: 'json',
      determinismLevel: 'strict'
    });
    expect(result.receiptId).toBe('a'.repeat(64));
    expect(result.expectedOutputFormat).toBe('json');
    expect(result.determinismLevel).toBe('strict');
  });

  it('should accept all output formats', () => {
    for (const format of ['json', 'markdown', 'text']) {
      const result = BlockMetadataSchema.parse({
        receiptId: 'a'.repeat(64),
        expectedOutputFormat: format,
        determinismLevel: 'strict'
      });
      expect(result.expectedOutputFormat).toBe(format);
    }
  });

  it('should accept all determinism levels', () => {
    for (const level of ['strict', 'lenient', 'best-effort']) {
      const result = BlockMetadataSchema.parse({
        receiptId: 'a'.repeat(64),
        expectedOutputFormat: 'json',
        determinismLevel: level
      });
      expect(result.determinismLevel).toBe(level);
    }
  });

  it('should reject invalid receiptId', () => {
    expect(() => BlockMetadataSchema.parse({
      receiptId: 'short',
      expectedOutputFormat: 'json',
      determinismLevel: 'strict'
    })).toThrow();
  });
});

// ============================================================================
// QUERY METADATA SCHEMA TESTS
// ============================================================================

describe('QueryMetadataSchema', () => {
  it('should parse valid query metadata', () => {
    const result = QueryMetadataSchema.parse({
      receiptId: 'a'.repeat(64),
      expectedOutputFormat: 'json',
      determinismLevel: 'strict',
      queryType: 'sparql',
      resultBounds: { minResults: 0, maxResults: 1000 },
      timeout: 5000
    });
    expect(result.queryType).toBe('sparql');
    expect(result.timeout).toBe(5000);
  });

  it('should accept all query types', () => {
    for (const qtype of ['sparql', 'n3', 'shacl']) {
      const result = QueryMetadataSchema.parse({
        receiptId: 'a'.repeat(64),
        expectedOutputFormat: 'json',
        determinismLevel: 'strict',
        queryType: qtype,
        resultBounds: { minResults: 0, maxResults: 100 },
        timeout: 1000
      });
      expect(result.queryType).toBe(qtype);
    }
  });

  it('should reject timeout below 100ms', () => {
    expect(() => QueryMetadataSchema.parse({
      receiptId: 'a'.repeat(64),
      expectedOutputFormat: 'json',
      determinismLevel: 'strict',
      queryType: 'sparql',
      resultBounds: { minResults: 0, maxResults: 100 },
      timeout: 50
    })).toThrow();
  });

  it('should reject timeout above 60000ms', () => {
    expect(() => QueryMetadataSchema.parse({
      receiptId: 'a'.repeat(64),
      expectedOutputFormat: 'json',
      determinismLevel: 'strict',
      queryType: 'sparql',
      resultBounds: { minResults: 0, maxResults: 100 },
      timeout: 60001
    })).toThrow();
  });
});

// ============================================================================
// EXTRACT METADATA SCHEMA TESTS
// ============================================================================

describe('ExtractMetadataSchema', () => {
  it('should parse valid extract metadata', () => {
    const result = ExtractMetadataSchema.parse({
      receiptId: 'a'.repeat(64),
      expectedOutputFormat: 'json',
      determinismLevel: 'lenient',
      extractionType: 'exports',
      fileGlobs: ['src/**/*.mjs']
    });
    expect(result.extractionType).toBe('exports');
    expect(result.includePrivate).toBe(false); // default
    expect(result.includeDocstrings).toBe(true); // default
  });

  it('should accept all extraction types', () => {
    for (const etype of ['exports', 'functions', 'classes', 'types', 'all']) {
      const result = ExtractMetadataSchema.parse({
        receiptId: 'a'.repeat(64),
        expectedOutputFormat: 'json',
        determinismLevel: 'lenient',
        extractionType: etype,
        fileGlobs: ['src/**/*']
      });
      expect(result.extractionType).toBe(etype);
    }
  });

  it('should reject empty fileGlobs', () => {
    expect(() => ExtractMetadataSchema.parse({
      receiptId: 'a'.repeat(64),
      expectedOutputFormat: 'json',
      determinismLevel: 'lenient',
      extractionType: 'exports',
      fileGlobs: []
    })).toThrow();
  });
});

// ============================================================================
// RENDER METADATA SCHEMA TESTS
// ============================================================================

describe('RenderMetadataSchema', () => {
  it('should parse valid render metadata', () => {
    const result = RenderMetadataSchema.parse({
      receiptId: 'a'.repeat(64),
      expectedOutputFormat: 'markdown',
      determinismLevel: 'strict',
      templateName: 'api-reference',
      sectionTitle: 'API Functions'
    });
    expect(result.templateName).toBe('api-reference');
    expect(result.includeTableOfContents).toBe(false); // default
  });

  it('should reject empty templateName', () => {
    expect(() => RenderMetadataSchema.parse({
      receiptId: 'a'.repeat(64),
      expectedOutputFormat: 'markdown',
      determinismLevel: 'strict',
      templateName: '',
      sectionTitle: 'API Functions'
    })).toThrow();
  });
});

// ============================================================================
// PROOF METADATA SCHEMA TESTS
// ============================================================================

describe('ProofMetadataSchema', () => {
  it('should parse valid proof metadata', () => {
    const result = ProofMetadataSchema.parse({
      receiptId: 'a'.repeat(64),
      expectedOutputFormat: 'json',
      determinismLevel: 'strict',
      proofType: 'merkle'
    });
    expect(result.proofType).toBe('merkle');
    expect(result.verifyChain).toBe(true); // default
    expect(result.validateSignatures).toBe(false); // default
  });

  it('should accept all proof types', () => {
    for (const ptype of ['merkle', 'hash-chain', 'single']) {
      const result = ProofMetadataSchema.parse({
        receiptId: 'a'.repeat(64),
        expectedOutputFormat: 'json',
        determinismLevel: 'strict',
        proofType: ptype
      });
      expect(result.proofType).toBe(ptype);
    }
  });
});

// ============================================================================
// RECEIPT SCHEMA TESTS
// ============================================================================

describe('ReceiptSchema', () => {
  const validReceipt = () => ({
    id: 'a'.repeat(64),
    timestamp: '2025-12-26T13:45:00Z',
    o_hash: 'b'.repeat(64),
    block_type: 'kgc:query',
    input_hash: 'c'.repeat(64),
    output_hash: 'd'.repeat(64),
    decision: 'ADMIT'
  });

  it('should parse valid receipt', () => {
    const result = ReceiptSchema.parse(validReceipt());
    expect(result.id).toBe('a'.repeat(64));
    expect(result.decision).toBe('ADMIT');
  });

  it('should accept all block types', () => {
    for (const btype of ['kgc:query', 'kgc:proof', 'kgc:extract', 'kgc:render']) {
      const result = ReceiptSchema.parse({ ...validReceipt(), block_type: btype });
      expect(result.block_type).toBe(btype);
    }
  });

  it('should accept all decisions', () => {
    for (const decision of ['ADMIT', 'REJECT', 'PARTIAL']) {
      const result = ReceiptSchema.parse({ ...validReceipt(), decision });
      expect(result.decision).toBe(decision);
    }
  });

  it('should accept optional dependencies', () => {
    const result = ReceiptSchema.parse({
      ...validReceipt(),
      dependencies: ['e'.repeat(64), 'f'.repeat(64)]
    });
    expect(result.dependencies).toHaveLength(2);
  });

  it('should accept optional merkle_proof', () => {
    const result = ReceiptSchema.parse({
      ...validReceipt(),
      merkle_proof: {
        siblings: ['e'.repeat(64)],
        root: 'f'.repeat(64),
        index: 0,
        totalLeaves: 2
      }
    });
    expect(result.merkle_proof?.root).toBe('f'.repeat(64));
  });

  it('should reject invalid id hash', () => {
    expect(() => ReceiptSchema.parse({ ...validReceipt(), id: 'short' })).toThrow();
  });
});

// ============================================================================
// MERKLE PROOF SCHEMA TESTS
// ============================================================================

describe('MerkleProofSchema', () => {
  it('should parse valid merkle proof', () => {
    const result = MerkleProofSchema.parse({
      siblings: ['a'.repeat(64), 'b'.repeat(64)],
      root: 'c'.repeat(64),
      index: 1,
      totalLeaves: 4
    });
    expect(result.siblings).toHaveLength(2);
    expect(result.index).toBe(1);
    expect(result.totalLeaves).toBe(4);
  });

  it('should accept empty siblings', () => {
    const result = MerkleProofSchema.parse({
      siblings: [],
      root: 'a'.repeat(64),
      index: 0,
      totalLeaves: 1
    });
    expect(result.siblings).toHaveLength(0);
  });

  it('should reject negative index', () => {
    expect(() => MerkleProofSchema.parse({
      siblings: [],
      root: 'a'.repeat(64),
      index: -1,
      totalLeaves: 1
    })).toThrow();
  });

  it('should reject totalLeaves below 1', () => {
    expect(() => MerkleProofSchema.parse({
      siblings: [],
      root: 'a'.repeat(64),
      index: 0,
      totalLeaves: 0
    })).toThrow();
  });
});

// ============================================================================
// DYNAMIC SECTION SCHEMA TESTS
// ============================================================================

describe('DynamicSectionSchema', () => {
  it('should parse valid dynamic section', () => {
    const result = DynamicSectionSchema.parse({
      name: 'api-functions',
      receiptId: 'a'.repeat(64),
      lineStart: 100,
      lineEnd: 200
    });
    expect(result.name).toBe('api-functions');
    expect(result.lineStart).toBe(100);
  });

  it('should accept optional contentHash', () => {
    const result = DynamicSectionSchema.parse({
      name: 'section',
      receiptId: 'a'.repeat(64),
      lineStart: 1,
      lineEnd: 50,
      contentHash: 'b'.repeat(64)
    });
    expect(result.contentHash).toBe('b'.repeat(64));
  });

  it('should reject empty name', () => {
    expect(() => DynamicSectionSchema.parse({
      name: '',
      receiptId: 'a'.repeat(64),
      lineStart: 1,
      lineEnd: 50
    })).toThrow();
  });
});

// ============================================================================
// ERROR SCHEMA TESTS
// ============================================================================

describe('ProbeErrorSchema', () => {
  it('should parse valid error', () => {
    const result = ProbeErrorSchema.parse({
      type: 'InvalidFrontmatter',
      severity: 'error',
      message: 'o_hash is too short'
    });
    expect(result.type).toBe('InvalidFrontmatter');
    expect(result.severity).toBe('error');
  });

  it('should accept all error types', () => {
    const errorTypes = [
      'InvalidFrontmatter', 'MissingReceipt', 'MismatchedHash', 'BoundsExceeded',
      'NonDeterministic', 'InvalidBlockStructure', 'CyclicDependency', 'MissingDependency',
      'TimestampOrderViolation', 'UniverseHashMismatch', 'OrphanedSection', 'UnmappedReceipt',
      'BrokenLink', 'HeadingHierarchyViolation', 'DuplicateHeading', 'SchemaViolation'
    ];
    for (const etype of errorTypes) {
      const result = ProbeErrorSchema.parse({
        type: etype,
        severity: 'error',
        message: 'Test message'
      });
      expect(result.type).toBe(etype);
    }
  });

  it('should accept optional location', () => {
    const result = ProbeErrorSchema.parse({
      type: 'InvalidFrontmatter',
      severity: 'error',
      message: 'Test',
      location: { file: 'doc.kgcmd', line: 10, field: 'o_hash' }
    });
    expect(result.location?.line).toBe(10);
  });

  it('should accept optional remediation', () => {
    const result = ProbeErrorSchema.parse({
      type: 'InvalidFrontmatter',
      severity: 'error',
      message: 'Test',
      remediation: 'Use sha256sum to generate hash'
    });
    expect(result.remediation).toBe('Use sha256sum to generate hash');
  });
});

describe('ProbeWarningSchema', () => {
  it('should parse valid warning with severity warning', () => {
    const result = ProbeWarningSchema.parse({
      type: 'BoundsExceeded',
      severity: 'warning',
      message: 'Resource utilization high'
    });
    expect(result.severity).toBe('warning');
  });

  it('should reject non-warning severity', () => {
    expect(() => ProbeWarningSchema.parse({
      type: 'BoundsExceeded',
      severity: 'error',
      message: 'Test'
    })).toThrow();
  });
});

// ============================================================================
// FACTORY FUNCTION TESTS
// ============================================================================

describe('createFrontmatter', () => {
  it('should create frontmatter with defaults', () => {
    const fm = createFrontmatter({});
    expect(fm.o_hash).toBe('0'.repeat(64));
    expect(fm.version).toBe('1.0.0');
    expect(fm.views).toEqual(['reference']);
  });

  it('should override defaults with provided values', () => {
    const fm = createFrontmatter({
      o_hash: 'a'.repeat(64),
      version: '2.0.0'
    });
    expect(fm.o_hash).toBe('a'.repeat(64));
    expect(fm.version).toBe('2.0.0');
  });

  it('should set createdAt and lastProved to now', () => {
    const before = new Date();
    const fm = createFrontmatter({});
    const after = new Date();

    const created = new Date(fm.createdAt);
    expect(created >= before).toBe(true);
    expect(created <= after).toBe(true);
  });
});

describe('createBounds', () => {
  it('should create bounds with defaults', () => {
    const bounds = createBounds();
    expect(bounds.maxQueries).toBe(100);
    expect(bounds.maxRuntime).toBe(5000);
    expect(bounds.maxFileScans).toBe(50);
  });

  it('should override defaults', () => {
    const bounds = createBounds({ maxQueries: 200 });
    expect(bounds.maxQueries).toBe(200);
    expect(bounds.maxRuntime).toBe(5000); // default
  });
});

describe('createSource', () => {
  it('should create source with defaults', () => {
    const source = createSource({});
    expect(source.path).toBe('src/index.mjs');
    expect(source.lineStart).toBe(1);
    expect(source.lineEnd).toBe(100);
  });

  it('should override defaults', () => {
    const source = createSource({ path: 'lib/util.mjs', lineStart: 10 });
    expect(source.path).toBe('lib/util.mjs');
    expect(source.lineStart).toBe(10);
  });
});

describe('createAuthor', () => {
  it('should create author with defaults', () => {
    const author = createAuthor({});
    expect(author.name).toBe('Anonymous');
  });

  it('should override name', () => {
    const author = createAuthor({ name: 'Alice', role: 'reviewer' });
    expect(author.name).toBe('Alice');
    expect(author.role).toBe('reviewer');
  });
});

describe('createBlockMetadata', () => {
  it('should create block metadata with defaults', () => {
    const meta = createBlockMetadata({});
    expect(meta.receiptId).toBe('0'.repeat(64));
    expect(meta.expectedOutputFormat).toBe('json');
    expect(meta.determinismLevel).toBe('strict');
  });
});

describe('createQueryMetadata', () => {
  it('should create query metadata with defaults', () => {
    const meta = createQueryMetadata({});
    expect(meta.queryType).toBe('sparql');
    expect(meta.timeout).toBe(5000);
    expect(meta.resultBounds.minResults).toBe(0);
    expect(meta.resultBounds.maxResults).toBe(1000);
  });
});

describe('createExtractMetadata', () => {
  it('should create extract metadata with defaults', () => {
    const meta = createExtractMetadata({});
    expect(meta.extractionType).toBe('exports');
    expect(meta.fileGlobs).toEqual(['src/**/*.mjs']);
    expect(meta.includePrivate).toBe(false);
    expect(meta.includeDocstrings).toBe(true);
  });
});

describe('createRenderMetadata', () => {
  it('should create render metadata with defaults', () => {
    const meta = createRenderMetadata({});
    expect(meta.templateName).toBe('api-reference');
    expect(meta.sectionTitle).toBe('API Reference');
    expect(meta.includeTableOfContents).toBe(true);
  });
});

describe('createProofMetadata', () => {
  it('should create proof metadata with defaults', () => {
    const meta = createProofMetadata({});
    expect(meta.proofType).toBe('merkle');
    expect(meta.verifyChain).toBe(true);
    expect(meta.validateSignatures).toBe(false);
  });
});

describe('createReceipt', () => {
  it('should create receipt with defaults', () => {
    const receipt = createReceipt({});
    expect(receipt.id).toBe('0'.repeat(64));
    expect(receipt.block_type).toBe('kgc:query');
    expect(receipt.decision).toBe('ADMIT');
  });

  it('should override defaults', () => {
    const receipt = createReceipt({
      id: 'a'.repeat(64),
      block_type: 'kgc:extract',
      decision: 'REJECT'
    });
    expect(receipt.id).toBe('a'.repeat(64));
    expect(receipt.block_type).toBe('kgc:extract');
    expect(receipt.decision).toBe('REJECT');
  });
});

describe('createMerkleProof', () => {
  it('should create merkle proof with defaults', () => {
    const proof = createMerkleProof({});
    expect(proof.siblings).toEqual([]);
    expect(proof.root).toBe('0'.repeat(64));
    expect(proof.index).toBe(0);
    expect(proof.totalLeaves).toBe(1);
  });
});

describe('createDynamicSection', () => {
  it('should create dynamic section with defaults', () => {
    const section = createDynamicSection({});
    expect(section.name).toBe('section');
    expect(section.receiptId).toBe('0'.repeat(64));
  });
});

describe('createProbeError', () => {
  it('should create probe error with defaults', () => {
    const error = createProbeError({});
    expect(error.type).toBe('SchemaViolation');
    expect(error.severity).toBe('error');
    expect(error.message).toBe('Validation failed');
  });

  it('should override defaults', () => {
    const error = createProbeError({
      type: 'MissingReceipt',
      message: 'Receipt not found'
    });
    expect(error.type).toBe('MissingReceipt');
    expect(error.message).toBe('Receipt not found');
  });
});

describe('createProbeWarning', () => {
  it('should create probe warning with defaults', () => {
    const warning = createProbeWarning({});
    expect(warning.type).toBe('BoundsExceeded');
    expect(warning.severity).toBe('warning');
  });
});

describe('createDomainResult', () => {
  it('should create domain result with defaults', () => {
    const result = createDomainResult({});
    expect(result.domain).toBe('frontmatter');
    expect(result.passed).toBe(true);
    expect(result.checks).toBe(10);
  });
});

describe('createProbeReport', () => {
  it('should create probe report with defaults', () => {
    const report = createProbeReport({});
    expect(report.version).toBe('1.0');
    expect(report.status).toBe('PASS');
    expect(report.domains).toEqual([]);
    expect(report.summary.totalChecks).toBe(0);
  });
});

describe('createProbeConfig', () => {
  it('should create probe config with defaults', () => {
    const config = createProbeConfig({});
    expect(config.universe_id).toBe('default-universe');
    expect(config.distributed).toBe(false);
    expect(config.persist).toBe(true);
  });

  it('should override defaults', () => {
    const config = createProbeConfig({
      universe_id: 'my-universe',
      distributed: true
    });
    expect(config.universe_id).toBe('my-universe');
    expect(config.distributed).toBe(true);
  });
});

// ============================================================================
// VALIDATION FUNCTION TESTS
// ============================================================================

describe('validateFrontmatter', () => {
  it('should return validated frontmatter', () => {
    const fm = createFrontmatter({});
    const result = validateFrontmatter(fm);
    expect(result.o_hash).toBe('0'.repeat(64));
  });

  it('should throw on invalid data', () => {
    expect(() => validateFrontmatter({ invalid: true })).toThrow();
  });
});

describe('validateReceipt', () => {
  it('should return validated receipt', () => {
    const receipt = createReceipt({});
    const result = validateReceipt(receipt);
    expect(result.decision).toBe('ADMIT');
  });

  it('should throw on invalid data', () => {
    expect(() => validateReceipt({ invalid: true })).toThrow();
  });
});

describe('tryValidateFrontmatter', () => {
  it('should return validated frontmatter on success', () => {
    const fm = createFrontmatter({});
    const result = tryValidateFrontmatter(fm);
    expect(result).not.toBeNull();
    expect(result?.version).toBe('1.0.0');
  });

  it('should return null on invalid data', () => {
    const result = tryValidateFrontmatter({ invalid: true });
    expect(result).toBeNull();
  });
});

describe('tryValidateReceipt', () => {
  it('should return validated receipt on success', () => {
    const receipt = createReceipt({});
    const result = tryValidateReceipt(receipt);
    expect(result).not.toBeNull();
    expect(result?.decision).toBe('ADMIT');
  });

  it('should return null on invalid data', () => {
    const result = tryValidateReceipt({ invalid: true });
    expect(result).toBeNull();
  });
});

// ============================================================================
// BLOCK TYPE SCHEMA TESTS
// ============================================================================

describe('BlockTypeSchema', () => {
  it('should accept all valid block types', () => {
    const types = ['kgc:query', 'kgc:proof', 'kgc:extract', 'kgc:render'];
    for (const btype of types) {
      expect(BlockTypeSchema.parse(btype)).toBe(btype);
    }
  });

  it('should reject invalid block type', () => {
    expect(() => BlockTypeSchema.parse('kgc:invalid')).toThrow();
  });
});

describe('DecisionSchema', () => {
  it('should accept all valid decisions', () => {
    for (const decision of ['ADMIT', 'REJECT', 'PARTIAL']) {
      expect(DecisionSchema.parse(decision)).toBe(decision);
    }
  });

  it('should reject invalid decision', () => {
    expect(() => DecisionSchema.parse('DENY')).toThrow();
  });
});

// ============================================================================
// DOMAIN RESULT SCHEMA TESTS
// ============================================================================

describe('DomainResultSchema', () => {
  it('should parse valid domain result', () => {
    const result = DomainResultSchema.parse({
      domain: 'frontmatter',
      passed: true,
      checks: 10,
      errors: [],
      warnings: []
    });
    expect(result.domain).toBe('frontmatter');
    expect(result.passed).toBe(true);
  });

  it('should accept errors array', () => {
    const result = DomainResultSchema.parse({
      domain: 'receipts',
      passed: false,
      checks: 5,
      errors: [{
        type: 'MissingReceipt',
        severity: 'error',
        message: 'Receipt not found'
      }],
      warnings: []
    });
    expect(result.errors).toHaveLength(1);
    expect(result.errors[0].type).toBe('MissingReceipt');
  });
});

// ============================================================================
// PROBE REPORT SCHEMA TESTS
// ============================================================================

describe('ProbeReportSchema', () => {
  it('should parse valid probe report', () => {
    const report = createProbeReport({
      documentPath: 'test.kgcmd',
      status: 'PASS',
      domains: [
        { domain: 'frontmatter', passed: true, checks: 10, errors: [], warnings: [] }
      ],
      summary: {
        totalChecks: 10,
        totalPassed: 10,
        totalErrors: 0,
        totalWarnings: 0,
        coverage: 1.0
      }
    });

    const result = ProbeReportSchema.parse(report);
    expect(result.version).toBe('1.0');
    expect(result.status).toBe('PASS');
    expect(result.domains).toHaveLength(1);
  });

  it('should accept all status values', () => {
    for (const status of ['PASS', 'FAIL', 'PARTIAL']) {
      const report = createProbeReport({ status });
      expect(report.status).toBe(status);
    }
  });
});
