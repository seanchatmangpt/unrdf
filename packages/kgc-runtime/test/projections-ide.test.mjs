/**
 * Tests for IDE Metadata Projections (Π_ide)
 */

import { describe, it, expect } from 'vitest';
import {
  projectFunctionToHover,
  projectSchemaToCompletions,
  projectToDefinition,
  projectToDiagnostic,
  projectToSymbol,
  projectToCodeAction,
  projectToCodeLens,
  projectToSignatureHelp,
  projectToSemanticTokens,
  IDEProjectionSchema,
} from '../src/projections-ide.mjs';

describe('Π_ide - IDE Metadata Projections', () => {
  describe('projectFunctionToHover', () => {
    it('should project function to LSP hover information', () => {
      const fnInfo = {
        name: 'calculateSum',
        signature: 'function calculateSum(a: number, b: number): number',
        documentation: 'Calculates the sum of two numbers',
        parameters: [
          { name: 'a', type: 'number', description: 'First number' },
          { name: 'b', type: 'number', description: 'Second number' },
        ],
        returns: { type: 'number', description: 'The sum' },
      };

      const projection = projectFunctionToHover(fnInfo);

      expect(projection.type).toBe('ide');
      expect(projection.format).toBe('hover');
      expect(projection.content.kind).toBe('markdown');
      expect(projection.content.value).toContain('calculateSum');
      expect(projection.content.value).toContain('@param');
      expect(projection.content.value).toContain('@returns');

      // Validate schema
      IDEProjectionSchema.parse(projection);
    });
  });

  describe('projectSchemaToCompletions', () => {
    it('should project schema to LSP completions', () => {
      const fields = {
        name: { type: 'string', description: 'User name' },
        age: { type: 'number', description: 'User age' },
        email: { type: 'string', description: 'User email address' },
      };

      const projection = projectSchemaToCompletions('UserSchema', fields);

      expect(projection.type).toBe('ide');
      expect(projection.format).toBe('completion');
      expect(projection.content.items).toHaveLength(3);
      expect(projection.content.items[0].label).toBe('name');
      expect(projection.content.items[0].kind).toBe(10); // Property
      expect(projection.content.isIncomplete).toBe(false);
    });
  });

  describe('projectToDefinition', () => {
    it('should project to LSP definition location', () => {
      const location = {
        uri: 'file:///home/user/project/src/main.mjs',
        range: {
          start: { line: 10, character: 5 },
          end: { line: 10, character: 20 },
        },
      };

      const projection = projectToDefinition(location);

      expect(projection.type).toBe('ide');
      expect(projection.format).toBe('definition');
      expect(projection.uri).toBe(location.uri);
      expect(projection.range).toEqual(location.range);
      expect(projection.content.uri).toBe(location.uri);
    });
  });

  describe('projectToDiagnostic', () => {
    it('should project to LSP diagnostic (error)', () => {
      const issue = {
        message: 'Variable not defined',
        severity: 'error',
        range: {
          start: { line: 5, character: 10 },
          end: { line: 5, character: 15 },
        },
        code: 'E001',
      };

      const diagnostic = projectToDiagnostic(issue);

      expect(diagnostic.message).toBe('Variable not defined');
      expect(diagnostic.severity).toBe(1); // Error
      expect(diagnostic.code).toBe('E001');
      expect(diagnostic.source).toBe('kgc-runtime');
    });

    it('should handle warning severity', () => {
      const issue = {
        message: 'Unused variable',
        severity: 'warning',
        range: {
          start: { line: 0, character: 0 },
          end: { line: 0, character: 1 },
        },
      };

      const diagnostic = projectToDiagnostic(issue);

      expect(diagnostic.severity).toBe(2); // Warning
    });
  });

  describe('projectToSymbol', () => {
    it('should project to LSP document symbol', () => {
      const symbolInfo = {
        name: 'calculateSum',
        kind: 'function',
        containerName: 'math.utils',
        location: {
          uri: 'file:///project/math.mjs',
          range: {
            start: { line: 10, character: 0 },
            end: { line: 15, character: 1 },
          },
        },
      };

      const symbol = projectToSymbol(symbolInfo);

      expect(symbol.name).toBe('calculateSum');
      expect(symbol.kind).toBe(12); // Function
      expect(symbol.containerName).toBe('math.utils');
    });
  });

  describe('projectToCodeAction', () => {
    it('should project to LSP code action', () => {
      const actionInfo = {
        title: 'Fix import statement',
        kind: 'quickfix',
        edit: {
          changes: {
            'file:///project/main.mjs': [
              {
                range: { start: { line: 0, character: 0 }, end: { line: 0, character: 10 } },
                newText: 'import { x } from "./utils.mjs"',
              },
            ],
          },
        },
        isPreferred: true,
      };

      const codeAction = projectToCodeAction(actionInfo);

      expect(codeAction.title).toBe('Fix import statement');
      expect(codeAction.kind).toBe('quickfix');
      expect(codeAction.isPreferred).toBe(true);
    });
  });

  describe('projectToCodeLens', () => {
    it('should project work item to code lens', () => {
      const workItem = {
        id: 'item-001',
        goal: 'Run tests',
      };

      const range = {
        start: { line: 5, character: 0 },
        end: { line: 5, character: 20 },
      };

      const codeLens = projectToCodeLens(workItem, range);

      expect(codeLens.range).toEqual(range);
      expect(codeLens.command.title).toContain('Run tests');
      expect(codeLens.command.command).toBe('kgc.executeWorkItem');
      expect(codeLens.command.arguments).toEqual(['item-001']);
    });
  });

  describe('projectToSignatureHelp', () => {
    it('should project to LSP signature help', () => {
      const fnInfo = {
        name: 'processData',
        documentation: 'Processes data with options',
        parameters: [
          { name: 'data', type: 'any', description: 'Data to process' },
          { name: 'options', type: 'object', description: 'Processing options' },
        ],
        returns: { type: 'Promise<any>', description: 'Processed data' },
      };

      const projection = projectToSignatureHelp(fnInfo, 1);

      expect(projection.type).toBe('ide');
      expect(projection.format).toBe('lsp');
      expect(projection.content.signatures).toHaveLength(1);
      expect(projection.content.activeParameter).toBe(1);
      expect(projection.content.signatures[0].parameters).toHaveLength(2);
    });
  });

  describe('projectToSemanticTokens', () => {
    it('should project to LSP semantic tokens', () => {
      const tokens = [
        { line: 0, char: 0, length: 6, tokenType: 'function' },
        { line: 0, char: 7, length: 4, tokenType: 'variable' },
        { line: 1, char: 2, length: 5, tokenType: 'class', modifiers: ['declaration'] },
      ];

      const semanticTokens = projectToSemanticTokens(tokens);

      expect(Array.isArray(semanticTokens.data)).toBe(true);
      expect(semanticTokens.data.length).toBeGreaterThan(0);
      // Delta encoding: [deltaLine, deltaChar, length, typeIndex, modifierBits]
      expect(semanticTokens.data.length % 5).toBe(0);
    });
  });
});
