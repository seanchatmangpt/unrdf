/**
 * @fileoverview Π_ide - IDE Metadata Projections
 * Transforms KGC structures into LSP-compatible metadata for IDE integration
 */

import { z } from 'zod';

/**
 * IDE projection schema (LSP-compatible)
 */
export const IDEProjectionSchema = z.object({
  type: z.literal('ide'),
  format: z.enum(['lsp', 'hover', 'completion', 'definition']),
  content: z.record(z.any()),
  uri: z.string().optional(),
  range: z.object({
    start: z.object({ line: z.number(), character: z.number() }),
    end: z.object({ line: z.number(), character: z.number() }),
  }).optional(),
});

/**
 * @typedef {z.infer<typeof IDEProjectionSchema>} IDEProjection
 */

/**
 * LSP Position
 * @typedef {{line: number, character: number}} Position
 */

/**
 * LSP Range
 * @typedef {{start: Position, end: Position}} Range
 */

/**
 * Project function to LSP hover information
 * @param {object} fnInfo - Function information
 * @param {string} fnInfo.name - Function name
 * @param {string} fnInfo.signature - Function signature
 * @param {string} fnInfo.documentation - Documentation text
 * @param {Array<{name: string, type: string, description: string}>} fnInfo.parameters - Parameters
 * @param {object} fnInfo.returns - Return type info
 * @returns {IDEProjection} IDE projection
 */
export function projectFunctionToHover(fnInfo) {
  const paramDocs = fnInfo.parameters.map(p =>
    `@param {${p.type}} ${p.name} - ${p.description}`
  ).join('\n');

  const returnDoc = `@returns {${fnInfo.returns.type}} ${fnInfo.returns.description}`;

  const markdown = [
    `\`\`\`javascript`,
    fnInfo.signature,
    `\`\`\``,
    '',
    fnInfo.documentation,
    '',
    paramDocs,
    returnDoc,
  ].join('\n');

  return IDEProjectionSchema.parse({
    type: 'ide',
    format: 'hover',
    content: {
      kind: 'markdown',
      value: markdown,
    },
  });
}

/**
 * Project schema to LSP completion items
 * @param {string} schemaName - Schema name
 * @param {object} fields - Schema fields
 * @returns {IDEProjection} IDE projection with completions
 */
export function projectSchemaToCompletions(schemaName, fields) {
  const completionItems = Object.entries(fields).map(([key, info]) => ({
    label: key,
    kind: 10, // LSP CompletionItemKind.Property
    detail: info.type,
    documentation: {
      kind: 'markdown',
      value: info.description || `${key} field of ${schemaName}`,
    },
    insertText: key,
    insertTextFormat: 1, // PlainText
  }));

  return IDEProjectionSchema.parse({
    type: 'ide',
    format: 'completion',
    content: {
      isIncomplete: false,
      items: completionItems,
    },
  });
}

/**
 * Project function to LSP definition location
 * @param {object} location - Source location
 * @param {string} location.uri - File URI
 * @param {Range} location.range - Location range
 * @returns {IDEProjection} IDE projection
 */
export function projectToDefinition(location) {
  return IDEProjectionSchema.parse({
    type: 'ide',
    format: 'definition',
    content: {
      uri: location.uri,
      range: location.range,
    },
    uri: location.uri,
    range: location.range,
  });
}

/**
 * Project receipt to LSP diagnostic
 * @param {object} issue - Issue information
 * @param {string} issue.message - Error message
 * @param {string} issue.severity - Severity level (error, warning, info)
 * @param {Range} issue.range - Error range
 * @param {string} [issue.code] - Error code
 * @returns {object} LSP Diagnostic
 */
export function projectToDiagnostic(issue) {
  const severityMap = {
    error: 1,
    warning: 2,
    info: 3,
    hint: 4,
  };

  return {
    range: issue.range,
    severity: severityMap[issue.severity] || 1,
    code: issue.code,
    message: issue.message,
    source: 'kgc-runtime',
  };
}

/**
 * Generate LSP symbol information for workspace indexing
 * @param {object} symbolInfo - Symbol information
 * @param {string} symbolInfo.name - Symbol name
 * @param {string} symbolInfo.kind - Symbol kind (function, class, variable, etc.)
 * @param {string} symbolInfo.containerName - Container name
 * @param {object} symbolInfo.location - Symbol location
 * @returns {object} LSP DocumentSymbol
 */
export function projectToSymbol(symbolInfo) {
  const kindMap = {
    function: 12,
    class: 5,
    interface: 11,
    variable: 13,
    constant: 14,
    property: 7,
    method: 6,
  };

  return {
    name: symbolInfo.name,
    kind: kindMap[symbolInfo.kind] || 13,
    location: symbolInfo.location,
    containerName: symbolInfo.containerName,
  };
}

/**
 * Generate code action (quick fix) for common issues
 * @param {object} actionInfo - Action information
 * @param {string} actionInfo.title - Action title
 * @param {string} actionInfo.kind - Action kind (quickfix, refactor, etc.)
 * @param {object} actionInfo.edit - Workspace edit
 * @returns {object} LSP CodeAction
 */
export function projectToCodeAction(actionInfo) {
  return {
    title: actionInfo.title,
    kind: actionInfo.kind || 'quickfix',
    edit: actionInfo.edit,
    isPreferred: actionInfo.isPreferred || false,
  };
}

/**
 * Project work item to LSP code lens
 * @param {object} workItem - Work item
 * @param {Range} range - Code range
 * @returns {object} LSP CodeLens
 */
export function projectToCodeLens(workItem, range) {
  return {
    range,
    command: {
      title: `▶ ${workItem.goal}`,
      command: 'kgc.executeWorkItem',
      arguments: [workItem.id],
    },
  };
}

/**
 * Generate signature help for function calls
 * @param {object} fnInfo - Function information
 * @param {number} activeParameter - Active parameter index
 * @returns {IDEProjection} IDE projection with signature help
 */
export function projectToSignatureHelp(fnInfo, activeParameter = 0) {
  const parameters = fnInfo.parameters.map(p => ({
    label: `${p.name}: ${p.type}`,
    documentation: {
      kind: 'markdown',
      value: p.description,
    },
  }));

  const signature = {
    label: `${fnInfo.name}(${fnInfo.parameters.map(p => `${p.name}: ${p.type}`).join(', ')}) => ${fnInfo.returns.type}`,
    documentation: {
      kind: 'markdown',
      value: fnInfo.documentation,
    },
    parameters,
  };

  return IDEProjectionSchema.parse({
    type: 'ide',
    format: 'lsp',
    content: {
      signatures: [signature],
      activeSignature: 0,
      activeParameter,
    },
  });
}

/**
 * Generate semantic tokens for syntax highlighting
 * @param {Array<{line: number, char: number, length: number, tokenType: string, modifiers?: string[]}>} tokens - Token information
 * @returns {object} LSP SemanticTokens
 */
export function projectToSemanticTokens(tokens) {
  const tokenTypes = ['namespace', 'class', 'function', 'variable', 'property', 'parameter'];
  const tokenModifiers = ['declaration', 'readonly', 'static', 'async'];

  // Encode tokens as per LSP specification (delta encoding)
  const data = [];
  let prevLine = 0;
  let prevChar = 0;

  for (const token of tokens) {
    const deltaLine = token.line - prevLine;
    const deltaChar = deltaLine === 0 ? token.char - prevChar : token.char;
    const typeIndex = tokenTypes.indexOf(token.tokenType);
    const modifierBits = (token.modifiers || []).reduce(
      (acc, mod) => acc | (1 << tokenModifiers.indexOf(mod)),
      0
    );

    data.push(deltaLine, deltaChar, token.length, typeIndex, modifierBits);

    prevLine = token.line;
    prevChar = token.char;
  }

  return {
    data,
  };
}
